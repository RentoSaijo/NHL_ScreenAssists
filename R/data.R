# Settings ---------------------------------------------------------------

# Define research snapshot.
settings <- function() {
  base::list(
    seasons        = base::c(20232024L, 20242025L, 20252026L, 20262027L),
    cutoff         = base::as.Date('2026-10-19'),
    database_ref   = '3106adec4df1933dbef4f8bb6d3675504b7a6fa7',
    package_ref    = '58821f001469d5024766511e511051377c0f859f',
    analysis_seed  = 271828L
  )
}

# File Helpers -----------------------------------------------------------

# Write compact public JSON.
write_json <- function(x, path, pretty = FALSE, digits = 8) {
  base::dir.create(base::dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, auto_unbox = TRUE, na = 'null', null = 'null',
                      digits = digits, dataframe = 'rows', pretty = pretty)
}

# Cache public API response.
public_cache <- function(name, fetch) {
  path <- base::file.path('data', base::paste0(name, '.rds'))
  if (base::file.exists(path)) return(base::readRDS(path))
  x <- fetch()
  if (!base::NROW(x)) base::stop('Public source returned no rows: ', name)
  base::saveRDS(x, path)
  x
}

# Construct immutable public snapshot path.
snapshot_path <- function(season, kind) {
  remote <- if (kind == 'replays') {
    base::sprintf('data/event/replays/NHL_REPLAYS_%s.parquet', season)
  } else {
    base::sprintf('data/game/pbps/gc/NHL_PBPS_GC_%s.parquet', season)
  }
  base::list(
    local = base::file.path('data', base::basename(remote)),
    url   = base::sprintf('https://huggingface.co/datasets/RentoSaijo/NHL_DB/resolve/%s/%s',
                         settings()$database_ref, remote)
  )
}

# Download immutable public parquet.
fetch_snapshot <- function(season, kind) {
  source <- snapshot_path(season, kind)
  if (!base::file.exists(source$local)) {
    base::message('Download ', kind, ' ', season)
    temporary <- base::paste0(source$local, '.part')
    success   <- FALSE
    for (attempt in base::seq_len(3L)) {
      success <- base::tryCatch({
        curl::curl_download(source$url, temporary, quiet = TRUE,
                            handle = curl::new_handle(timeout = 1200, connecttimeout = 30))
        TRUE
      }, error = function(e) FALSE)
      if (success) break
      base::Sys.sleep(attempt * 2)
    }
    if (!success) base::stop('Could not download public snapshot: ', source$url)
    base::file.rename(temporary, source$local)
  }
  source$local
}

# Data Preparation -------------------------------------------------------

# Prepare public inputs and official exposure.
prepare_data <- function() {
  cfg <- settings()
  revision <- utils::packageDescription('nhlscraper')$RemoteSha
  if (!base::identical(revision, cfg$package_ref)) {
    base::stop('Install nhlscraper at commit ', cfg$package_ref,
                '; package versions are recorded in docs/data/provenance.json.')
  }
  base::dir.create('data', showWarnings = FALSE)
  tasks <- base::expand.grid(season = cfg$seasons, kind = base::c('pbp', 'replays'),
                             stringsAsFactors = FALSE)
  downloaded <- parallel::mclapply(base::seq_len(base::nrow(tasks)), function(i) {
    fetch_snapshot(tasks$season[i], tasks$kind[i])
  }, mc.cores = 3L)
  if (base::any(base::vapply(downloaded, base::inherits, base::logical(1), 'try-error'))) {
    base::stop('Public snapshot download failed.')
  }
  games <- public_cache('games', nhlscraper::games)
  games <- games |>
    dplyr::filter(.data$seasonId %in% cfg$seasons, .data$gameTypeId %in% base::c(2L, 3L),
                  base::as.Date(.data$gameDate) <= cfg$cutoff, .data$gameStateId == 7L) |>
    dplyr::distinct(.data$gameId, .keep_all = TRUE)
  base::saveRDS(games, 'data/games_selected.rds')
  players <- public_cache('players', nhlscraper::players)
  teams   <- public_cache('teams', nhlscraper::teams)
  exposure <- base::list()
  goals    <- base::list()
  for (season in cfg$seasons) {
    for (type in base::c(2L, 3L)) {
      eligible <- games$gameId[games$seasonId == season & games$gameTypeId == type]
      if (!base::length(eligible)) next
      x <- public_cache(base::sprintf('exposure_%s_%s', season, type), function() {
        nhlscraper::skater_game_report(season = season, game_type = type, category = 'timeonice')
      })
      x <- x |>
        dplyr::filter(.data$gameId %in% eligible) |>
        dplyr::mutate(seasonId = season, gameTypeId = type) |>
        dplyr::distinct(.data$gameId, .data$playerId, .keep_all = TRUE)
      exposure[[base::length(exposure) + 1L]] <- x
    }
    pbp <- arrow::read_parquet(snapshot_path(season, 'pbp')$local)
    absent <- base::setdiff(games$gameId[games$seasonId == season], base::unique(pbp$gameId))
    if (base::length(absent)) {
      fresh <- base::lapply(absent, function(id) {
        public_cache(base::paste0('pbp_', id), function() nhlscraper::gc_play_by_play(id))
      })
      pbp <- dplyr::bind_rows(base::c(base::list(pbp), fresh))
    }
    pbp <- pbp |>
      dplyr::filter(.data$gameId %in% games$gameId) |>
      dplyr::arrange(.data$gameId, .data$sortOrder)
    base::saveRDS(pbp, base::sprintf('data/pbp_%s.rds', season))
    goals[[base::as.character(season)]] <- pbp |>
      dplyr::filter(.data$eventTypeDescKey == 'goal', .data$periodType != 'SO') |>
      dplyr::distinct(.data$gameId, .data$eventId, .keep_all = TRUE) |>
      dplyr::left_join(dplyr::select(games, 'gameId', 'gameDate', 'homeTeamId',
                                    'visitingTeamId'), by = 'gameId')
  }
  exposure <- dplyr::bind_rows(exposure)
  goals    <- dplyr::bind_rows(goals)
  base::stopifnot(base::nrow(exposure) > 100000L, base::nrow(goals) > 25000L)
  base::saveRDS(exposure, 'data/exposure.rds')
  base::saveRDS(goals, 'data/goals.rds')
  files <- base::unlist(downloaded)
  provenance <- base::list(
    cutoff = base::as.character(cfg$cutoff),
    database_commit = cfg$database_ref,
    nhlscraper_commit = cfg$package_ref,
    nhlscraper_version = base::as.character(utils::packageVersion('nhlscraper')),
    acquired_utc = base::format(base::Sys.time(), tz = 'UTC', usetz = TRUE),
    r_version = R.version.string,
    files = base::as.list(tools::md5sum(files)),
    seasons = cfg$seasons
  )
  packages <- base::c('nhlscraper', 'arrow', 'curl', 'jsonlite', 'dplyr', 'tidyr',
                      'tidyselect', 'ggplot2', 'scales', 'parsnip', 'glmnet', 'DBI',
                      'odbc', 'multilevelmod', 'lme4', 'reformulas', 'knitr', 'rmarkdown')
  provenance$packages <- stats::setNames(base::lapply(packages, function(package) {
    base::as.character(utils::packageVersion(package))
  }), packages)
  provenance$quarto_version <- base::tryCatch(
    base::system2(quarto_path(), '--version', stdout = TRUE),
    error = function(e) NA_character_
  )
  provenance$exposure_records <- base::nrow(exposure)
  provenance$completed_games <- base::nrow(games)
  provenance$public_sources <- base::list(
    schedule = 'NHL schedule via nhlscraper::games',
    exposure = 'NHL skater game time-on-ice report',
    replay = 'NHL replay parquet at pinned NHL_DB commit, supplemented with live completed games through cutoff'
  )
  write_json(provenance, 'docs/data/provenance.json', pretty = TRUE)
  base::message('Prepared ', base::nrow(goals), ' goals and ', base::nrow(exposure),
                ' player-game appearances.')
  base::invisible(provenance)
}
