# Private Reference Helpers ---------------------------------------------

# Open read-only research connection.
connect_csa <- function() {
  required <- base::c('CSA_SERVER', 'CSA_USERNAME', 'CSA_PASSWORD')
  if (!base::all(base::nzchar(base::Sys.getenv(required)))) {
    base::stop('CSA environment credentials are unavailable.')
  }
  base::tryCatch(
    DBI::dbConnect(odbc::odbc(), Driver = 'ODBC Driver 18 for SQL Server',
                   Server = base::Sys.getenv('CSA_SERVER'), Database = 'CSAHockeyStats',
                   UID = base::Sys.getenv('CSA_USERNAME'), PWD = base::Sys.getenv('CSA_PASSWORD'),
                   Encrypt = 'yes', TrustServerCertificate = 'yes', timeout = 10),
    error = function(e) base::stop('CSA connection could not be established.', call. = FALSE)
  )
}

# Read bounded query with connection retries.
query_csa <- function(sql) {
  for (attempt in base::seq_len(3L)) {
    result <- base::tryCatch({
      con <- connect_csa()
      base::tryCatch(DBI::dbGetQuery(con, sql),
                     finally = DBI::dbDisconnect(con))
    }, error = function(e) NULL)
    if (!base::is.null(result)) return(result)
    base::Sys.sleep(attempt * 2)
  }
  base::stop('CSA read failed after three connection attempts.', call. = FALSE)
}

# Read scoped CSA events without caching records.
read_csa <- function(goals_only = TRUE) {
  seasons <- if (goals_only) '2023,2024,2025' else '2024,2025'
  outcome <- if (goals_only) ' AND s.Result = 8' else ''
  sql <- base::paste0(
    'SELECT TOP (10000) s.ShotID AS private_id, g.Season AS season_start, g.NHLGameID AS game_code,
     g.SeasonSubset AS game_type, s.NhlApiEventId AS api_event_id,
     s.Period AS period, s.TimeElapsed AS elapsed, s.ShotType AS shot_type,
     s.ScreenOnPass AS screen_on_pass, s.Result AS result, s.Post AS post,
     s.ReleaseType AS release_type, s.PreciseShotLocation AS zone,
     s.PrecisePassLocation AS pass_zone, s.PrecisePass2Location AS second_pass_zone,
     s.MainSituation AS context, s.PowerPlay AS strength,
     s.BrokenPlay AS broken_play, s.NoShot AS no_shot,
     p.NHLPlayerID AS shooter_id, q.NHLPlayerID AS goalie_id,
     ts.NHLTeamNum AS shooting_team, td.NHLTeamNum AS defending_team,
     th.NHLTeamNum AS home_team, tv.NHLTeamNum AS away_team
     FROM dbo.Shot s JOIN dbo.Game g ON g.GameID = s.GameID
     LEFT JOIN dbo.Player p ON p.PlayerID = s.Shooter
     LEFT JOIN dbo.Player q ON q.PlayerID = s.Goalie
     LEFT JOIN dbo.Team ts ON ts.TeamID = s.TeamShooting
     LEFT JOIN dbo.Team td ON td.TeamID = s.TeamDefending
     LEFT JOIN dbo.Team th ON th.TeamID = g.HomeTeam
     LEFT JOIN dbo.Team tv ON tv.TeamID = g.VistingTeam
     WHERE g.Season IN (', seasons, ')
       AND g.SeasonSubset IN (2,3) AND g.GameReviewComplete = 1
       AND s.QCStatus = 5 AND s.NoShot = 0 AND s.ShotType <> 27
       AND s.Goalie > 0 AND q.NHLPlayerID > 0
       AND LEN(g.NHLGameID) = 6 AND g.NHLGameID NOT LIKE \'%[^0-9]%\'
       AND s.Result BETWEEN 1 AND 10', outcome
  )
  pages <- base::list()
  last_id <- 0
  repeat {
    page <- query_csa(base::paste0(sql, ' AND s.ShotID > ', last_id, ' ORDER BY s.ShotID'))
    if (!base::nrow(page)) break
    last_id <- base::max(base::as.numeric(page$private_id))
    pages[[base::length(pages) + 1L]] <- page
    if (base::nrow(page) < 10000L) break
  }
  shots <- dplyr::bind_rows(pages)
  shots$private_id <- base::as.character(shots$private_id)
  shots$game_id <- shots$season_start * 1000000 + base::as.integer(shots$game_code)
  participants <- NULL
  if (goals_only) {
    participants <- query_csa(
      'SELECT sp.ShotID AS private_id, p.NHLPlayerID AS player_id, sp.OffenseDefense AS side
       FROM dbo.ScreenPlayer sp
       JOIN dbo.Player p ON p.PlayerID = sp.PlayerID
       JOIN dbo.Shot s ON s.ShotID = sp.ShotID
       JOIN dbo.Game g ON g.GameID = s.GameID
       WHERE g.Season IN (2023,2024,2025) AND s.Result = 8
         AND g.GameReviewComplete = 1 AND s.QCStatus = 5
         AND s.NoShot = 0 AND s.ShotType <> 27 AND p.NHLPlayerID > 0')
    participants$private_id <- base::as.character(participants$private_id)
    participants <- dplyr::distinct(participants)
  }
  base::list(shots = shots, participants = participants)
}

# Convert imported elapsed clock to seconds.
clock_seconds <- function(clock) {
  base::vapply(base::strsplit(base::as.character(clock), ':', fixed = TRUE), function(x) {
    x <- base::suppressWarnings(base::as.numeric(x))
    if (base::length(x) < 2L || base::anyNA(x)) return(NA_real_)
    base::sum(base::rev(x) * 60^(base::seq_along(x) - 1L))
  }, base::numeric(1))
}

# Match reviewed goals with explicit context checks.
match_reference <- function(reference, goals) {
  shots <- reference$shots
  shots$clock <- clock_seconds(shots$elapsed)
  public <- goals |>
    dplyr::transmute(key = base::paste(.data$gameId, .data$eventId, sep = '_'),
                     game_id = .data$gameId, event_id = .data$eventId,
                     public_period = .data$periodNumber,
                     public_clock = .data$secondsElapsedInPeriod,
                     public_scorer = .data$scoringPlayerId,
                     public_goalie = .data$goalieInNetId,
                     public_team = .data$eventOwnerTeamId,
                     public_home = .data$homeTeamId, public_away = .data$visitingTeamId)
  candidates <- dplyr::inner_join(shots, public, by = 'game_id', relationship = 'many-to-many') |>
    dplyr::filter(.data$shooter_id == .data$public_scorer,
                  .data$period == .data$public_period,
                  base::abs(.data$clock - .data$public_clock) <= 1,
                  .data$shooting_team == .data$public_team,
                  .data$home_team == .data$public_home,
                  .data$away_team == .data$public_away,
                  .data$goalie_id == .data$public_goalie) |>
    dplyr::mutate(exact_id = !base::is.na(.data$api_event_id) &
                    .data$api_event_id == .data$event_id,
                  exact_clock = .data$clock == .data$public_clock) |>
    dplyr::filter(.data$exact_id | .data$exact_clock) |>
    dplyr::group_by(.data$private_id) |>
    dplyr::filter(.data$exact_id == base::max(.data$exact_id)) |>
    dplyr::filter(dplyr::n() == 1L) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$key) |>
    dplyr::filter(dplyr::n() == 1L) |>
    dplyr::ungroup()
  candidates
}

# Construct label-resolved event and player reference sets.
build_reference <- function(features, events, goals) {
  raw <- read_csa()
  matched <- match_reference(raw, goals)
  offense <- raw$participants |>
    dplyr::filter(.data$side == 'O') |>
    dplyr::inner_join(dplyr::select(matched, 'private_id', 'key', 'shooter_id'),
                      by = 'private_id') |>
    dplyr::filter(.data$player_id != .data$shooter_id) |>
    dplyr::distinct(.data$key, .data$player_id)
  n_offense <- offense |> dplyr::count(.data$key, name = 'n_offense')
  eligible <- matched |>
    dplyr::left_join(n_offense, by = 'key') |>
    dplyr::mutate(n_offense = dplyr::coalesce(.data$n_offense, 0L),
                  positive = .data$shot_type %in% base::c(1L, 13L, 16L, 17L) &
                    .data$n_offense > 0L,
                  negative = .data$shot_type %in% base::c(3L, 5L) &
                    .data$n_offense == 0L) |>
    dplyr::filter(!base::is.na(.data$screen_on_pass), !.data$screen_on_pass,
                  .data$positive | .data$negative) |>
    dplyr::inner_join(dplyr::select(events, 'key', 'status'), by = 'key') |>
    dplyr::filter(.data$status == 'evaluated') |>
    dplyr::transmute(key = .data$key, game_id = .data$game_id,
                     season = .data$season_start * 10000 + .data$season_start + 1,
                     event_label = .data$positive)
  positive <- offense |>
    dplyr::semi_join(eligible, by = 'key') |>
    dplyr::mutate(named_participant = TRUE)
  benchmark <- features |>
    dplyr::inner_join(dplyr::select(eligible, 'key', 'event_label'), by = 'key') |>
    dplyr::left_join(positive, by = base::c('key', 'player_id')) |>
    dplyr::mutate(label = dplyr::coalesce(.data$named_participant, FALSE),
                  available = TRUE)
  missing <- positive |>
    dplyr::anti_join(features, by = base::c('key', 'player_id')) |>
    dplyr::inner_join(eligible, by = 'key') |>
    dplyr::mutate(label = TRUE, available = FALSE)
  benchmark <- dplyr::bind_rows(benchmark, missing)
  summary <- matched |>
    dplyr::count(.data$season_start, name = 'matched_goals') |>
    dplyr::left_join(eligible |>
                      dplyr::mutate(season_start = .data$season %/% 10000) |>
                      dplyr::group_by(.data$season_start) |>
                      dplyr::summarise(benchmark_goals = dplyr::n(),
                                       screened_goals = base::sum(.data$event_label),
                                       .groups = 'drop'), by = 'season_start')
  base::list(players = benchmark, events = eligible, summary = summary)
}
