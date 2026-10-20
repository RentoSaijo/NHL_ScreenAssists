# Report Helpers ---------------------------------------------------------

# Locate Quarto without depending on one editor installation.
quarto_path <- function() {
  candidates <- base::c(
    base::Sys.getenv('QUARTO'), base::Sys.which('quarto'),
    '/Applications/Positron.app/Contents/Resources/app/quarto/bin/quarto',
    '/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto'
  )
  candidates <- candidates[base::nzchar(candidates) & base::file.exists(candidates)]
  if (!base::length(candidates)) base::stop('Install Quarto or set QUARTO to its executable.')
  candidates[1L]
}

# Render paper from public artifacts and copy finished formats.
render_report <- function() {
  status <- base::system2(quarto_path(), base::c('render', 'reports/paper.qmd', '--to', 'all'))
  if (status != 0L) base::stop('Quarto rendering failed.')
  base::dir.create('docs/reports', recursive = TRUE, showWarnings = FALSE)
  for (extension in base::c('html', 'pdf')) {
    source <- if (extension == 'html') 'reports/_output/index.html' else 'reports/_output/paper.pdf'
    if (!base::file.exists(source)) base::stop('Expected paper output is absent: ', source)
    for (directory in base::c('reports', 'docs/reports')) {
      base::file.copy(source, base::file.path(directory, base::paste0('paper.', extension)),
                      overwrite = TRUE)
    }
  }
  base::invisible(TRUE)
}

# Public Reconstruction -------------------------------------------------

# Rebuild ranking arithmetic and figures from distributed public files.
rebuild_public <- function() {
  snapshot <- read_public_json('rankings')
  exposure <- utils::read.csv('docs/data/player-games.csv')
  credits  <- utils::read.csv('docs/data/screen-assists.csv')
  totals <- exposure |>
    dplyr::group_by(.data$player, .data$season, .data$type, .data$team) |>
    dplyr::summarise(games = dplyr::n_distinct(.data$game),
                     seconds = base::sum(.data$seconds), .groups = 'drop')
  counts <- credits |>
    dplyr::group_by(.data$player, .data$season, .data$type, .data$team) |>
    dplyr::summarise(conservative = base::sum(.data$conservative),
                     broader = base::sum(.data$broader), .groups = 'drop')
  snapshot$rows <- totals |>
    dplyr::left_join(counts, by = base::c('player', 'season', 'type', 'team')) |>
    dplyr::mutate(conservative = dplyr::coalesce(.data$conservative, 0L),
                  broader = dplyr::coalesce(.data$broader, 0L))
  base::stopifnot(base::sum(snapshot$rows$conservative) == base::sum(credits$conservative),
                  base::sum(snapshot$rows$broader) == base::nrow(credits),
                  base::all(snapshot$rows$conservative <= snapshot$rows$broader))
  write_json(snapshot, 'docs/data/rankings.json')
  utils::write.csv(snapshot$rows, 'docs/data/rankings.csv', row.names = FALSE, na = '')
  index <- read_public_json('goals')
  key   <- index$examples[['20252026']]
  shard <- jsonlite::read_json(base::file.path('docs/data', index$events[[key]]$file))
  clip  <- shard[[key]]
  clip$time <- base::unlist(clip$time)
  matrix <- function(x) {
    base::do.call(base::rbind, base::lapply(x, function(row) {
      base::vapply(row, function(value) if (base::is.null(value)) NA_real_ else value,
                    base::numeric(1))
    }))
  }
  clip$puck <- matrix(clip$puck)
  clip$players <- base::lapply(clip$players, function(player) {
    player$xy <- matrix(player$xy)
    player
  })
  build_figures(snapshot$rows, snapshot$players, read_public_json('validation'),
                 read_public_json('outcome'), clip)
  render_report()
}
