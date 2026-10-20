# Publication Helpers ----------------------------------------------------

# Read public JSON artifact.
read_public_json <- function(name) {
  jsonlite::read_json(base::file.path('docs/data', base::paste0(name, '.json')),
                      simplifyVector = TRUE)
}

# Save figure for website and manuscript.
save_figure <- function(plot, name, width = 7, height = 4.2) {
  base::dir.create('docs/figures', showWarnings = FALSE)
  ggplot2::ggsave(base::paste0('docs/figures/', name, '.svg'), plot,
                  device = grDevices::svg, width = width, height = height, bg = 'white')
  ggplot2::ggsave(base::paste0('docs/figures/', name, '.png'), plot,
                  width = width, height = height, dpi = 300, bg = 'white')
}

# Apply consistent grid-free figure style.
research_theme <- function() {
  ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(text = ggplot2::element_text(colour = '#142c3f'),
                    axis.title = ggplot2::element_text(size = 10),
                    legend.position = 'top', legend.title = ggplot2::element_blank(),
                    plot.title = ggplot2::element_text(face = 'bold', size = 13),
                    plot.subtitle = ggplot2::element_text(size = 10, colour = '#596a75'),
                    plot.margin = ggplot2::margin(12, 15, 8, 8))
}

# Build research figures from public aggregates.
build_figures <- function(rankings, players, validation, outcome, example) {
  leaders <- rankings |>
    dplyr::filter(.data$season == 20252026L, .data$type == 2L) |>
    dplyr::group_by(.data$player) |>
    dplyr::summarise(conservative = base::sum(.data$conservative),
                     broader = base::sum(.data$broader), .groups = 'drop') |>
    dplyr::left_join(dplyr::select(players, 'id', 'name'), by = base::c('player' = 'id')) |>
    dplyr::arrange(dplyr::desc(.data$conservative), dplyr::desc(.data$broader), .data$name) |>
    dplyr::slice_head(n = 12) |>
    dplyr::mutate(name = base::factor(.data$name, levels = base::rev(.data$name)))
  plot <- ggplot2::ggplot(leaders, ggplot2::aes(y = .data$name)) +
    ggplot2::geom_col(ggplot2::aes(x = .data$broader, fill = 'Broader'), width = 0.7) +
    ggplot2::geom_col(ggplot2::aes(x = .data$conservative, fill = 'Conservative'), width = 0.4) +
    ggplot2::scale_fill_manual(values = base::c(Conservative = '#153e56', Broader = '#a5d8ce')) +
    ggplot2::labs(x = 'Estimated screen assists', y = NULL,
                   title = 'Screen-assist leaders',
                   subtitle = '2025–26 regular season; ordering uses conservative credits') +
    research_theme()
  save_figure(plot, 'leaders', height = 5)
  final <- validation$metrics[['20252026']]
  performance <- base::list()
  for (definition in base::c('conservative', 'broader')) {
    for (level in base::c('player', 'event')) {
      m <- final[[definition]][[level]]
      for (metric in base::c('precision', 'recall')) {
        performance[[base::length(performance) + 1L]] <- base::data.frame(
          definition = definition, level = level, metric = metric,
          estimate = m[[metric]], lower = m[[base::paste0(metric, '_low')]],
          upper = m[[base::paste0(metric, '_high')]]
        )
      }
    }
  }
  performance <- dplyr::bind_rows(performance)
  plot <- ggplot2::ggplot(performance,
                          ggplot2::aes(x = .data$estimate, y = .data$metric,
                                        colour = .data$definition)) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
                            orientation = 'y', width = 0.15,
                            position = ggplot2::position_dodge(width = 0.4)) +
    ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 0.4)) +
    ggplot2::facet_wrap(~ level, labeller = ggplot2::as_labeller(
      base::c(event = 'Goal-level detection', player = 'Player attribution'))) +
    ggplot2::scale_x_continuous(limits = base::c(0, 1), labels = scales::label_percent()) +
    ggplot2::scale_colour_manual(values = base::c(conservative = '#153e56', broader = '#008879'),
                                  labels = base::c('Broader', 'Conservative')) +
    ggplot2::labs(x = NULL, y = NULL, title = 'Performance on the reserved 2025–26 season',
                   subtitle = 'Intervals account for observations clustered within games') +
    research_theme()
  save_figure(plot, 'validation', height = 3.4)
  curve <- validation$curves[['20252026']]
  plot <- ggplot2::ggplot(curve, ggplot2::aes(x = .data$recall, y = .data$precision)) +
    ggplot2::geom_line(colour = '#153e56', linewidth = 0.8) +
    ggplot2::scale_x_continuous(limits = base::c(0, 1), labels = scales::label_percent()) +
    ggplot2::scale_y_continuous(limits = base::c(0, 1), labels = scales::label_percent()) +
    ggplot2::labs(x = 'Recall', y = 'Precision', title = 'Player-level precision and recall') +
    research_theme()
  save_figure(plot, 'precision-recall')
  risk <- dplyr::bind_rows(
    base::data.frame(group = 'Offensive screen', base::as.data.frame(outcome$adjusted$screened_rate)),
    base::data.frame(group = 'Clear sight', base::as.data.frame(outcome$adjusted$clear_rate))
  )
  plot <- ggplot2::ggplot(risk, ggplot2::aes(x = .data$estimate, y = .data$group)) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
                            orientation = 'y', width = 0.15, colour = '#153e56') +
    ggplot2::geom_point(size = 4, colour = '#008879') +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 0.1),
                                  limits = base::c(0, base::max(risk$upper) * 1.15)) +
    ggplot2::labs(x = 'Standardized scoring probability', y = NULL,
                   title = 'Scoring with and without an offensive screen',
                   subtitle = 'CSA 2024–25 and 2025–26; adjusted association with 95% intervals') +
    research_theme()
  save_figure(plot, 'scoring', height = 3.2)
  if (!base::is.null(final$calibration)) {
    bins <- final$calibration$bins
    plot <- ggplot2::ggplot(bins, ggplot2::aes(x = .data$predicted, y = .data$observed)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 'dashed', colour = '#9da9b0') +
      ggplot2::geom_line(colour = '#153e56') +
      ggplot2::geom_point(ggplot2::aes(size = .data$n), colour = '#008879') +
      ggplot2::scale_x_continuous(limits = base::c(0, 1), labels = scales::label_percent()) +
      ggplot2::scale_y_continuous(limits = base::c(0, 1), labels = scales::label_percent()) +
      ggplot2::labs(x = 'Mean predicted probability', y = 'Observed participant frequency',
                     title = 'Calibration within the reference benchmark') + research_theme()
    save_figure(plot, 'calibration')
  }
  frame <- base::which.min(base::abs(example$time))
  points <- dplyr::bind_rows(base::lapply(example$players, function(p) {
    base::data.frame(id = p$id, team = p$team, x = p$xy[frame, 1], y = p$xy[frame, 2])
  }))
  points$role <- base::ifelse(points$id == example$goalie, 'Goalie',
                              base::ifelse(points$team == example$team, 'Attacker', 'Defender'))
  eye <- points[points$id == example$goalie, ]
  puck <- example$puck[frame, ]
  origin <- base::c(eye$x, eye$y)
  vector <- puck - origin
  normal <- base::c(-vector[2], vector[1]) / base::sqrt(base::sum(vector^2)) * 1.5
  band <- base::rbind(origin + normal, puck + normal, puck - normal, origin - normal)
  plot <- ggplot2::ggplot(points, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::annotate('rect', xmin = 25, xmax = 100, ymin = -42.5, ymax = 42.5,
                       fill = '#f7fafb', colour = '#aab8c0') +
    ggplot2::annotate('segment', x = 89, xend = 89, y = -42.5, yend = 42.5,
                       colour = '#d4a5a2') +
    ggplot2::annotate('rect', xmin = 89, xmax = 93, ymin = -3, ymax = 3,
                       fill = 'white', colour = '#c56b66') +
    ggplot2::annotate('polygon', x = band[, 1], y = band[, 2],
                       fill = '#008879', alpha = 0.13) +
    ggplot2::annotate('segment', x = eye$x, y = eye$y, xend = puck[1], yend = puck[2],
                       colour = '#008879', linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$role), size = 3) +
    ggplot2::annotate('point', x = puck[1], y = puck[2], size = 2, colour = 'black') +
    ggplot2::scale_colour_manual(values = base::c(Attacker = '#008879', Defender = '#8b9aa4',
                                                  Goalie = '#153e56')) +
    ggplot2::coord_fixed(xlim = base::c(25, 100), ylim = base::c(-42.5, 42.5)) +
    ggplot2::labs(x = 'Feet toward attacking goal', y = 'Feet across rink',
                   title = 'Goalie-to-puck geometry\nat estimated release',
                   subtitle = 'Tracked sensors; 1.50 ft sightline tolerance') +
    research_theme()
  save_figure(plot, 'geometry', width = 5.5, height = 5.5)
}

# Public Export ----------------------------------------------------------

# Build public rankings, replay explorer data, and figures.
publish_results <- function() {
  model      <- read_public_json('model')
  validation <- read_public_json('validation')
  outcome    <- read_public_json('outcome')
  features   <- read_features()
  events     <- read_features('events')
  goals      <- base::readRDS('data/goals.rds')
  exposure   <- base::readRDS('data/exposure.rds')
  games      <- base::readRDS('data/games_selected.rds')
  registry   <- base::readRDS('data/players.rds')
  teams      <- base::readRDS('data/teams.rds')
  goals$key  <- base::paste(goals$gameId, goals$eventId, sep = '_')
  features$score <- predict_detector(features, model)
  credits <- features |>
    dplyr::filter(.data$score >= model$thresholds$broader) |>
    dplyr::transmute(key = .data$key, player = .data$player_id, team = .data$team_id,
                     season = .data$season, score = .data$score,
                     conservative = base::as.integer(.data$score >= model$thresholds$conservative),
                     broader = 1L) |>
    dplyr::left_join(dplyr::select(goals, 'key', 'gameTypeId', 'scoringPlayerId'), by = 'key') |>
    dplyr::rename(type = 'gameTypeId')
  base::stopifnot(!base::anyDuplicated(base::paste(credits$key, credits$player)),
                  base::all(credits$conservative <= credits$broader),
                  base::all(credits$player != credits$scoringPlayerId))
  exposure <- exposure |>
    dplyr::left_join(dplyr::select(games, 'gameId', 'homeTeamId', 'visitingTeamId'), by = 'gameId') |>
    dplyr::mutate(team_id = base::ifelse(.data$homeRoad == 'H', .data$homeTeamId,
                                        .data$visitingTeamId))
  exposure_rows <- exposure |>
    dplyr::transmute(player = .data$playerId, game = .data$gameId, season = .data$seasonId,
                     type = .data$gameTypeId, team = .data$team_id,
                     seconds = .data$timeOnIce, date = .data$gameDate)
  totals <- exposure_rows |>
    dplyr::group_by(.data$player, .data$season, .data$type, .data$team) |>
    dplyr::summarise(games = dplyr::n_distinct(.data$game),
                     seconds = base::sum(.data$seconds), .groups = 'drop')
  counts <- credits |>
    dplyr::group_by(.data$player, .data$season, .data$type, .data$team) |>
    dplyr::summarise(conservative = base::sum(.data$conservative),
                     broader = dplyr::n(), .groups = 'drop')
  unmatched <- dplyr::anti_join(counts, totals, by = base::c('player', 'season', 'type', 'team'))
  if (base::nrow(unmatched)) base::stop('Credits lack matching official exposure.')
  rankings <- totals |>
    dplyr::left_join(counts, by = base::c('player', 'season', 'type', 'team')) |>
    dplyr::mutate(conservative = dplyr::coalesce(.data$conservative, 0L),
                  broader = dplyr::coalesce(.data$broader, 0L))
  quality <- events |>
    dplyr::count(.data$season, .data$status, name = 'goals')
  used_ids <- base::unique(base::c(exposure_rows$player, goals$scoringPlayerId, goals$goalieInNetId))
  players <- registry |>
    dplyr::filter(.data$playerId %in% used_ids) |>
    dplyr::transmute(id = .data$playerId, name = .data$playerFullName, position = .data$positionCode)
  missing_names <- exposure |>
    dplyr::filter(!.data$playerId %in% players$id) |>
    dplyr::distinct(.data$playerId, .keep_all = TRUE) |>
    dplyr::transmute(id = .data$playerId, name = .data$skaterFullName, position = .data$positionCode)
  players <- dplyr::bind_rows(players, missing_names)
  team_rows <- teams |>
    dplyr::filter(.data$teamId %in% exposure_rows$team) |>
    dplyr::transmute(id = .data$teamId, code = .data$teamTriCode, name = .data$teamFullName)
  snapshot <- base::list(
    cutoff = base::as.character(settings()$cutoff),
    goals = base::nrow(goals), evaluated = base::sum(events$status == 'evaluated'),
    quality = quality, players = players, teams = team_rows, rows = rankings,
    model = base::list(kind = model$kind, thresholds = model$thresholds),
    test_performance = validation$metrics[['20252026']]
  )
  write_json(snapshot, 'docs/data/rankings.json')
  utils::write.csv(exposure_rows, 'docs/data/player-games.csv', row.names = FALSE, na = '')
  utils::write.csv(dplyr::select(credits, -'scoringPlayerId'), 'docs/data/screen-assists.csv',
                   row.names = FALSE, na = '')
  utils::write.csv(rankings, 'docs/data/rankings.csv', row.names = FALSE, na = '')
  utils::write.csv(dplyr::select(events, 'key', 'game_id', 'event_id', 'season', 'status'),
                   'docs/data/goal-status.csv', row.names = FALSE, na = '')
  index <- base::list()
  selected_example <- NULL
  examples <- base::list()
  base::dir.create('docs/data/replays', recursive = TRUE, showWarnings = FALSE)
  base::unlink(base::list.files('docs/data/replays', pattern = '^[0-9]{8}-[0-9]{3}\\.json$',
                                full.names = TRUE))
  for (season in settings()$seasons) {
    clips <- base::readRDS(base::sprintf('data/clips_%s.rds', season))
    keys <- base::unique(base::c(credits$key[credits$season == season],
                                base::intersect(base::names(clips),
                                                base::c('2023030417_866', '2024020151_172'))))
    chunks <- base::split(keys, (base::seq_along(keys) - 1L) %/% 80L)
    for (j in base::seq_along(chunks)) {
      file <- base::sprintf('replays/%s-%03d.json', season, j)
      write_json(clips[chunks[[j]]], base::file.path('docs/data', file))
      for (key in chunks[[j]]) {
        g <- goals[goals$key == key, , drop = FALSE]
        c <- credits[credits$key == key, , drop = FALSE]
        index[[key]] <- base::list(
          key = key, game = g$gameId, event = g$eventId, season = g$seasonId,
          type = g$gameTypeId, date = g$gameDate, home = g$homeTeamId, away = g$visitingTeamId,
          team = g$eventOwnerTeamId, scorer = g$scoringPlayerId, shot = g$shotType,
          period = g$periodNumber, clock = g$secondsElapsedInPeriod,
          replay_url = base::sprintf('https://www.nhl.com/ppt-replay/goal/%s/%s', g$gameId, g$eventId),
          video_url = if ('highlightClipSharingUrl' %in% base::names(g)) g$highlightClipSharingUrl else '',
          file = file, credits = dplyr::select(c, 'player', 'conservative', 'broader', 'score')
        )
      }
    }
    season_credits <- credits[credits$season == season, , drop = FALSE]
    if (base::nrow(season_credits)) {
      strong <- season_credits$key[base::which.max(season_credits$score)]
      examples[[base::as.character(season)]] <- strong
      if (season == 20252026L) selected_example <- clips[[strong]]
    }
    base::rm(clips)
    base::gc()
  }
  write_json(base::list(events = index, examples = examples), 'docs/data/goals.json')
  build_figures(rankings, players, validation, outcome, selected_example)
  base::message('Published ', base::nrow(credits), ' broader credits across ',
                base::length(base::unique(credits$key)), ' goals.')
  base::invisible(snapshot)
}
