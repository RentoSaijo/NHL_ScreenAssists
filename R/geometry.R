# Tracking Helpers -------------------------------------------------------

# Recover identities independently of tracking slots.
unpack_tracking <- function(replay) {
  replay <- replay[base::order(replay$timeStamp), , drop = FALSE]
  replay <- replay[!base::duplicated(replay$timeStamp), , drop = FALSE]
  slots <- base::sub('PlayerId$', '', base::grep('^player[0-9]+PlayerId$',
                                               base::names(replay), value = TRUE))
  extract <- function(suffix) {
    base::sapply(slots, function(slot) base::as.numeric(replay[[base::paste0(slot, suffix)]]))
  }
  base::list(
    time = (base::as.numeric(replay$timeStamp) - base::min(replay$timeStamp)) / 10,
    puck = base::cbind(replay$puckXCoord, replay$puckYCoord),
    id = extract('PlayerId'), team = extract('TeamId'),
    x = extract('XCoord'), y = extract('YCoord')
  )
}

# Extract one player's changing slot positions.
player_track <- function(tracking, id) {
  match <- tracking$id == id
  match[base::is.na(match)] <- FALSE
  slot <- base::max.col(match, ties.method = 'first')
  ix   <- base::cbind(base::seq_len(base::nrow(match)), slot)
  out  <- base::cbind(tracking$x[ix], tracking$y[ix])
  out[base::rowSums(match) != 1L, ] <- NA_real_
  out
}

# Infer shot interval from goal-plane crossing and preceding flight.
shot_interval <- function(tracking, goal, goalie) {
  direction <- base::sign(stats::median(goalie[, 1], na.rm = TRUE))
  if (!base::is.finite(direction) || direction == 0) return(NULL)
  puck <- tracking$puck * direction
  time <- tracking$time
  n    <- base::nrow(puck)
  if (n < 8L) return(NULL)
  dx <- base::diff(puck[, 1])
  dy <- base::diff(puck[, 2])
  dt <- base::diff(time)
  speed <- base::sqrt(dx^2 + dy^2) / dt
  crossing <- base::which(puck[-n, 1] < 89 & puck[-1L, 1] >= 89 & dx > 0)
  if (!base::length(crossing)) return(NULL)
  y_cross <- puck[crossing, 2] + (89 - puck[crossing, 1]) *
    dy[crossing] / dx[crossing]
  crossing <- crossing[base::is.finite(y_cross) & base::abs(y_cross) <= 4.5]
  if (!base::length(crossing)) return(NULL)
  scorer <- player_track(tracking, goal$scoringPlayerId) * direction
  tip    <- goal$shotType %in% base::c('tip-in', 'deflected')
  candidates <- base::list()
  for (edge in crossing) {
    before <- base::seq.int(base::max(1L, edge - 7L), edge)
    peak   <- base::max(speed[before], na.rm = TRUE)
    if (!base::is.finite(peak) || peak < 15 || peak > 220) next
    threshold <- base::max(12, base::min(30, peak * 0.30))
    start <- edge
    while (start > 1L && time[edge + 1L] - time[start - 1L] <= 2 &&
           base::is.finite(speed[start - 1L]) && speed[start - 1L] >= threshold &&
           dt[start - 1L] <= 0.21 && dx[start - 1L] >= -0.5) {
      start <- start - 1L
    }
    if (start < 6L || time[start] < 0.5) next
    location <- base::c(goal$xCoord, goal$yCoord) * direction
    location_error <- base::sqrt(base::sum((puck[start, ] - location)^2))
    scorer_error   <- base::sqrt(base::sum((puck[start, ] - scorer[start, ])^2))
    if (tip) {
      near <- base::seq.int(base::max(start, edge - 3L), edge + 1L)
      location_error <- base::min(base::sqrt(base::rowSums(
        base::sweep(puck[near, , drop = FALSE], 2, location)^2)), na.rm = TRUE)
      scorer_error <- base::min(base::sqrt(base::rowSums(
        (puck[near, , drop = FALSE] - scorer[near, , drop = FALSE])^2)), na.rm = TRUE)
    }
    if (!base::is.finite(location_error) || !base::is.finite(scorer_error)) next
    if (location_error > 22 || scorer_error > 12) next
    window <- base::which(time >= time[start] - 0.6 & time <= time[edge + 1L])
    if (base::any(base::diff(time[window]) > 0.21) ||
        base::any(!base::is.finite(puck[window, ])) ||
        base::any(!base::is.finite(goalie[window, ]))) next
    if (base::any(base::abs(puck[window, 1]) > 110) ||
        base::any(base::abs(puck[window, 2]) > 50)) next
    candidates[[base::length(candidates) + 1L]] <- base::list(
      release = start, arrival = edge + 1L, direction = direction,
      cost = location_error / 10 + scorer_error / 5,
      location_error = location_error, scorer_error = scorer_error
    )
  }
  if (!base::length(candidates)) return(NULL)
  order <- base::order(base::vapply(candidates, function(x) x$cost, base::numeric(1)))
  best  <- candidates[[order[1L]]]
  if (base::length(order) > 1L) {
    second <- candidates[[order[2L]]]
    if (second$cost - best$cost < 0.25 &&
        base::abs(time[second$arrival] - time[best$arrival]) > 0.5) return(NULL)
  }
  best
}

# Calculate time-aware sightline features.
sightline_features <- function(puck, goalie, player, time, release, arrival,
                               radius = 1.5, shift = 0) {
  target_time <- time[release] + shift
  pre  <- time >= target_time - 0.5 & time <= target_time + 0.001
  near <- time >= target_time - 0.2 & time <= target_time + 0.101
  flight <- time > target_time & time <= time[arrival] &
    puck[, 1] <= goalie[, 1] + 1
  window <- pre | near | flight
  valid  <- base::rowSums(base::is.finite(player)) == 2L &
    base::rowSums(base::is.finite(goalie)) == 2L &
    base::rowSums(base::is.finite(puck)) == 2L
  if (base::sum(window & valid) < 3L ||
      base::mean(valid[window]) < 0.8) return(NULL)
  v <- puck - goalie
  w <- player - goalie
  length2 <- base::rowSums(v^2)
  along   <- base::rowSums(v * w) / length2
  lateral <- (v[, 1] * w[, 2] - v[, 2] * w[, 1]) / base::sqrt(length2)
  between <- along > 0 & along < 1 & length2 > 4 & valid
  between[base::is.na(between)] <- FALSE
  distance <- base::abs(lateral)
  distance[!between] <- 50
  goalie_distance <- base::sqrt(base::rowSums(w^2))
  sample_dt <- stats::median(base::diff(time))
  pre_weight <- base::pmax(0, base::pmin(time + sample_dt / 2, target_time) -
                            base::pmax(time - sample_dt / 2, target_time - 0.5))
  flight_weight <- base::pmax(0, base::pmin(time + sample_dt / 2, time[arrival]) -
                               base::pmax(time - sample_dt / 2, target_time))
  strength <- base::exp(-0.5 * (distance / radius)^2) /
    (1 + goalie_distance / 20)
  strength[!base::is.finite(strength)] <- 0
  block <- between & distance <= radius
  preceding <- base::which(block & time <= target_time + 0.001 &
                             time >= target_time - 1)
  gap <- if (base::length(preceding)) {
    base::max(0, target_time - base::max(time[preceding]))
  } else 1
  summarize <- function(x, mask, fun, default = 0) {
    values <- x[mask & valid]
    if (!base::length(values)) default else fun(values, na.rm = TRUE)
  }
  run <- base::rle(block[pre])
  runs <- base::rep(base::seq_along(run$lengths), run$lengths)
  run_duration <- base::tapply(pre_weight[pre], runs, base::sum)
  longest <- if (base::any(run$values)) {
    base::max(run_duration[run$values])
  } else 0
  min_ix <- base::which.min(base::ifelse(near, distance, Inf))
  cross <- base::any(base::diff(base::sign(lateral[near & between])) != 0, na.rm = TRUE)
  x <- base::c(
    near_distance = summarize(distance, near, base::min, 50),
    release_distance = distance[release],
    pre_duration = base::sum(pre_weight[block & pre]),
    narrow_duration = base::sum(pre_weight[pre & between & distance <= radius * 0.5]),
    wide_duration = base::sum(pre_weight[pre & between & distance <= radius * 1.5]),
    longest_duration = longest,
    flight_duration = base::sum(flight_weight[block & flight]),
    near_strength = summarize(strength, near, base::max),
    pre_strength = summarize(strength, pre, base::mean),
    flight_strength = summarize(strength, flight, base::max),
    reacquisition_gap = base::min(gap, 1),
    goalie_distance = base::min(80, goalie_distance[min_ix]),
    depth_fraction = base::pmin(1, base::pmax(0, along[min_ix])),
    crossing = base::as.numeric(cross),
    shot_distance = base::sqrt(base::sum((puck[release, ] - goalie[release, ])^2)),
    puck_flight_time = time[arrival] - time[release]
  )
  x[!base::is.finite(x)] <- 0
  x <- base::c(x, baseline = base::max(x['near_strength'], x['flight_strength']) *
                (0.25 + base::min(1, x['pre_duration'] / 0.3)) *
                base::exp(-x['reacquisition_gap'] / 0.4))
  base::names(x)[base::length(x)] <- 'baseline'
  x
}

# Analyze one goal and retain short public replay.
analyze_goal <- function(replay, goal, goalie_ids) {
  key <- base::paste(goal$gameId, goal$eventId, sep = '_')
  fail <- function(status) base::list(
    event = base::data.frame(key = key, game_id = goal$gameId, event_id = goal$eventId,
                            season = goal$seasonId, status = status)
  )
  if (base::isTRUE(goal$isEmptyNetAgainst == 1)) return(fail('empty_net'))
  goalie_id <- goal$goalieInNetId
  if (!base::length(goalie_id) || base::is.na(goalie_id) || goalie_id <= 0) {
    goalie_id <- goal$goaliePlayerIdAgainst
  }
  if (!base::length(goalie_id) || base::is.na(goalie_id) || goalie_id <= 0) {
    return(fail('unknown_goalie'))
  }
  if (base::is.null(replay) || base::nrow(replay) < 8L) return(fail('missing_tracking'))
  tracking <- unpack_tracking(replay)
  goalie   <- player_track(tracking, goalie_id)
  if (base::sum(stats::complete.cases(goalie)) < 6L) return(fail('untracked_goalie'))
  interval <- shot_interval(tracking, goal, goalie)
  if (base::is.null(interval)) return(fail('unresolved_timing'))
  direction <- interval$direction
  puck   <- tracking$puck * direction
  goalie <- goalie * direction
  release <- interval$release
  arrival <- interval$arrival
  relevant <- base::which(tracking$time >= tracking$time[release] - 0.5 &
                           tracking$time <= tracking$time[arrival])
  attack <- tracking$id[relevant, , drop = FALSE]
  attack[tracking$team[relevant, , drop = FALSE] != goal$eventOwnerTeamId] <- NA_real_
  ids <- base::setdiff(base::unique(base::as.vector(attack)),
                      base::c(NA_real_, 0, goal$scoringPlayerId, goalie_ids))
  features <- base::list()
  for (id in ids) {
    player <- player_track(tracking, id) * direction
    values <- sightline_features(puck, goalie, player, tracking$time, release, arrival)
    if (base::is.null(values)) next
    variants <- base::lapply(base::c(1, 2), function(radius) {
      sightline_features(puck, goalie, player, tracking$time, release, arrival, radius)
    })
    timings <- base::lapply(base::c(-0.1, 0.1), function(shift) {
      sightline_features(puck, goalie, player, tracking$time, release, arrival, shift = shift)
    })
    row <- base::data.frame(key = key, game_id = goal$gameId, event_id = goal$eventId,
                            season = goal$seasonId, player_id = id,
                            team_id = goal$eventOwnerTeamId)
    row <- base::cbind(row, base::as.data.frame(base::as.list(values)))
    for (j in base::seq_along(variants)) {
      v <- if (base::is.null(variants[[j]])) values else variants[[j]]
      row <- base::cbind(row, stats::setNames(base::as.data.frame(base::as.list(v)),
                                            base::paste0(base::names(v), '_r', j)))
    }
    for (j in base::seq_along(timings)) {
      v <- if (base::is.null(timings[[j]])) values else timings[[j]]
      row <- base::cbind(row, stats::setNames(base::as.data.frame(base::as.list(v)),
                                            base::paste0(base::names(v), '_t', j)))
    }
    features[[base::length(features) + 1L]] <- row
  }
  if (!base::length(features)) return(fail('untracked_teammates'))
  frames <- base::which(tracking$time >= tracking$time[release] - 1 &
                         tracking$time <= tracking$time[arrival] + 0.3)
  all_ids <- base::unique(base::as.vector(tracking$id[frames, , drop = FALSE]))
  all_ids <- all_ids[base::is.finite(all_ids) & all_ids > 0]
  tracks <- base::lapply(all_ids, function(id) {
    xy <- player_track(tracking, id)[frames, , drop = FALSE] * direction
    team <- tracking$team[tracking$id == id]
    team <- team[base::is.finite(team)]
    base::list(id = id, team = if (base::length(team)) team[1L] else NA_real_,
               xy = base::round(xy, 2))
  })
  clip <- base::list(
    key = key, goalie = goalie_id, scorer = goal$scoringPlayerId,
    team = goal$eventOwnerTeamId, direction = direction,
    time = base::round(tracking$time[frames] - tracking$time[release], 2),
    arrival = base::round(tracking$time[arrival] - tracking$time[release], 2),
    puck = base::round(puck[frames, , drop = FALSE], 2), players = tracks
  )
  event <- base::data.frame(
    key = key, game_id = goal$gameId, event_id = goal$eventId,
    season = goal$seasonId, status = 'evaluated',
    release_frame = release, arrival_frame = arrival,
    release_time = tracking$time[release], arrival_time = tracking$time[arrival],
    location_error = interval$location_error, scorer_error = interval$scorer_error,
    sample_interval = stats::median(base::diff(tracking$time)),
    shot_type = goal$shotType
  )
  base::list(event = event, features = dplyr::bind_rows(features), clip = clip)
}

# Feature Preparation ----------------------------------------------------

# Calculate public features and replay segments by season.
prepare_features <- function() {
  goals   <- base::readRDS('data/goals.rds')
  players <- base::readRDS('data/players.rds')
  goalie_ids <- players$playerId[players$positionCode == 'G' & !base::is.na(players$positionCode)]
  for (season in settings()$seasons) {
    output <- base::sprintf('data/features_%s.rds', season)
    if (base::file.exists(output)) next
    base::message('Extract features ', season)
    replay <- arrow::read_parquet(
      snapshot_path(season, 'replays')$local,
      col_select = tidyselect::matches(
        '^(gameId|eventId|timeStamp|puckXCoord|puckYCoord|player[0-9]+(PlayerId|TeamId|XCoord|YCoord))$'
      )
    )
    indices <- base::split(base::seq_len(base::nrow(replay)),
                           base::paste(replay$gameId, replay$eventId, sep = '_'))
    selected <- goals[goals$seasonId == season, , drop = FALSE]
    events   <- base::list()
    features <- base::list()
    clips    <- base::list()
    for (i in base::seq_len(base::nrow(selected))) {
      goal <- selected[i, , drop = FALSE]
      key  <- base::paste(goal$gameId, goal$eventId, sep = '_')
      ix   <- indices[[key]]
      if (base::is.null(ix) && season == 20262027L) {
        clip <- public_cache(base::paste0('live_', key), function() {
          x <- nhlscraper::replay(goal$gameId, goal$eventId)
          if (!base::nrow(x)) return(base::list(unavailable = TRUE))
          x
        })
        if (!base::is.data.frame(clip)) clip <- NULL
      } else {
        clip <- if (base::is.null(ix)) NULL else replay[ix, , drop = FALSE]
      }
      result <- analyze_goal(clip, goal, goalie_ids)
      events[[i]] <- result$event
      if (!base::is.null(result$features)) features[[key]] <- result$features
      if (!base::is.null(result$clip)) clips[[key]] <- result$clip
      if (i %% 500L == 0L) base::message('  ', i, ' / ', base::nrow(selected))
    }
    base::saveRDS(dplyr::bind_rows(events), base::sprintf('data/events_%s.rds', season))
    base::saveRDS(clips, base::sprintf('data/clips_%s.rds', season))
    base::saveRDS(dplyr::bind_rows(features), output)
    base::rm(replay, clips, features)
    base::gc()
  }
  base::invisible(TRUE)
}
