# Model Helpers ----------------------------------------------------------

# Read public feature tables.
read_features <- function(prefix = 'features') {
  dplyr::bind_rows(base::lapply(settings()$seasons, function(season) {
    base::readRDS(base::sprintf('data/%s_%s.rds', prefix, season))
  }))
}

# Define identity-free detector predictors.
detector_predictors <- function() {
  base::c('near_distance', 'release_distance', 'pre_duration', 'narrow_duration',
           'wide_duration', 'longest_duration', 'flight_duration', 'near_strength',
           'pre_strength', 'flight_strength', 'reacquisition_gap', 'goalie_distance',
           'depth_fraction', 'crossing', 'shot_distance', 'puck_flight_time')
}

# Summarize tied scores on precision-recall curve.
precision_recall <- function(label, score) {
  ordering <- base::order(score, decreasing = TRUE)
  y <- base::as.integer(label[ordering])
  s <- score[ordering]
  tp <- base::cumsum(y)
  end <- base::which(base::c(base::diff(s) != 0, TRUE))
  end <- end[base::is.finite(s[end])]
  base::data.frame(threshold = s[end], precision = tp[end] / end,
                    recall = tp[end] / base::sum(y), tp = tp[end],
                    fp = end - tp[end], fn = base::sum(y) - tp[end])
}

# Calculate average precision with tied-score groups.
average_precision <- function(label, score) {
  curve <- precision_recall(label, score)
  base::sum(base::diff(base::c(0, curve$recall)) * curve$precision)
}

# Select operating threshold using specified F measure.
choose_threshold <- function(label, score, beta, ceiling = Inf) {
  curve <- precision_recall(label, score)
  curve <- curve[curve$threshold <= ceiling, , drop = FALSE]
  f <- (1 + beta^2) * curve$precision * curve$recall /
    (beta^2 * curve$precision + curve$recall)
  f[!base::is.finite(f)] <- 0
  best <- base::which(f == base::max(f))[1L]
  base::as.numeric(curve$threshold[best])
}

# Calculate game-clustered interval for ratio.
ratio_interval <- function(numerator, denominator, game) {
  total <- base::sum(denominator)
  if (!total) return(base::c(estimate = NA_real_, lower = NA_real_, upper = NA_real_))
  estimate <- base::sum(numerator) / total
  influence <- base::tapply(numerator - estimate * denominator, game, base::sum)
  n <- base::length(influence)
  variance <- if (n > 1L) n / (n - 1) * base::sum(influence^2) / total^2 else NA_real_
  base::c(estimate = estimate, lower = base::max(0, estimate - 1.96 * base::sqrt(variance)),
           upper = base::min(1, estimate + 1.96 * base::sqrt(variance)))
}

# Measure threshold-specific performance.
classification_metrics <- function(label, score, game, threshold) {
  pred <- score >= threshold
  tp   <- pred & label
  p    <- ratio_interval(base::as.numeric(tp), base::as.numeric(pred), game)
  r    <- ratio_interval(base::as.numeric(tp), base::as.numeric(label), game)
  base::list(
    n = base::length(label), positives = base::sum(label), predictions = base::sum(pred),
    true_positive = base::sum(tp), false_positive = base::sum(pred & !label),
    false_negative = base::sum(!pred & label),
    precision = base::unname(p[1]), precision_low = base::unname(p[2]),
    precision_high = base::unname(p[3]),
    recall = base::unname(r[1]), recall_low = base::unname(r[2]),
    recall_high = base::unname(r[3]),
    f1 = if (p[1] + r[1] > 0) base::unname(2 * p[1] * r[1] / (p[1] + r[1])) else 0
  )
}

# Apply explicit public model parameters.
predict_detector <- function(features, model, variant = '') {
  if (model$kind == 'geometry') {
    score <- features[[base::paste0('baseline', variant)]]
  } else {
    predictors <- base::unlist(model$predictors)
    x <- base::as.matrix(features[, base::paste0(predictors, variant), drop = FALSE])
    weights <- base::as.numeric(base::unlist(model$coefficients))
    score <- stats::plogis(weights[1L] + base::as.vector(x %*% weights[-1L]))
  }
  score[!base::is.finite(score)] <- -Inf
  if ('available' %in% base::names(features)) score[!features$available] <- -Inf
  score
}

# Summarize calibration without releasing event labels.
calibration_summary <- function(label, probability) {
  p <- base::pmin(1 - 1e-8, base::pmax(1e-8, probability))
  bin <- base::pmin(9L, base::floor(p * 10))
  data <- base::data.frame(bin = bin, predicted = p, observed = base::as.numeric(label))
  bins <- data |>
    dplyr::group_by(.data$bin) |>
    dplyr::summarise(n = dplyr::n(), predicted = base::mean(.data$predicted),
                     observed = base::mean(.data$observed), .groups = 'drop')
  base::list(brier = base::mean((p - label)^2), bins = bins)
}

# Detector Estimation ----------------------------------------------------

# Fit model and freeze thresholds before final-season evaluation.
fit_detector <- function() {
  features <- read_features()
  events   <- read_features('events')
  goals    <- base::readRDS('data/goals.rds')
  reference <- build_reference(features, events, goals)
  benchmark <- reference$players
  predictors <- detector_predictors()
  training <- benchmark |>
    dplyr::filter(.data$season == 20232024L, .data$available) |>
    dplyr::mutate(label = base::factor(base::ifelse(.data$label, 'yes', 'no'),
                                      levels = base::c('no', 'yes'))) |>
    dplyr::select('label', tidyselect::all_of(predictors))
  tuning <- benchmark[benchmark$season == 20242025L, , drop = FALSE]
  base::stopifnot(base::nrow(training) > 1000L, base::sum(training$label == 'yes') > 100L,
                  base::nrow(tuning) > 1000L)
  fit <- parsnip::logistic_reg(penalty = 0.01, mixture = 0) |>
    parsnip::set_engine('glmnet', standardize = TRUE) |>
    parsnip::fit(label ~ ., data = training)
  candidates <- base::list(base::list(kind = 'geometry', predictors = predictors))
  for (penalty in base::c(0.0001, 0.001, 0.01, 0.1, 1)) {
    coefficient <- base::as.matrix(stats::coef(fit$fit, s = penalty))
    coefficient <- coefficient[base::c('(Intercept)', predictors), 1]
    candidates[[base::length(candidates) + 1L]] <- base::list(
      kind = 'ridge_logistic', predictors = predictors, requested_penalty = penalty,
      penalty = base::max(base::min(fit$fit$lambda), penalty),
      coefficients = base::unname(coefficient)
    )
  }
  scores <- base::lapply(candidates, function(model) predict_detector(tuning, model))
  aps <- base::vapply(scores, function(score) average_precision(tuning$label, score),
                      base::numeric(1))
  best <- base::which.max(aps)
  model <- candidates[[best]]
  selected_score <- scores[[best]]
  conservative <- choose_threshold(tuning$label, selected_score, 0.5)
  broader <- choose_threshold(tuning$label, selected_score, 1, ceiling = conservative)
  model$thresholds <- base::list(conservative = conservative, broader = broader)
  model$fit_season <- 20232024L
  model$tune_season <- 20242025L
  model$test_season <- 20252026L
  model$effective_radius_ft <- 1.5
  model$feature_window_seconds <- base::list(pre_release = 0.5, release_band = base::c(-0.2, 0.1))
  model$timing_resolution_seconds <- 0.1
  write_json(model, 'docs/data/model.json', pretty = TRUE, digits = 16)
  model <- jsonlite::read_json('docs/data/model.json', simplifyVector = TRUE)
  comparison <- base::data.frame(
    method = base::c('Geometry', base::paste0('Ridge, penalty ', base::c(0.0001, 0.001, 0.01, 0.1, 1))),
    average_precision = aps, selected = base::seq_along(aps) == best
  )
  metrics <- base::list()
  curves  <- base::list()
  for (season in base::c(20232024L, 20242025L, 20252026L)) {
    b <- benchmark[benchmark$season == season, , drop = FALSE]
    b$score <- predict_detector(b, model)
    er <- reference$events[reference$events$season == season, , drop = FALSE]
    event_scores <- b |>
      dplyr::group_by(.data$key) |>
      dplyr::summarise(score = base::max(.data$score), .groups = 'drop')
    er <- dplyr::left_join(er, event_scores, by = 'key')
    er$score[base::is.na(er$score)] <- -Inf
    result <- base::list(
      average_precision = average_precision(b$label, b$score),
      event_average_precision = average_precision(er$event_label, er$score)
    )
    for (name in base::names(model$thresholds)) {
      result[[name]] <- base::list(
        player = classification_metrics(b$label, b$score, b$game_id, model$thresholds[[name]]),
        event = classification_metrics(er$event_label, er$score, er$game_id, model$thresholds[[name]])
      )
    }
    if (model$kind == 'ridge_logistic') {
      result$calibration <- calibration_summary(b$label, base::pmax(0, b$score))
    }
    metrics[[base::as.character(season)]] <- result
    curve <- precision_recall(b$label, b$score)
    selected <- base::unique(base::round(base::seq(1, base::nrow(curve), length.out = 200)))
    curves[[base::as.character(season)]] <- curve[selected, base::c('precision', 'recall')]
  }
  sensitivity <- base::list()
  for (variant in base::c('', '_r1', '_r2', '_t1', '_t2')) {
    score <- predict_detector(features, model, variant)
    for (season in settings()$seasons) {
      keep <- features$season == season
      sensitivity[[base::length(sensitivity) + 1L]] <- base::data.frame(
        scenario = if (variant == '') 'Primary' else variant, season = season,
        conservative = base::sum(score[keep] >= conservative),
        broader = base::sum(score[keep] >= broader)
      )
    }
  }
  validation <- base::list(
    reference = reference$summary, comparison = comparison, metrics = metrics,
    curves = curves, sensitivity = dplyr::bind_rows(sensitivity),
    reference_definition = base::paste(
      'Reviewed, QC-complete goals with resolved replay timing.',
      'Positive events require explicit offensive or mixed screen classification and a named offensive teammate.',
      'Negatives require explicit clear-sight or defensive-only classification.',
      'Within participant-bearing screen events, attribution is evaluated against the recorded participant set;',
      'its completeness is a reference-standard assumption.',
      'Screen-on-pass events and screen events without named offensive teammates are excluded.'
    ),
    regularization = 'Requested penalties below the fitted glmnet path minimum use its terminal coefficients.'
  )
  write_json(validation, 'docs/data/validation.json', pretty = TRUE)
  base::message('Selected ', model$kind, '; tuning AP ', base::round(aps[best], 3),
                '; thresholds ', base::signif(conservative, 4), ' / ', base::signif(broader, 4))
  base::message('Final-season player precision: ',
                base::round(metrics[['20252026']]$conservative$player$precision, 3), ' / ',
                base::round(metrics[['20252026']]$broader$player$precision, 3))
  base::invisible(validation)
}
