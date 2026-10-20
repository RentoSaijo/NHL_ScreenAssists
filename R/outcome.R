# Outcome Helpers --------------------------------------------------------

# Standardize probabilities over screened-shot covariates.
standardized_contrast <- function(engine, data) {
  coefficients <- lme4::fixef(engine)
  covariance   <- base::as.matrix(stats::vcov(engine))
  fixed_terms  <- stats::delete.response(stats::terms(reformulas::nobars(stats::formula(engine))))
  screened <- data[data$screen == 1, , drop = FALSE]
  x <- stats::model.matrix(fixed_terms, screened)
  x <- x[, base::names(coefficients), drop = FALSE]
  screen_column <- base::match('screen', base::colnames(x))
  variance <- base::sum(base::as.data.frame(lme4::VarCorr(engine))$vcov)
  offsets  <- stats::qnorm((base::seq_len(41L) - 0.5) / 41) * base::sqrt(variance)
  calculate <- function(value) {
    design <- x
    design[, screen_column] <- value
    eta <- base::as.vector(design %*% coefficients)
    probabilities <- base::sapply(offsets, function(offset) stats::plogis(eta + offset))
    mean_probability <- base::mean(probabilities)
    weight <- base::rowMeans(probabilities * (1 - probabilities))
    gradient <- base::colMeans(design * weight)
    standard_error <- base::sqrt(base::as.numeric(
      base::t(gradient) %*% covariance %*% gradient))
    base::list(estimate = mean_probability, gradient = gradient, se = standard_error)
  }
  screened_result <- calculate(1)
  clear_result    <- calculate(0)
  gradient <- screened_result$gradient - clear_result$gradient
  difference <- screened_result$estimate - clear_result$estimate
  difference_se <- base::sqrt(base::as.numeric(
    base::t(gradient) %*% covariance %*% gradient))
  interval <- function(estimate, se) {
    base::list(estimate = estimate, lower = estimate - 1.96 * se, upper = estimate + 1.96 * se)
  }
  beta <- coefficients['screen']
  beta_se <- base::sqrt(covariance['screen', 'screen'])
  base::list(
    screened_rate = interval(screened_result$estimate, screened_result$se),
    clear_rate = interval(clear_result$estimate, clear_result$se),
    risk_difference = interval(difference, difference_se),
    odds_ratio = base::list(estimate = base::unname(base::exp(beta)),
                            lower = base::unname(base::exp(beta - 1.96 * beta_se)),
                            upper = base::unname(base::exp(beta + 1.96 * beta_se)))
  )
}

# Outcome Estimation -----------------------------------------------------

# Estimate adjusted screened-versus-clear-sight association.
fit_outcome <- function() {
  base::loadNamespace('multilevelmod')
  raw <- read_csa(goals_only = FALSE)$shots
  eligible <- raw |>
    dplyr::filter(.data$shot_type %in% base::c(3L, 13L),
                  !base::is.na(.data$screen_on_pass), !.data$screen_on_pass,
                  .data$result != 9L, !dplyr::coalesce(.data$post, FALSE),
                  .data$release_type > 0L, .data$release_type != 2L,
                  .data$zone > 0L, .data$shooter_id > 0L,
                  .data$shooting_team > 0L, .data$defending_team > 0L,
                  .data$strength %in% base::c('EV', 'PP', 'SH')) |>
    dplyr::mutate(screen = base::as.integer(.data$shot_type == 13L),
                  goal = base::factor(base::ifelse(.data$result == 8L, 'yes', 'no'),
                                       levels = base::c('no', 'yes')))
  support <- eligible |>
    dplyr::group_by(.data$season_start, .data$strength, .data$zone, .data$release_type) |>
    dplyr::summarise(screened = base::sum(.data$screen == 1L),
                     clear = base::sum(.data$screen == 0L), .groups = 'drop') |>
    dplyr::filter(.data$screened > 0L, .data$clear > 0L)
  data <- dplyr::semi_join(eligible, support,
                           by = base::c('season_start', 'strength', 'zone', 'release_type'))
  factors <- base::c('season_start', 'game_type', 'strength', 'zone', 'release_type',
                     'context', 'pass_zone', 'second_pass_zone', 'shooting_team',
                     'defending_team', 'shooter_id', 'goalie_id', 'game_id')
  for (name in factors) {
    value <- base::as.character(data[[name]])
    value[base::is.na(value)] <- 'Unknown'
    data[[name]] <- base::factor(value)
  }
  base::stopifnot(base::nrow(data) > 10000L, base::sum(data$screen) > 1000L)
  formula <- goal ~ screen + season_start + game_type + strength + zone + release_type +
    context + pass_zone + second_pass_zone + shooting_team + defending_team +
    (1 | shooter_id) + (1 | goalie_id) + (1 | game_id)
  base::message('Fit mixed-effects outcome model on ', base::nrow(data), ' shots.')
  fit <- parsnip::logistic_reg() |>
    parsnip::set_engine(
      'glmer', nAGQ = 0,
      control = lme4::glmerControl(optimizer = 'bobyqa', calc.derivs = FALSE,
                                   optCtrl = base::list(maxfun = 100000))
    ) |>
    parsnip::fit(formula, data = data)
  engine <- fit$fit
  if (!base::is.null(engine@optinfo$conv$opt) && engine@optinfo$conv$opt != 0L) {
    base::stop('Outcome model did not converge.')
  }
  contrast <- standardized_contrast(engine, data)
  observed <- data |>
    dplyr::group_by(.data$season_start, .data$screen) |>
    dplyr::summarise(shots = dplyr::n(), goals = base::sum(.data$goal == 'yes'),
                     rate = base::mean(.data$goal == 'yes'), .groups = 'drop')
  summary <- base::list(
    screened_shots = base::sum(data$screen == 1L),
    clear_shots = base::sum(data$screen == 0L),
    screened_retention = base::sum(data$screen == 1L) / base::sum(eligible$screen == 1L),
    seasons = base::c(20242025L, 20252026L),
    games = base::nlevels(data$game_id),
    shooters = base::nlevels(data$shooter_id),
    goalies = base::nlevels(data$goalie_id),
    observed = observed, adjusted = contrast,
    random_effects = base::as.data.frame(lme4::VarCorr(engine))[, base::c('grp', 'vcov', 'sdcor')],
    singular = lme4::isSingular(engine),
    optimizer_converged = TRUE,
    model = base::paste(base::deparse(formula), collapse = ' '),
    estimation = 'Binomial mixed model; penalized iteratively reweighted least squares (glmer, nAGQ = 0).',
    standardization = base::paste(
      'Standardized to screened-shot covariates within common-support strata;',
      'integrated over estimated normal shooter, goalie, and game intercept distributions',
      'using 41 deterministic normal-quantile nodes.'
    ),
    interval_method = base::paste(
      'Fixed-effect Wald and delta-method 95% intervals.',
      'Variance components are held at their fitted estimates.'
    ),
    population = base::paste(
      'Reviewed, QC-complete CSA Offensive Screen and Clear Sight classified shots with a goalie.',
      'Screen-on-pass, posts, and deflection releases excluded.',
      'Each included season-strength-location-release stratum contains both groups.',
      'This is an association within the observed CSA shot population, not all NHL shot attempts.'
    )
  )
  write_json(summary, 'docs/data/outcome.json', pretty = TRUE)
  base::message('Adjusted odds ratio: ', base::round(contrast$odds_ratio$estimate, 2),
                '; standardized difference: ',
                base::round(100 * contrast$risk_difference$estimate, 2), ' percentage points.')
  base::invisible(summary)
}
