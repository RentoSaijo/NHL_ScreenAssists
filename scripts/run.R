# Configure Pipeline -----------------------------------------------------

# Locate project and select stages.
arguments <- base::commandArgs(trailingOnly = TRUE)
stage     <- if (base::length(arguments)) arguments[[1L]] else 'all'
script    <- base::sub('^--file=', '', base::grep('^--file=', base::commandArgs(),
                                               value = TRUE)[1L])
base::setwd(base::dirname(base::dirname(base::normalizePath(script))))
base::options(timeout = 1200, stringsAsFactors = FALSE)
base::Sys.setenv(OMP_NUM_THREADS = '2', OPENBLAS_NUM_THREADS = '2')
for (file in base::list.files('R', pattern = '\\.R$', full.names = TRUE)) base::source(file)
base::set.seed(settings()$analysis_seed)

# Run selected analysis stage.
if (stage %in% base::c('all', 'prepare')) prepare_data()
if (stage %in% base::c('all', 'features')) prepare_features()
if (stage %in% base::c('all', 'detect')) fit_detector()
if (stage %in% base::c('all', 'outcome')) fit_outcome()
if (stage %in% base::c('all', 'publish')) publish_results()
if (stage %in% base::c('all', 'publish', 'report')) render_report()
if (stage == 'rebuild') rebuild_public()
if (!stage %in% base::c('all', 'prepare', 'features', 'detect', 'outcome', 'publish',
                       'report', 'rebuild')) {
  base::stop('Choose all, prepare, features, detect, outcome, publish, report, or rebuild.')
}
