# NHL Screen Assists

We estimate offensive screening contributions on NHL goals using public replay tracking, validate attribution against independent Clear Sight Analytics annotations, and examine screening's association with scoring.

The research snapshot covers 2023–24 through 2025–26 and completed 2026–27 games dated through October 19, 2026.

## Reproduce

Run `Rscript scripts/run.R` from this checkout. Individual stages are `prepare`, `features`, `detect`, `outcome`, and `publish`. Public inputs are cached locally under `data/`; the source snapshot and package revision are recorded with the published results.

The detector and outcome stages require authorized read access to CSA through the existing `CSA_SERVER`, `CSA_USERNAME`, and `CSA_PASSWORD` environment variables and ODBC Driver 18 for SQL Server. CSA event records and labels remain in memory. Published files contain public NHL-derived results, aggregate research findings, and explicit model parameters.

R conventions follow the shared project standard. Models use `tidymodels`. The paper uses the supplied Quarto journal template, and `docs/` contains the static website.
