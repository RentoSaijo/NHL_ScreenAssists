# NHL Screen Assists

[Rankings and replay explorer](https://RentoSaijo.github.io/NHL_ScreenAssists/) · [Research paper](https://RentoSaijo.github.io/NHL_ScreenAssists/reports/paper.html) · [PDF](https://RentoSaijo.github.io/NHL_ScreenAssists/reports/paper.pdf)

We estimate offensive screening contributions on NHL goals using public replay tracking, validate player attribution against independent Clear Sight Analytics annotations, and examine screening's association with scoring. The fixed snapshot covers 2023–24 through 2025–26 and completed 2026–27 games dated through **October 19, 2026**.

Each qualifying offensive teammate receives one credit per goal, excluding the scorer. Conservative credits are a subset of broader credits. Rates use official appearances and ice time under identical season, competition, and team filters. These are estimated contributions on goals, with measurement error and unevaluable replays described in the paper.

## Reproduce published results

With R, Quarto, a TeX installation, and `jsonlite`, `dplyr`, `tidyr`, `ggplot2`, `scales`, `knitr`, and `rmarkdown` installed:

```sh
Rscript scripts/run.R rebuild
```

This reconstructs rankings from distributed goal–player credits and official player-game exposure, checks their totals, rebuilds research figures, and renders the paper in HTML and PDF. It uses the frozen public model results and needs no CSA access. Finished papers are saved in `reports/` and copied into `docs/reports/`.

Serve `docs/` with any static web server, for example `python3 -m http.server 8000 --directory docs`. GitHub Pages publishes `main:/docs`; the website has no JavaScript build step or server-side dependencies.

## Acquire data and estimate models

The full pipeline uses the development version of `nhlscraper` at commit `58821f001469d5024766511e511051377c0f859f` and public NHL_DB snapshot `3106adec4df1933dbef4f8bb6d3675504b7a6fa7`. Package versions and input checksums are recorded in [`docs/data/provenance.json`](docs/data/provenance.json). Install the additional packages listed there and the pinned development package:

```r
remotes::install_github(
  'RentoSaijo/nhlscraper@58821f001469d5024766511e511051377c0f859f',
  upgrade = 'never'
)
```

```sh
Rscript scripts/run.R
```

The single entry point also accepts individual stages:

| Stage | Work |
| --- | --- |
| `prepare` | Acquire public inputs and official player-game exposure |
| `features` | Resolve scoring shots and calculate geometric features |
| `detect` | Fit/select detector, freeze parameters, and evaluate reference agreement |
| `outcome` | Fit the separate adjusted scoring-association model |
| `publish` | Export public credits, rankings, replays, figures, and finished paper |
| `report` | Render the paper from existing public artifacts |
| `rebuild` | Reconstruct published ranking arithmetic, figures, and paper |

Public acquisition caches live under ignored `data/`. A fresh acquisition can differ after corrections to mutable NHL endpoints. The distributed public artifacts preserve the published snapshot.

The `detect` and `outcome` stages require authorized read access to CSA's historical reporting database through `CSA_SERVER`, `CSA_USERNAME`, and `CSA_PASSWORD`, with ODBC Driver 18 for SQL Server. Private records and event-level reference labels remain in memory. Models use `tidymodels`; exported parameters are explicit numeric coefficients rather than fitted objects containing training records. Re-estimation depends on authorized access and the database state at query time.

## Files

- `R/` contains preparation, geometry, reference matching, detection, outcome analysis, and publication functions.
- `reports/` contains editable Quarto source, references, the supplied journal extension, and finished HTML/PDF.
- `docs/` contains the static website, figures, and compact public outputs.
- `docs/data/screen-assists.csv` stores NHL-derived goal–player credits; `conservative` and `broader` are 0/1 indicators.
- `docs/data/player-games.csv` stores official appearance and ice-time denominators in seconds; team identity follows each game.
- `docs/data/goal-status.csv` distinguishes evaluated goals, empty nets, and unevaluable tracking.
- `docs/data/model.json`, `validation.json`, and `outcome.json` contain frozen parameters and aggregate findings.
- `docs/data/goals.json` indexes short replay shards and original NHL replay/video links.

NHL data and CSA annotations retain their respective source rights. CSA raw records and event-level labels are not distributed. The study is independent of the NHL and its teams. No scheduled refresh is configured.
