# NHL Screen Assists

[Rankings and replays](https://rentosaijo.github.io/NHL_ScreenAssists/) · [Paper](https://rentosaijo.github.io/NHL_ScreenAssists/reports/paper.html) · [PDF](https://rentosaijo.github.io/NHL_ScreenAssists/reports/paper.pdf)

This project estimates offensive screen assists from public NHL goal-replay tracking. Independent Clear Sight Analytics (CSA) annotations provide the attribution reference, and a separate CSA shot analysis examines screening’s association with scoring. The fixed snapshot covers 2023–24 through 2025–26 and completed 2026–27 games through October 19, 2026.

The website pairs conservative and broader rankings with rates based on official appearances and ice time and animated replays of credited goals. The [methodology](https://rentosaijo.github.io/NHL_ScreenAssists/methodology.html) and paper explain the definitions, evaluation and limitations.

## Reproduce published outputs

Install R, Quarto, LaTeX, and the R packages `jsonlite`, `dplyr`, `tidyr`, `ggplot2`, `scales`, `knitr`, and `rmarkdown`, then run:

```sh
Rscript scripts/run.R rebuild
```

This command rebuilds ranking arithmetic, figures and the HTML/PDF paper from public files, without CSA access. Website files are in `docs/`; paper source and rendered copies are in `reports/`.

Full acquisition and model estimation use the [pipeline entry point](scripts/run.R) and require authorized CSA access. Private records and event-level labels remain undistributed. [Provenance](docs/data/provenance.json) records input revisions, package versions and the acquisition cutoff.
