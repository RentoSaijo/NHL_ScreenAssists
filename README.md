# NHL Screen Assists

[Rankings and replays](https://rentosaijo.github.io/NHL_ScreenAssists/) · [Paper](https://rentosaijo.github.io/NHL_ScreenAssists/reports/paper.html) · [PDF](https://rentosaijo.github.io/NHL_ScreenAssists/reports/paper.pdf)

We estimate offensive screen assists from public NHL goal-replay tracking, evaluate attribution against independent Clear Sight Analytics annotations, and examine screening’s association with scoring. The snapshot covers 2023–24 through 2025–26 and completed 2026–27 games through October 19, 2026.

The website provides conservative and broader rankings, rates based on official appearances and ice time, and an animated replay for each credited goal. Definitions, validation results, and limitations are explained in the [methodology](https://rentosaijo.github.io/NHL_ScreenAssists/methodology.html) and paper.

## Reproduce published outputs

Install R, Quarto, LaTeX, and the R packages `jsonlite`, `dplyr`, `tidyr`, `ggplot2`, `scales`, `knitr`, and `rmarkdown`, then run:

```sh
Rscript scripts/run.R rebuild
```

This rebuilds ranking arithmetic, figures, and the HTML/PDF paper from the distributed public files without CSA access. The website is in `docs/`; editable paper source and finished copies are in `reports/`.

Full data acquisition and model estimation use the same [pipeline entry point](scripts/run.R) and require authorized CSA access. Private records and event-level labels are not distributed. [Provenance](docs/data/provenance.json) records input revisions, package versions, and the acquisition cutoff.
