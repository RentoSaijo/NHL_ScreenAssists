# NHL Screen Assists

[Website](https://rentosaijo.github.io/NHL_ScreenAssists/) · [Paper](https://rentosaijo.github.io/NHL_ScreenAssists/reports/paper.html) · [PDF](https://rentosaijo.github.io/NHL_ScreenAssists/reports/paper.pdf)

This project estimates offensive screen assists from public NHL goal-replay tracking, with player rankings, playing-time rates and animated replays of credited goals. The paper evaluates attribution against Clear Sight Analytics (CSA) annotations and examines screening’s association with scoring.

## Reproduce published outputs

Install R, Quarto, LaTeX, and the R packages `jsonlite`, `dplyr`, `tidyr`, `ggplot2`, `scales`, `knitr`, and `rmarkdown`, then run:

```sh
Rscript scripts/run.R rebuild
```

The command rebuilds rankings, figures and both paper formats from public files. Re-estimating the models requires authorized CSA access; private records and event-level labels are not distributed. [Provenance](docs/data/provenance.json) records input versions and the snapshot cutoff.
