# Local Heavy Vignette Runs

This folder is for heavyweight vignette runs that may be too slow for routine
CI (`R CMD check`).

Use:

```bash
Rscript tools/render_vignettes_src.R
```

The script renders `vignettes_src/*.Rmd` and copies resulting `*.html` outputs
into `vignettes/` with matching filenames.

Those rendered HTML files can then be included in pkgdown deployments as static
analysis artefacts, while package vignettes in `vignettes/` remain fully
runnable and CI-safe.
