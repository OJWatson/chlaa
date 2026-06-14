# chlaa

`chlaa` is an R package for cholera outbreak simulation, fitting, intervention scenario analysis, and health economic evaluation.

It provides tools for:
- simulating a stochastic cholera outbreak model
- fitting the model to outbreak data
- forecasting from fitted models
- running intervention and counterfactual scenarios
- comparing outcomes across scenarios
- carrying out health economic and budget optimisation analyses

## Installation

```r
# install.packages("pak")
pak::pak("OJWatson/chlaa")
```

## Development install

```r
pak::pak(local::local_package("."), dependencies = TRUE)
```

## Package structure

The repository is package-first and contains:

- `R/` — package functions
- `man/` — documentation
- `tests/` — test suite
- `vignettes/` — user-facing worked examples
- `vignettes_src/` — source material for vignette generation where needed
- `inst/odin/` — odin model source
- `inst/dust/` — generated model code committed for package use
- `inst/extdata/` — example/fitting/economics data bundled with the package
- `.github/workflows/` — CI and pkgdown deployment

## Website

The pkgdown site is built and deployed via GitHub Actions to GitHub Pages.

## Notes

The package vendors generated model code so simulation and fitting can run without requiring `odin2` at runtime. `odin2` is only needed if you want to regenerate the bundled model code during development.
