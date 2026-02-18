# chlaa

Stochastic cholera outbreak simulation and anticipatory action (AA) modelling tools.

This package vendors the generated dust2 C++ model code, so you can run simulation and fitting without having **odin2** installed at runtime. (odin2 is only needed to regenerate the bundled code; see `docs/packaging.md`.)

## Minimal simulation

```r
library(chlaa)

pars <- chlaa_parameters(Sev0 = 2)
sim <- chlaa_simulate(pars, time = 0:180, n_particles = 50, dt = 0.25, seed = 1)
sim 
```

