# Packaging / generator strategy

This package contains model code in a few forms, and it is important to be explicit about which files are *sources* vs *generated artefacts*.

This document is intended to answer (a) “where should I edit the model?” and (b) “which files are expected to be committed vs created during build?”.

## Source-of-truth

- `inst/odin/cholera_model.R` and `inst/odin/cholera_model_fit.R` are the **source-of-truth** for the model specification (human-editable).

## Generated / derived files (committed)

Today the repository also contains derived files that are committed so that:

- users can install from a clean checkout without requiring the generator toolchain, and
- CI/CRAN-like checks can run without additional system dependencies.

These derived files should be treated as *generated* and should only change when the model changes:

- `inst/dust/cholera_model.cpp`, `inst/dust/cholera_model_fit.cpp` — generated C++ targets used by `dust2`.
- `src/cholera_model.cpp`, `src/cholera_model_fit.cpp` — generated/vendored C++ code used by the package build.
- `R/dust.R`, `R/cpp11.R`, `src/cpp11.cpp` — generated bindings/registration.
- `inst/odin/generation_manifest.csv` — md5 stamp used to detect drift.

If these files change without a corresponding change in `inst/odin/*.R`, that is a red flag.

## Compiled artefacts (never commit)

The following are build outputs and must never be committed:

- `*.o`, `*.so`, `*.dll`, `*.dylib`, `*.a`

## Developer workflow (local)

1. Edit the odin model (`inst/odin/*.R`).
2. Regenerate derived outputs:
   - `make regen` (or `Rscript tools/regenerate_model.R`)
3. Optionally check that nothing is out of sync:
   - `make check-sync` (or `Rscript tools/check_model_sync.R`)
4. Ensure no compiled artefacts are tracked:
   - `make check-no-binaries` (or `bash tools/check_no_binaries.sh`)
5. Build/check the package.

## CI expectation

CI should be able to build the package from a clean checkout without any locally-generated binaries.

As a minimal invariant, CI should fail if compiled artefacts are tracked, or if regenerated C++ outputs are out of sync with the odin source.
