# Packaging / generator strategy

This package contains model code in a few forms, and it is important to be explicit about which files are *sources* vs *generated artefacts*.

This document is intended to answer (a) “where should I edit the model?” and (b) “which files are expected to be committed vs created during build?”.

## Source-of-truth

- `inst/odin/cholera_model.R` is the **source-of-truth** for the model specification (human-editable).

## Generated / derived files (committed)

Today the repository also contains derived files that are committed so that:

- users can install from a clean checkout without requiring the generator toolchain, and
- CI/CRAN-like checks can run without additional system dependencies.

These derived files should be treated as *generated* and should only change when the model changes:

- `inst/dust/cholera_model.cpp` — generated C++ target used by `dust`.
- `src/cholera_model.cpp` — generated/vendored C++ code used by the package build.

If these files change without a corresponding change in `inst/odin/cholera_model.R`, that is a red flag.

## Compiled artefacts (never commit)

The following are build outputs and must never be committed:

- `*.o`, `*.so`, `*.dll`, `*.dylib`, `*.a`

## Developer workflow (local)

1. Edit the odin model (`inst/odin/cholera_model.R`).
2. Regenerate derived C++ outputs (a dedicated regeneration script is added in a later milestone).
3. Build/check the package.

## CI expectation

CI should be able to build the package from a clean checkout without any locally-generated binaries.

As a minimal invariant, CI should fail if compiled artefacts are tracked, or if regenerated C++ outputs are out of sync with the odin source.
