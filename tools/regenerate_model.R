#!/usr/bin/env Rscript

# Regenerate bundled model code from inst/odin
#
# This script is for developers/maintainers. It requires odin2 (Suggests).
# It will:
#   1) run odin2::odin_package('.') to regenerate packaged dust2 generators
#   2) update inst/odin/generation_manifest.csv
#   3) run a minimal smoke check (load generator + run a couple of steps)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || !nzchar(x)) y else x

args <- commandArgs(trailingOnly = TRUE)

usage <- function(status = 0) {
  cat(
    "Usage: tools/regenerate_model.R [--no-smoke]\n\n",
    "Regenerates generated code (R bindings + C++) from inst/odin/*.R.\n",
    "Requires odin2 to be installed.\n",
    sep = ""
  )
  quit(status = status)
}

if (any(args %in% c("-h", "--help"))) {
  usage(0)
}

run_smoke <- !any(args %in% "--no-smoke")

if (!requireNamespace("odin2", quietly = TRUE)) {
  stop("odin2 must be installed to regenerate model code. Try: remotes::install_github('mrc-ide/odin2')")
}

# Find repo root (directory containing DESCRIPTION)
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
script_dir <- if (length(script_file) == 1) dirname(normalizePath(script_file)) else getwd()
root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)

if (!file.exists(file.path(root, "DESCRIPTION"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}

message("Regenerating odin2 packaged generators in: ", root)
old <- setwd(root)
on.exit(setwd(old), add = TRUE)

# 1) Regenerate
odin2::odin_package(".")

# 2) Write generation manifest (md5 over sources + key generated artefacts)
manifest_path <- file.path("inst", "odin", "generation_manifest.csv")

files <- c(
  sort(list.files(file.path("inst", "odin"), pattern = "\\.R$", full.names = TRUE)),
  sort(list.files(file.path("inst", "dust"), pattern = "\\.cpp$", full.names = TRUE)),
  file.path("R", "dust.R"),
  file.path("R", "cpp11.R"),
  file.path("src", "cholera_model.cpp"),
  file.path("src", "cholera_model_fit.cpp"),
  file.path("src", "cpp11.cpp")
)
files <- unique(files[file.exists(files)])

md5 <- unname(tools::md5sum(files))
rel <- gsub("\\\\", "/", files)

manifest <- data.frame(file = rel, md5 = md5, stringsAsFactors = FALSE)
utils::write.csv(manifest, manifest_path, row.names = FALSE)
message("Wrote ", manifest_path)

# 3) Smoke test: install into a temp library then run a tiny simulation
if (isTRUE(run_smoke)) {
  message("Running smoke test (install + 2-step simulate)…")
  tmp_lib <- tempfile("chlaa-lib-")
  dir.create(tmp_lib, recursive = TRUE)

  cmd <- c(
    "CMD", "INSTALL", ".",
    "--no-html", "--no-help", "--no-demo", "--no-multiarch",
    "--library", shQuote(tmp_lib)
  )
  status <- system2("R", cmd)
  if (!identical(status, 0L)) {
    stop("R CMD INSTALL failed during smoke test")
  }

  code <- paste(
    sprintf(".libPaths(c(%s, .libPaths()))", dQuote(tmp_lib)),
    "library(chlaa)",
    "pars <- cholera_parameters(validate = TRUE)",
    "out <- cholera_simulate(pars, time = 0:1, n_particles = 1, dt = 1, seed = 1, which = 'simulate')",
    "stopifnot(is.data.frame(out), nrow(out) > 0)",
    sep = "; "
  )
  status <- system2("Rscript", c("-e", code))
  if (!identical(status, 0L)) {
    stop("Smoke test failed: could not run a tiny simulation")
  }

  message("Smoke test OK")
}

# Clean up compiled artefacts that R CMD INSTALL may have left in src/
artefacts <- list.files(
  "src",
  pattern = "\\.(o|so|dll|dylib|a)$",
  full.names = TRUE
)
if (length(artefacts) > 0) {
  unlink(artefacts)
}
