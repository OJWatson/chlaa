#!/usr/bin/env Rscript

# Check that committed generated artefacts match inst/odin/generation_manifest.csv.
#
# Intended for CI / developer use. This script does *not* require odin2.

args <- commandArgs(trailingOnly = TRUE)

if (any(args %in% c("-h", "--help"))) {
  cat(
    "Usage: tools/check_model_sync.R\n\n",
    "Verifies that the md5 sums in inst/odin/generation_manifest.csv match the\n",
    "current working tree. Exits non-zero if drift is detected.\n",
    sep = ""
  )
  quit(status = 0)
}

# Find repo root (directory containing DESCRIPTION)
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
script_dir <- if (length(script_file) == 1) dirname(normalizePath(script_file)) else getwd()
root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)

if (!file.exists(file.path(root, "DESCRIPTION"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}

old <- setwd(root)
on.exit(setwd(old), add = TRUE)

manifest_path <- file.path("inst", "odin", "generation_manifest.csv")
if (!file.exists(manifest_path)) {
  stop("Missing ", manifest_path, ". Run: Rscript tools/regenerate_model.R")
}

manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
if (!all(c("file", "md5") %in% names(manifest))) {
  stop("Malformed manifest: expected columns 'file' and 'md5'")
}

manifest$file <- gsub("\\\\", "/", manifest$file)
missing <- manifest$file[!file.exists(manifest$file)]
if (length(missing) > 0) {
  cat("Missing files listed in manifest:\n")
  cat(paste0("- ", missing, collapse = "\n"), "\n", sep = "")
  quit(status = 1)
}

md5_now <- unname(tools::md5sum(manifest$file))
wrong <- which(unname(manifest$md5) != unname(md5_now))
if (length(wrong) > 0) {
  cat("Generated artefacts are out of sync with manifest:\n")
  for (i in wrong) {
    cat("- ", manifest$file[[i]], " (expected ", manifest$md5[[i]], ", got ", md5_now[[i]], ")\n", sep = "")
  }
  cat("\nFix by running: Rscript tools/regenerate_model.R\n")
  quit(status = 1)
}

cat("OK: generated artefacts match ", manifest_path, "\n", sep = "")
