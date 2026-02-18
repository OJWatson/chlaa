#!/usr/bin/env Rscript

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("rmarkdown is required. Install with install.packages('rmarkdown')")
}

src_dir <- "vignettes_src"
out_dir <- "vignettes"

if (!dir.exists(src_dir)) stop("Missing directory: ", src_dir)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

files <- list.files(src_dir, pattern = "\\.Rmd$", full.names = TRUE)
if (length(files) == 0) {
  message("No Rmd files found in ", src_dir)
  quit(status = 0)
}

for (f in files) {
  message("Rendering ", f)
  html <- rmarkdown::render(
    input = f,
    output_format = "html_document",
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )

  target <- file.path(out_dir, basename(html))
  ok <- file.copy(html, target, overwrite = TRUE)
  if (!ok) stop("Failed copying rendered file to ", target)
  message("Copied to ", target)
}

message("Done")
