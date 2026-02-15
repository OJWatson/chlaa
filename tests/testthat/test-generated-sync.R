test_that("bundled odin/dust artefacts match generation manifest", {
  manifest_file <- system.file("odin", "generation_manifest.csv", package = "chlaa")
  expect_true(nzchar(manifest_file))

  manifest <- utils::read.csv(manifest_file, stringsAsFactors = FALSE)
  expect_true(all(c("file", "md5") %in% names(manifest)))
  expect_true(nrow(manifest) > 0)

  # Installed package root (contains DESCRIPTION)
  pkg_root <- dirname(dirname(manifest_file))

  # The manifest is written relative to the *source* tree, but during R CMD check
  # we only have access to the *installed* package.
  #
  # - inst/* is installed at the package root (e.g. inst/odin -> odin)
  # - src/* and R/* are not present as files post-install
  src_paths <- gsub("\\\\", "/", manifest$file)
  installed_paths <- sub("^inst/", "", src_paths)
  keep <- grepl("^(odin|dust)/", installed_paths)

  files <- file.path(pkg_root, installed_paths[keep])
  expect_true(length(files) > 0)
  expect_true(all(file.exists(files)))

  md5_now <- unname(tools::md5sum(files))
  md5_old <- manifest$md5[keep]

  changed <- installed_paths[keep][md5_now != md5_old]
  expect_equal(changed, character(0))
})
