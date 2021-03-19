test_that("`manage_crf_archive()` keeps all recent files", {
  dates <- seq(Sys.Date() - 13L, Sys.Date(), 1L)
  files <- paste0("CRF_NBS_", dates, ".xlsx")

  dir <- fs::dir_create(fs::file_temp())
  on.exit(fs::dir_delete(dir), add = TRUE)

  paths <- coviData::path_create(dir, files)
  purrr::walk(paths, fs::file_create)

  manage_crf_archive(dir = dir)

  managed_paths <- unname(fs::dir_ls(dir))

  expect_equal(managed_paths, paths)
})
