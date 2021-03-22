test_that("`manage_crf_archive()` works", {
  dates <- seq(Sys.Date() - 8L, Sys.Date(), 1L)
  files <- paste0("CRF_NBS_", dates, ".xlsx")

  dir <- fs::dir_create(fs::file_temp())
  archive <- fs::dir_create(coviData::path_create(dir, "archive"))
  on.exit(fs::dir_delete(dir), add = TRUE)

  paths <- coviData::path_create(dir, files) %>% sort()

  expected_archive_paths <- coviData::path_create(
    fs::path_dir(paths[-NROW(paths)]),
    "archive",
    fs::path_file(paths[-NROW(paths)])
  )

  purrr::walk(paths, function(x) {fs::file_create(x); Sys.sleep(1)})

  manage_crf_archive(dir = dir)

  top_level_paths <- dir %>%
    fs::dir_ls() %>%
    stringr::str_subset(".*[.]xlsx$") %>%
    fs::as_fs_path() %>%
    unname()

  archive_paths <- unname(fs::dir_ls(archive)) %>% sort()

  expect_equal(top_level_paths, paths[NROW(paths)])
  expect_equal(archive_paths, expected_archive_paths)
})

test_that("`old_crf_files()` works", {
  dates <- seq(Sys.Date() - 8L, Sys.Date(), 1L)
  files <- paste0("CRF_NBS_", dates, ".xlsx")

  dir <- fs::dir_create(fs::file_temp())
  on.exit(fs::dir_delete(dir), add = TRUE)

  paths <- coviData::path_create(dir, files) %>% sort()
  purrr::walk(paths, function(x) {fs::file_create(x); Sys.sleep(1)})

  expect_equal(old_crf_files(dir), paths[-NROW(paths)])
})
