test_that("`save_crf_wb()` saves using the expected filename", {
  crf_nbs <- dplyr::mutate(
    test_crf_ptype,
    in_nbs = logical(),
    recent_test = logical()
  )
  crf <- prep_crf(crf_nbs, template = test_crf_template_fields)
  wb <- create_crf_wb(crf)

  dir <- fs::dir_create(fs::file_temp())
  on.exit(fs::dir_delete(dir), add = TRUE)

  save_crf_wb(wb, dir = dir)

  file_name <- dir %>% fs::dir_ls() %>% fs::path_file()
  expected_name <- paste0("CRF_NBS_", lubridate::today(), ".xlsx")

  expect_equal(file_name, expected_name)
})

test_that("`save_crf_wb()` outputs an `fs_path`", {
  crf_nbs <- dplyr::mutate(
    test_crf_ptype,
    in_nbs = logical(),
    recent_test = logical()
  )
  crf <- prep_crf(crf_nbs, template = test_crf_template_fields)
  wb <- create_crf_wb(crf)

  dir <- fs::dir_create(fs::file_temp())
  on.exit(fs::dir_delete(dir), add = TRUE)

  path <- save_crf_wb(wb, dir = dir)

  expect_s3_class(path, class = "fs_path")
})

test_that("`save_crf_wb()` does not overwrite when `force = FALSE`", {
  crf_nbs <- dplyr::mutate(
    test_crf_ptype,
    in_nbs = logical(),
    recent_test = logical()
  )
  crf <- prep_crf(crf_nbs, template = test_crf_template_fields)
  wb <- create_crf_wb(crf)

  dir <- fs::dir_create(fs::file_temp())
  on.exit(fs::dir_delete(dir), add = TRUE)

  save_crf_wb(wb, dir = dir)
  expect_error(save_crf_wb(wb, dir = dir, force = FALSE))
})
