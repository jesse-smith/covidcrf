test_that("`download_crf()` produces a `tibble`", {
  skip_on_ci()
  crf_data <- download_crf()
  expect_true(tibble::is_tibble(crf_data))
})

test_that("`download_crf()` produces clean column names", {
  skip_on_ci()
  crf_data    <- download_crf()
  raw_names   <- colnames(crf_data)
  clean_names <- janitor::make_clean_names(raw_names)
  expect_equal(raw_names, clean_names)
})

test_that("`download_crf()` produces `character` columns", {
  skip_on_ci()
  crf_data <- download_crf()
  cols_are_chr <- as.matrix(
    dplyr::summarize(crf_data, dplyr::across(.fns = is.character))
  )
  expect_true(all(cols_are_chr))
})
