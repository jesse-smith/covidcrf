test_that("`filter_crf()` filters correctly", {
  crf_data <- test_filter_crf
  crf_actual <- dplyr::select(filter_crf(crf_data), "description")
  crf_expected <- dplyr::select(dplyr::filter(crf_data, keep), "description")
  expect_equal(crf_actual, crf_expected)
})

test_that("`filter_crf()` returns input columns only", {
  crf_data <- test_filter_crf
  cols_expected <- colnames(crf_data)
  cols_actual <- colnames(filter_crf(crf_data))
  expect_equal(cols_actual, cols_expected)
})
