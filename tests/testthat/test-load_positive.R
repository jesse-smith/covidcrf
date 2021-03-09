test_that("`load_pcr_positive()` outputs a tibble of character columns", {
  skip_on_ci()
  pcr_positive <- load_pcr_positive()
  pcr_chr <- pcr_positive %>%
    dplyr::summarize(dplyr::across(.fns = is.character)) %>%
    as.matrix() %>%
    as.vector()
  expect_true(tibble::is_tibble(pcr_positive))
  expect_true(all(pcr_chr))
})

test_that("`load_pcr_positive()` only loads positive tests", {
  skip_on_ci()
  pcr_positive <- load_pcr_positive()
  expect_true(all(pcr_positive$inv_case_status %in% c("C", "P")))
  expect_true(
    all(pcr_positive$lab_result %in% c("Positive", "Presumptive Positive"))
  )
})

test_that("`load_inv_positive()` outputs a tibble of character columns", {
  skip_on_ci()
  inv_positive <- load_inv_positive()
  inv_chr <- inv_positive %>%
    dplyr::summarize(dplyr::across(.fns = is.character)) %>%
    as.matrix() %>%
    as.vector()
  expect_true(tibble::is_tibble(inv_positive))
  expect_true(all(inv_chr))
})

test_that("`load_inv_positive()` only loads positive cases", {
  skip_on_ci()
  inv_positive <- load_inv_positive()
  expect_true(all(inv_positive$inv_case_status %in% c("C", "P")))
})

test_that("`load_positive()` outputs a tibble of character columns", {
  skip_on_ci()
  skip_if_not_installed("memoise")
  positive <- load_positive()
  pos_chr <- positive %>%
    dplyr::summarize(dplyr::across(.fns = is.character)) %>%
    as.matrix() %>%
    as.vector()
  expect_true(tibble::is_tibble(positive))
  expect_true(all(pos_chr))
})

test_that("`load_positive()` output contains ID columns", {
  skip_on_ci()
  positive <- load_positive()
  expected_cols <- c("patient_first_name", "patient_last_name", "patient_dob")
  cols <- positive %>%
    dplyr::select(dplyr::matches(paste0("^", expected_cols, "$"))) %>%
    colnames()
  expect_equal(sort(cols), sort(expected_cols))
})
