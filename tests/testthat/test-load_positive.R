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
  dir <- fs::dir_create(fs::file_temp(""))
  # on.exit(fs::dir_delete(dir), add = TRUE)
  pcr_path <- coviData::path_create(
    dir,
    "MSR - All PCRs_01012021.csv"
  )
  pcr_data <- tibble::tibble(
    inv_local_id = c("CAS01234567TN01", "CAS12345678TN01", "CAS23456789TN01"),
    inv_case_status = c("C", "P", "N"),
    lab_result = c("Positive", "Presumptive Positive", "Negative")
  )
  coviData::write_file_delim(pcr_data, pcr_path)

  test_pcr_positive <- load_pcr_positive

  mockery::stub(test_pcr_positive, "coviData::path_pcr", pcr_path)

  expect_equal(test_pcr_positive(), pcr_data[-3L,])

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
  dir <- fs::dir_create(fs::file_temp(""))
  # on.exit(fs::dir_delete(dir), add = TRUE)
  inv_path <- coviData::path_create(
    dir,
    "2021-01-01 Final Data Pull.csv"
  )
  inv_data <- tibble::tibble(
    inv_local_id = c("CAS01234567TN01", "CAS12345678TN01", "CAS23456789TN01"),
    inv_case_status = c("C", "P", "N"),
    lab_result = c("Positive", "Presumptive Positive", "Negative")
  )
  coviData::write_file_delim(inv_data, path = inv_path)

  report_data <- tibble::tibble(
    inv_local_id = c("CAS01234567TN01", "CAS12345678TN01", "CAS23456789TN01"),
    report_date  = rep(as.Date("2021-01-01"), times = 3L)
  )

  expected_data <- inv_data %>%
    dplyr::mutate(report_date = rep("2021-01-01", times = 3L)) %>%
    dplyr::filter(inv_case_status %in% c("C", "P"))

  test_inv_positive <- load_inv_positive
  mockery::stub(test_inv_positive, "coviData::path_inv", inv_path)
  mockery::stub(test_inv_positive, "coviData::load_report_date", report_data)

  expect_equal(test_inv_positive(), expected_data)

  skip_on_ci()
  inv_positive <- load_inv_positive()
  expect_true(all(inv_positive$inv_case_status %in% c("C", "P")))
})

test_that("`load_positive()` works", {
  pcr_data <- tibble::tibble(
    inv_local_id = c("CAS01234567TN01", "CAS12345678TN01", "CAS23456789TN01"),
    inv_case_status = c("C", "P", "N"),
    lab_result = c("Positive", "Presumptive Positive", "Negative")
  )
  inv_data <- tibble::tibble(
    inv_local_id = c("CAS01234567TN01", "CAS12345678TN01", "CAS23456789TN01"),
    inv_case_status = c("C", "P", "N"),
    lab_result = c("Positive", "Presumptive Positive", "Negative"),
    report_date = rep("2021-01-01", times = 3L)
  )
  expected_data <- dplyr::filter(inv_data, inv_case_status %in% c("C", "P"))

  pcr_path <- "./MSR - All PCRs_01012021.csv"
  mockery::stub(load_positive, "coviData::path_pcr", pcr_path)
  mockery::stub(load_positive, "load_pcr_positive", pcr_data[-3L,])
  mockery::stub(load_positive, "load_inv_positive", inv_data[-3L,])

  expect_equal(load_positive(), expected_data)
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
