test_that("`add_crf_ids()` produces expected ID columns", {

  ptype <- tibble::tibble(
    .firstname_id_tmp_ = character(),
    .lastname_id_tmp_ = character(),
    .dob_id_tmp_ = lubridate::Date(),
    .test_dt_id_tmp_ = lubridate::Date()
  ) %>%
    vctrs::vec_ptype() %>%
    dplyr::relocate(sort(colnames(.)))


  ids_test <- add_crf_ids(test_nbs_comparison_crf) %>%
    dplyr::select(dplyr::matches("^[.].*_id_tmp_$")) %>%
    vctrs::vec_ptype() %>%
    dplyr::relocate(sort(colnames(.)))

  expect_vector(ids_test, ptype = ptype)
})

test_that("`add_nbs_ids()` produces expected ID columns", {
  ptype <- tibble::tibble(
    .firstname_id_tmp_ = character(),
    .lastname_id_tmp_ = character(),
    .dob_id_tmp_ = lubridate::Date(),
    .test_dt_id_tmp_ = lubridate::Date()
  ) %>%
    vctrs::vec_ptype() %>%
    dplyr::relocate(sort(colnames(.)))

  ids_test <- add_nbs_ids(test_nbs_comparison_nbs) %>%
    dplyr::select(dplyr::matches("^[.].*_id_tmp_$")) %>%
    vctrs::vec_ptype() %>%
    dplyr::relocate(sort(colnames(.)))

  expect_vector(ids_test, ptype = ptype)
})

test_that("`add_in_nbs()` identifies matches correctly", {
  crf <- test_nbs_comparison_crf
  nbs <- test_nbs_comparison_nbs
  crf_actual <- add_in_nbs(crf, nbs = nbs) %>%
    dplyr::select("in_nbs", "description") %>%
    dplyr::mutate(
      description = dplyr::case_when(
        .data[["in_nbs"]] == crf[["in_nbs"]] ~ .data[["description"]],
        is.na(.data[["in_nbs"]]) & is.na(crf[["in_nbs"]]) ~ .data[["description"]],
        TRUE ~ paste("[X]", .data[["description"]])
      )
    )
  crf_expected <- dplyr::select(crf, "in_nbs", "description")
  expect_equal(crf_actual$in_nbs, crf_expected$in_nbs)
})

test_that("`add_recent_test()` identifies matches correctly", {
  crf <- test_nbs_comparison_crf
  nbs <- test_nbs_comparison_nbs
  crf_actual <- add_recent_test(crf, nbs = nbs) %>%
    dplyr::select("in_nbs", "recent_test", "description") %>%
    dplyr::mutate(
      description = dplyr::case_when(
        .data[["recent_test"]] == crf[["recent_test"]] ~ .data[["description"]],
        is.na(.data[["recent_test"]]) & is.na(crf[["recent_test"]]) ~ .data[["description"]],
        TRUE ~ paste("[X]", .data[["description"]])
      )
    )
  crf_expected <- dplyr::select(crf, "in_nbs", "recent_test", "description")
  expect_equal(crf_actual, crf_expected)
})

test_that("`add_in_nbs()` propagates missing values correctly", {
  skip_on_ci()
  skip_if_offline()

  data <- add_in_nbs() %>%
    add_crf_ids() %>%
    dplyr::mutate(
      any_na = dplyr::across(dplyr::matches("[.].*_id_tmp_"), is.na) %>%
        rowSums() %>%
        as.logical()
    )

  na_components <- data %>%
    dplyr::filter(.data[["any_na"]]) %>%
    dplyr::select("record_id", "firstname", "lastname", "dob", "specimendate")

  na_in_nbs <- data %>%
    dplyr::filter(is.na(.data[["in_nbs"]])) %>%
    dplyr::select("record_id", "firstname", "lastname", "dob", "specimendate")

  expect_equal(na_in_nbs, na_components)
})

test_that("`add_recent_test()` propagates missing values correctly", {
  skip_on_ci()
  skip_if_offline()

  data <- add_recent_test() %>%
    dplyr::filter(is.na(.data[["in_nbs"]])) %>%
    dplyr::select("record_id", "in_nbs", "recent_test")

  expected_data <- dplyr::mutate(data, recent_test = NA)

  expect_equal(data, expected_data)
})
