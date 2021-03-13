test_that("`distinct_crf()` ", {

  expected_crf <- dplyr::select(
    test_nbs_comparison_crf,
    -c("in_nbs", "recent_test", "description")
  )

  crf <- expected_crf %>%
    dplyr::add_row(
      tibble::tibble(
        record_id = "8",
        firstname = "John",
        lastname = "Smith",
        dob = "1989-01-01",
        specimendate = "2021-01-10"
      )
    )
  expect_equal(distinct_crf(crf), expected_crf)
})

test_that("`distinct_crf()` produces distinct rows", {
  expected_data <- add_crf_ids(filter_crf()) %>%
    dplyr::arrange(as.integer(.data[["record_id"]])) %>%
    dplyr::distinct(dplyr::across(dplyr::matches("[.].*_id_tmp_")), .keep_all = TRUE) %>%
    dplyr::select("record_id", "firstname", "lastname", "dob", "specimendate")

  data <- dplyr::select(
    distinct_crf(),
    c("record_id", "firstname", "lastname", "dob", "specimendate")
  )

  expect_equal(data, expected_data)
})
