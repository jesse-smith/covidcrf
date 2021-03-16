test_that("`prep_crf()` works", {
  crf_nbs <- dplyr::mutate(
    test_crf_ptype,
    in_nbs = logical(),
    recent_test = logical()
  )

  expect_snapshot(prep_crf(crf_nbs))
})

test_that("`transmute_crf()` works", {
  crf_nbs <- dplyr::mutate(
    test_crf_ptype,
    in_nbs = logical(),
    recent_test = logical()
  )

  expect_error(
    transmute_crf(test_crf_ptype),
    regexp = "Can't subset columns that don't exist"
  )
  expect_snapshot(transmute_crf(crf_nbs))
})

test_that("`rename_crf()` works", {

  on_ci    <- isTRUE(as.logical(Sys.getenv("CI")))
  offline  <- !is.null(curl::nslookup("r-project.org", error = FALSE))
  dwnld    <- !(on_ci || offline)

  template <- if (dwnld) download_crf_template() else test_crf_template_fields

  crf_ptype <- test_crf_ptype %>%
    dplyr::mutate(in_nbs = logical(), recent_test = logical()) %>%
    transmute_crf()

  expect_snapshot(rename_crf(crf_ptype, template = template))
})

test_that("`map_crf_cols()` works with REDcap columns", {
  on_ci    <- isTRUE(as.logical(Sys.getenv("CI")))
  offline  <- !is.null(curl::nslookup("r-project.org", error = FALSE))
  dwnld    <- !(on_ci || offline)

  template <- if (dwnld) download_crf_template() else test_crf_template_fields

  expect_equal(
    map_crf_cols(template$field_name, template = template),
    template$field_label
  )
})

test_that("`map_crf_cols()` works with non-REDcap columns", {
  on_ci    <- isTRUE(as.logical(Sys.getenv("CI")))
  offline  <- !is.null(curl::nslookup("r-project.org", error = FALSE))
  dwnld    <- !(on_ci || offline)

  template <- if (dwnld) download_crf_template() else test_crf_template_fields

  new_cols   <- c("new_col_1", "new_col_2")
  all_cols   <- c(new_cols, template$field_name)
  all_labels <- c(new_cols, template$field_label)

  expect_equal(
    map_crf_cols(new_cols, template = template),
    new_cols
  )
  expect_equal(
    map_crf_cols(all_cols, template = template),
    all_labels
  )
})
