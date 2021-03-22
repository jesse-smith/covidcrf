test_that("`prep_crf()` works", {
  crf_nbs <- dplyr::mutate(
    test_crf_ptype,
    in_nbs = logical(),
    recent_test = logical()
  )

  crf <- prep_crf(crf_nbs, template = test_crf_template_fields)

  expected_crf <- tibble::tibble(
    `Record ID` = integer(),
    `Patient's First Name` = character(),
    `Patient's Last Name` = character(),
    `Patient's Date of Birth` = lubridate::Date(),
    `Specimen Collection Date` = lubridate::Date(),
    `Name of Facility` = character(),
    in_nbs = logical(),
    recent_test = logical()
  )

  expect_vector(crf, ptype = expected_crf)
})

test_that("`transmute_crf()` works", {
  crf_nbs <- dplyr::mutate(
    test_crf_ptype,
    in_nbs = logical(),
    recent_test = logical()
  )

  expected_ptype <- tibble::tibble(
    record_id = integer(),
    firstname = character(),
    lastname = character(),
    dob = lubridate::Date(),
    specimendate = lubridate::Date(),
    facility = character(),
    in_nbs = logical(),
    recent_test = logical()
  )

  expect_error(
    transmute_crf(test_crf_ptype),
    regexp = "Can't subset columns that don't exist"
  )
  expect_vector(transmute_crf(crf_nbs), ptype = expected_ptype)
})

test_that("`rename_crf()` works", {

  on_ci    <- isTRUE(as.logical(Sys.getenv("CI")))
  offline  <- !is.null(curl::nslookup("r-project.org", error = FALSE))
  dwnld    <- !(on_ci || offline)

  template <- dplyr::mutate(
    if (dwnld) download_crf_template() else test_crf_template_fields,
    field_label = janitor::make_clean_names(.data[["field_label"]])
  )

  expected_ptype <- tibble::tibble(
    `Record ID` = character(),
    `Patient's First Name` = character(),
    `Patient's Last Name` = character(),
    `Patient's Date of Birth` = character(),
    `Specimen Collection Date` = character(),
    `Name of Facility` = character(),
    in_nbs = logical(),
    recent_test = logical()
  ) %>%
    janitor::clean_names() %>%
    vctrs::vec_ptype()

  cols <- colnames(expected_ptype)

  crf_ptype <- test_crf_ptype %>%
    dplyr::mutate(in_nbs = logical(), recent_test = logical()) %>%
    rename_crf(template = template) %>%
    dplyr::select(dplyr::contains(cols))

  expect_vector(crf_ptype, ptype = expected_ptype)
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
