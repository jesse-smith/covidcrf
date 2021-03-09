#' Determine Whether Inviduals are in NBS
#'
#' `add_in_nbs()` adds a `logical` column called `in_nbs` that indicates whether
#' the individual in a row has a match in NBS using their first name, last name,
#' and date of birth.
#'
#' @param crf Case Report Form data, usually from
#'   \code{\link[covidcrf:filter_crf]{filter_crf()}}
#'
#' @param nbs Positive NBS data from
#'   \code{\link[covidcrf:load_positive]{load_positive()}}
#'
#' @return The input `crf` data with a `logical` column `in_nbs` added
#'
#' @export
add_in_nbs <- function(crf = filter_crf(), nbs = load_positive()) {

  crf <- add_crf_ids(crf)
  nbs <- add_nbs_ids(nbs)

  by <- dplyr::intersect(
    stringr::str_subset(colnames(crf), "[.].*_id_tmp_"),
    stringr::str_subset(colnames(nbs), "[.].*_id_tmp_")
  )

  rows_in_nbs <- crf %>%
    dplyr::mutate(.row_id_tmp_ = dplyr::row_number())
    dplyr::semi_join(nbs, by = by) %>%
    dplyr::pull(".row_id_tmp_")

  crf %>%
    dplyr::mutate(in_nbs = .data[["row_id_tmp_"]] %in% rows_in_nbs) %>%
    dplyr::select(-dplyr::matches("[.].*_id_tmp_"))
}

#' Determine Whether Individuals Have Another Test Within `days`
#'
#' `add_recent_test()` adds a column indicating whether a person has tested
#' positive within `days` of the given test date.
#'
#' @param crf Case Report Form data, usually output from
#'   \code{\link[covidcrf:crf_in_nbs]{crf_in_nbs()}}
#'
#' @param nbs Positive NBS data from
#'   \code{\link[covidcrf:load_positive]{load_positive()}}
#'
#' @param days Range of days for matching test dates
#'
#' @return The input `crf` data with a `logical` `recent_test`
#'
#' @export
add_recent_test <- function(
  crf = crf_in_nbs(),
  nbs = load_positive(),
  days = 90L
) {

  crf <- add_crf_ids(crf)
  nbs <- add_nbs_ids(nbs)

  by <- dplyr::intersect(
    stringr::str_subset(colnames(crf), "[.].*_id_tmp_"),
    stringr::str_subset(colnames(nbs), "[.].*_id_tmp_")
  )

  crf_tmp <- dplyr::mutate(
    crf,
    .row_id_tmp_ = dplyr::row_number(),
    .test_dt_tmp_ = lubridate::as_date(.data[["specimendate"]])
  )
  remove(crf)

  nbs_tmp <- dplyr::mutate(
    nbs,
    .test_dt_tmp_ = coviData::std_dates(
      .data[["specimen_coll_dt"]],
      force = "dt",
      train = FALSE,
      orders = "YmdT"
    )
  )
  remove(nbs)

  crf_tmp %>%
    dplyr::left_join(nbs_tmp, by = by, suffix = c("", "_nbs_")) %>%
    dplyr::mutate(
      .recent_test_tmp_ = .data[[".test_dt_tmp_"]] %>%
        subtract(.data[["test_dt_tmp__nbs_"]]) %>%
        as.integer() %>%
        abs() %>%
        is_weakly_less_than(days) %>%
        tidyr::replace_na(na_keep)
    ) %>%
    dplyr::group_by(.data[[".row_id_tmp_"]]) %>%
    dplyr::mutate(recent_test = any(.data[[".recent_test_tmp_"]])) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data[[".row_id_tmp_"]], .keep_all = TRUE) %>%
    dplyr::select(
      -dplyr::ends_with("_nbs_"),
      -dplyr::matches("[.].*_id_tmp_"),
      -".test_dt_tmp_"
    )
}

#' Add ID Columns to CRF Data
#'
#' @param data Data from `filter_crf()`
#'
#' @return `data` with added columns, all with suffix `_id_tmp_`
add_crf_ids <- function(data = filter_crf()) {
  dplyr::mutate(
    data,
    .firstname_id_tmp_ = coviData::std_names(.data[["firstname"]]),
    .lastname_id_tmp_ = coviData::std_names(.data[["lastname"]]),
    .dob_id_tmp_ = lubridate::as_date(.data[["dob"]])
  )
}

#' Add ID Columns to NBS Data
#'
#' @param data Data from `filter_crf()`
#'
#' @return `data` with added columns, all with suffix `_id_tmp_`
add_nbs_ids <- function(data = load_positive()) {
  dplyr::mutate(
    data,
    .firstname_id_tmp_ = coviData::std_names(.data[["patient_first_name"]]),
    .lastname_id_tmp_ = coviData::std_names(.data[["patient_last_name"]]),
    .dob_id_tmp_ = coviData::std_dates(
      .data[["patient_dob"]],
      force = "dt",
      train = FALSE,
      orders = "ymdT"
    )
  )
}
