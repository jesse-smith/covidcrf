#' Determine Whether Inviduals are in NBS
#'
#' `add_in_nbs()` adds a `logical` column called `in_nbs` that indicates whether
#' the individual in a row has a match in NBS using their first name, last name,
#' date of birth, and test date. The resulting `in_nbs` column is:
#' \itemize{
#'   \item{`TRUE` if a match is found (including `NA == NA`)}
#'   \item{`FALSE` if no match is found with complete data}
#'   \item{`NA` if no match is found with incomplete data}
#' }
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
add_in_nbs <- function(crf = distinct_crf(), nbs = load_positive()) {

  crf_cols <- colnames(crf)

  crf <- add_crf_ids(crf)
  nbs <- add_nbs_ids(nbs)

  by <- dplyr::intersect(
    stringr::str_subset(colnames(crf), "^[.].*_id_tmp_$"),
    stringr::str_subset(colnames(nbs), "^[.].*_id_tmp_$")
  )

  record_in_nbs <- crf %>%
    dplyr::semi_join(nbs, by = by) %>%
    dplyr::pull("record_id")

  crf %>%
    dplyr::mutate(
      .in_nbs_tmp_ = .data[["record_id"]] %in% record_in_nbs,
      .by_na_tmp_ = rowSums(dplyr::across({{ by }}, is.na)) %>% as.logical(),
      in_nbs = dplyr::if_else(
        !.data[[".in_nbs_tmp_"]] & .data[[".by_na_tmp_"]],
        NA,
        .data[[".in_nbs_tmp_"]]
      )
    ) %>%
    dplyr::select({{ crf_cols }}, "in_nbs")
}

#' Determine Whether Individuals Have Another Test Within `days`
#'
#' `add_recent_test()` adds a column indicating whether a person
#' (identified using first name, last name, and date of birth) has previously
#' tested positive within `days` of the given test date. The resulting
#' `recent_test` column is:
#' \itemize{
#'   \item{`TRUE` if any previous tests are found}
#'   \item{`FALSE` if no previous tests are found with complete data}
#'   \item{`NA` if no previous tests are found with incomplete test date data}
#' }
#'
#' @param crf Case Report Form data, usually output from
#'   \code{\link[covidcrf:add_in_nbs]{add_in_nbs()}}
#'
#' @param nbs Positive NBS data from
#'   \code{\link[covidcrf:load_positive]{load_positive()}}
#'
#' @param days Range of days for matching test dates
#'
#' @return The input `crf` data with a `logical` column `recent_test` added
#'
#' @export
add_recent_test <- function(
  crf = add_in_nbs(),
  nbs = load_positive(),
  days = 90L
) {

  crf_cols <- colnames(crf)

  crf <- add_crf_ids(crf) %>% dplyr::rename(.test_dt_tmp_ = ".test_dt_id_tmp_")
  nbs <- add_nbs_ids(nbs) %>% dplyr::rename(.test_dt_tmp_ = ".test_dt_id_tmp_")

  by <- dplyr::intersect(
    stringr::str_subset(colnames(crf), "^[.].*_id_tmp_$"),
    stringr::str_subset(colnames(nbs), "^[.].*_id_tmp_$")
  )

  crf %>%
    dplyr::left_join(nbs, by = by, suffix = c("", "_nbs_")) %>%
    dplyr::mutate(
      .recent_test_tmp_ = .data[[".test_dt_tmp_"]] %>%
        subtract(.data[[".test_dt_tmp__nbs_"]]) %>%
        as.integer() %>%
        dplyr::between(1L, as.integer(days))
    ) %>%
    dplyr::group_by(.data[["record_id"]]) %>%
    # The not-all-not-true pattern is `TRUE` if any are `TRUE`, `FALSE` if all
    # all `FALSE`, and `NA` otherwise (i.e. none are `TRUE`, some are `FALSE`)
    dplyr::mutate(.recent_test_tmp_ = !all(!.data[[".recent_test_tmp_"]])) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data[["record_id"]], .keep_all = TRUE) %>%
    dplyr::mutate(
      .by_na_tmp_ = rowSums(dplyr::across({{ by }}, is.na)) %>% as.logical(),
      recent_test = dplyr::if_else(
        !.data[[".recent_test_tmp_"]] & .data[[".by_na_tmp_"]],
        NA,
        .data[[".recent_test_tmp_"]]
      )
    ) %>%
    dplyr::select({{ crf_cols }}, "recent_test")
}

#' Add ID Columns to CRF Data
#'
#' @param data Data from `filter_crf()`
#'
#' @return `data` with added columns, all with suffix `_id_tmp_`
#'
#' @keywords internal
add_crf_ids <- function(data = filter_crf()) {
  dplyr::mutate(
    data,
    .firstname_id_tmp_ = .data[["firstname"]] %>%
      coviData::std_names() %>%
      stringr::str_replace("^$", NA_character_),
    .lastname_id_tmp_ = .data[["lastname"]] %>%
      coviData::std_names() %>%
      stringr::str_replace("^$", NA_character_),
    .dob_id_tmp_ = lubridate::as_date(.data[["dob"]]),
    .test_dt_id_tmp_ = lubridate::as_date(.data[["specimendate"]])
  )
}

#' Add ID Columns to NBS Data
#'
#' @param data Data from `filter_crf()`
#'
#' @return `data` with added columns, all with suffix `_id_tmp_`
#'
#' @keywords internal
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
    ),
    .test_dt_id_tmp_ = coviData::std_dates(
      .data[["specimen_coll_dt"]],
      force = "dt",
      train = FALSE,
      orders = "ymdT"
    )
  )
}
