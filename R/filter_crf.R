#' Filter to Cases of Interest
#'
#' `filter_crf()` filters out cases which have previously answered the survey
#' or were not at work within `days` days of their onset/test date.
#'
#' @param data A data frame with data from the CRF REDcap project, as output by
#'   \code{\link[covidcrf:download_crf]{download_crf()}}
#'
#' @param days The maxmimum allowed difference between last day worked
#'   (`last_worked`) and test date (`specimendate`)
#'
#' @param na_keep Should missing data be kept or excluded?
#'
#' @return The input `data` filtered according to the description above
#'
#' @export
filter_crf <- function(
  data = download_crf(),
  days = 5L,
  na_keep = TRUE
) {
  data %>%
    dplyr::mutate(
      .last_work_tmp_ = lubridate::as_date(.data[["last_work"]]),
      .dt_min_tmp_ = dplyr::coalesce(
        lubridate::as_date(.data[["symptdate"]]),
        lubridate::as_date(.data[["specimendate"]])
      ) - lubridate::days(days),
      .within_days_tmp_ = tidyr::replace_na(
        .data[[".last_work_tmp_"]] > .data[[".dt_min_tmp_"]],
        replace = na_keep
      ),
      .already_surveyed_tmp_ = tidyr::replace_na(
        as.integer(.data[["multi_ltcf_2"]]) != 1L,
        replace = na_keep
      )
    ) %>%
    dplyr::filter(
      .data[[".already_surveyed_tmp_"]],
      .data[[".within_days_tmp_"]]
    ) %>%
    dplyr::select(
      -c(".last_work_tmp_", ".dt_min_tmp_"),
      -c(".within_days_tmp_", ".already_surveyed_tmp_")
    )
}
