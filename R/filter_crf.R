#' Filter to Cases of Interest
#'
#' `filter_crf()` filters out cases which have previously answered the survey
#' or were not at work within `days` days of their onset/test date.
#'
#' @param data A data frame with data from the CRF REDcap project, as output by
#'   \code{\link[covidcrf:download_crf]{download_crf()}}
#'
#' @return The input `data` filtered according to the description above
#'
#' @export
filter_crf <- function(data = download_crf(), days = 5L) {
  data %>%
    dplyr::mutate(
      .last_work_tmp_ = lubridate::as_date(.data[["last_work"]]),
      .dt_min_tmp_ = dplyr::coalesce(
        lubridate::as_date(.data[["specimendate"]]),
        lubridate::as_date(.data[["symptdate"]])
      ) - lubridate::days(days),
      .within_days_tmp_ = .data[[".last_work_tmp_"]] > .data[[".dt_min_tmp_"]]
    ) %>%
    dplyr::filter(
      .data[["multi_ltcf_2"]] != "1",
      .data[[".within_days_tmp_"]]
    ) %>%
    dplyr::select(-c(".last_work_tmp_", ".dt_min_tmp_", ".within_days_tmp_"))
}
