#' Load PCR Positives from a Given Date with Investigations File Data
#'
#' `load_positive()` loads positive PCR tests and investigations, and it joins
#' the results together.
#'
#' @param date Optional. Character (formatted as "YYYY-MM-DD") or `Date`.
#'
#' @return A `tibble`
load_positive <- function(date = NULL) {

  # Get date of latest NBS PCR file
  if (rlang::is_empty(date)) {
    date <- coviData::path_pcr() %>%
      stringr::str_extract(pattern = "[0-9]{8}") %>%
      lubridate::mdy()
  } else {
    date <- lubridate::as_date(date)
  }

  # Load PCR file for `date`
  pcr <- load_pcr_positive(date = date) %>%
    janitor::clean_names()

  # Load investigations file
  inv <- load_inv_positive(date = date) %>%
    janitor::clean_names()

  # Generate random suffix to identify variables to drop from inv file
  suffix <- c(LETTERS, letters, 0L:9L) %>%
    sample(size = 23L, replace = TRUE) %>%
    append("_") %>%
    rev() %>%
    append("_") %>%
    rev() %>%
    paste0(collapse = "")

  pcr %>%
    dplyr::left_join(inv, by = "inv_local_id", suffix = c("", suffix)) %>%
    dplyr::select(-dplyr::ends_with(suffix))
}

#' Load Positive Tests or Cases From NBS Snapshot File
#'
#' `load_pcr_positive()` loads positive tests from the latest PCR snapshot file,
#' or the one specified by `date`, if provided. `load_inv_positive()` does the
#' same for the latest investigations snapshot file.
#'
#' @param date Optional. Character (formatted as "YYYY-MM-DD") or `Date`.
#'
#' @return A `tibble`
#'
#' @name load_nbs_positive
#'
#' @aliases load_inv_positive load_pcr_positive
NULL

#' @rdname load_nbs_positive
load_pcr_positive <- function(date = NULL) {

  coviData::path_pcr(date = date) %>%
    read_pcr_positive() %>%
    janitor::clean_names() %>%
    dplyr::filter(
      .data[["inv_case_status"]] %in% c("C", "P"),
      .data[["lab_result"]] %in% c("Positive", "Presumptive Positive")
    )
}

#' @rdname load_nbs_positive
load_inv_positive <- function(date = NULL) {

  coviData::path_inv(date = date) %>%
    read_inv_positive() %>%
    janitor::clean_names() %>%
    dplyr::filter(.data[["inv_case_status"]] %in% c("C", "P")) %>%
    dplyr::left_join(
      coviData::load_report_date() %>% extract(1L:2L),
      by = "inv_local_id"
    ) %>%
    dplyr::mutate(
      report_date = .data[["report_date"]] %>%
        tidyr::replace_na(lubridate::today())
    )
}
