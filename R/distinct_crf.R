#' Deduplicate Cases and Coalesce Information Among Duplicates
#'
#' `distinct_crf()` de-duplicates cases in the CRF data using first name,
#' last name, date of birth, and specimen collection date. Where possible,
#' missing data in one duplicate record is filled in from data in others.
#'
#' @param data A `tibble` with CRF data, usually output from
#'   \code[\link[covidcrf:filter_crf]{filter_crf()}]
#'
#' @return The de-duplicated and coalesced input, sorted by `record_id`
#'
#' @export
distinct_crf <- function(data = add_recent_test()) {

  cols <- colnames(data)

  data %>%
    add_crf_ids() %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ stringr::str_replace(.x, "^$", replacement = NA_character_)
      )
    ) %>%
    dplyr::arrange(
      dplyr::desc(.data[[".test_dt_id_tmp_"]]),
      .data[[".firstname_id_tmp_"]],
      .data[[".lastname_id_tmp_"]],
      .data[[".dob_id_tmp_"]]
    ) %>%
    coviData::coalesce_dupes(
      .data[[".firstname_id_tmp_"]],
      .data[[".lastname_id_tmp_"]],
      .data[[".dob_id_tmp_"]],
      .data[[".test_dt_id_tmp_"]]
    ) %>%
    dplyr::select({{ cols }}) %>%
    dplyr::arrange(as.integer(.data[["record_id"]])) %>%
    dplyr::mutate(
      dplyr::across(where(is.character), ~ stringr::str_replace_na(.x, ""))
    )
}
