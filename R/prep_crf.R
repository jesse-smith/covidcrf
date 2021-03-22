#' Prepare CRF Data for Export
#'
#' `prep_crf()` select relevant columns, renames those columns using labels from
#' the REDcap CRF project, and formats the contents of CRF data to prepare for
#' export to Excel. Non-export columns are dropped.
#'
#' @param data Data ready for export preparation, as produced by
#'   \code{\link[covidcrf:add_recent_test]{add_recent_test()}}
#'
#' @param template The template for the CRF REDcap project
#'
#' @return The input `data` with export columns selected, renamed, and formatted
#'
#' @export
prep_crf <- function(
  data = add_recent_test(),
  template = download_crf_template()
) {
  data %>%
    transmute_crf() %>%
    rename_crf(template = template)
}


#' Select and Mutate CRF Columns for Export
#'
#' @param data Data ready for export preparation, as produced by
#'   \code{\link[covidcrf:add_recent_test]{add_recent_test()}}
#'
#' @return The input `data` with export columns selected and renamed; other
#'   columns are dropped
#'
#' @keywords internal
transmute_crf <- function(data = add_recent_test()) {
  data %>%
    dplyr::select(
      c("record_id", "firstname", "lastname", "dob", "specimendate"),
      c("facility", "in_nbs", "recent_test")
    ) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ stringr::str_replace(.x, "^$", replacement = NA_character_)
      ),
      record_id = as.integer(.data[["record_id"]]),
      dob = coviData::std_dates(
        .data[["dob"]],
        force = "dt",
        train = FALSE,
        orders = c("ymd", "ymdT", "ymdR")
      ),
      specimendate = coviData::std_dates(
        .data[["specimendate"]],
        force = "dt",
        train = FALSE,
        orders = c("ymd", "ymdT", "ymdR")
      )
    )
}


#' Rename Export Columns in CRF Data
#'
#' @param data CRF data containing only export columns, as produced by
#'   \code{\link[covidcrf:transmute_crf]{transmute_crf()}}
#'
#' @inheritParams prep_crf
#'
#' @return Input data with REDcap columns renamed
#'
#' @keywords internal
rename_crf <- function(
  data = transmute_crf(),
  template = download_crf_template()
) {
  dplyr::rename_with(data, map_crf_cols, template = template)
}

#' Map Raw Variable Names to Labels in CRF Data
#'
#' @param cols Character. Raw column names in CRF data.
#'
#' @inheritParams prep_crf
#'
#' @return `character` vector with raw column names replaced by column labels
#'
#' @keywords internal
map_crf_cols <- function(cols, template = download_crf_template()) {

  field_name  <- janitor::make_clean_names(template[["field_name"]])
  field_label <- template[["field_label"]]
  remove(template)

  new_cols <- cols[!cols %in% field_name]

  vars   <- c(field_name,  new_cols)
  labels <- c(field_label, new_cols)

  loc <- match(cols, vars, incomparables = NA_character_) %>% extract(!is.na(.))

  vctrs::vec_slice(labels, i = loc)
}
