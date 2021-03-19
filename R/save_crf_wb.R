#' Save Workbook with CRF Data to Disk
#'
#' `save_crf_wb()` saves the output of
#' \code{\link[covidcrf:create_crf_wb]{create_crf_wb()}} to the specified
#' directory under the file name `crf_nbs_overlap_YYYY-MM-DD.xlsx`, where
#' `YYYY-MM-DD` is the date at the time of saving.
#'
#' @param wb An openxlsx `Workbook` object, as output by `create_crf_wb()`
#'
#' @param dir The directory to save the file
#'
#' @param force Should an existing file at this location be overwritten? Unlike
#'   most save functions in the covid* package universe, the default here is
#'   `TRUE`.
#'
#' @return The path to the saved file (invisibly)
#'
#' @export
save_crf_wb <- function(
  wb = create_crf_wb(),
  dir = "V:/EPI DATA ANALYTICS TEAM/CRF/CRF_NBS",
  force = TRUE
) {
  file <- paste0("CRF_NBS_", lubridate::today())
  path <- coviData::path_create(dir, file, ext = "xlsx")

  coviData::assert_any(
    force,
    !fs::file_exists(path),
    message = paste(
      "A file already exists at this location.",
      "To overwrite, set `force = TRUE`."
    )
  )

  success <- openxlsx::saveWorkbook(
    wb,
    file = path,
    overwrite = force,
    returnValue = TRUE
  )

  coviData::assert(success, message = "CRF data was not saved successfully")

  invisible(path)
}
