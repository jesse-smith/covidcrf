#' Create Excel Workbook with Categorized Sheets of CRF Data
#'
#' `create_crf_wb()` creates an Excel workbook object using the
#' \href{https://ycphs.github.io/openxlsx/}{openxlsx} package. The data are
#' categorized into 5 sheets:
#' \describe{NBS Cases (NBS - No Recent Test)}{Records in NBS without a recent
#' test. New cases.}
#' \describe{NBS - Recent Test}{Records in NBS, but with another recent test.
#' Not new cases, since they've already tested positive recently.}
#' \describe{NBS - Test Status Unknown}{Records in NBS, but without sufficient
#' information to determine whether they've had a prior positive test recently.}
#' \describe{Not NBS}{Records not (yet) in NBS}
#' \describe{Not Determined}{Records without sufficient identifying information
#' to determine whether they're in NBS.}
#'
#' @param data CRF data prepared by \code{\link[covidcrf:prep_crf]{prep_crf()}}
#'
#' @return An openxlsx WorkBook object
#'
#' @export
create_crf_wb <- function(data = prep_crf()) {

  data <- dplyr::arrange(data, as.integer(.data[["Record ID"]]))

  wb <- openxlsx::createWorkbook(title = "CRF Workbook")

  openxlsx::addWorksheet(wb, "Cases (NBS - No Recent Test)")
  openxlsx::addWorksheet(wb, "NBS - Recent Test")
  openxlsx::addWorksheet(wb, "NBS - Test Status Unknown")
  openxlsx::addWorksheet(wb, "Not NBS")
  openxlsx::addWorksheet(wb, "Not Determined")

  openxlsx::writeDataTable(
    wb,
    sheet = "Cases (NBS - No Recent Test)",
    x = dplyr::filter(data, .data[["in_nbs"]], !.data[["recent_test"]])
  )

  openxlsx::writeDataTable(
    wb,
    sheet = "NBS - Recent Test",
    x = dplyr::filter(data, .data[["in_nbs"]], .data[["recent_test"]])
  )

  openxlsx::writeDataTable(
    wb,
    sheet = "NBS - Test Status Unknown",
    x = dplyr::filter(data, .data[["in_nbs"]], is.na(.data[["recent_test"]]))
  )

  openxlsx::writeDataTable(
    wb,
    sheet = "Not NBS",
    x = dplyr::filter(data, !.data[["in_nbs"]])
  )

  openxlsx::writeDataTable(
    wb,
    sheet = "Not Determined",
    x = dplyr::filter(data, is.na(.data[["in_nbs"]]))
  )

  wb
}
