create_crf_wb <- function(data = add_recent_test()) {

  data <- data %>%
    dplyr::arrange(as.integer(.data[["record_id"]])) %>%


  wb <- openxlsx::createWorkbook(title = "CRF Workbook")

  wb <- openxlsx::addWorksheet(wb, "NBS - No Recent Test")
  wb <- openxlsx::addWorksheet(wb, "NBS - Recent Test")
  wb <- openxlsx::addWorksheet(wb, "NBS - Test Status Unknown")
  wb <- openxlsx::addWorksheet(wb, "Not NBS")
  wb <- openxlsx::addWorksheet(wb, "Not Determined")

  wb <- openxlsx::writeDataTable(
    wb,
    sheet = "NBS - No Recent Test",
    x = dplyr::filter(data, .data[["in_nbs"]], !.data[["recent_test"]])
  )

  wb <- openxlsx::writeDataTable(
    wb,
    sheet = "NBS - Recent Test",
    x = dplyr::filter(data, .data[["in_nbs"]], .data[["recent_test"]])
  )

  wb <- openxlsx::writeDataTable(
    wb,
    sheet = "NBS - Test Status Unknown",
    x = dplyr::filter(data, .data[["in_nbs"]], is.na(.data[["recent_test"]]))
  )

  wb <- openxlsx::writeDataTable(
    wb,
    sheet = "Not NBS",
    x = dplyr::filter(data, !.data[["in_nbs"]])
  )

  wb <- openxlsx::writeDataTable(
    wb,
    sheet = "Not Determined",
    x = dplyr::filter(data, is.na(.data[["is_nbs"]]))
  )

  wb
}
