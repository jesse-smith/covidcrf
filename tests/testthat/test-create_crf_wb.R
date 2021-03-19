test_that("`create_crf_wb()` outputs `Workbook` object", {
  crf_nbs <- dplyr::mutate(
    test_crf_ptype,
    in_nbs = logical(),
    recent_test = logical()
  )
  crf <- prep_crf(crf_nbs, template = test_crf_template_fields)
  wb <- create_crf_wb(crf)

  class <- "Workbook"
  attr(class, "package") <- "openxlsx"

  expect_equal(class(wb), class)
})

test_that("`create_crf_wb()` outputs correct sheets", {
  crf_nbs <- dplyr::mutate(
    test_crf_ptype,
    in_nbs = logical(),
    recent_test = logical()
  )
  crf <- prep_crf(crf_nbs, template = test_crf_template_fields)
  wb <- create_crf_wb(crf)

  sheets <- c(
    "Cases (NBS - No Recent Test)",
    "NBS - Recent Test",
    "NBS - Test Status Unknown",
    "Not NBS",
    "Not Determined"
  )

  expect_equal(names(wb), sheets)
})
