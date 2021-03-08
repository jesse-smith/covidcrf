match_nbs_crf <- function(data = filter_crf()) {
  crf <- dplyr::mutate(
      data,
      .firstname_tmp_ = coviData::std_names(.data[["firstname"]]),
      .lastname_tmp_ = coviData::std_names(.data[["lastname"]]),
      .dob_tmp_ = lubridate::as_date(.data[["dob"]])
    )

  nbs <- load_positive()

}
