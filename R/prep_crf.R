prep_crf <- function(data = add_recent_test()) {
  data %>%
    transmute_crf() %>%
    janitor::remove_empty(which = "rows") %>%
    rename_crf()
}

transmute_crf <- function(data = add_recent_test()) {
  data %>%
    dplyr::select(
      c("record_id", "firstname", "lastname", "dob", "specimendate"),
      c("in_nbs", "recent_test")
    ) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ stringr::str_replace(.x, "^$", replacement = NA_character_)
      ),
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

rename_crf <- function(data = transmute_crf()) {
  dplyr::rename_with(data, map_crf_cols)
}

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
