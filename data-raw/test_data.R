test_filter_crf <- tibble::tribble(
  ~ last_work,  ~ symptdate, ~ specimendate, ~ multi_ltcf_2, ~ keep, ~ description,
  "2021-01-01", "2021-01-01",   "2021-01-02",            "2",   TRUE, "Test on complete data",
  "", "2021-01-01",   "2021-01-02",            "2",   TRUE, "Test missingness in `last_work`",
  "2021-01-01",           "",   "2021-01-02",            "2",   TRUE, "Test missingness in `syptdate`",
  "2021-01-01", "2021-01-01",             "",            "2",   TRUE, "Test missingness in `specimendate`",
  "2021-01-01", "2021-01-01",   "2021-01-02",             "",   TRUE, "Test missingness in `multi_ltcf_2`",
  "2021-01-01", "2022-01-01",   "2022-01-02",            "2",  FALSE, "Test date-based exclusion",
  "2021-01-01", "2021-01-01",   "2021-01-02",            "1",  FALSE, "Test multi-facility exclusion",
  "2021-01-01",           "",   "2022-01-02",            "2",  FALSE, "Test fallback to specimendate",
  "2021-01-01", "2021-01-01",   "2022-01-02",            "2",   TRUE, "Test colescing order",
  "2021-01-01",           "",             "",            "1",  FALSE, "Test missingness in both onset columns",
  "",           "",             "",             "",   TRUE, "Test missingness in all columns"
)

test_nbs_comparison_crf <- tibble::tribble(
   ~ record_id, ~ firstname, ~ lastname,        ~ dob, ~ specimendate, ~ in_nbs, ~ recent_test, ~ description,
           "1",      "John",    "Smith", "1989-01-01",   "2021-01-10",     TRUE,         FALSE, "Test exact match",
           "2",      "John", "Smithers", "1989-01-01",   "2021-01-10",    FALSE,            NA, "Test complete non-match",
           "3",          "",    "Smith", "1989-01-01",   "2021-01-10",       NA,            NA, "Test missing `firstname`",
           "4",      "John",         "", "1989-01-01",   "2021-01-10",       NA,            NA, "Test missing `lastname`",
           "5",      "John",    "Smith",           "",   "2021-01-10",       NA,            NA, "Test missing `dob`",
           "6",      "John",    "Smith", "1989-01-01",             "",       NA,            NA, "Test missing `specimendate`",
           "7",      "John",    "Smith", "1989-01-01",   "2021-01-15",    FALSE,          TRUE, "Test recent"
)

test_nbs_comparison_nbs <- tibble::tribble(
  ~ patient_first_name, ~ patient_last_name,          ~ patient_dob,     ~ specimen_coll_dt,
                "John",             "Smith", "1989-01-01T00:00:01Z", "2021-01-10T00:00:01Z"
)

test_crf_template_fields <- dplyr::select(
  download_crf_template(),
  c("field_name", "field_label")
)

test_crf_ptype <- vctrs::vec_ptype(download_crf())

usethis::use_data(
  test_filter_crf,
  test_nbs_comparison_crf,
  test_nbs_comparison_nbs,
  test_crf_template_fields,
  test_crf_ptype,
  internal = TRUE,
  overwrite = TRUE
)
