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

usethis::use_data(test_filter_crf, internal = TRUE, overwrite = TRUE)
