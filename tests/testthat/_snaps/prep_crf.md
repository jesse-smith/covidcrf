# `prep_crf()` works

    Code
      prep_crf(crf_nbs)
    Output
      # A tibble: 0 x 7
      # ... with 7 variables: `Record ID` <chr>, `Patient's First Name` <chr>,
      #   `Patient's Last Name` <chr>, `Patient's Date of Birth` <date>, `Specimen
      #   Collection Date` <date>, in_nbs <lgl>, recent_test <lgl>

# `transmute_crf()` works

    Code
      transmute_crf(crf_nbs)
    Output
      # A tibble: 0 x 7
      # ... with 7 variables: record_id <chr>, firstname <chr>, lastname <chr>,
      #   dob <date>, specimendate <date>, in_nbs <lgl>, recent_test <lgl>

# `rename_crf()` works

    Code
      rename_crf(crf_ptype, template = template)
    Output
      # A tibble: 0 x 7
      # ... with 7 variables: `Record ID` <chr>, `Patient's First Name` <chr>,
      #   `Patient's Last Name` <chr>, `Patient's Date of Birth` <date>, `Specimen
      #   Collection Date` <date>, in_nbs <lgl>, recent_test <lgl>

