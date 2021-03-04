download_crf <- function(api_token = Sys.getenv("redcap_CRF_token")) {
  uri <- "https://redcap.shelbycountytn.gov/api/"

  params <- list(
    token   = api_token,
    content = "record",
    format  = "json"
  )

  httr::RETRY(
    "POST",
    url = uri,
    body = params,
    encode = "form",
    times = 1
  ) %>%
    httr::stop_for_status(
      task = paste0("download case reports: ", httr::content(.))
    )
}
