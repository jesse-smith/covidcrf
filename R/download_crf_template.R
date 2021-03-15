download_crf_template <- function(api_token = Sys.getenv("redcap_CRF_token")) {
  uri <- "https://redcap.shelbycountytn.gov/api/"

  params <- list(
    token   = api_token,
    content = "metadata",
    format  = "json"
  )

  httr::RETRY(
    "POST",
    url = uri,
    body = params,
    encode = "form",
    times = 12
  ) %>%
    httr::stop_for_status(
      task = paste0("download case report template: ", httr::content(.))
    ) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble() %>%
    janitor::clean_names()
}
