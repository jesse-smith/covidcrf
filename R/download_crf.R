#' Download All Records from CRF REDcap Project
#'
#' `download_crf()` downloads all records from the CRF REDcap project and cleans
#' variables names with \code{\link[janitor:clean_names]{clean_names()}}. The
#' download will retry up to 12 times before failing. All variables are returned
#' as `character`.
#'
#' @param api_token API token/key for the CRF REDcap project
#'
#' @return A `tibble` in `character` format
#'
#' @export
download_crf <- function(api_token = Sys.getenv("redcap_CRF_token")) {
  uri <- "https://redcap.shelbycountytn.gov/api/"

  params <- list(
    token   = api_token,
    content = "record",
    format  = "json",
    rawOrLabel = "label",
    rawOrLabelHeaders = "raw",
    exportCheckboxLabel = "true"
  )

  httr::RETRY(
    "POST",
    url = uri,
    body = params,
    encode = "form",
    times = 12
  ) %>%
    httr::stop_for_status(
      task = paste0("download case reports: ", httr::content(.))
    ) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble() %>%
    janitor::clean_names()
}
