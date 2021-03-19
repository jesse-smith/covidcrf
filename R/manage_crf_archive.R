#' Manage CRF Data Archive
#'
#' `manage_crf_archive()` deletes old version of the CRF data. By default,
#' it will retain at least the 7 most recent versions, and will retain all
#' versions created in the last 7 days.
#'
#' @param dir The CRF data directory
#'
#' @param min The minimum number of files and days to keep
#'
#' @inherit coviData::trim_backups return
#'
#' @export
manage_crf_archive <- function(
  dir = "V:/EPI DATA ANALYTICS TEAM/CRF/CRF_NBS",
  min = 7L
) {
  coviData::trim_backups(
    directory = coviData::path_create(dir),
    pattern = ".*/CRF_NBS(_[0-9]{1-4}-[0-9]{1-2}-[0-9]{1-4})?[.]xlsx$",
    min_backups = min
  )
}
