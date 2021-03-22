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
  dir = paste0(
    "V:/OPERATIONS Surveillance Coordinator/Clusters/Case Report Forms/CRF_NBS"
  ),
  min = 7L
) {

  regex <- ".*/CRF_NBS[^/]*[.]xlsx$"

  old_files <- old_crf_files(dir = dir, regex = regex)

  archive_paths <- coviData::path_create(
    fs::path_dir(old_files),
    "archive",
    fs::path_file(old_files)
  )

  fs::file_move(old_files, new_path = archive_paths)

  coviData::trim_backups(
    directory = coviData::path_create(dir, "archive"),
    pattern = regex,
    min_backups = min
  )
}

#' Identify Old CRF Files
#'
#' `old_crf_files()` identifies all CRF files in a directory except the most
#' recent
#'
#' @param dir Directory to search
#'
#' @param regex Regular expression matching CRF files
#'
#' @return An `fs_path` vector
#'
#' @keywords internal
old_crf_files <- function(
  dir = paste0(
    "V:/OPERATIONS Surveillance Coordinator/Clusters/Case Report Forms/CRF_NBS"
  ),
  regex = ".*/CRF_NBS[^/]*[.]xlsx$"
) {
  fs::dir_info(
    coviData::path_create(dir),
    type = "file",
    regexp = regex
  ) %>%
    dplyr::filter(
      .data[["birth_time"]] != max(.data[["birth_time"]], na.rm = TRUE)
    ) %>%
    dplyr::pull("path") %>%
    vctrs::vec_sort()
}
