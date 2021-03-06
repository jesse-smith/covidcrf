#' Magrittr Pipe Utilities
#'
#' The magrittr pipe is a common programming tool in \strong{R} that often makes
#' code much easier to read. In addition to the primary pipe operator, magrittr
#' offers several additional pipes, as well as functions to help perform common
#' infix-operations in a pipe-friendly manner.
#'
#' These functions are currently bulk imported to allow easy reading of code
#' with the pipe operator. This decision may be re-thought at a later date.
#'
#' @md
#' @name utils-magrittr
#' @keywords internal
#' @import magrittr
NULL

#' @importFrom magrittr %>%
#'
#' @export
magrittr::`%>%`

# Suppress "no visible binding for global variable" when using `.`
if (getRversion() >= "2.15.1") utils::globalVariables(".")
