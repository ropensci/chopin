# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Turn on logging
#' @author Insang Song
#' @param expr expression. Any function call to be logged.
#' @param dolog logical(1). Will the messages be logged.
#' @param logpath character(1). Log file path with the full log file name.
#' @return Nothing. It will export a log file in the specified path as logpath.
#' @export
initate_log <- function(
  expr,
  dolog = FALSE,
  logpath) {
  if (!dolog) {
    return(NULL)
  }
  logr::log_path(logpath)
  try(expr)
  logr::log_close()
  return(NULL)
}


