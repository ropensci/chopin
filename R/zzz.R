#' @noRd
.onLoad <- function(libname, pkgname) {
  if (interactive()) {
    message(pkgname, " ", packageVersion(pkgname))
  }
}