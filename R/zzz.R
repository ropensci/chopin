#' @importFrom utils packageVersion
#' @noRd
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage(pkgname, " ", packageVersion(pkgname))
  }
}