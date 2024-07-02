#' @noRd
.onLoad <- function(libname, pkgname) {
  options(chopin.backend = "mirai")
  if (interactive()) {
    message(pkgname, " ", packageVersion(pkgname))
  }
}