# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title Return the package the input object is based on
#' @description Detect whether the input object is sf or Spat* object.
#' @title check_packbound
#' @author Insang Song
#' @param input Spat* in terra or sf object.
#' @return A character object; one of terra and sf
#' @export
check_packbound <- function(input) {
  cl_inobj = class(input)[1]
  stopifnot("Input should be one of sf or Spat* object.\n" = is(input, "sf") || is(input, "stars") || is(input, "SpatVector") || is(input, "SpatRaster"))
  if (is(cl_inobj, "SpatVector") || is(cl_inobj, "SpatRaster")) {
    return("terra")
  }
  return("sf")
}
