# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title Return the package the input object is based on
#' @description Detect whether the input object is sf or Spat* object.
#' @author Insang Song
#' @param input Spat* in terra or sf object.
#' @return A character object; one of 'terra' and 'sf'
#' @export
check_packbound <- function(input) {
  # cl_inobj = class(input)[1]
  stopifnot("Input should be one of sf or Spat* object.\n" = any(is(input, "sf"), is(input, "stars"), is(input, "SpatVector"), is(input, "SpatRaster")))
  if (is(input, "SpatVector") || is(input, "SpatRaster")) {
    return("terra")
  }
  return("sf")
}

check_datatype <- function(input) {
  stopifnot("Input should be one of sf or Spat* object.\n" = any(is(input, "sf"), is(input, "stars"), is(input, "SpatVector"), is(input, "SpatRaster")))
  if (any(is(input, "SpatVector"), is(input, "sf"))) {
    return("vector")
  }
  if (any(is(input, "SpatRaster"), is(input, "stars"))) {
    return("raster")
  }
}
