# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title Switch spatial data class
#' @description Convert stars into SpatRaster and vice versa; sf into SpatVector and vice versa.
#' @author Insang Song
#' @param input Spat* in terra or sf object.
#' @return Data converted to the other package class (if sf, terra; if terra, sf)
#' @export
switch_packbound <- function(input) {
  stopifnot("Input should be one of sf or Spat* object.\n" = any(methods::is(input, "sf"), methods::is(input, "stars"), methods::is(input, "SpatVector"), methods::is(input, "SpatRaster")))
  cls_input <- check_packbound(input)
  type_input <- check_datatype(input)

  switched <- 
  switch(cls_input,
    sf = switch(type_input,
      vector = terra::vect(input),
      raster = terra::rast(input)
    ),
    terra = switch(type_input,
      vector = sf::st_as_sf(input),
      raster = stars::st_as_stars(input)))

  invisible(switched)
}
