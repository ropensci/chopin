# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Check if the boundary of the vector/raster object is inside the reference
#' @param input_object sf/stars/SpatVector/SpatRaster object.
#' @param reference sf/stars/SpatVector/SpatRaster object.
#' @return logical
#' @author Insang Song \email{geoissong@@gmail.com}
#' @export 
check_within_reference <- function(input_object, reference) {
    stopifnot("Input is invalid.\n" = (is(x, "sf") || is(x, "stars") || is(x, "SpatVector") || is(x, "SpatRaster")))
    stopifnot("Reference is invalid.\n" = (is(x, "sf") || is(x, "stars") || is(x, "SpatVector") || is(x, "SpatRaster")))

  bbox_input <- input_object
    sf::st_bbox() |>
    sf::st_as_sfc()
  
  bbox_reference = reference |>
    sf::st_bbox() |>
    sf::st_as_sfc()

  iswithin = sf::st_covered_by(bbox_input, bbox_reference)
  iswithin = length(iswithin[[1]])
  iswithin = (iswithin == 1)
  invisible(iswithin)
}


