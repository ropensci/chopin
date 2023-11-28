# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Validate and repair input vector data
#' @description It tries repairing input vector data.
#' Vector validity violation usually appears in polygon data with
#' self-crossing or
#' hole orders. This function will pass the input_vector object to
#' sf::st_make_valid() (if input_vector is sf) or
#' terra::makeValid() (if input_vector is SpatVector).
#' May take some time depending on the geometry complexity.
#' @author Insang Song
#' @param input_vector One of sf or vect class. Target points of computation.
#' @returns A repaired sf or SpatVector object depending on
#'  the class of input_vector.
#' @export
validate_and_repair_vectors <- function(input_vector) {
  detected <- check_packbound(input_vector)

  validated <- switch(detected,
    terra = terra::makeValid(input_vector),
    sf = sf::st_make_valid(input_vector))

  return(validated)
}

