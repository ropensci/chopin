# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title check_crs2: Coordinate system checker
#' @description The input is checked whether its coordinate system is present. If not, it is reprojected to EPSG:5179.
#' @param input Input object one of sf or terra::Spat* object
#' @return A (reprojected) sf or SpatVector object.
#' @export
check_crs2 <- function(input, crs_standard = "EPSG:4326") {
  check_crs.sf <- function(input){
    if (is.na(st_crs(input))){cat('Please check the coordinate system or its EPSG code of your input object.'); return(NULL)}
    if (sf::st_crs(input)$epsg == crs_standard ) {
      return(input)
    } 
    input_transformed <- sf::st_transform(input, sf::st_crs(crs_standard))
    return(input_transformed)
  }

  check_crs.terra <- function(input) {
    if (terra::crs(input, describe = TRUE)$code == crs_standard) {
      return(input)
    }
    input_transformed = terra::project(input, terra::crs(crs_standard))
    return(input_transformed)
  }
    detected = check_packbound(input)
    switch(detected,
          terra = check_crs.terra(input),
          sf = check_crs.sf(input))
  }
