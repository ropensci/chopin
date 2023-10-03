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

#' Check if the data extent is inside the reference bounding box
#' 
#' @description One of the most common errors in spatial computation is rooted in the entirely or partly incomparable spatial extents of input datasets. This function returns whether your data is inside the target computational extent. It is assumed that you know and have the exact computational region. This function will return TRUE if the reference region completely contains your data's extent and FALSE otherwise.
#' @param data_query sf*/stars/SpatVector/SpatRaster object.
#' @param reference sf*/stars/SpatVector/SpatRaster object or a named numeric vector with four names (xmin, ymin, xmax, and ymax).
#' @param reference_crs Well-known-text-formatted or EPSG code of the reference's coordinate system. Only required when a named numeric vector is passed to reference. 
#' @return TRUE (the queried data extent is completely within the reference bounding box) or FALSE 
#' @author Insang Song \email{geoissong@@gmail.com}
#' 
#' @export
check_bbox <- function(
  data_query, reference, reference_crs = NULL
) {
  if (is.numeric(reference) && is.null(reference_crs)) {
    stop("CRS should be entered when the reference extent is a vector.\n")
  }
  if (is.numeric(reference) && !is.null(reference_crs)) {
    reference = sf::st_as_sfc(sf::st_bbox(reference), crs = reference_crs)
  }
  query_crs = check_crs(data_query)
  ref_crs = check_crs(reference)
  if (is.na(query_crs) || is.null(query_crs)) {
    stop("The dataset you queried has no CRS. Please make sure your dataset has the correct CRS.\n")
  }

  # ...

  check_result
  return(check_result)
}



#' Check Coordinate Reference System
#' @param x sf/stars/SpatVector/SpatRaster object.
#' @return A st_crs or crs object.
#' @description 
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples 
#' # data
#' library(sf)
#' ncpath = system.file("shape/nc.shp", package = "sf")
#' nc = read_sf(ncpath)
#' check_crs(nc)
#' 
#' @export 
check_crs <- function(x) {
    stopifnot("Input is invalid.\n" = (is(x, "sf") || is(x, "stars") || is(x, "SpatVector") || is(x, "SpatRaster")))
    
    if (is(x, "sf") || is(x, "stars")) {
        crs_wkt = sf::st_crs(x)
    } else {
        crs_wkt = terra::crs(x)
    }

    stopifnot("No CRS is defined in the input. Please consult the metadata or the data source.\n" = !is.na(crs_wkt) || crs_wkt != "")
    return(crs_wkt)
}

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


