# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title Return the package the input object is based on
#' @description Detect whether the input object is sf or Spat* object.
#' @author Insang Song
#' @param input Spat* in terra or sf object.
#' @return A character object; one of 'terra' and 'sf'
#' @export
check_packbound <- function(input) {
  # cl_inobj = class(input)[1]
  stopifnot("Input should be one of sf or Spat* object.\n" = any(methods::is(input, "sf"), methods::is(input, "stars"), methods::is(input, "SpatVector"), methods::is(input, "SpatRaster")))
  if (methods::is(input, "SpatVector") || methods::is(input, "SpatRaster")) {
    return("terra")
  }
  return("sf")
}

check_datatype <- function(input) {
  stopifnot("Input should be one of sf or Spat* object.\n" = any(methods::is(input, "sf"), methods::is(input, "stars"), methods::is(input, "SpatVector"), methods::is(input, "SpatRaster")))
  if (any(methods::is(input, "SpatVector"), methods::is(input, "sf"))) {
    return("vector")
  }
  if (any(methods::is(input, "SpatRaster"), methods::is(input, "stars"))) {
    return("raster")
  }
}

#' @title check_crs_align: Check coordinate system then reproject
#' @description The input is checked whether its coordinate system is
#'  present. If not, it is reprojected to the CRS specified in
#' \code{crs_standard}.
#' @param input Input object one of sf or terra::Spat* object
#' @param crs_standard character(1). A standard definition of
#'  coordinate reference system. Default is "EPSG:4326"
#'  Consult [epsg.io](https://epsg.io) for details of other CRS.
#' @return A (reprojected) sf or SpatVector object.
#' @export
check_crs_align <-
  function(
    input,
    crs_standard = "EPSG:4326") {
  
  if (!is.character(crs_standard)) {
    stop("crs_standard seems to be in invalid format.
      It should be '[authority]:[code]' format.
      Please refer to epsg.io and ?sf::st_crs or ?terra::crs.\n")
  }
  check_crs_sf <- function(input, crs_standard) {
    invisible(check_crs(input))
    input_crs <- sf::st_crs(input)$epsg
    standard_crs <- sf::st_crs(crs_standard)$epsg
    if (input_crs == standard_crs) {
      return(input)
    } 
    input_transformed <-
      sf::st_transform(input, sf::st_crs(crs_standard))
    return(input_transformed)
  }

  check_crs_terra <- function(input, crs_standard) {
    invisible(check_crs(input))
    input_crs <- terra::crs(input, describe = TRUE)$code
    standard_crs <- terra::crs(crs_standard, describe = TRUE)$code
    if (input_crs == standard_crs) {
      return(input)
    }
    input_transformed <-
      terra::project(x = input, y = crs_standard)
    return(input_transformed)
  }

  detected <- check_packbound(input)
  switch(detected,
    terra = check_crs_terra(input = input, crs_standard = crs_standard),
    sf = check_crs_sf(input = input, crs_standard = crs_standard))
}

#' Generate a rectangular polygon from extent
#' 
#' @param extent input extent.
#'  A numeric vector with xmin/xmax/ymin/ymax,
#'  sf::st_bbox() or terra::ext() outputs.
#' @param output_class character(1).
#'  Class of the output polygon. One of "sf" or "terra"
#' @param crs character(1). Coordinate reference system definition.
#' @author Insang Song
#' @export
extent_to_polygon <- function(
    extent,
    output_class = "terra",
    crs = "EPSG:4326") {
  if (!output_class %in% c("sf", "terra")) {
    stop("output_class should be one of 'sf' or 'terra'.\n")
  }
  if (methods::is(extent, "numeric")) {
    if (is.null(attr(extent, "names"))) {
      stop("Your extent is an unnamed numeric vector.
           Please define names xmin/xmax/ymin/ymax explicitly.\n")
    }
    extent <- switch(
      output_class,
      sf = sf::st_bbox(extent),
      terra = terra::ext(extent)
    )
  }

  extent_polygon <- switch(
    output_class,
    sf = sf::st_as_sf(sf::st_as_sfc(extent)),
    terra = terra::vect(extent)
  )

  extent_polygon <- switch(
    output_class,
    sf = sf::st_set_crs(extent_polygon, sf::st_crs(crs)),
    terra = terra::set.crs(extent_polygon, terra::crs(crs))
  )

  return(extent_polygon)

}


#' Check if the data extent is inside the reference bounding box
#'
#' @description One of the most common errors in
#'  spatial computation is rooted in
#'  the entirely or partly incomparable spatial extents of input datasets.
#'  This function returns whether your data is inside
#'  the target computational extent.
#'  It is assumed that you know and have the exact computational region.
#'  This function will return TRUE if the reference region
#'  completely contains your data's extent and FALSE otherwise.
#' @param data_query sf*/stars/SpatVector/SpatRaster object.
#' @param reference sf*/stars/SpatVector/SpatRaster object
#' @return TRUE (the queried data extent is completely within
#'  the reference bounding box) or FALSE
#' @author Insang Song \email{geoissong@@gmail.com}
#'
#' @export
check_bbox <- function(
  data_query,
  reference
) {
  reference <- sf::st_as_sfc(sf::st_bbox(reference))
  print(sf::st_crs(reference))
  # invisible check data_query CRS check
  invisible(check_crs(data_query))

  data_query_bb <-
    sf::st_as_sfc(sf::st_bbox(data_query),
                  crs = sf::st_crs(data_query))
  print(sf::st_crs(data_query_bb))
  query_matched <- sf::st_transform(data_query_bb, sf::st_crs(reference))
  check_result <- as.logical(unlist(sf::st_within(query_matched, reference)))
  return(check_result)
}



#' Check Coordinate Reference System
#' @param x sf/stars/SpatVector/SpatRaster object.
#' @return A st_crs or crs object.
#' @description It returns st_crs object from sf/Spat* objects.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' # data
#' library(sf)
#' ncpath = system.file("shape/nc.shp", package = "sf")
#' nc = read_sf(ncpath)
#' check_crs(nc)
#' @importFrom sf st_crs
#' @importFrom terra crs
#' @importFrom methods is
#' @export
check_crs <- function(x) {
  ref_class <- c("sf", "stars", "SpatVector",
                 "SpatRaster", "SpatRasterDataset")

  if (!any(ref_class %in% class(x))) {
    stop("Input is invalid.\n")
  }
  class_type <- check_packbound(x)
  if (class_type == "sf" && is.na(sf::st_crs(x))) {
    stop("No CRS is defined in the input.
    Please consult the metadata or the data source.\n")
  }
  if (class_type == "terra" && any(is.na(terra::crs(x)), terra::crs(x) == "")) {
    stop("No CRS is defined in the input.
    Please consult the metadata or the data source.\n")
  }

  if (methods::is(x, "sf") || methods::is(x, "stars")) {
    crs_wkt <- sf::st_crs(x)
  } else {
    crs_wkt <- terra::crs(x)
  }

  return(crs_wkt)
}

#' Check if the boundary of the vector/raster object is inside the reference
#' @param input_object sf/stars/SpatVector/SpatRaster object.
#' @param reference sf/stars/SpatVector/SpatRaster object.
#' @return logical
#' @author Insang Song \email{geoissong@@gmail.com}
#' @importFrom methods is
#' @export 
check_within_reference <- function(input_object, reference) {
  if (!any(
    methods::is(input_object, "sf"),
    methods::is(input_object, "stars"),
    methods::is(input_object, "SpatVector"),
    methods::is(input_object, "SpatRaster")
  )) {
    stop("Input is invalid.\n")
  }

  if (!any(
    methods::is(reference, "sf"),
    methods::is(reference, "stars"),
    methods::is(reference, "SpatVector"),
    methods::is(reference, "SpatRaster")
  )) {
    stop("Reference is invalid.\n")
  }

  bbox_input <- input_object |>
    sf::st_bbox() |>
    sf::st_as_sfc()

  bbox_reference <- reference |>
    sf::st_bbox() |>
    sf::st_as_sfc()

  iswithin <- sf::st_covered_by(bbox_input, bbox_reference)
  iswithin <- length(iswithin[[1]])
  iswithin <- (iswithin == 1)
  invisible(iswithin)
}



#' Detect classes in function arguments
#' @param args Any list, but preferably generated by \code{list(...)} inside
#' a function.
#' @param search character(1). Class name to search. Partial match is supported.
#' @returns logical vector.
#' @author Insang Song
#' @description When a R function is defined in an ordinary
#' fashion (i.e., assigning a function by \code{<- function(...)})
#' would be subject to ambiguity particularly if the function
#' name is the same as the generic function name(s).
#' This function supports detecting classes of arguments in
#' a loosely defined function.
#' @export

detect_class <- function(
  args,
  search
) {
  searchphrase <- sprintf("(%s)", search)
  args_scanned <- lapply(args, function(x) any(grepl(searchphrase, class(x))))
  args_scanned <- sapply(args_scanned, any)
  return(args_scanned)
}


