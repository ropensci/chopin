#' Return the package the input object is based on
#' @family Helper functions
#' @keywords internal
#' @description Detect whether the input object is sf or Spat* object.
#' @author Insang Song
#' @param input Spat* in terra or sf object.
#' @returns A character object; one of `"terra"` and `"sf"`
#' @examples
#' library(sf)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' nc_path <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc_sf <- sf::st_read(nc_path)
#' dep_check(nc_sf)
#' nc_vect <- terra::vect(nc_sf)
#' dep_check(nc_vect)
dep_check <- function(input) {
  if (
    !inherits(
      input,
      c("sf", "stars", "SpatVector", "SpatRaster", "SpatVectorProxy")
    )
  ) {
    cli::cli_abort("Input should be one of sf or Spat* object.\n")
  }
  if (
    inherits(input, c("SpatVector", "SpatRaster", "SpatVectorProxy"))
  ) {
    return("terra")
  }
  return("sf")
}


#' Return the input's GIS data model type
#' @family Helper functions
#' @keywords internal
#' @description This function returns one of 'vector' or 'raster'
#' depending on the input class.
#' @param input Spat*/sf/stars object.
#' @note Although \code{stars} object is a little ambiguous
#' whether to classify vector or raster,
#' it will be considered raster in this package.
#' @author Insang Song
#' @returns character(1). One of `"vector"` or `"raster"`.
#' @examples
#' library(sf)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' nc_path <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc_sf <- sf::st_read(nc_path)
#' datamod(nc_sf)
#'
#' ra_path <- system.file("ex/elev.tif", package = "terra")
#' ra <- terra::rast(ra_path)
#' datamod(ra)
datamod <- function(input) {
  if (
    !inherits(
      input,
      c("sf", "stars", "SpatVector", "SpatRaster")
    )
  ) {
    stop("Input should be one of sf or Spat* object.\n")
  }
  if (inherits(input, c("sf", "SpatVector"))) {
    return("vector")
  }
  if (inherits(input, c("stars", "SpatRaster"))) {
    return("raster")
  }
}

#' @title Check coordinate system then reproject
#' @family Helper functions
#' @keywords internal
#' @description The input is checked whether its coordinate system is
#'  present. If not, it is reprojected to the CRS specified in
#' \code{crs_standard}.
#' @param input Input object one of sf or terra::Spat* object
#' @param crs_standard character(1). A standard definition of
#'  coordinate reference system. Default is `"EPSG:4326"`
#'  Consult [epsg.io](https://epsg.io) for details of other CRS.
#' @note This function works well with EPSG codes.
#' @returns A (reprojected) `sf` or `SpatVector` object.
#' @author Insang Song
#' @examples
#' library(sf)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' base_crs <- "EPSG:5070"
#' nc_path <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc_sf <- sf::st_read(nc_path)
#' reproject_std(nc_sf, base_crs)
#'
#' nc_vect <- terra::vect(nc_sf)
#' reproject_std(nc_vect, base_crs)
#' @importFrom sf st_crs
#' @importFrom sf st_transform
#' @importFrom terra crs
#' @importFrom terra project
#' @export
reproject_std <-
  function(
    input,
    crs_standard = "EPSG:4326"
  ) {

    bound_package <- dep_check(input)
    input_crs <- switch(
      bound_package,
      sf = sf::st_crs(input)$epsg,
      terra = terra::crs(input, describe = TRUE)$code
    )
    standard_crs <- switch(
      bound_package,
      sf = sf::st_crs(crs_standard)$epsg,
      terra = terra::crs(crs_standard, describe = TRUE)$code
    )
    if (input_crs == standard_crs) {
      return(input)
    }
    input_transformed <- switch(
      bound_package,
      sf = sf::st_transform(input, sf::st_crs(crs_standard)),
      terra = terra::project(x = input, y = crs_standard)
    )
    return(input_transformed)
}

#' Validate and repair input vector data
#' @family Helper functions
#' @keywords internal
#' @description It tries repairing input vector data.
#' Vector validity violation usually appears in polygon data with
#' self-crossing or
#' hole orders. This function will pass the input_vector object to
#' [`sf::st_make_valid`] (if input_vector is sf) or
#' [`terra::makeValid`] (if input_vector is SpatVector).
#' May take some time depending on the geometry complexity.
#' @author Insang Song
#' @param input_vector One of sf or vect class. Target points of computation.
#' @returns A repaired `sf` or `SpatVector` object depending on
#' the class of input_vector.
#' @note This function works with GEOS (>=3.8).
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(ncpath)
#'
#' nc_valid <- vect_valid_repair(nc)
#' }
#' @importFrom terra makeValid
#' @importFrom sf st_make_valid
vect_valid_repair <- function(input_vector) {
  detected <- dep_check(input_vector)

  validated <- switch(detected,
    terra = terra::makeValid(input_vector),
    sf = sf::st_make_valid(input_vector)
  )

  return(validated)
}


#' Generate a rectangular polygon from extent
#' @family Helper functions
#' @keywords internal
#' @param extent input extent.
#'  A numeric vector with xmin/xmax/ymin/ymax,
#'  [sf::st_bbox] or [terra::ext] outputs.
#' @param output_class character(1).
#'  Class of the output polygon. One of `"sf"` or `"terra"`
#' @param crs character(1). Coordinate reference system definition.
#' @returns `sf` or `SpatVector` object of a rectangular polygon.
#' @author Insang Song
#' @examples
#' library(sf)
#' library(terra)
#' numext1 <- c(-100, -70, 30, 40)
#' names(numext1) <- c("xmin", "xmax", "ymin", "ymax")
#' ext2poly(numext1, "sf")
#' ext2poly(numext1, "terra")
#' @importFrom sf st_as_sf
#' @importFrom sf st_bbox
#' @importFrom sf st_set_crs
#' @importFrom terra vect
#' @importFrom terra ext
#' @importFrom terra set.crs
ext2poly <- function(
    extent = NULL,
    output_class = c("sf", "terra"),
    crs = "EPSG:4326") {
  output_class <- match.arg(output_class)
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
#' @family Helper functions
#' @keywords internal
#' @description One of the most common errors in spatial computation is rooted
#' in the entirely or partly incomparable spatial extents of input datasets.
#' This function returns whether your data is inside the target computational
#' extent.
#' It is assumed that you know and have the exact computational region.
#' This function will return `TRUE` if the reference region
#' completely contains your data's extent and `FALSE` otherwise.
#' @param data_query sf*/stars/SpatVector/SpatRaster object.
#' @param reference sf*/stars/SpatVector/SpatRaster object
#' @returns logical(1). `TRUE` (the queried data extent is completely within
#'  the reference bounding box) or `FALSE`
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' library(sf)
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- sf::st_read(ncpath)
#' nc <- sf::st_transform(nc, "EPSG:4326")
#'
#' refextnum <- c(-100, -60, 20, 40)
#' names(refextnum) <- c("xmin", "xmax", "ymin", "ymax")
#' refext <- ext2poly(refextnum)
#' is_bbox_within_reference(nc, refext)
#' @importFrom sf st_as_sfc st_crs st_bbox st_transform st_within
is_bbox_within_reference <- function(
  data_query = NULL,
  reference = NULL
) {
  reference <- sf::st_as_sfc(sf::st_bbox(reference))
  print(sf::st_crs(reference))

  data_query_bb <-
    sf::st_as_sfc(sf::st_bbox(data_query),
                  crs = sf::st_crs(data_query))
  print(sf::st_crs(data_query_bb))
  query_matched <- sf::st_transform(data_query_bb, sf::st_crs(reference))
  check_result <- as.logical(unlist(sf::st_within(query_matched, reference)))
  return(check_result)
}



#' Check Coordinate Reference System
#' @family Helper functions
#' @keywords internal
#' @param x `sf`/`stars`/`SpatVector`/`SpatRaster` object.
#' @returns A st_crs or crs object.
#' @description It returns st_crs object from `sf`/Spat* objects.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' # data
#' library(sf)
#' ncpath = system.file("shape/nc.shp", package = "sf")
#' nc = read_sf(ncpath)
#' crs_check(nc)
#' @importFrom sf st_crs
#' @importFrom terra crs
#' @importFrom methods is
crs_check <- function(x = NULL) {
  ref_class <- c("sf", "stars", "SpatVector",
                 "SpatRaster", "SpatRasterDataset")

  if (!any(ref_class %in% class(x))) {
    stop("Input is invalid.\n")
  }
  class_type <- dep_check(x)
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
#' @family Helper functions
#' @param input_object sf/stars/SpatVector/SpatRaster object.
#' @param reference sf/stars/SpatVector/SpatRaster object.
#' @returns logical
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' library(sf)
#' sf_use_s2(FALSE)
#' ncpath <- system.file("shape/nc.shp", package = "sf")
#' nc <- sf::read_sf(ncpath)
#' nc <- sf::st_transform(nc, "EPSG:4326")
#' mainland_vec <- c(xmin = -128, xmax = -62, ymin = 22, ymax = 52)
#' mainland_box <- ext2poly(mainland_vec, output_class = "sf")
#' within_res <- is_within_ref(nc, mainland_box)
#' within_res
#' @importFrom methods is
#' @importFrom sf st_bbox
#' @importFrom sf st_as_sfc
#' @importFrom sf st_covered_by
#' @export
is_within_ref <- function(input_object, reference) {
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
#' @family Helper functions
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
#' @examples
#' df <- data.frame(a = 1, b = 3)
#' any_class_args(list(df), "data.frame")
any_class_args <- function(
  args = NULL,
  search = NULL
) {
  searchphrase <- sprintf("(%s)", search)
  args_scanned <- lapply(args, function(x) any(grepl(searchphrase, class(x))))
  args_scanned <- vapply(args_scanned, FUN = any, FUN.VALUE = logical(1))
  return(args_scanned)
}



#' Check the subject object and perform necessary conversions if needed.
#' @description
#' This function checks the class of the input object and
#'   performs necessary conversions if needed.
#' @keywords internal
#' @param subject The input object to be checked.
#' @param extent The extent of the subject object.
#' @param subject_id Optional. The ID of the subject object.
#' @returns The checked and converted subject object.
#' @examples
#' # Check a SpatVector object
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' ncsf <- sf::st_read(ncpath)
#' extent <- c(-80, -77, 35, 36)
#' check_subject(subject = ncsf, extent = extent, subject_id = "FIPS")
#'
#' # Check a character object
#' check_subject(subject = ncpath, extent = extent, subject_id = "FIPS")
check_subject <- function(
  subject,
  extent = NULL,
  subject_id = NULL
) {
  # type check
  if (!any(
    inherits(subject, c("SpatVector", "sf", "character"))
  )) {
    cli::cli_abort("Check class of the input object.\n")
  }
  if (is.character(subject)) {
    cli::cli_inform("Input is a character. Trying to read with terra.")
    subject <- try(terra::vect(subject, extent = extent))
  } else {
    if (dep_check(subject) == "sf") {
      cli::cli_inform("Input is an sf object. Trying to convert to terra.")
      subject <- dep_switch(subject)
    }
  }
  if (!is.null(subject_id)) {
    if (!subject_id %in% names(subject)) {
      cli::abort("id should exist in the input object\n")
    }
  }
  return(subject)
}


# check if alphahull points reported the distance to
# the nearest computational grid boundary
#' @title Check if alphahull points reported the distance to
#'   the nearest computational grid boundary
#' @family Helper functions
#' @description This function checks if the alphahull points reported
#'  the distance to the nearest computational grid boundary.
#' @param points SpatVector point object.
#' @param grid plain grid (i.e., not padded from [`par_make_gridset`])
#' @param dist_calc Calculated distance.
#' @returns logical vector.
#' @importFrom alphahull ahull
#' @export
check_dist_incorrect <-
  function(
    points,
    grid,
    dist_calc
  ) {
    grid_line <- terra::as.lines(grid)
    pcoords <- terra::crds(points)

    # get the outermost point row indices
    points_ahull <- alphahull::ahull(pcoords[, 1], pcoords[, 2], alpha = 1)
    points_check <- points_ahull$ashape.obj$alpha.extremes
    points_check <- points[points_check, ]
    dist_calc_check <- dist_calc[points_check, ]
    dist_check <- terra::nearest(points_check, grid_line)

    if (any(dist_calc_check == dist_check)) {
      cli::cli_warn("Suspected records found.")
      return(dist_calc_check[dist_calc_check == dist_check])
    } else {
      cli::cli_inform("No suspected records.")
      return(FALSE)
    }

  }
