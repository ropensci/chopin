#' Return the package the input object is based on
#' @family Helper functions
#' @keywords internal
#' @description Detect whether the input object is sf or Spat* object.
#' @author Insang Song
#' @param input Spat* in terra or sf object.
#' @returns A character object; one of `"character"`, `"terra"` and `"sf"`
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
      c("sf", "stars",
        "SpatVector", "SpatRaster", "SpatVectorProxy",
        "character")
    )
  ) {
    cli::cli_abort("Input should be one of sf or Spat* object.\n")
  }
  if (inherits(input, "character")) {
    return("character")
  }
  if (
    inherits(input, c("SpatVector", "SpatRaster", "SpatVectorProxy"))
  ) {
    return("terra")
  }
  return("sf")
}

#' Switch spatial data class
#' @family Helper functions
#' @description Convert class between `sf`/`stars`-`terra`
#' @author Insang Song
#' @param input Spat* in terra or sf object.
#' @returns Data converted to the other package class
#' (if sf, terra; if terra, sf)
#' @examples
#' library(sf)
#' library(stars)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' ## generate a random raster
#' ras_rand <- terra::rast(nrow = 30, ncol = 30)
#' terra::values(ras_rand) <- runif(900)
#' stars_rand <- dep_switch(ras_rand)
#' stars_rand
#' inherits(sf_rand, "stars") # TRUE
#' # should return stars object
#'
#' vec_rand <- terra::spatSample(ras_rand, size = 10L, as.points = TRUE)
#' sf_rand <- dep_switch(vec_rand)
#' inherits(sf_rand, "sf") # TRUE
#' sf_rand
#' # should return sf object
#' @importFrom terra vect rast
#' @importFrom sf st_as_sf
#' @importFrom stars st_as_stars
#' @importFrom cli cli_abort cli_inform
#' @keywords internal
dep_switch <- function(input) {
  if (!inherits(input, c("sf", "stars", "SpatVector", "SpatRaster"))) {
    cli::cli_abort("Input should be one of sf or Spat* object.\n")
  }
  cls_input <- dep_check(input)
  type_input <- datamod(input)
  # search strings. can be expanded.
  candidates <- c("sf", "terra")
  cli::cli_inform(
    sprintf(
      "Switch %s class to %s...",
      cls_input, setdiff(candidates, cls_input)
    )
  )

  switched <-
    switch(cls_input,
      sf = switch(type_input,
        vector = terra::vect(input),
        raster = terra::rast(input)
      ),
      terra = switch(type_input,
        vector = sf::st_as_sf(input),
        raster = stars::st_as_stars(input)
      )
    )

  return(switched)
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
#' @importFrom cli cli_abort
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
      c("sf", "stars", "SpatVector", "SpatRaster",
        "SpatVectorProxy", "SpatRasterDataset", "SpatRasterCollection")
    )
  ) {
    cli::cli_abort("Input should be one of sf or Spat* object.\n")
  }
  if (
    inherits(
      input,
      c("sf", "SpatVector", "SpatVectorProxy", "SpatVectorCollection")
    )
  ) {
    return("vector")
  }
  if (
    inherits(
      input,
      c("stars", "SpatRaster", "SpatRasterDataset", "SpatRasterCollection")
    )
  ) {
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
#' @importFrom sf st_crs st_transform
#' @importFrom terra crs project
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
    if (input_crs != standard_crs) {
      cli::cli_inform(
        sprintf("Reprojecting input:...\n\n--- CRS ---\n%s\n", standard_crs)
      )
      input <- switch(
        bound_package,
        sf = sf::st_transform(input, sf::st_crs(crs_standard)),
        terra = terra::project(x = input, y = crs_standard)
      )
    }
    return(input)
  }



#' @title Align vector CRS to raster's
#' @family Helper functions
#' @keywords internal soft-deprecated
#' @param vector `sf`/`stars`/`SpatVector`/`SpatRaster` object
#' @param raster `SpatRaster` object
#' @returns Reprojected object in the same class as \code{vector}
#' @author Insang Song
#' @examples
#' library(terra)
#' library(sf)
#' options(sf_use_s2 = FALSE)
#'
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' elev <- system.file("ex/elev.tif", package = "terra")
#' nc <- terra::vect(ncpath)
#' elev <- terra::rast(elev)
#' reproject_to_raster(nc, elev)
#' @importFrom sf st_transform
#' @importFrom terra project
#' @importFrom terra crs
reproject_to_raster <-
  function(
    vector = NULL,
    raster = NULL
  ) {
    detected_vec <- dep_check(vector)
    switch(detected_vec,
           sf = sf::st_transform(vector, terra::crs(raster)),
           terra = terra::project(vector, terra::crs(raster)))
  }



#' Validate and repair input vector data
#' @family Helper functions
#' @keywords internal soft-validated
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
#' library(terra)
#' library(sf)
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(ncpath)
#'
#' nc_valid <- vect_validate(nc)
#' @importFrom terra makeValid
#' @importFrom sf st_make_valid
vect_validate <- function(input_vector) {
  detected <- dep_check(input_vector)

  validated <- switch(detected,
    terra = terra::makeValid(input_vector),
    sf = sf::st_make_valid(input_vector)
  )

  return(validated)
}


#' Generate a rectangular polygon from extent
#' @family Helper functions
#' @keywords internal soft-deprecated
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
#' ext_to_poly(numext1, "sf")
#' ext_to_poly(numext1, "terra")
#' @importFrom sf st_as_sf st_bbox st_set_crs
#' @importFrom terra vect ext set.crs
ext_to_poly <- function(
    extent = NULL,
    output_class = c("sf", "terra"),
    crs = "EPSG:4326") {
  output_class <- match.arg(output_class)
  if (is.numeric(extent)) {
    if (output_class == "sf") {
      if (is.null(attr(extent, "names"))) {
        cli::cli_abort(
          paste0(
            "Your extent is an unnamed numeric vector.\n",
            "Please define names xmin/xmax/ymin/ymax explicitly.\n"
          )
        )
      }
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
#' @keywords internal soft-deprecated
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
#' refext <- ext_to_poly(refextnum)
#' is_bbox_within_reference(nc, refext)
#' @importFrom sf st_as_sfc st_crs st_bbox st_transform st_within
is_bbox_within_reference <- function(
  data_query = NULL,
  reference = NULL
) {
  reference <- sf::st_as_sfc(sf::st_bbox(reference))
  cli::cli_inform(
    paste(
      "Full CRS:",
      as.character(sf::st_crs(reference)),
      "--- CRS ---",
      collapse = "\n"
    )
  )

  data_query_bb <-
    sf::st_as_sfc(sf::st_bbox(data_query),
                  crs = sf::st_crs(data_query))

  query_matched <- sf::st_transform(data_query_bb, sf::st_crs(reference))
  check_result <- as.logical(unlist(sf::st_within(query_matched, reference)))
  return(check_result)
}



#' Check Coordinate Reference System
#' @family Helper functions
#' @keywords internal soft-deprecated
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
crs_check <- function(x = NULL) {
  ref_class <- c("sf", "stars", "SpatVector",
                 "SpatRaster", "SpatRasterDataset")

  if (!inherits(x, ref_class)) {
    cli::cli_abort("Input is invalid.\n")
  }
  class_type <- dep_check(x)
  if (class_type == "sf" && is.na(sf::st_crs(x))) {
    cli::cli_abort(
      "No CRS is defined in the input.",
      "Please consult the metadata or the data source."
    )
  }
  if (class_type == "terra" && any(is.na(terra::crs(x)), terra::crs(x) == "")) {
    cli::cli_abort(
      "No CRS is defined in the input.",
      "Please consult the metadata or the data source."
    )
  }

  if (inherits(x, c("sf", "stars"))) {
    crs_wkt <- sf::st_crs(x)
  } else {
    crs_wkt <- terra::crs(x)
  }
  cli::cli_inform(
    sprintf("CRS:\n%s\n--- Returned CRS ---", as.character(crs_wkt))
  )
  return(crs_wkt)
}



## .intersect_extent ####
setGeneric(
  ".intersect_extent",
  function(input = NULL, out_class = NULL, ...) {
    if (is.null(input)) {
      return(input)
    }
  }
)

#' @keywords internal
#' @noRd
setMethod(
  ".intersect_extent",
  signature(input = "sf"),
  function(input, out_class = NULL, ...) {
    extent <- sf::st_as_sfc(sf::st_bbox(input))
    if (!is.null(out_class)) {
      if (out_class == "terra") {
        extent <- terra::ext(sf::st_bbox(extent))
      }
    }
    return(extent)
  }
)

# setMethod(
#   ".intersect_extent",
#   signature(input = "bbox"),
#   function(input, out_class = NULL, ...) {
#     extent <- sf::st_as_sfc(input)
#     return(extent)
#   }
# )

#' @keywords internal
#' @noRd
setMethod(
  ".intersect_extent",
  signature(input = "SpatVector"),
  function(input, out_class = NULL, ...) {
    extent <- terra::ext(input)
    if (!is.null(out_class)) {
      if (out_class == "sf") {
        extent <- sf::st_bbox(extent)
      }
    }
    return(extent)
  }
)

#' @keywords internal
#' @noRd
setMethod(
  ".intersect_extent",
  signature(input = "numeric", out_class = "character"),
  function(input, out_class = NULL, ...) {
    out_class <- match.arg(out_class, c("sf", "terra"))

    if (out_class == "sf") {
      extent <- input[c(1, 3, 2, 4)]
      extent <- stats::setNames(extent, c("xmin", "ymin", "xmax", "ymax"))
      extent <- sf::st_as_sfc(sf::st_bbox(extent))
    }
    if (out_class == "terra") {
      extent <- terra::ext(input)
    }
    return(extent)
  }
)


#' @keywords internal
#' @noRd
.check_id <- function(input, input_id = NULL) {
  if (!is.null(input_id)) {
    stopifnot(is.character(input_id))
    if (!input_id %in% names(input)) {
      cli::cli_abort("id should exist in the input object\n")
    }
  }
  return(input)
}



#' @keywords internal
#' @noRd
setMethod(
  "[",
  signature(x = "SpatVector", i = "bbox", j = "missing"),
  function(x, i, j) {
    x[terra::ext(i), ]
  }
)

#' @keywords internal
#' @noRd
setMethod(
  "[",
  signature(x = "sf", i = "SpatExtent", j = "missing"),
  function(x, i, j) {
    x[sf::st_as_sfc(sf::st_bbox(i)), ]
  }
)

#' @keywords internal
#' @noRd
setMethod(
  "[",
  signature(x = "sf", i = "SpatVector", j = "missing"),
  function(x, i, j) {
    x[sf::st_as_sf(i), ]
  }
)


#' @keywords internal
#' @noRd
setMethod(
  "[",
  signature(x = "SpatVector", i = "sf", j = "missing"),
  function(x, i, j) {
    x[terra::vect(i), ]
  }
)

#' @keywords internal
#' @noRd
setMethod(
  "[",
  signature(x = "SpatVector", i = "sfc", j = "missing"),
  function(x, i, j) {
    x[terra::vect(sf::st_as_sf(i)), ]
  }
)


#' @keywords internal
#' @noRd
setMethod(
  "[",
  signature(x = "SpatVector", i = "SpatExtent", j = "missing"),
  function(x, i, j) {
    x[sf::st_as_sfc(sf::st_bbox(i)), ]
  }
)


## .check_vector ####
#' Check the subject object and perform necessary conversions if needed.
#' @description
#' This function checks the class of the input object and
#'   performs necessary conversions if needed.
#' @keywords internal
#' @param input sf/SpatVector/character. The input object to be checked.
#' @param input_id character(1). ID field of the subject object.
#' @param extent numeric(4). The extent of the subject object.
#'   Numeric vector should be put in order of
#'  `c(xmin, xmax, ymin, ymax)`.
#' @param out_class character(1). The class of the output object.
#'   Should be one of `c("sf", "terra")`.
#' @param ... Placeholder.
#' @returns The checked and converted subject object.
#' @importFrom terra vect
#' @importFrom sf st_read st_as_text st_as_sfc st_bbox
#' @importFrom cli cli_abort cli_inform
#' @importFrom stats setNames
#' @examples
#' # Check a SpatVector object
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(ncpath)
#' extent <- c(-80, -77, 35, 36)
#' .check_vector(input = nc, extent = extent, input_id = "FIPS")
#'
#' # Check a sf object
#' ncsf <- sf::st_read(ncpath)
#' .check_vector(input = ncsf, extent = extent, input_id = "FIPS")
#'
#' # Check a character object
#' .check_vector(
#'   input = ncpath,
#'   extent = extent,
#'   out_class = "terra",
#'   input_id = "FIPS"
#' )
#' @name .check_vector
NULL


setGeneric(
  ".check_vector",
  function(input = NULL, input_id = NULL,
           extent = NULL, out_class = NULL, ...) {
    if (is.null(extent)) {
      res <- .check_id(input, input_id)
      return(res)
    }

    extent <- .intersect_extent(extent, out_class)
    input <- input[extent, ]

    .check_id(input, input_id)
  }
)

#' @keywords internal
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "character", input_id = "ANY",
            extent = "ANY", out_class = "character"),
  function(input, input_id = NULL, extent = NULL, out_class = "terra", ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra.\n"))
    }
    cli::cli_inform(
      c("i" =
          sprintf("Input is a character. Trying to read with %s\n.", out_class)
      )
    )
    if (out_class == "sf") {
      extent <- if (is.null(extent)) {
        character(0)
      } else {
        extent <- extent[c(1, 3, 2, 4)]
        extent <- stats::setNames(extent, c("xmin", "ymin", "xmax", "ymax"))
        extent <- sf::st_as_text(sf::st_as_sfc(sf::st_bbox(extent)))
      }
    }
    input <- switch(
      out_class,
      terra = try(terra::vect(input, extent = extent), silent = TRUE),
      sf = try(sf::st_read(input, wkt_filter = extent), silent = TRUE)
    )
    return(input)
  }
)


#' Check Raster Input
#'
#' This function checks the input object to ensure
#' it is a valid raster object or a character path to a raster file.
#' It also provides warnings and informative messages based on the input type.
#'
#' @param input The input object to be checked. It can be either
#'   a SpatRaster object or a character path to a raster file.
#' @param extent The extent of the raster. Defaults to NULL.
#'   Numeric vector should be put in order of
#'  `c(xmin, xmax, ymin, ymax)`.
#' @param ... Placeholder.
#'
#' @returns The validated input object.
#'
#' @examples
#' .check_raster(system.file("extdata/nc_srtm15_otm.tif", package = "chopin"))
#'
#' @importFrom terra rast
#' @importFrom cli cli_abort cli_inform cli_warn
#' @keywords internal
.check_raster <- function(
  input,
  extent = NULL,
  ...
) {
  # type check
  if (
    !inherits(input, c("SpatRaster", "character"))
  ) {
    if (inherits(input, "SpatRasterCollection")) {
      cli::cli_abort(
        paste0(
          "SpatRasterCollection is not directly supported.\n",
          "Convert it into SpatRaster object to process.\n"
        )
      )
    }
    cli::cli_abort("Check class of the input object.\n")
  }

  # character ingestion
  if (is.character(input)) {
    cli::cli_inform(
      sprintf("Input is a character. Attempt to read it with terra::rast...\n")
    )
    input <-
      try(terra::rast(input, win = extent, snap = "out"), silent = TRUE)
  }

  # to be future-proof... not run in terra 1.7.46
  # if (terra::has.time(input)) {
  #   cli::cli_inform(
  #     paste0(
  #       "The input contains time information.\n",
  #       "Each time point is treated as a layer."
  #     )
  #   )
  # }
  return(input)
}


#' Check the class of an input object
#'
#' This function checks the class of an input object and
#'  returns "raster" if it is a raster object,
#' or "vector" if it is a vector object.
#'
#' @param input The input object to be checked
#'
#' @returns A character string indicating the class of
#'   the input object ("raster" or "vector")
#' @keywords internal
#' @importFrom terra vect rast
.check_character <- function(
  input
) {
  # type check
  if (!is.character(input)) {
    cli::cli_alert_info("Input is not a character.\n")
    res <- datamod(input)
    return(res)
  }

  suppressWarnings(
    try_vect <- tryCatch(terra::vect(input, proxy = TRUE),
                         error = function(e) {
                           structure(0L, class = "chopin-try-error")
                         })
  )
  suppressWarnings(
    try_rast <- tryCatch(terra::rast(input),
                         error = function(e) {
                           structure(0L, class = "chopin-try-error")
                         })
  )
  not_vect <- inherits(try_vect, "chopin-try-error")
  not_rast <- inherits(try_rast, "chopin-try-error")

  if (not_vect && not_rast) {
    cli::cli_abort("Check class of the input object.\n")
  }
  if (not_vect) {
    res <- "raster"
    attr(res, "crs") <- terra::crs(try_rast)
    return(res)
  }
  res <- "vector"
  attr(res, "crs") <- terra::crs(try_vect)
  return(res)
}
