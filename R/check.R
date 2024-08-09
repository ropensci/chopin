#' Return the package the input object is based on
#' @family Helper functions
#' @keywords internal
#' @description Detect whether the input object is sf or Spat* object.
#' @author Insang Song
#' @param input Spat* in terra or sf object.
#' @returns A character object; one of `"character"`, `"terra"` and `"sf"`
#' @examples
#' \dontrun{
#' library(sf)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' nc_path <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc_sf <- sf::st_read(nc_path)
#' dep_check(nc_sf)
#' nc_vect <- terra::vect(nc_sf)
#' dep_check(nc_vect)
#' }
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
#' \dontrun{
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
#' }
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
#' \dontrun{
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
#' }
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
#' \dontrun{
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
#' }
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
      sf = sf::st_crs(input)$wkt,
      terra = terra::crs(input)
    )
    standard_crs <- switch(
      bound_package,
      sf = sf::st_crs(crs_standard)$wkt,
      terra = terra::crs(crs_standard)
    )
    if (!terra::same.crs(input_crs, standard_crs)) {
      cli::cli_inform(
        sprintf("Reprojecting input:\n\n--- CRS ---\n\n%s\n", standard_crs)
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
#' \dontrun{
#' library(terra)
#' library(sf)
#' options(sf_use_s2 = FALSE)
#'
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' elev <- system.file("ex/elev.tif", package = "terra")
#' nc <- terra::vect(ncpath)
#' elev <- terra::rast(elev)
#' reproject_to_raster(nc, elev)
#' }
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
#' @keywords internal soft-deprecated
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
#' nc_valid <- vect_validate(nc)
#' }
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


# ## .intersect_extent ####
#' Get intersection extent
#' @param input sf/SpatExtent/SpatVector/numeric
#' @param out_class character(1). "sf" or "terra"
#' @param ... other arguments. Placeholder.
#' @name .intersect_extent
#' @rdname dot-intersect_extent
setGeneric(
  ".intersect_extent",
  function(input, out_class, ...) standardGeneric(".intersect_extent")
)

#' @keywords internal
#' @rdname dot-intersect_extent
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


#' @keywords internal
#' @rdname dot-intersect_extent
setMethod(
  ".intersect_extent",
  signature(input = "SpatExtent"),
  function(input, out_class = NULL, ...) {
    extent <- input
    if (!is.null(out_class)) {
      if (out_class == "sf") {
        extent <- sf::st_bbox(extent)
      }
    }
    return(extent)
  }
)

#' @keywords internal
#' @rdname dot-intersect_extent
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
#' @rdname dot-intersect_extent
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
#' @param input sf/SpatVector/data.frame
#' @param input_id character(1) ID field name.
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
    attr(res, "crs") <- terra::crs(input)
    return(res)
  }

  suppressWarnings(
    try_vect <- tryCatch({
      vct <- terra::vect(input, proxy = TRUE)
      # if the input is not a vector in ambiguous formats
      if (terra::geomtype(vct) == "none") {
        vct <- structure(0L, class = "chopin-try-error")
      }
      vct
    },
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



# `[` extension ####
#' Subset for nonidentical package class objects
#' @docType methods
#' @keywords internal
#' @param x Dataset to be subset.
#' @param i Dataset used to subset x.
#' @param j Column indices or names.
#' @importFrom sf st_bbox
#' @name indexing
#' @rdname indexing
NULL

#' @rdname indexing
#' @export
setMethod(
  "[",
  signature(x = "SpatVector", i = "bbox", j = "missing"),
  function(x, i, j) {
    x[terra::ext(i), ]
  }
)


#' @rdname indexing
#' @export
setMethod(
  "[",
  signature(x = "SpatVector", i = "sf", j = "missing"),
  function(x, i, j) {
    x[terra::vect(i), ]
  }
)

#' @rdname indexing
#' @export
setMethod(
  "[",
  signature(x = "SpatVector", i = "sfc", j = "missing"),
  function(x, i, j) {
    x[terra::vect(sf::st_as_sf(i)), ]
  }
)


#' @rdname indexing
#' @export
setMethod(
  "[",
  signature(x = "SpatVector", i = "SpatExtent", j = "missing"),
  function(x, i, j) {
    x[sf::st_as_sfc(sf::st_bbox(i)), ]
  }
)


#' Intersect different data model objects
#' @param x SpatVector/sf/SpatRaster object to be intersected.
#' @param y SpatVector/sf object. Intersecting object.
#' @keywords internal
#' @rdname indexing
.intersect <- function(x, y) {
  datamodel_x <- datamod(x)
  if (datamodel_x == "raster") {
    return(x)
  }
  dep_x <- dep_check(x)
  dep_y <- dep_check(y)

  if (dep_x != dep_y) {
    y <- dep_switch(y)
  }
  x[y, ]

}



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
#' \dontrun{
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
#' }
#' @name .check_vector
# nolint start
setGeneric(
  ".check_vector",
  function(input, input_id = NULL, extent = NULL, out_class = character(1), ...) standardGeneric(".check_vector"),
  signature = c("input", "input_id", "extent", "out_class")
)
# nolint end

#' @keywords internal
#' @name .check_vector
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "character", input_id = "ANY",
            extent = "NULL", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }

    cli::cli_inform(
      c("i" =
          sprintf("Input is a character. Trying to read with %s.", out_class)
      )
    )
    if (!is.null(extent)) {
      cli::cli_alert_warning("Non-null extent is ignored.")
    }

    if (out_class == "sf") {
      if (is.null(extent)) {
        extent <- character(0)
      }
    }

    input <- switch(
      out_class,
      terra = try(terra::vect(input, extent = extent), silent = TRUE),
      sf = try(sf::st_read(input, wkt_filter = extent), silent = TRUE)
    )
    .check_id(input = input, input_id = input_id)

    return(input)
  }
)

#' @keywords internal
#' @name .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "character", input_id = "ANY",
            extent = "numeric", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }

    cli::cli_inform(
      c("i" =
          sprintf("Input is a character. Trying to read with %s.", out_class)
      )
    )
    if (out_class == "sf") {
      if (is.null(extent)) {
        extent <- character(0)
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
    .check_id(input = input, input_id = input_id)
    return(input)
  }
)


#' @keywords internal
#' @name .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "character", input_id = "ANY",
            extent = "sf", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra.\n"))
    }

    cli::cli_inform(
      c("i" =
          sprintf("Input is a character. Trying to read with %s\n.", out_class)
      )
    )
    if (out_class == "sf") {
      input <- try(sf::st_read(input), silent = TRUE)
      if (sf::st_crs(input) != sf::st_crs(extent)) {
        extent <- sf::st_transform(extent, sf::st_crs(input))
      }
    }
    if (out_class == "terra") {
      input <- try(terra::vect(input), silent = TRUE)
      extent <- terra::vect(extent)
      if (!terra::same.crs(terra::crs(input), terra::crs(extent))) {
        extent <- reproject_std(extent, terra::crs(input))
      }
    }
    .check_id(input = input, input_id = input_id)
    input <- input[extent, ]
    return(input)
  }
)


#' @keywords internal
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "character", input_id = "ANY",
            extent = "SpatVector", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }

    cli::cli_inform(
      c("i" =
          sprintf("Input is a character. Trying to read with %s.", out_class)
      )
    )
    if (out_class == "sf") {
      input <- try(sf::st_read(input), silent = TRUE)
      extent <- sf::st_as_sf(extent)
      if (sf::st_crs(input) != sf::st_crs(extent)) {
        extent <- sf::st_transform(extent, sf::st_crs(input))
      }
    }
    if (out_class == "terra") {
      input <- try(terra::vect(input), silent = TRUE)
      if (!terra::same.crs(terra::crs(input), terra::crs(extent))) {
        extent <- reproject_std(extent, terra::crs(input))
      }
    }
    .check_id(input = input, input_id = input_id)

    input <- input[extent, ]
    return(input)
  }
)

#' @keywords internal
#' @name .check_vector
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "character", input_id = "ANY",
            extent = "SpatExtent", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }

    cli::cli_inform(
      c("i" =
          sprintf("Input is a character. Trying to read with %s.", out_class)
      )
    )
    cli::cli_alert_danger(
      "SpatExtent is detected in the extent argument. Assuming the same CRS..."
    )
    if (out_class == "sf") {
      input <- try(sf::st_read(input), silent = TRUE)
      extent <- sf::st_as_sfc(sf::st_bbox(extent))
    }
    if (out_class == "terra") {
      input <- try(terra::vect(input), silent = TRUE)
    }
    .check_id(input = input, input_id = input_id)

    input <- input[extent, ]
    return(input)
  }
)


#' @keywords internal
#' @name .check_vector
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "sf", input_id = "ANY",
            extent = "NULL", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }
    input <- .check_id(input, input_id)

    if (out_class == "terra") {
      input <- terra::vect(input)
      if (!is.null(extent)) {
        extent <- terra::ext(extent)
        input <- input[extent, ]
      }
    }
    if (out_class == "sf") {
      if (!is.null(extent)) {
        extent <- extent[c(1, 3, 2, 4)]
        extent <- stats::setNames(extent, c("xmin", "ymin", "xmax", "ymax"))
        extent <- sf::st_as_sfc(sf::st_bbox(extent))
        extent <- sf::st_set_crs(extent, sf::st_crs(input))
        input <- input[extent, ]
      }
    }
    return(input)
  }
)


#' @keywords internal
#' @name .check_vector
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "sf", input_id = "ANY",
            extent = "numeric", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }
    input <- .check_id(input, input_id)

    if (out_class == "terra") {
      extent <- terra::ext(extent)
      input <- input[extent, ]
    }
    if (out_class == "sf") {
      extent <- extent[c(1, 3, 2, 4)]
      extent <- stats::setNames(extent, c("xmin", "ymin", "xmax", "ymax"))
      extent <- sf::st_as_sfc(sf::st_bbox(extent))
      extent <- sf::st_set_crs(extent, sf::st_crs(input))
      input <- input[extent, ]
    }
    input <- input[extent, ]
    return(input)
  }
)


#' @keywords internal
#' @name .check_vector
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "sf", input_id = "ANY",
            extent = "SpatExtent", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }
    input <- .check_id(input, input_id)

    if (out_class == "sf") {
      extent <- sf::st_as_sfc(sf::st_bbox(extent))
      extent <- sf::st_set_crs(extent, sf::st_crs(input))
      input <- input[extent, ]
    }
    if (out_class == "terra") {
      input <- terra::vect(input)
      input <- input[extent, ]
    }
    return(input)
  }
)



#' @keywords internal
#' @name .check_vector
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "sf", input_id = "ANY",
            extent = "sf", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }
    input <- .check_id(input, input_id)

    if (!terra::same.crs(terra::crs(input), terra::crs(extent))) {
      extent <- reproject_std(extent, sf::st_crs(input))
    }
    input <- input[extent, ]

    if (out_class == "terra") {
      input <- terra::vect(input)
    }
    return(input)
  }
)


#' @keywords internal
#' @name .check_vector
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "sf", input_id = "ANY",
            extent = "SpatVector", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }
    input <- .check_id(input, input_id)

    if (!is.null(extent)) {
      if (!terra::same.crs(terra::crs(input), terra::crs(extent))) {
        extent <- reproject_std(extent, terra::crs(input))
      }
      input <- input[extent, ]
    }
    if (out_class == "terra") {
      input <- terra::vect(input)
    }
    return(input)
  }
)


#' @keywords internal
#' @name .check_vector
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "SpatVector", input_id = "ANY",
            extent = "SpatExtent", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }
    input <- .check_id(input, input_id)

    input <- input[extent, ]
    if (out_class == "sf") {
      input <- dep_switch(input)
    }
    return(input)
  }
)


#' @keywords internal
#' @name .check_vector
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "SpatVector", input_id = "ANY",
            extent = "ANY", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }
    input <- .check_id(input, input_id)

    if (out_class == "sf") {
      input <- dep_switch(input)
    }
    return(input)
  }
)


#' @keywords internal
#' @name .check_vector
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "SpatVector", input_id = "ANY",
            extent = "NULL", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }
    input <- .check_id(input, input_id)

    if (out_class == "sf") {
      input <- dep_switch(input)
    }
    return(input)
  }
)


#' @keywords internal
#' @name .check_vector
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "SpatVector", input_id = "NULL",
            extent = "NULL", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }

    if (out_class == "sf") {
      input <- dep_switch(input)
    }
    return(input)
  }
)


#' @keywords internal
#' @name .check_vector
#' @rdname .check_vector
#' @noRd
setMethod(
  ".check_vector",
  signature(input = "SpatVector", input_id = "ANY",
            extent = "SpatVector", out_class = "character"),
  function(input, input_id, extent, out_class, ...) {
    if (!out_class %in% c("sf", "terra")) {
      cli::cli_abort(c("out_class should be one of sf or terra."))
    }
    input <- .check_id(input, input_id)

    if (!is.null(extent)) {
      if (!terra::same.crs(terra::crs(input), terra::crs(extent))) {
        extent <- reproject_std(extent, terra::crs(input))
      }
      input <- input[extent, ]
    }
    if (out_class == "sf") {
      input <- dep_switch(input)
    }
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
#' \dontrun{
#' .check_raster(system.file("extdata/nc_srtm15_otm.tif", package = "chopin"))
#' }
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


#' Check SpatRaster input then get the source file path
#' @keywords internal
#' @param input SpatRaster.
#' @noRd
.check_par_spatraster <- function(input) {
  if (inherits(input, c("SpatRaster"))) {
    if (!inherits(future::plan(), "multicore")) {
      cli::cli_alert_info(
        paste0(
          "SpatRaster class input is detected.\n",
          "Attempt to track the data source file path...\n"
        )
      )
      suppressWarnings(
        input <- try(terra::sources(input), silent = TRUE)
      )
      if (inherits(input, "try-error")) {
        cli::cli_abort(
          paste0(
            "Failed to track the data source file path.\n",
            "Please retry with the file path.\n"
          )
        )
      }
    }
  }
  return(input)
}


#' Check the parent package of a function
#' @param fun character(1). Function name
#' @returns character(1). Package name. Only one of "sf", "terra", "chopin"
#' @importFrom utils find
#' @noRd
.check_package <-
  function(fun) {
    library(sf)
    library(terra)
    options(sf_use_s2 = FALSE)

    funname <- find(fun)
    pkgname <- gsub("package:", "", funname)
    pkgname <- grep("terra|sf|chopin", pkgname, value = TRUE)
    if (length(pkgname) == 0) {
      cli::cli_abort("No parent package is found.\n")
    }
    if (length(pkgname) > 1) {
      cli::cli_abort("There are multiple parent packages matched.\n")
    }
    if (!pkgname %in% c("sf", "terra", "chopin")) {
      cli::cli_abort("Function should be one from sf, terra, or chopin.\n")
    }
    return(pkgname)
  }


#' Check the alignment of a function and the input objects
#' @keywords internal
#' @noRd
#' @param f The package name to be checked.
#' @param x The first input object.
#' @param y The second input object.
#' @description This function will check if `f` is a sf or terra function
#'  then get x and y classes. It compares the parent packages of
#'  `f`, `x`, `y`. It is internally designed that `x` and `y` match
#'  `x` and `y` in sf/terra/chopin function arguments. The packages to
#'  be checked must be loaded in the current R session.
.check_align_fxy <-
  function(
    f, x, y
  ) {
    if (is.character(x) && is.character(y)) {
      return(invisible(TRUE))
    }
    if (f == "chopin") {
      return(invisible(TRUE))
    }
    dep_x <- dep_check(x)
    dep_y <- dep_check(y)

    checkvec <- c(f, dep_x, dep_y)
    checkdup1 <- duplicated(checkvec)
    checkdup2 <- duplicated(checkvec, fromLast = TRUE)
    if (all("terra" == checkvec)) {
      if (!inherits(future::plan(), "multicore")) {
        cli::cli_abort(
          c("x" =
              paste(
                "terra inputs detected in both x and y.",
                "Please replace x and y to file paths to proceed.\n"
              )
          )
        )
      }
    }
    if (!all(checkdup1 | checkdup2)) {
      cli::cli_abort("The function should be applied to the same class.\n")
    }
    return(invisible(TRUE))
  }
