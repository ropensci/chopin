#' Setting the clipping extent
#' @family Helper functions
#' @description Return clipping extent with buffer radius.
#'  It assumes the input CRS is projected and linear unit is meters.
#' @author Insang Song
#' @param pnts One of sf or SpatVector object. Target points of computation.
#' @param radius numeric(1). Buffer radius. It is assumed to be in meters
#' @param extrusion numeric(1). The extent extrusion factor.
#' @returns A `terra::ext` or sfc_POLYGON object of the computation extent.
#' @examples
#' library(sf)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' nc_path <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc_sf <- sf::st_read(nc_path)
#' nc_sf <- sf::st_transform(nc_sf, "EPSG:5070")
#' get_clip_ext(nc_sf, 2.5e4)
#' nc_vect <- terra::vect(nc_sf)
#' get_clip_ext(nc_vect, 2.5e4)
#' @importFrom terra ext
#' @importFrom sf st_bbox
#' @importFrom sf st_as_sfc
#' @keywords internal
get_clip_ext <- function(
  pnts,
  radius,
  extrusion = 1.1
) {
  detected <- dep_check(pnts)
  if (detected == "terra") {
    ext_input <- terra::ext(pnts)
    # Note that `+` operation on
    # terra::ext output accounts for the operand as it is.
    ext_input <- ext_input + (extrusion * radius)
  }
  if (detected == "sf") {
    ext_input <- sf::st_bbox(pnts)
    # Note that `+` operation on st_bbox output
    # simply adds the number; we add a vector here.
    ext_input <- ext_input + ((extrusion * c(-1, -1, 1, 1) * radius))
    ext_input <- sf::st_as_sfc(ext_input)
  }
  return(ext_input)
}


#' Clip to the buffered extent of input vector
#' @family Helper functions
#' @description Clip input vector by
#'  the expected maximum extent of computation.
#' @keywords internal
#' @author Insang Song
#' @param x `sf` or `SpatVector` object to be clipped
#' @param y `sf` or `SpatVector` object
#' @param radius `numeric(1)`. Circular buffer radius.
#'  this value will be automatically multiplied by 1.1
#' @returns A clipped `sf` or `SpatVector` object.
#' @examples
#' library(sf)
#' library(stars)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
#' bcsd <- stars::read_stars(bcsd_path)
#' bcsd <- sf::st_as_sf(bcsd)
#' bcsd_rpnt <- sf::st_as_sf(sf::st_sample(bcsd, 4L))
#' bcsd_rpntm <- sf::st_as_sf(sf::st_sample(bcsd, 1000L))
#' clip_vec_ext(bcsd_rpntm, 1000, bcsd_rpnt)
#' @importFrom sf st_intersection
#' @importFrom terra intersect
clip_vec_ext <- function(
  x,
  y,
  radius
) {
  if (any(
    vapply(
      list(x, y, radius),
      FUN = is.null,
      FUN.VALUE = logical(1)
    )
  )) {
    cli::cli_abort(
      c("x" = "One or more required arguments are NULL. Please check.\n")
    )
  }
  detected_pnts <- dep_check(y)
  detected_target <- dep_check(x)

  if (detected_pnts != detected_target) {
    cli::cli_warn(c("Inputs are not the same class.\n"))
    target_input <- dep_switch(x)
  }

  ext_input <- get_clip_ext(y, radius)

  cli::cli_inform(
    c("i" = "Clip target features with the input feature extent...\n")
  )
  if (detected_pnts == "sf") {
    cae <-
      sf::st_intersection(x = target_input, y = ext_input)
  }
  if (detected_pnts == "terra") {
    cae <- terra::intersect(target_input, ext_input)
  }

  return(cae)
}

#' Clip input raster with a buffered vector extent.
#' @family Helper functions
#' @keywords internal soft-deprecated
#' @description Clip input raster by the expected maximum extent of
#' computation.
#' @param x `SpatRaster` object to be clipped
#' @param y `sf` or `SpatVector` object
#' @param radius numeric(1). buffer radius.
#' This value will be automatically multiplied by `extrusion`
#' @param nqsegs integer(1). the number of points per a quarter circle
#' @param extrusion numeric(1). Extrusion factor for the extent. Default is 1.1
#' @returns A clipped `SpatRaster` object.
#' @author Insang Song
#' @examples
#' library(terra)
#'
#' ras_rand <- terra::rast(nrow = 20, ncol = 20)
#' terra::values(ras_rand) <- runif(400L)
#' vec_rand_p <-
#'   data.frame(
#'     x = c(3, 5, 3.2, 8),
#'     y = c(12, 10, 15, 12),
#'     z = c(0, 1, 2, 3)
#'   )
#' ras_rand_p <- terra::vect(vec_rand_p, geom = c("x", "y"))
#' clip_ras_ext(x = ras_rand, y = vec_rand_p, radius = 1.5)
#' @importFrom terra vect crop
clip_ras_ext <- function(
  x = NULL,
  y = NULL,
  radius = NULL,
  nqsegs = 180L,
  extrusion = 1.1
) {
  if (any(
    vapply(list(y, radius, x),
           FUN = is.null,
           FUN.VALUE = logical(1))
  )) {
    cli::cli_abort(
      c("x" = "Any of required arguments are NULL. Please check.\n")
    )
  }
  radius <- extrusion * radius
  ext_input <- get_clip_ext(y, radius)
  ext_input <- terra::vect(ext_input)

  cae <- terra::crop(x, ext_input, snap = "out")
  return(cae)
}