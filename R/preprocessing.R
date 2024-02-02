# Generated from chopin_rmarkdown_litr.rmd: do not edit by hand

#' @title Switch spatial data class
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
#' # should return stars object
#'
#' vec_rand <- terra::spatSample(ras_rand, size = 10L, as.points = TRUE)
#' sf_rand <- dep_switch(vec_rand)
#' sf_rand
#' # should return sf object
#' @importFrom terra vect
#' @importFrom terra rast
#' @importFrom sf st_as_sf
#' @importFrom stars st_as_stars
#' @export
dep_switch <- function(input) {
  if (!any(class(input) %in% c("sf", "stars", "SpatVector", "SpatRaster"))) {
    stop("Input should be one of sf or Spat* object.\n")
  }
  cls_input <- dep_check(input)
  type_input <- datamod(input)

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

#' Setting the clipping extent
#' @family Helper functions
#' @description Return clipping extent with buffer radius.
#'  It assumes the input CRS is projected and linear unit is meters.
#' @author Insang Song
#' @param pnts One of sf or SpatVector object. Target points of computation.
#' @param radius numeric(1). Buffer radius. It is assumed to be in meters
#' @returns A terra::ext or sfc_POLYGON object of the computation extent.
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
#' @export
get_clip_ext <- function(
  pnts,
  radius
) {
  detected <- dep_check(pnts)
  if (detected == "terra") {
    ext_input <- terra::ext(pnts)
    # Note that `+` operation on
    # terra::ext output accounts for the operand as it is.
    ext_input <- ext_input + (1.1 * radius)
  }
  if (detected == "sf") {
    ext_input <- sf::st_bbox(pnts)
    # Note that `+` operation on st_bbox output
    # simply adds the number; we add a vector here.
    ext_input <- ext_input + ((1.1 * c(-1, -1, 1, 1) * radius))
    ext_input <- sf::st_as_sfc(ext_input)
  }
  return(ext_input)
}

#' Quick call for SpatRaster with a window
#' @family Helper functions
#' @param rasterpath character(1). Path to the raster file.
#' @param win Named integer vector (4) or terra::ext() results.
#' @returns SpatRaster object.
#' @author Insang Song
#' @examples
#' library(terra)
#' bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
#' ext_small <- terra::ext(
#'   c(xmin = -80, xmax = -76, ymin = 35, ymax = 36)
#' )
#' rast_short(bcsd_path, ext_small)
#' @importFrom methods is
#' @importFrom terra rast
#' @export
rast_short <- function(rasterpath = NULL, win = NULL) {
  if (!(all(is.numeric(win), !is.null(attr(win, "names")), length(win) == 4) ||
          methods::is(win, "SpatExtent"))) {
    stop(
      "Argument win should be one of named numeric vector or SpatExtent object.
      \n"
    )
  }
  rast_sub <- terra::rast(rasterpath, win = win)
  return(rast_sub)
}

