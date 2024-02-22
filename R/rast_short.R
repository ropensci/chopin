# Generated from chopin_rmarkdown_litr.rmd: do not edit by hand

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

