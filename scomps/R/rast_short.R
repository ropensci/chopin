# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Quick call for SpatRaster with a window
#' 
#' @param rasterpath character(1). Path to the raster file.
#' @param win Named integer vector (4) or terra::ext() results.
#' @param author Insang Song 
#' @export 
rast_short <- function(rasterpath, win) {
  terra::rast(rasterpath, win = win)
}

