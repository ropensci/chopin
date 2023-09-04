# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title clip_as_extent_ras: Clip input raster.
#' @description Clip input raster by the expected maximum extent of computation. 
#' @author Insang Song
#' @param pnts sf or SpatVector object
#' @param buffer_r numeric(1). buffer radius. this value will be automatically multiplied by 1.25
#' @param nqsegs integer(1). the number of points per a quarter circle
#' @param ras SpatRaster object to be clipped
#' @export
clip_as_extent_ras <- function(pnts, buffer_r, nqsegs = 180, ras){
  if (any(sapply(list(pnts, buffer_r, ras), is.null))) {
    stop("Any of required arguments are NULL. Please check.\n")
  }
  ext_input = set_clip_extent(pnts, buffer_r) |>
    terra::ext()

  cae <- terra::crop(ras, ext_input, snap = 'out')
  return(cae)
}
