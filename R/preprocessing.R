# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Setting the clipping extent
#' @description Return clipping extent with buffer radius. It assumes the input CRS is projected and linear unit is meters.
#' @author Insang Song
#' @param pnts One of sf or vect class. Target points of computation.
#' @param buffer_r numeric(1). Buffer radius. It is assumed in metres 
#' @return A terra::ext or sfc_POLYGON object of the computation extent.
#' @export
set_clip_extent <- function(
  pnts,
  buffer_r) {
  detected <- check_packbound(pnts)
  if (detected == "terra") {
    ext_input <- terra::ext(pnts)
    # Note that `+` operation on terra::ext output accounts for the operand as it is.
    ext_input <- ext_input + (1.1 * buffer_r)
  }
  if (detected == "sf") {
    ext_input <- pnts |> sf::st_bbox()
    # Note that `+` operation on st_bbox output simply adds the number; we add a vector here.
    ext_input <- ext_input + ((1.1 * c(-1, -1, 1, 1) * buffer_r))
    ext_input <- sf::st_as_sfc(ext_input)
  }
  return(ext_input)
}

#' Quick call for SpatRaster with a window
#' 
#' @param rasterpath character(1). Path to the raster file.
#' @param win Named integer vector (4) or terra::ext() results.
#' @return SpatRaster object.
#' @author Insang Song
#' @export 
rast_short <- function(rasterpath, win) {
  if (!(all(is.numeric(win), !is.null(attr(win, "names")), length(win) == 4) ||
        is(win, "SpatExtent"))) {
    stop("Argument win should be one of named numeric vector or SpatExtent object.\n")
  }
  if (is.numeric(win) && !all(grepl("(xmax|xmin|ymax|ymin)", names(win)))) {
    stop("Numeric win without names is detected. Set valid names for all win elements.\n")
  }
  invisible(terra::rast(rasterpath, win = win))
}

