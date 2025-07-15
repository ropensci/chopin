#' Setting the clipping extent
#' @family Helper functions
#' @description Return clipping extent with buffer radius.
#'  It assumes the input CRS is projected and linear unit is meters.
#' @author Insang Song
#' @param pnts One of sf or SpatVector object. Target points of computation.
#' @param radius numeric(1). Buffer radius. It is assumed to be in meters
#' @param extrusion numeric(1). The extent extrusion factor.
#'   Default is 1.1, meaning that the actual padding is 10 percent
#'   wider than `radius`.
#' @returns A [`terra::ext`] or sfc_POLYGON object of the computation extent.
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
