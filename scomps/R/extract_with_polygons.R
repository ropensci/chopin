# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title Extract summarized values from raster with generic polygons
#' 
#' @description For simplicity, it is assumed that the coordinate systems of the points and the raster are the same. Kernel function is not yet implemented. 
#' @param polys SpatVector object. Polygons.
#' @param surf SpatRaster object. A raster of whatnot a summary will be calculated
#' @param id character(1). Unique identifier of each point.
#' @param func a function taking one argument. For example, function(x) mean(x, na.rm = TRUE) or \(x) mode(x, na.rm = TRUE)
#' @param na.rm logical(1). NA values are omitted when summary is calculated.
#' @return a data.frame object with function value
#' @author Insang Song \email{geoissong@@gmail.com}
#' 
#' @export
extract_with_polygons <- function(
    polys, surf, id, func = mean, na.rm = TRUE
    ) {
    # type check
    stopifnot("Check class of the input points.\n" = is(polys, "SpatVector"))
    stopifnot(is.character(id))

    extracted = terra::extract(surf, polys)
    extracted = extracted |>
      group_by(!!sym(id)) |>
      summarize(across(-!!sym(id), ~func)) |>
      ungroup()
    return(extracted)

}



