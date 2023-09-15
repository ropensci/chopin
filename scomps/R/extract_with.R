# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Extract raster values with point buffers or polygons
#' 
#' @param raster SpatRaster object. 
#' @param vector SpatVector object.
#' @param id character(1). Unique identifier of each point.
#' @param func function taking one numeric vector argument.
#' @param mode one of "polygon" (generic polygons to extract raster values with) or "buffer" (point with buffer radius)
#' @param ... various. Passed to extract_with_buffer. See \code{?extract_with_buffer} for details.
#' @return 
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples 
#' # data
#' 
#' # run
#' clip_with()
#' 
#' @export 
extract_with <- function(raster, vector, id, func = mean, mode = "polygon", ...) {
    if (!mode %in% c("polygon", "buffer")) {
      stop("Argument 'mode' should be one of 'polygon' or 'buffer'.\n")
    }
    stopifnot(is(id, character))
    stopifnot(id %in% names(vector))

    extracted = 
      switch(mode,
        polygon = extract_with_polygons(vector, raster, id, func),
        buffer = extract_with_buffer(vector, raster, id = id, func = func, ...))
    return(extracted)
}

