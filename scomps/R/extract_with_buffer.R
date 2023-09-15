# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title Extract summarized values from raster with points and a buffer radius (to be written)
#' 
#' @description For simplicity, it is assumed that the coordinate systems of the points and the raster are the same. Kernel function is not yet implemented. 
#' @param points SpatVector object. Coordinates where buffers will be generated
#' @param surf SpatRaster object. A raster of whatnot a summary will be calculated
#' @param radius numeric(1). Buffer radius. here we assume circular buffers only
#' @param id character(1). Unique identifier of each point.
#' @param qsegs integer(1). Number of vertices at a quarter of a circle. Default is 90.
#' @param func a function taking a numeric vector argument.
#' @param kernel character(1). Name of a kernel function (yet to be implemented)
#' @param bandwidth numeric(1). Kernel bandwidth.
#' @return a data.frame object with mean value
#' @author Insang Song \email{geoissong@@gmail.com}
#' 
#' @export
extract_with_buffer <- function(
    points, surf, radius, id, qsegs = 90, func = mean, kernel = NULL, bandwidth = NULL
    ) {
    # type check
    stopifnot("Check class of the input points.\n" = is(points, "SpatVector"))
    stopifnot("Check class of the input radius.\n" = is(radius, integer))
    stopifnot(is(id, character))
    stopifnot(is(qsegs, integer))

    if (!is.null(kernel)) {
        extracted = extract_with_buffer.flat(points = points,
                                      surf = surf,
                                      radius = radius,
                                      id = id,
                                      func = func,
                                      qsegs = qsegs)
        return(extracted)
    }

    extracted = extract_with_buffer.kernel(points = points,
                                           surf = surf,
                                           radius = radius,
                                           id = id,
                                           func = func,
                                           qsegs = qsegs,
                                           kernel = kernel,
                                           bandwidth = bandwidth)
    return(extracted)

}



