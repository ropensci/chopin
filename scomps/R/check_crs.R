# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Check Coordinate Reference System
#' @param x sf/stars/SpatVector/SpatRaster object.
#' @return A st_crs or crs object.
#' @description 
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples 
#' # data
#' install.packages('spData')
#' library(spData)
#' data(us_states)
#' check_crs(us_states)
#' 
#' @export 
check_crs <- function(x) {
    stopifnot("Input is invalid.\n" = (is(x, "sf") || is(x, "stars") || is(x, "SpatVector") || is(x, "SpatRaster")))
    
    if (is(x, "sf") || is(x, "stars")) {
        crs_wkt = sf::st_crs(x)
    } else {
        crs_wkt = terra::crs(x)
    }

    stopifnot("No CRS is defined in the input. Please consult the metadata or the data source.\n" = !is.na(crs_wkt) || crs_wkt != "")
    return(crs_wkt)
}
