# Generated from litr_package_template.rmd: do not edit by hand

#' Computing area weighted covariates (to be written)
#' @param poly_in 
#' @param poly_weight 
#' @return A data.frame object
#' @description 
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples 
#' # data
#' 
#' # run
#' aw_covariates()
#' 
#' @export
aw_covariates <- function(poly_in, poly_weight) {
    sf::st_interpolate_aw(poly_weight, poly_in)
    
    terra::clip()
    terra::gridDist()
    terra::rast
}
