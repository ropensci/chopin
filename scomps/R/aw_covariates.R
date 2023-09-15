# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Computing area weighted covariates using two polygon sf or SpatVector objects
#' @param poly_in A sf/SpatVector object at weighted means will be calculated.
#' @param poly_weight A sf/SpatVector object from which weighted means will be calculated.
#' @param id_poly_in character(1). The unique identifier of each polygon in poly_in
#' @return A data.frame with all numeric fields of area-weighted means.
#' @description When poly_in and poly_weight are different classes, poly_weight will be converted to the class of poly_in. 
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples 
#' # package
#' library(sf)
#' library(tictoc)
#' 
#' # run
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' nc = st_transform(nc, 5070)
#' pp = st_sample(nc, size = 300)
#' pp[["id"]] = seq(1,nrow(pp))
#' st_crs(pp) = "EPSG:5070"
#' ppb = st_buffer(pp, nQuadSegs=180, dist = units::set_units(20, 'km'))
#' 
#' tic()
#' ppb_nc_aw = aw_covariates(ppb, nc, 'id')
#' toc()
#' summary(ppb_nc_aw)
#' #### Example of aw_covariates ends ####
#' @export
aw_covariates <- function(poly_in, poly_weight, id_poly_in = "ID") {
    stopifnot("Inputs have invalid classes.\n" = 
      is(poly_in, "sf") || is(poly_weight, "sf") || is(poly_in, "SpatVector") || is(poly_weight, "SpatVector"))
    #check_crs()
    aw_covariates.terra = function(poly_in, poly_weight, id_poly_in = id_poly_in) {
        
        poly_intersected = terra::intersect(poly_in, poly_weight)
        poly_intersected[["area_segment_"]] = terra::expanse(poly_intersected)
        poly_intersected = data.frame(poly_intersected) |>
            dplyr::group_by(!!rlang::sym(id_poly_in)) |>
            dplyr::summarize(across(is.numeric,
                        ~weighted.mean(., w = area_segment_))) |>
            dplyr::ungroup()
        return(poly_intersected)
    }

    class_poly_in = check_packbound(poly_in)
    class_poly_weight = check_packbound(poly_weight)

    if (class_poly_in != class_poly_weight) {
      class_poly_weight = as(class_poly_weight, class(class_poly_in)[1])
    }
    
    switch(class_poly_in,
        sf = sf::st_interpolate_aw(poly_weight, poly_in, extensive = FALSE),
        terra = aw_covariates.terra(poly_in, poly_weight, id_poly_in = id_poly_in))
    
}

# ncbuf = terra::intersect(vect(ppb), vect(nc))
# ncbuf_a = ncbuf
# ncbuf_a$segarea = expanse(ncbuf_a)
# ncbuf_k = data.frame(ncbuf_a) |>
#   dplyr::group_by(id) |>
#   dplyr::summarize(across(is.numeric,
#                ~weighted.mean(., w = segarea))) |>
#   dplyr::ungroup()

#ncbufagg = terra::aggregate(ncbuf, by = 'id', fun = weighted.mean, w = ncbuf_a$segarea)

