# Generated from litr_package_template.rmd: do not edit by hand

#' Extent clipping
#' @description Clip input vector by the expected maximum extent of computation. 
#' @author Insang Song
#' @param pnts sf or SpatVector object
#' @param buffer_r numeric(1). buffer radius. this value will be automatically multiplied by 1.25
#' @param nqsegs integer(1). the number of points per a quarter circle; SOON TO BE DEPRECATED
#' @param target_input sf or SpatVector object to be clipped
#' @return A clipped sf or SpatVector object.
#' @export
clip_as_extent <- function(pnts, buffer_r, nqsegs=NULL, target_input){
  if (any(sapply(list(pnts, buffer_r, target_input), is.null))) {
    stop("One or more required arguments are NULL. Please check.\n")
  }
  detected_pnts = check_packbound(pnts)
  detected_target = check_packbound(target_input)

  if (detected_pnts != detected_target) {
    warning("Inputs are not the same class.\n")
    target_input = switch(detected_target,
        sf = terra::vect(target_input),
        terra = sf::st_as_sf(target_input))
  }
  
  ext_input = set_clip_extent(pnts, buffer_r)
  cat("Clip target features with the input feature extent...\n")
  if (detected_pnts == "sf") {
    cae = ext_input |>
      sf::st_intersection(x = target_input)
  }
  if (detected_pnts == "terra") {
    cae = terra::intersect(target_input, ext_input)
  }
   
  return(cae)
}
