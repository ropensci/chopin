# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Check if the data extent is inside the reference bounding box
#' 
#' @description One of the most common errors in spatial computation is rooted in the entirely or partly incomparable spatial extents of input datasets. This function returns whether your data is inside the target computational extent. It is assumed that you know and have the exact computational region. This function will return TRUE if the reference region completely contains your data's extent and FALSE otherwise.
#' @param data_query sf*/stars/SpatVector/SpatRaster object.
#' @param reference sf*/stars/SpatVector/SpatRaster object or a named numeric vector with four names (xmin, ymin, xmax, and ymax).
#' @param reference_crs Well-known-text-formatted or EPSG code of the reference's coordinate system. Only required when a named numeric vector is passed to reference. 
#' @return TRUE (the queried data extent is completely within the reference bounding box) or FALSE 
#' @author Insang Song \email{geoissong@@gmail.com}
#' 
#' @export
check_bbox <- function(
  data_query, reference, reference_crs = NULL
) {
  if (is.numeric(reference) && is.null(reference_crs)) {
    stop("CRS should be entered when the reference extent is a vector.\n")
  }
  if (is.numeric(reference) && !is.null(reference_crs)) {
    reference = sf::st_as_sfc(sf::st_bbox(reference), crs = reference_crs)
  }
  query_crs = check_crs(data_query)
  ref_crs = check_crs(reference)
  if (is.na(query_crs) || is.null(query_crs)) {
    stop("The dataset you queried has no CRS. Please make sure your dataset has the correct CRS.\n")
  }

  # ...

  check_result
  return(check_result)
}


