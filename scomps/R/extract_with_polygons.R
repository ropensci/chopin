# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title Extract summarized values from raster with generic polygons
#' 
#' @description For simplicity, it is assumed that the coordinate systems of the points and the raster are the same. Kernel function is not yet implemented. 
#' @param polys sf/SpatVector object. Polygons.
#' @param surf stars/SpatRaster object. A raster of whatnot a summary will be calculated
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
    stopifnot("Check class of the input points.\n" = any(is(polys, "sf"), is(polys, "SpatVector")))
    stopifnot("Check class of the input raster.\n" = any(is(surf, "stars"), is(surf, "SpatRaster")))
    stopifnot(is.character(id))

    cls_polys = check_packbound(polys)
    cls_surf = check_packbound(surf)

    if (cls_polys != cls_surf) {
      polys = switch_packbound(polys)
    }

    extract_with_polygons.sf = function(polys, surf, id, func) {
        extracted = stars::st_extract(x = surf, at = polys, FUN = func)
        # extracted = extracted |>
        #   group_by(!!sym(id)) |>
        #   summarize(across(-!!sym(id), ~func)) |>
        #   ungroup()
        return(extracted)
    }

    extract_with_polygons.terra = function(polys, surf, id, func) {
        extracted = terra::extract(surf, polys)
        extracted = extracted |>
          group_by(!!sym(id)) |>
          summarize(across(-!!sym(id), ~func)) |>
          ungroup()
        return(extracted)
    }

    extracted_poly = switch(
        cls_surf,
        sf = extract_with_polygons.sf(polys = polys, surf = surf, id = id, func = func),
        terra = extract_with_polygons.terra(polys = polys, surf = surf, id = id, func = func)
    )
    return(extracted_poly)
}

