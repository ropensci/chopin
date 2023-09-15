# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Subfunction: extract with buffers (flat weight; simple mean)
#' 
#' @export 
extract_with_buffer.flat <- function(
        points, surf, radius, id, qsegs, func = mean, kernel = NULL, bandwidth = NULL
    ) {
    # generate buffers
    bufs = terra::buffer(points, width = radius, quadsegs = qsegs)
    # crop raster
    bufs_extent = terra::ext(bufs)
    surf_cropped = terra::crop(surf, bufs_extent)
    name_surf_val = names(surf)
    # extract raster values
    surf_at_bufs = terra::extract(surf_cropped, bufs)
    surf_at_bufs_summary = 
        surf_at_bufs |> 
            group_by(ID) |> 
            summarize(across(all_of(name_surf_val), ~mean, na.rm=T)) |> 
            ungroup()
    return(surf_at_bufs_summary)
}
