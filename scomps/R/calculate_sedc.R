# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Calculate SEDC covariates
#' @param point_from SpatVector object. Locations where the sum of SEDCs are calculated.
#' @param point_to SpatVector object. Locations where each SEDC is calculated.
#' @param sdec_bandwidth numeric(1). Distance at which the source concentration is reduced to exp(-3) (approximately 95 %)  
#' @param threshold numeric(1). For computational efficiency, the nearest points in threshold will be selected
#' @param target_fields character(varying). Field names in characters.
#' @description NOTE: sf implementation is pending. Only available for terra.
#' @author Insang Song
#' @export
calculate_sedc <- function(point_from, point_to, id, sedc_bandwidth, threshold, target_fields) {
  # define sources, set SEDC exponential decay range
  len_point_from = seq_len(nrow(point_from))
  len_point_to = seq_len(nrow(point_to))

  point_from$from_id = len_point_from
  # select egrid_v only if closer than 3e5 meters from each aqs
  point_from_buf = terra::buffer(point_from_buf, threshold = threshold, quadsegs = 90)
  point_to = point_to[point_from_buf,]
  point_to$to_id = len_point_to

  # near features with distance argument: only returns integer indices
  near_from_to = terra::nearby(point_from, point_to, distance = threshold)
  # attaching actual distance
  dist_near_to = terra::distance(point_from, point_to)
  dist_near_to_df = as.vector(dist_near_to)
  # adding integer indices
  dist_near_to_tdf = expand.grid(from_id = len_point_from, to_id = len_point_to)
  dist_near_to_df = cbind(dist_near_to_tdf, dist = dist_near_to_df)

  # summary
  near_from_to = near_from_to |>
    as_tibble() |>
    left_join(data.frame(point_from)) |>
    left_join(data.frame(point_to)) |>
    left_join(dist_near_to_df) |>
    # per the definition in https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html
    # exp(-3) is about 0.05
    mutate(w_sedc = exp((-3 * dist) / sedc_bandwidth)) |>
    group_by(!!sym(id)) |>
    summarize(across(all_of(target_fields), list(sedc = ~sum(w_sedc * ., na.rm = TRUE)))) |>
    ungroup()

  invisible(near_from_to)
  
}

