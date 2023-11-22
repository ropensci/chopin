# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Extent clipping
#' @description Clip input vector by the expected maximum extent of computation. 
#' @author Insang Song
#' @param pnts sf or SpatVector object
#' @param buffer_r numeric(1). buffer radius. this value will be automatically multiplied by 1.25
#' @param target_input sf or SpatVector object to be clipped
#' @return A clipped sf or SpatVector object.
#' @export
clip_as_extent <- function(
  pnts,
  buffer_r,
  target_input) {
  if (any(sapply(list(pnts, buffer_r, target_input), is.null))) {
    stop("One or more required arguments are NULL. Please check.\n")
  }
  detected_pnts <- check_packbound(pnts)
  detected_target <- check_packbound(target_input)

  if (detected_pnts != detected_target) {
    warning("Inputs are not the same class.\n")
    target_input <- switch_packbound(target_input)
  }

  ext_input <- set_clip_extent(pnts, buffer_r)
  cat("Clip target features with the input feature extent...\n")
  if (detected_pnts == "sf") {
    cae <- ext_input |>
      sf::st_intersection(x = target_input)
  }
  if (detected_pnts == "terra") {
    cae <- terra::intersect(target_input, ext_input)
  }

  return(cae)
}

#' @title clip_as_extent_ras: Clip input raster.
#' @description Clip input raster by the expected maximum extent of computation. 
#' @author Insang Song
#' @param pnts sf or SpatVector object
#' @param buffer_r numeric(1). buffer radius.
#' This value will be automatically multiplied by 1.25
#' @param ras SpatRaster object to be clipped
#' @param nqsegs integer(1). the number of points per a quarter circle
#' @export
clip_as_extent_ras <- function(
  pnts,
  buffer_r,
  ras,
  nqsegs = 180L
  ) {
  if (any(sapply(list(pnts, buffer_r, ras), is.null))) {
    stop("Any of required arguments are NULL. Please check.\n")
  }
  ext_input <- set_clip_extent(pnts, buffer_r) |>
    terra::vect()

  cae <- terra::crop(ras, ext_input, snap = "out")
  return(cae)
}

#' @title Extract summarized values from raster with points and
#'  a buffer radius (to be written)
#' 
#' @description For simplicity, it is assumed that the coordinate systems of
#'  the points and the raster are the same.
#'  Kernel function is not yet implemented. 
#' @param points SpatVector object. Coordinates where buffers will be generated
#' @param surf SpatRaster object.
#'  A raster of whatnot a summary will be calculated
#' @param radius numeric(1). Buffer radius. here we assume circular buffers only
#' @param id character(1). Unique identifier of each point.
#' @param qsegs integer(1). Number of vertices at a quarter of a circle.
#'  Default is 90.
#' @param func a function taking a numeric vector argument.
#' @param kernel character(1). Name of a kernel function (yet to be implemented)
#' @param bandwidth numeric(1). Kernel bandwidth.
#' @param grid_ref SpatVector object.
#'  A unit grid polygon that is used to get a subset inside the polygon
#' @return a data.frame object with mean value
#' @author Insang Song \email{geoissong@@gmail.com}
#'
#' @export
extract_with_buffer <- function(
    points,
    surf,
    radius,
    id,
    qsegs = 90L,
    func = "mean",
    kernel = NULL,
    bandwidth = NULL,
    grid_ref = NULL
    ) {
  # type check
  stopifnot("Check class of the input points.\n" = methods::is(points, "SpatVector"))
  stopifnot("Check class of the input radius.\n" = is.numeric(radius))
  stopifnot(is.character(id))
  stopifnot(is.numeric(qsegs))

  if (!is.null(grid_ref)) {
    points <- points[grid_ref, ]
  }

  if (!is.null(kernel)) {
    extracted <- extract_with_buffer_kernel(points = points,
                                    surf = surf,
                                    radius = radius,
                                    id = id,
                                    func = func,
                                    qsegs = qsegs,
                                    kernel = kernel,
                                    bandwidth = bandwidth)
    return(extracted)
  }

  extracted <- extract_with_buffer_flat(points = points,
                                surf = surf,
                                radius = radius,
                                id = id,
                                func = func,
                                qsegs = qsegs)
  return(extracted)

}

# Subfunction: extract with buffers (flat weight; simple mean)
extract_with_buffer_flat <- function(
  points,
  surf,
  radius,
  id,
  qsegs,
  func = "mean",
  kernel = NULL,
  bandwidth = NULL
  ) {
  # generate buffers
  bufs <- terra::buffer(points, width = radius, quadsegs = qsegs)
  # crop raster
  bufs_extent <- terra::ext(bufs)
  surf_cropped <- terra::crop(surf, bufs_extent)
  name_surf_val <- names(surf)
  # extract raster values
  surf_at_bufs <- exactextractr::exact_extract(surf_cropped, sf::st_as_sf(bufs),
    fun = func, force_df = TRUE, append_cols = id)
  surf_at_bufs_summary <-
    surf_at_bufs #|>
      # dplyr::group_by(ID) |>
      # dplyr::summarize(dplyr::across(dplyr::all_of(name_surf_val), ~func(.) , na.rm = TRUE)) |> 
      # dplyr::ungroup()
  # colnames(surf_at_bufs_summary)[1] <- id
  return(surf_at_bufs_summary)
}


# Subfunction: extract with buffers (kernel weight; weighted mean)
extract_with_buffer_kernel <- function(
  points,
  surf,
  radius,
  id,
  qsegs,
  func = "mean",
  kernel,
  bandwidth
) {
  # generate buffers
  bufs <- terra::buffer(points, width = radius, quadsegs = qsegs)
  # crop raster
  bufs_extent <- terra::ext(bufs)
  surf_cropped <- terra::crop(surf, bufs_extent)
  name_surf_val <- names(surf)

  # TODO: kernel implementation


  # extract raster values
  surf_at_bufs <- exactextractr::exact_extract(surf_cropped, sf::st_as_sf(bufs),
    fun = func, force_df = TRUE, append_cols = id)
  surf_at_bufs_summary <-
    surf_at_bufs |>
      dplyr::group_by(ID) |>
      dplyr::summarize(
        dplyr::across(dplyr::all_of(name_surf_val), ~func(.), na.rm = T)
      ) |> 
      dplyr::ungroup()
  colnames(surf_at_bufs_summary)[1] <- id
  return(surf_at_bufs_summary)
}


#' @title Extract summarized values from raster with generic polygons
#' 
#' @description For simplicity, it is assumed that the coordinate systems of the points and the raster are the same. Kernel function is not yet implemented. 
#' @param polys sf/SpatVector object. Polygons.
#' @param surf SpatRaster object. A raster from which a summary will be calculated
#' @param id character(1). Unique identifier of each point.
#' @param func a generic function name in string or a function taking two arguments that are
#'  compatible with \code{\link[exactextractr]{exact_extract}}.
#'  For example, "mean" or or \code{\(x, w) weighted.mean(x, w, na.rm = TRUE)}
#' @param na.rm logical(1). NA values are omitted when summary is calculated.
#' @param grid_ref A character or sf/SpatVector object. To subset \code{polys} in \code{distribute_*} functions.
#' @return a data.frame object with function value
#' @author Insang Song \email{geoissong@@gmail.com}
#' @importFrom rlang sym
#' @importFrom dplyr across
#' @import exactextractr
#' @export
extract_with_polygons <- function(
    polys,
    surf,
    id,
    func = "mean",
    na.rm = TRUE,
    grid_ref = NULL
    ) {
  # type check
  stopifnot("Check class of the input points.\n" = any(methods::is(polys, "sf"), methods::is(polys, "SpatVector")))
  stopifnot("Check class of the input raster.\n" = methods::is(surf, "SpatRaster"))
  stopifnot(is.character(id))

  cls_polys <- check_packbound(polys)
  cls_surf <- check_packbound(surf)

  if (cls_polys != cls_surf) {
    polys <- switch_packbound(polys)
  }

  if (!is.null(grid_ref)) {
    polys <- polys[grid_ref, ]
  }

  extracted_poly <-
    exactextractr::exact_extract(
      x = surf,
      y = sf::st_as_sf(polys),
      fun = func,
      force_df = TRUE,
      append_cols = id
    )
  return(extracted_poly)
}


#' Extract raster values with point buffers or polygons
#'
#' @param vector sf/SpatVector object.
#' @param raster SpatRaster object.
#' @param id character(1). Unique identifier of each point.
#' @param func function taking one numeric vector argument.
#' @param mode one of "polygon" (generic polygons to extract raster values with) or "buffer" (point with buffer radius)
#' @param ... various. Passed to extract_with_buffer. See \code{?extract_with_buffer} for details.
#' @return A data.frame object with summarized raster values with respect to the mode (polygon or buffer) and the function.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @export
extract_with <- function(
  vector,
  raster,
  id,
  func = "mean",
  mode = c("polygon", "buffer"),
  ...) {

  mode <- match.arg(mode)

  stopifnot(is.character(id))
  stopifnot(id %in% names(vector))

  extracted <-
    switch(mode,
      polygon = extract_with_polygons(vector, raster, id = id, func = func, ...),
      buffer = extract_with_buffer(vector, raster, id = id, func = func, ...))
  return(extracted)
}


#' Calculate SEDC covariates
#' @param point_from SpatVector object. Locations where the sum of SEDCs are calculated.
#' @param point_to SpatVector object. Locations where each SEDC is calculated.
#' @param id character(1). Name of the unique id field in point_to.
#' @param sedc_bandwidth numeric(1).
#' Distance at which the source concentration is reduced to
#'  exp(-3) (approximately -95 %)
#' @param threshold numeric(1). For computational efficiency,
#'  the nearest points in threshold will be selected.
#'  Default is \code{2 * sedc_bandwidth}.
#' @param target_fields character(varying). Field names in characters.
#' @return data.frame (tibble) object with input field names with
#'  a suffix \code{"_sedc"} where the sums of EDC are stored.
#'  Additional attributes are attached for the EDC information.
#'    - attr(result, "sedc_bandwidth"): the bandwidth where
#'  concentration reduces to approximately five percent
#'    - attr(result, "sedc_threshold"): the threshold distance
#'  at which emission source points are excluded beyond that
#' @note Distance calculation is done with terra functions internally.
#'  Thus, the function internally converts sf objects in \code{point_*} arguments
#'  to terra.
#'  The optimal EDC should be carefully chosen by users.
#' @author Insang Song
#' @export
calculate_sedc <-
  function(
    point_from,
    point_to,
    id,
    sedc_bandwidth,
    threshold,
    target_fields) {
  # define sources, set SEDC exponential decay range
  len_point_from <- seq_len(nrow(point_from))
  len_point_to <- seq_len(nrow(point_to))

    pkginfo_from <- check_packbound(point_from)
    pkginfo_to <- check_packbound(point_to)

    if (any(pkginfo_from == "sf", pkginfo_to == "sf")) {
      point_from <- switch_packbound(point_from)
      point_to <- switch_packbound(point_to)
    }
    point_from$from_id <- len_point_from
    # select egrid_v only if closer than 3e5 meters from each aqs
    point_from_buf <-
      terra::buffer(
                    point_from,
                    width = threshold,
                    quadsegs = 90)
    point_to <- point_to[point_from_buf, ]
    point_to$to_id <- len_point_to

    # near features with distance argument: only returns integer indices
    near_from_to <- terra::nearby(point_from, point_to, distance = threshold)
    # attaching actual distance
    dist_near_to <- terra::distance(point_from, point_to)
    dist_near_to_df <- as.vector(dist_near_to)
    # adding integer indices
    dist_near_to_tdf <-
      expand.grid(
                  from_id = len_point_from,
                  to_id = len_point_to)
    dist_near_to_df <- cbind(dist_near_to_tdf, dist = dist_near_to_df)

    # summary
    near_from_to <- near_from_to |>
      dplyr::as_tibble() |>
      dplyr::left_join(data.frame(point_from)) |>
      dplyr::left_join(data.frame(point_to)) |>
      dplyr::left_join(dist_near_to_df) |>
      # per the definition in
      # https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html
      # exp(-3) is about 0.05
      dplyr::mutate(w_sedc = exp((-3 * dist) / sedc_bandwidth)) |>
      dplyr::group_by(!!rlang::sym(id)) |>
      dplyr::summarize(
        dplyr::across(
                      dplyr::all_of(target_fields),
                      list(sedc = ~sum(w_sedc * ., na.rm = TRUE)))
      ) |>
      dplyr::ungroup()
    
    attr(near_from_to, "sedc_bandwidth") <- sedc_bandwidth
    attr(near_from_to, "sedc_threshold") <- threshold

    return(near_from_to)
}


#' Computing area weighted covariates using two polygon sf or SpatVector objects
#' @param poly_in A sf/SpatVector object at weighted means will be calculated.
#' @param poly_weight A sf/SpatVector object from
#'  which weighted means will be calculated.
#' @param id_poly_in character(1).
#'  The unique identifier of each polygon in poly_in
#' @return A data.frame with all numeric fields of area-weighted means.
#' @description When poly_in and poly_weight are different classes,
#'  poly_weight will be converted to the class of poly_in.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples 
#' # package
#' library(sf)
#'
#' # run
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' nc <- sf::st_transform(nc, 5070)
#' pp <- sf::st_sample(nc, size = 300)
#' pp <- sf::st_as_sf(pp)
#' pp[["id"]] <- seq(1, nrow(pp))
#' sf::st_crs(pp) <- "EPSG:5070"
#' ppb <- sf::st_buffer(pp, nQuadSegs=180, dist = units::set_units(20, 'km'))
#'
#' system.time({ppb_nc_aw <- aw_covariates(ppb, nc, 'id')})
#' summary(ppb_nc_aw)
#' #### Example of aw_covariates ends ####
#' @export
aw_covariates <- function(
  poly_in, 
  poly_weight, 
  id_poly_in = "ID") {
  stopifnot("Inputs have invalid classes.\n" = 
    methods::is(poly_in, "sf") || methods::is(poly_weight, "sf") || methods::is(poly_in, "SpatVector") || methods::is(poly_weight, "SpatVector"))
  #check_crs()
  ## distinguish numeric and nonnumeric columns
  index_numeric <- grep("(integer|numeric)", unlist(sapply(poly_weight, class)))

  aw_covariates_terra <- function(
    poly_in, 
    poly_weight, 
    id_poly_in = id_poly_in) {
      poly_intersected <- terra::intersect(poly_in, poly_weight)
      poly_intersected[["area_segment_"]] <- terra::expanse(poly_intersected)
      poly_intersected <- data.frame(poly_intersected) |>
        dplyr::group_by(!!rlang::sym(id_poly_in)) |>
        dplyr::summarize(dplyr::across(dplyr::where(is.numeric),
          ~stats::weighted.mean(., w = area_segment_))) |>
        dplyr::ungroup()
      return(poly_intersected)
  }

  class_poly_in <- check_packbound(poly_in)
  class_poly_weight <- check_packbound(poly_weight)

  if (class_poly_in != class_poly_weight) {
    poly_weight <- switch_packbound(poly_weight)
  }

  switch(class_poly_in,
    sf = suppressWarnings(sf::st_interpolate_aw(poly_weight[, index_numeric],
      poly_in, extensive = FALSE)),
    terra = aw_covariates_terra(poly_in, poly_weight[, index_numeric],
      id_poly_in = id_poly_in))
    
}


