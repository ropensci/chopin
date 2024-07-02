#' Kernel functions
#' @family Macros for calculation
#' @param kernel Kernel type. One of
#' `"uniform"`, `"quartic"`, `"triweight"`, and `"epanechnikov"`
#' @param d Distance
#' @param bw Bandwidth of a kernel
#' @returns numeric. Kernel weights.
#' @references \href{https://github.com/JanCaha/SpatialKDE}{SpatialKDE source}
#' @examples
#' v_dist <- c(1, 10, 100, 25, 50, 0.1)
#' bw_dist1 <- 1
#' bw_dist2 <- 10
#' kernelfunction(v_dist, bw_dist1, "uniform")
#' kernelfunction(v_dist, bw_dist1, "quartic")
#' kernelfunction(v_dist, bw_dist1, "triweight")
#' kernelfunction(v_dist, bw_dist1, "epanechnikov")
#' kernelfunction(v_dist, bw_dist2, "uniform")
#' kernelfunction(v_dist, bw_dist2, "quartic")
#' kernelfunction(v_dist, bw_dist2, "triweight")
#' kernelfunction(v_dist, bw_dist2, "epanechnikov")
#' @export
kernelfunction <-
  function(
    d,
    bw,
    kernel = c("uniform", "quartic", "triweight", "epanechnikov")
  ) {
    kernel <- match.arg(kernel)
    if (kernel == "uniform") {
      d <- ifelse(d > bw, 0, 0.5)
    } else {
      d <- ifelse(d > bw, bw, d)
    }
    switch(kernel,
      uniform = d,
      quartic = (15 / 16) * (1 - ((d / bw)^2))^2,
      triweight = 1 - ((d / bw) ^ 3),
      epanechnikov = (3 / 4) * (1 - ((d / bw)^2))
    )

  }

#' Clip to the buffered extent of input vector
#' @family Helper functions
#' @description Clip input vector by
#'  the expected maximum extent of computation.
#' @author Insang Song
#' @param pnts `sf` or `SpatVector` object
#' @param radius `numeric(1)`. Circular buffer radius.
#'  this value will be automatically multiplied by 1.1
#' @param target_input `sf` or `SpatVector` object to be clipped
#' @returns A clipped `sf` or `SpatVector` object.
#' @examples
#' library(sf)
#' library(stars)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
#' bcsd <- stars::read_stars(bcsd_path)
#' bcsd <- sf::st_as_sf(bcsd)
#' bcsd_rpnt <- sf::st_as_sf(sf::st_sample(bcsd, 4L))
#' bcsd_rpntm <- sf::st_as_sf(sf::st_sample(bcsd, 1000L))
#' clip_vec_ext(bcsd_rpntm, 1000, bcsd_rpnt)
#' @importFrom sf st_intersection
#' @importFrom terra intersect
#' @export
clip_vec_ext <- function(
  pnts,
  radius,
  target_input
) {
  if (any(
    vapply(
      list(pnts, radius, target_input),
      FUN = is.null,
      FUN.VALUE = logical(1)
    )
  )) {
    cli::cli_abort("One or more required arguments are NULL. Please check.\n")
  }
  detected_pnts <- dep_check(pnts)
  detected_target <- dep_check(target_input)

  if (detected_pnts != detected_target) {
    cli::cli_warn("Inputs are not the same class.\n")
    target_input <- dep_switch(target_input)
  }

  ext_input <- get_clip_ext(pnts, radius)
  cli::cli_inform("Clip target features with the input feature extent...\n")
  if (detected_pnts == "sf") {
    cae <- ext_input |>
      sf::st_intersection(x = target_input)
  }
  if (detected_pnts == "terra") {
    cae <- terra::intersect(target_input, ext_input)
  }

  return(cae)
}

#' Clip input raster with a buffered vector extent.
#' @family Helper functions
#' @description Clip input raster by the expected maximum extent of
#' computation.
#' @param pnts `sf` or `SpatVector` object
#' @param radius numeric(1). buffer radius.
#' This value will be automatically multiplied by 1.25
#' @param ras `SpatRaster` object to be clipped
#' @param nqsegs `integer(1)`. the number of points per a quarter circle
#' @returns A clipped `SpatRaster` object.
#' @author Insang Song
#' @examples
#' library(terra)
#'
#' ras_rand <- terra::rast(nrow = 20, ncol = 20)
#' terra::values(ras_rand) <- runif(400L)
#' ras_rand_p <-
#'   data.frame(
#'     x = c(3, 5, 3.2, 8),
#'     y = c(12, 10, 15, 12),
#'     z = c(0, 1, 2, 3)
#'   )
#' ras_rand_p <- terra::vect(ras_rand_p, geom = c("x", "y"))
#' clip_ras_ext(ras_rand_p, 1.5, ras_rand)
#' @importFrom terra vect
#' @importFrom terra crop
#' @export
clip_ras_ext <- function(
  pnts = NULL,
  radius = NULL,
  ras = NULL,
  nqsegs = 180L
) {
  if (any(
    vapply(list(pnts, radius, ras),
           FUN = is.null,
           FUN.VALUE = logical(1))
  )) {
    cli::cli_abort("Any of required arguments are NULL. Please check.\n")
  }
  ext_input <- get_clip_ext(pnts, radius) |>
    terra::vect()

  cae <- terra::crop(ras, ext_input, snap = "out")
  return(cae)
}

#' Extract summarized values from raster with points and a buffer radius
#' @family Macros for calculation
#' @description For simplicity, it is assumed that the coordinate systems of
#'  the points and the raster are the same.
#' @note
#' When `Sys.setenv("CHOPIN_FORCE_CROP" = "TRUE")` is set, the raster will be
#' cropped to the extent of the polygons (with `snap` = `"out"`).
#' To note, the function is designed to work with the `exactextractr` package.
#' Arguments of `exactextractr::exact_extract` are set as below
#' (default otherwise listed):
#' * `force_df` = `TRUE`
#' * `stack_apply` = `TRUE`
#' * `max_cells_in_memory` = `2e8`
#' * `progress` = `FALSE`
#' @param points `sf`/`SpatVector` object.
#' Coordinates where buffers will be generated.
#' @param surf `SpatRaster` object or file path(s) with extensions
#' that are GDAL-compatible. A raster from which a summary will be calculated
#' @param radius numeric(1). Buffer radius.
#'  Here we assume circular buffers only
#' @param id character(1). Unique identifier of each point.
#' @param qsegs integer(1). Number of vertices at a quarter of a circle.
#'  Default is `90L`.
#' @param func a function taking a numeric vector argument.
#' @param kernel character(1). Name of a kernel function
#' One of `"uniform"`, `"triweight"`, `"quartic"`, and `"epanechnikov"`
#' @param bandwidth numeric(1). Kernel bandwidth.
#' @param extent numeric(4) or SpatExtent. Extent of clipping vector.
#'   It only works with `points` of character(1) file path.
#'   When using numeric(4), it should be in the order of
#'   `c(xmin, xmax, ymin, ymax)`. The coordinate system should be the same
#'   as the `points`.
#' @param max_cells integer(1). Maximum number of cells in memory.
#' See [`exactextractr::exact_extract`] for more details.
#' @param ... Placeholder.
#' @returns a data.frame object with mean value
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' library(terra)
#' rrast <- terra::rast(nrow = 100, ncol = 100)
#' terra::crs(rrast) <- "EPSG:5070"
#' terra::values(rrast) <- rgamma(1e4, 4, 2)
#' rpnt <- terra::spatSample(rrast, 100L, as.points = TRUE)
#' rpnt$pid <- sprintf("id_%03d", seq(1, 100))
#' extract_at_buffer(rpnt, rrast, 4, "pid")
#' @importFrom exactextractr exact_extract
#' @importFrom methods is
#' @importFrom terra ext
#' @importFrom terra crop
#' @importFrom terra buffer
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr all_of
#' @importFrom dplyr across
#' @importFrom dplyr ungroup
#' @export
extract_at_buffer <- function(
  points = NULL,
  surf = NULL,
  radius = NULL,
  id = NULL,
  qsegs = 90L,
  func = "mean",
  kernel = NULL,
  bandwidth = NULL,
  extent = NULL,
  max_cells = 2e7,
  ...
) {

  if (!methods::is(surf, "SpatRaster")) {
    surf <- try(terra::rast(surf))
    if (inherits(surf, "try-error")) {
      cli::cli_abort("Check class of the input raster.\n")
    }
  }
  if (!is.numeric(radius)) {
    cli::cli_abort("Check class of the input radius.\n")
  }
  if (!is.numeric(qsegs)) {
    cli::cli_abort("qsegs should be numeric.\n")
  }
  points <- check_subject(points, extent = extent, subject_id = id)

  if (!is.null(kernel)) {
    extracted <-
      extract_at_buffer_kernel(
        points = points,
        surf = surf,
        radius = radius,
        id = id,
        func = func,
        qsegs = qsegs,
        kernel = kernel,
        bandwidth = bandwidth,
        max_cells = max_cells
      )
    return(extracted)
  }

  extracted <-
    extract_at_buffer_flat(
      points = points,
      surf = surf,
      radius = radius,
      id = id,
      func = func,
      qsegs = qsegs,
      max_cells = max_cells
    )
  return(extracted)

}

# Subfunction: extract at buffers with uniform weights
#' @rdname extract_at_buffer
#' @export
extract_at_buffer_flat <- function(
  points = NULL,
  surf = NULL,
  radius = NULL,
  id = NULL,
  qsegs = NULL,
  func = "mean",
  kernel = NULL,
  bandwidth = NULL,
  max_cells = 2e7,
  ...
) {
  # generate buffers
  bufs <- terra::buffer(points, width = radius, quadsegs = qsegs)
  bufs <- reproject_b2r(bufs, surf)
  # crop raster
  if (Sys.getenv("CHOPIN_FORCE_CROP") == "TRUE") {
    bufs_extent <- terra::ext(bufs)
    surf_cropped <- terra::crop(surf, bufs_extent, snap = "out")
  } else {
    surf_cropped <- surf
  }

  # extract raster values
  surf_at_bufs <-
    exactextractr::exact_extract(
      x = surf_cropped,
      y = sf::st_as_sf(bufs),
      fun = func,
      force_df = TRUE,
      stack_apply = TRUE,
      append_cols = id,
      progress = FALSE,
      max_cells_in_memory = max_cells
    )
  return(surf_at_bufs)
}


# Subfunction: extract at buffers with kernel weight
#' @rdname extract_at_buffer
#' @export
extract_at_buffer_kernel <- function(
  points = NULL,
  surf = NULL,
  radius = NULL,
  id = NULL,
  qsegs = NULL,
  func = stats::weighted.mean,
  kernel = NULL,
  bandwidth = NULL,
  max_cells = 2e7,
  ...
) {
  # generate buffers
  bufs <- terra::buffer(points, width = radius, quadsegs = qsegs)
  bufs <- reproject_b2r(bufs, surf)

  # crop raster
  if (Sys.getenv("CHOPIN_FORCE_CROP") == "TRUE") {
    bufs_extent <- terra::ext(bufs)
    surf_cropped <- terra::crop(surf, bufs_extent, snap = "out")
  } else {
    surf_cropped <- surf
  }

  name_surf_val <-
    ifelse(terra::nlyr(surf_cropped) == 1,
           "value", names(surf_cropped))
  # convert to data.frame
  coords_df <- as.data.frame(points, geom = "XY")
  # apply strict order
  coords_df <-
    coords_df[, grep(sprintf("^(%s|%s|%s)", id, "x", "y"), names(coords_df))]
  names(coords_df)[grep("(x|y)", names(coords_df))] <- c("xorig", "yorig")

  # for linter purpose
  xorig <- NULL
  yorig <- NULL
  x <- NULL
  y <- NULL
  pairdist <- NULL
  w_kernel <- NULL
  coverage_fraction <- NULL

  # extract raster values
  surf_at_bufs <-
    exactextractr::exact_extract(
      x = surf_cropped,
      y = sf::st_as_sf(bufs),
      force_df = TRUE,
      stack_apply = FALSE,
      include_cols = id,
      progress = FALSE,
      include_area = TRUE,
      include_xy = TRUE,
      max_cells_in_memory = max_cells
    )
  # post-processing
  surf_at_bufs <- do.call(rbind, surf_at_bufs)
  surf_at_bufs_summary <-
    surf_at_bufs |>
    dplyr::left_join(coords_df, by = id) |>
    # averaging with kernel weights
    dplyr::mutate(
      pairdist = terra::distance(
        x = cbind(xorig, yorig),
        y = cbind(x, y),
        pairwise = TRUE,
        lonlat = terra::is.lonlat(points)
      ),
      w_kernel = kernelfunction(pairdist, bandwidth, kernel),
      w_kernelarea = w_kernel * coverage_fraction
    ) |>
    dplyr::group_by(!!rlang::sym(id)) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(name_surf_val), ~func(., w = w_kernelarea))
    ) |>
    dplyr::ungroup()
  # restore the original identifier
  colnames(surf_at_bufs_summary)[1] <- id
  return(surf_at_bufs_summary)
}


#' @title Extract summarized values from raster with generic polygons
#' @family Macros for calculation
#' @description For simplicity, it is assumed that the coordinate systems of
#'  the points and the raster are the same.
#' @note
#' When `Sys.setenv("CHOPIN_FORCE_CROP" = "TRUE")` is set, the raster will be
#' cropped to the extent of the polygons (with `snap` = `"out"`).
#' To note, the function is designed to work with the `exactextractr` package.
#' Arguments of `exactextractr::exact_extract` are set as below
#' (default otherwise listed except for max_cells_in_memory,
#' which is set in the `max_cells` argument):
#' * `force_df` = `TRUE`
#' * `stack_apply` = `TRUE`
#' * `progress` = `FALSE`
#' @param polys `sf`/`SpatVector` object. Polygons.
#' @param surf `SpatRaster` object or file path(s) with extensions
#' that are GDAL-compatible. A raster from which a summary will be calculated
#' @param id character(1). Unique identifier of each point.
#' @param func a generic function name in string or
#'  a function taking two arguments that are
#'  compatible with \code{\link[exactextractr]{exact_extract}}.
#'  For example, `"mean"` or `\(x, w) weighted.mean(x, w, na.rm = TRUE)`
#' @param extent numeric(4) or SpatExtent. Extent of clipping vector.
#'   It only works with `polys` of character(1) file path.
#'   When using numeric(4), it should be in the order of
#'   `c(xmin, xmax, ymin, ymax)`. The coordinate system should be the same
#'   as the `polys`.
#' @param max_cells integer(1). Maximum number of cells in memory.
#' See [`exactextractr::exact_extract`] for more details.
#' @param ... Placeholder.
#' @returns a data.frame object with function value
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(ncpath)
#' nc <- terra::project(nc, "EPSG:5070")
#' rrast <- terra::rast(nc, nrow = 100, ncol = 220)
#' ncr <- terra::rasterize(nc, rrast)
#' terra::values(rrast) <- rgamma(2.2e4, 4, 2)
#' rpnt <- terra::spatSample(rrast, 16L, as.points = TRUE)
#' rpnt$pid <- sprintf("ID-%02d", seq(1, 16))
#' rpoly <-
#'   terra::buffer(rpnt, 5, capstyle = "square", joinstyle = "bevel")
#' extract_at_poly(rpoly, rrast, "pid")
#' @importFrom methods is
#' @importFrom rlang sym
#' @importFrom dplyr across
#' @importFrom exactextractr exact_extract
#' @export
extract_at_poly <- function(
  polys = NULL,
  surf = NULL,
  id = NULL,
  func = "mean",
  extent = NULL,
  max_cells = 2e7,
  ...
) {

  if (!methods::is(surf, "SpatRaster")) {
    surf <- try(terra::rast(surf))
    if (inherits(surf, "try-error")) {
      cli::cli_abort("Check class of the input raster.\n")
    }
  }
  polys <- check_subject(polys, extent = extent, subject_id = id)
  # reproject polygons to raster's crs
  polys <- reproject_b2r(polys, surf)
  # crop raster
  if (Sys.getenv("CHOPIN_FORCE_CROP") == "TRUE") {
    polys_extent <- terra::ext(polys)
    surf_cropped <- terra::crop(surf, polys_extent, snap = "out")
  } else {
    surf_cropped <- surf
  }

  extracted_poly <-
    exactextractr::exact_extract(
      x = surf_cropped,
      y = sf::st_as_sf(polys),
      fun = func,
      force_df = TRUE,
      stack_apply = TRUE,
      append_cols = id,
      progress = FALSE,
      max_cells_in_memory = max_cells
    )
  return(extracted_poly)
}


#' Extract raster values with point buffers or polygons
#' @family Macros for calculation
#' @param vector `sf`/`SpatVector` object.
#' @param raster `SpatRaster` object. or file path(s) with extensions
#' that are GDAL-compatible.
#' @param id character(1). Unique identifier of each point.
#' @param func function taking one numeric vector argument.
#' @param mode one of `"polygon"`
#'  (generic polygons to extract raster values with) or
#'  `"buffer"` (point with buffer radius)
#' @param ... Placeholder.
#'  See \code{?extract_at_buffer} for details.
#' @returns A data.frame object with summarized raster values with
#'  respect to the mode (polygon or buffer) and the function.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @seealso [extract_at_poly], [extract_at_buffer]
#' @examples
#' ## See ?extract_at_poly and ?extract_at_buffer
#' @export
extract_at <- function(
  vector = NULL,
  raster = NULL,
  id = NULL,
  func = "mean",
  mode = c("polygon", "buffer"),
  ...
) {

  mode <- match.arg(mode)
  stopifnot(is.character(id))

  extracted <-
    switch(mode,
      polygon =
      extract_at_poly(
        polys = vector,
        surf = raster,
        id = id,
        func = func,
        ...
      ),
      buffer =
      extract_at_buffer(
        points = vector,
        surf = raster,
        id = id,
        func = func,
        ...
      )
    )
  return(extracted)
}

#' @title Align vector CRS to raster's
#' @family Helper functions
#' @param vector `sf`/`stars`/`SpatVector`/`SpatRaster` object
#' @param raster `SpatRaster` object
#' @returns Reprojected object in the same class as \code{vector}
#' @author Insang Song
#' @examples
#' library(terra)
#' library(sf)
#' options(sf_use_s2 = FALSE)
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' elev <- system.file("ex/elev.tif", package = "terra")
#' nc <- terra::vect(ncpath)
#' elev <- terra::rast(elev)
#' reproject_b2r(nc, elev)
#' @importFrom sf st_transform
#' @importFrom terra project
#' @importFrom terra crs
#' @export
reproject_b2r <-
  function(
    vector = NULL,
    raster = NULL
  ) {
    detected_vec <- dep_check(vector)
    switch(detected_vec,
           sf = sf::st_transform(vector, terra::crs(raster)),
           terra = terra::project(vector, terra::crs(raster)))
  }


#' Calculate Sum of Exponentially Decaying Contributions (SEDC) covariates
#' @family Macros for calculation
#' @param point_from `SpatVector` object. Locations where
#'  the sum of SEDCs are calculated.
#' @param point_to `SpatVector` object.
#' Locations where each SEDC is calculated.
#' @param id character(1). Name of the unique id field in `point_to`.
#' @param sedc_bandwidth numeric(1).
#' Distance at which the source concentration is reduced to
#'  `exp(-3)` (approximately -95 %)
#' @param threshold numeric(1). For computational efficiency,
#'  the nearest points in threshold will be selected.
#'  \code{2 * sedc_bandwidth} is applied if this value remains `NULL`.
#' @param target_fields character. Field names to calculate SEDC.
#' @param extent_from numeric(4) or SpatExtent. Extent of clipping `point_from`.
#'   It only works with `point_from` of character(1) file path.
#'   See [`terra::ext`] for more details. Coordinate systems should match.
#' @param extent_to numeric(4) or SpatExtent. Extent of clipping `point_to`.
#' @param ... Placeholder.
#' @returns data.frame (tibble) object with input field names with
#'  a suffix \code{"_sedc"} where the sums of EDC are stored.
#'  Additional attributes are attached for the EDC information.
#'    - attr(result, "sedc_bandwidth"): the bandwidth where
#'  concentration reduces to approximately five percent
#'    - attr(result, "sedc_threshold"): the threshold distance
#'  at which emission source points are excluded beyond that
#' @note Distance calculation is done with `terra` functions internally.
#'  Thus, the function internally converts `sf` objects in
#'  \code{point_*} arguments to `terra`. Please note that any `NA` values
#'  in the input will be ignored in SEDC calculation.
#' @author Insang Song
#' @references
#' * [Messier KP, Akita Y, Serre ML. (2012).
#'   Integrating Address Geocoding, Land Use
#'   Regression, and Spatiotemporal Geostatistical Estimation
#'   for Groundwater Tetrachloroethylene.
#'   _Environmental Science & Technology_ 46(5), 2772-2780.
#'   ](https://dx.doi.org/10.1021/es203152a)
#' * Wiesner C. (n.d.). [Euclidean Sum of Exponentially Decaying
#'   Contributions Tutorial](
#'   https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html)
#' @examples
#' library(terra)
#' library(sf)
#' set.seed(101)
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(ncpath)
#' nc <- terra::project(nc, "EPSG:5070")
#' pnt_from <- terra::centroids(nc, inside = TRUE)
#' pnt_from <- pnt_from[, "NAME"]
#' pnt_to <- terra::spatSample(nc, 100L)
#' pnt_to$pid <- seq(1, 100)
#' pnt_to <- pnt_to[, "pid"]
#' pnt_to$val1 <- rgamma(100L, 1, 0.05)
#' pnt_to$val2 <- rgamma(100L, 2, 1)
#'
#' vals <- c("val1", "val2")
#' summarize_sedc(pnt_from, pnt_to, "NAME", 1e5, 2e5, vals)
#' @importFrom dplyr as_tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr all_of
#' @importFrom dplyr across
#' @importFrom dplyr ungroup
#' @importFrom terra nearby
#' @importFrom terra distance
#' @importFrom terra buffer
#' @importFrom rlang sym
#' @export
summarize_sedc <-
  function(
    point_from = NULL,
    point_to = NULL,
    id = NULL,
    sedc_bandwidth = NULL,
    threshold = NULL,
    target_fields = NULL,
    extent_from = NULL,
    extent_to = NULL,
    ...
  ) {

    point_from <-
      check_subject(point_from, extent = extent_from, subject_id = id)
    point_to <-
      check_subject(point_to, extent = extent_to, subject_id = NULL)

    # define sources, set SEDC exponential decay range
    len_point_from <- seq_len(nrow(point_from))
    len_point_to <- seq_len(nrow(point_to))

    pkginfo_from <- dep_check(point_from)
    pkginfo_to <- dep_check(point_to)

    if (any(pkginfo_from == "sf", pkginfo_to == "sf")) {
      point_from <- dep_switch(point_from)
      point_to <- dep_switch(point_to)
    }

    cn_overlap <- intersect(names(point_from), names(point_to))
    if (length(cn_overlap) > 0) {
      cli::cli_warn(
        sprintf(
          "There are %d fields with the same name.
The result may not be accurate.\n",
          length(cn_overlap)
        )
      )
    }
    point_from$from_id <- len_point_from
    if (is.null(threshold)) {
      threshold <- 2 * sedc_bandwidth
    }
    # select point_to with threshold
    # default threshold is 2 * sedc_bandwidth
    point_from_buf <-
      terra::buffer(
        point_from,
        width = threshold,
        quadsegs = 90
      )
    point_to <- point_to[point_from_buf, ]
    point_to$to_id <- len_point_to
    dist <- NULL

    # near features with distance argument: only returns integer indices
    near_from_to <-
      terra::nearby(point_from, point_to, distance = threshold)
    # attaching actual distance
    dist_near_to <- terra::distance(point_from, point_to)
    dist_near_to_df <- as.vector(dist_near_to)
    # adding integer indices
    dist_near_to_tdf <-
      expand.grid(
        from_id = len_point_from,
        to_id = len_point_to
      )
    dist_near_to_df <- cbind(dist_near_to_tdf, dist = dist_near_to_df)

    # summary
    near_from_to <- near_from_to |>
      dplyr::as_tibble() |>
      dplyr::left_join(data.frame(point_from), by = "from_id") |>
      dplyr::left_join(data.frame(point_to), by = "to_id") |>
      dplyr::left_join(dist_near_to_df, by = c("from_id", "to_id")) |>
      # per the definition in
      # https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html
      # exp(-3) is about 0.05
      dplyr::mutate(w_sedc = exp((-3 * dist) / sedc_bandwidth)) |>
      dplyr::group_by(!!rlang::sym(id)) |>
      dplyr::summarize(
        dplyr::across(
          dplyr::all_of(target_fields),
          list(sedc = ~sum(w_sedc * ., na.rm = TRUE))
        )
      ) |>
      dplyr::ungroup()

    attr(near_from_to, "sedc_bandwidth") <- sedc_bandwidth
    attr(near_from_to, "sedc_threshold") <- threshold

    return(near_from_to)
  }


#' Area weighted summary using two polygon sf or SpatVector objects
#' @family Macros for calculation
#' @param poly_in A sf/SpatVector object or file path of polygons detectable
#'   with GDAL driver at weighted means will be calculated.
#' @param poly_weight A sf/SpatVector object or file path of polygons from
#'  which weighted means will be calculated.
#' @param target_fields character. Field names to calculate area-weighted.
#' @param id_poly_in character(1).
#'  The unique identifier of each polygon in `poly_in`.
#'  Default is `"ID"`.
#' @param fun function(1). The function to calculate the weighted summary.
#' Default is [`stats::weighted.mean`]. The function must have a `w` argument.
#' @param extent numeric(4) or SpatExtent object. Extent of clipping `poly_in`.
#' It only works with `poly_in` of character(1) file path.
#' See [`terra::ext`] for more details. Coordinate systems should match.
#' @returns A data.frame with all numeric fields of area-weighted means.
#' @description When `poly_in` and `poly_weight` are different classes,
#'  `poly_weight` will be converted to the class of `poly_in`.
#' @note If `poly_in` and `poly_weight` are characters, they will be
#'   read as `terra::vect` objects.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @importFrom terra expanse
#' @importFrom rlang sym
#' @importFrom dplyr where
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr across
#' @importFrom dplyr ungroup
#' @importFrom terra intersect
#' @importFrom sf st_interpolate_aw
#' @importFrom stats weighted.mean
summarize_aw_old <-
  function(
    poly_in = NULL,
    poly_weight = NULL,
    target_fields = NULL,
    id_poly_in = "ID",
    fun = stats::weighted.mean,
    extent = NULL
  ) {

    poly_in <- check_subject(poly_in, extent = extent, subject_id = id_poly_in)
    poly_weight <- check_subject(poly_weight, extent = extent)

    summarize_aw_terra <-
      function(
        poly_in = NULL,
        poly_weight = NULL,
        target_fields = NULL,
        id_poly_in = id_poly_in
      ) {
        poly_intersected <- terra::intersect(poly_in, poly_weight)
        poly_intersected[["area_segment_"]] <-
          terra::expanse(poly_intersected)
        poly_intersected <- data.frame(poly_intersected) |>
          dplyr::group_by(!!rlang::sym(id_poly_in)) |>
          dplyr::summarize(
            dplyr::across(
              dplyr::all_of(target_fields),
              ~fun(., w = area_segment_)
            )
          ) |>
          dplyr::ungroup()
        return(poly_intersected)
      }

    class_poly_in <- dep_check(poly_in)
    class_poly_weight <- dep_check(poly_weight)

    if (class_poly_in != class_poly_weight) {
      poly_weight <- dep_switch(poly_weight)
    }

    switch(class_poly_in,
      sf =
        suppressWarnings(
          sf::st_interpolate_aw(
            poly_weight[, target_fields],
            poly_in, extensive = FALSE
          )
        ),
      terra =
        summarize_aw_terra(
          poly_in, poly_weight,
          target_fields = target_fields,
          id_poly_in = id_poly_in
        )
    )

  }

#' @title Area weighted summary using two polygon objects
#' @rdname summarize_aw
#' @name summarize_aw
#' @family Macros for calculation
#' @param x A sf object or file path of polygons detectable
#'   with GDAL driver at weighted means will be calculated.
#' @param y A sf object or file path of polygons from
#'  which weighted means will be calculated.
#' @param target_fields character. Field names to calculate area-weighted.
#' @param id_x character(1).
#'  The unique identifier of each polygon in `x`.
#'  Default is `"ID"`.
#' @param fun function(1). The function to calculate the weighted summary.
#' Default is [`stats::weighted.mean`]. The function must have a `w` argument.
#' @param extent numeric(4) or SpatExtent object. Extent of clipping `x`.
#' It only works with `x` of character(1) file path.
#' See [`terra::ext`] for more details. Coordinate systems should match.
#' @returns A data.frame with all numeric fields of area-weighted means.
#' @description When `x` and `y` are different classes,
#'  `poly_weight` will be converted to the class of `x`.
#' @note If `x` and `y` are characters, they will be
#'   read as `sf` objects.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' # package
#' library(sf)
#' sf_use_s2(FALSE)
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' nc <- sf::st_transform(nc, 5070)
#' pp <- sf::st_sample(nc, size = 300)
#' pp <- sf::st_as_sf(pp)
#' pp[["id"]] <- seq(1, nrow(pp))
#' sf::st_crs(pp) <- "EPSG:5070"
#' ppb <- sf::st_buffer(pp, nQuadSegs=180, dist = units::set_units(20, "km"))
#'
#' system.time(ppb_nc_aw <- summarize_aw(ppb, nc, c("BIR74", "BIR79"), "id"))
#' summary(ppb_nc_aw)
#'
#' # terra examples
#' library(terra)
#' library(sf)
#' options(sf_use_s2 = FALSE)
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' elev <- system.file("ex/elev.tif", package = "terra")
#' nc <- terra::vect(ncpath)
#' elev <- terra::rast(elev)
#' pp <- terra::spatSample(nc, size = 300)
#' pp <- terra::project(pp, crs(elev))
#' pp <- terra::as.points(pp)
#' pp[["id"]] <- seq(1, nrow(pp))
#' ppb <- terra::buffer(pp, 20000)
#'
#' system.time(
#'   ppb_nc_aw <- summarize_aw(ppb, nc, c("BIR74", "BIR79"), "id")
#' )
#' summary(ppb_nc_aw)
#'
NULL

#' @rdname summarize_aw
#' @export
setGeneric("summarize_aw", function(x, y, ...) {
  standardGeneric("summarize_aw")
})

#' @rdname summarize_aw
#' @importFrom rlang sym
#' @importFrom dplyr where group_by summarize across ungroup
#' @importFrom terra intersect expanse area
#' @importFrom stats weighted.mean
#' @export
setMethod("summarize_aw", signature(x = "SpatVector", y = "SpatVector"), function(
  x = NULL,
  y = NULL,
  target_fields = NULL,
  id_x = "ID",
  fun = stats::weighted.mean,
  extent = NULL
) {

  x <- check_subject(x, extent = extent, subject_id = id_x)
  y <- check_subject(y, extent = extent)

  poly_intersected <- terra::intersect(x, y)
  poly_intersected[["area_segment_"]] <-
    terra::expanse(poly_intersected)
  poly_intersected <- data.frame(poly_intersected) |>
    dplyr::group_by(!!rlang::sym(id_x)) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(target_fields),
        ~fun(., w = area_segment_)
      )
    ) |>
    dplyr::ungroup()
  return(poly_intersected)
})


#' @rdname summarize_aw
#' @importFrom sf st_interpolate_aw
#' @export
setMethod("summarize_aw", signature(x = "sf", y = "sf"), function(
  x = NULL,
  y = NULL,
  target_fields = NULL,
  id_x = "ID",
  fun = NULL,
  extent = NULL
) {
  x <- check_subject(x, extent = extent, subject_id = id_x)
  y <- check_subject(y, extent = extent)

  poly_intersected <- sf::st_interpolate_aw(x, y)
  return(poly_intersected)
})
