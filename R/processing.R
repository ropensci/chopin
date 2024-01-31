# Generated from chopin_rmarkdown_litr.rmd: do not edit by hand

#' Kernel functions
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
#' bcsd_rpnt <- sf::st_sample(bcsd, 4L)
#' bcsd_rpntm <- sf::st_sample(bcsd, 1000L)
#' clip_vec_ext(bcsd_rpntm, 1000, bcsd_rpnt)
#' @importFrom sf st_intersection
#' @importFrom terra intersect
#' @export
clip_vec_ext <- function(
  pnts,
  radius,
  target_input
) {
  if (any(sapply(list(pnts, radius, target_input), is.null))) {
    stop("One or more required arguments are NULL. Please check.\n")
  }
  detected_pnts <- dep_check(pnts)
  detected_target <- dep_check(target_input)

  if (detected_pnts != detected_target) {
    warning("Inputs are not the same class.\n")
    target_input <- dep_switch(target_input)
  }

  ext_input <- get_clip_ext(pnts, radius)
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

#' @title Clip input raster with a buffered vector extent.
#' @description Clip input raster by the expected maximum extent of
#' computation.
#' @param pnts `sf` or `SpatVector` object
#' @param radius numeric(1). buffer radius.
#' This value will be automatically multiplied by 1.25
#' @param ras `SpatRaster` object to be clipped
#' @param nqsegs `integer(1)`. the number of points per a quarter circle
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
  if (any(sapply(list(pnts, radius, ras), is.null))) {
    stop("Any of required arguments are NULL. Please check.\n")
  }
  ext_input <- get_clip_ext(pnts, radius) |>
    terra::vect()

  cae <- terra::crop(ras, ext_input, snap = "out")
  return(cae)
}

#' @title Extract summarized values from raster with points and a buffer radius
#' @description For simplicity, it is assumed that the coordinate systems of
#'  the points and the raster are the same.
#' @param points `sf`/`SpatVector` object.
#' Coordinates where buffers will be generated.
#' @param surf `SpatRaster` object.
#'  A raster at which summary will be calculated
#' @param radius numeric(1). Buffer radius. here we assume circular buffers only
#' @param id character(1). Unique identifier of each point.
#' @param qsegs integer(1). Number of vertices at a quarter of a circle.
#'  Default is `90L`.
#' @param func a function taking a numeric vector argument.
#' @param kernel character(1). Name of a kernel function
#' One of `"uniform"`, `"triweight"`, `"quartic"`, and `"epanechnikov"`
#' @param bandwidth numeric(1). Kernel bandwidth.
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
  bandwidth = NULL
) {
  # type check
  if (!methods::is(points, "SpatVector")) {
    if (!methods::is(points, "sf")) {
      stop("Check class of the input points.\n")
    }
    points <- terra::vect(points)
  }
  if (!is.numeric(radius)) {
    stop("Check class of the input radius.\n")
  }
  if (!is.character(id)) {
    stop("id should be a character.\n")
  }
  if (!is.numeric(qsegs)) {
    stop("qsegs should be numeric.\n")
  }

  if (!is.null(kernel)) {
    extracted <-
      extract_at_buffer_kernel(points = points,
        surf = surf,
        radius = radius,
        id = id,
        func = func,
        qsegs = qsegs,
        kernel = kernel,
        bandwidth = bandwidth
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
      qsegs = qsegs
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
  bandwidth = NULL
) {
  # generate buffers
  bufs <- terra::buffer(points, width = radius, quadsegs = qsegs)
  bufs <- reproject_b2r(bufs, surf)
  # crop raster
  bufs_extent <- terra::ext(bufs)
  surf_cropped <- terra::crop(surf, bufs_extent, snap = "out")

  # extract raster values
  surf_at_bufs <-
    exactextractr::exact_extract(
      x = surf_cropped,
      y = sf::st_as_sf(bufs),
      fun = func,
      force_df = TRUE,
      append_cols = id,
      progress = FALSE,
      max_cells_in_memory = 5e07
    )
  surf_at_bufs_summary <-
    surf_at_bufs

  return(surf_at_bufs_summary)
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
  bandwidth = NULL
) {
  # generate buffers
  bufs <- terra::buffer(points, width = radius, quadsegs = qsegs)
  bufs <- reproject_b2r(bufs, surf)

  # crop raster
  bufs_extent <- terra::ext(bufs)
  surf_cropped <- terra::crop(surf, bufs_extent, snap = "out")
  name_surf_val <-
    ifelse(terra::nlyr(surf_cropped) == 1,
           "value", names(surf_cropped))

  coords_df <- as.data.frame(points, geom = "XY")
  coords_df <-
    coords_df[, grep(sprintf("^(%s|%s|%s)", id, "x", "y"), names(coords_df))]
  names(coords_df)[grep("(x|y)", names(coords_df))] <- c("xorig", "yorig")
  xorig <- yorig <- NULL
  x <- y <- NULL
  pairdist <- NULL
  w_kernel <- NULL
  # extract raster values
  surf_at_bufs <-
    exactextractr::exact_extract(
      x = surf_cropped,
      y = sf::st_as_sf(bufs),
      force_df = TRUE,
      include_cols = id,
      progress = FALSE,
      include_area = TRUE,
      include_xy = TRUE,
      max_cells_in_memory = 5e07
    )
  surf_at_bufs <- do.call(rbind, surf_at_bufs)
  surf_at_bufs_summary <-
    surf_at_bufs |>
    dplyr::left_join(coords_df, by = id) |>
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
  colnames(surf_at_bufs_summary)[1] <- id
  return(surf_at_bufs_summary)
}


#' @title Extract summarized values from raster with generic polygons
#' @description For simplicity, it is assumed that the coordinate systems of
#'  the points and the raster are the same.
#'  Kernel function is not yet implemented.
#' @param polys `sf`/`SpatVector` object. Polygons.
#' @param surf `SpatRaster` object.
#'  A raster from which a summary will be calculated
#' @param id character(1). Unique identifier of each point.
#' @param func a generic function name in string or
#'  a function taking two arguments that are
#'  compatible with \code{\link[exactextractr]{exact_extract}}.
#'  For example, `"mean"` or `\(x, w) weighted.mean(x, w, na.rm = TRUE)`
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
  func = "mean"
) {
  # type check
  if (!methods::is(polys, "SpatVector")) {
    if (!methods::is(polys, "sf")) {
      stop("Check class of the input points.\n")
    }
    polys <- terra::vect(polys)
  }
  if (!methods::is(surf, "SpatRaster")) {
    stop("Check class of the input raster.\n")
  }
  if (!is.character(id)) {
    stop("id should be a character.\n")
  }

  cls_polys <- dep_check(polys)
  cls_surf <- dep_check(surf)

  if (cls_polys != cls_surf) {
    polys <- dep_switch(polys)
  }

  polys <- reproject_b2r(polys, surf)

  extracted_poly <-
    exactextractr::exact_extract(
      x = surf,
      y = sf::st_as_sf(polys),
      fun = func,
      force_df = TRUE,
      append_cols = id,
      progress = FALSE,
      max_cells_in_memory = 5e07
    )
  return(extracted_poly)
}


#' Extract raster values with point buffers or polygons
#' @param vector `sf`/`SpatVector` object.
#' @param raster `SpatRaster` object.
#' @param id character(1). Unique identifier of each point.
#' @param func function taking one numeric vector argument.
#' @param mode one of `"polygon"`
#'  (generic polygons to extract raster values with) or
#'  `"buffer"` (point with buffer radius)
#' @param ... various. Passed to extract_at_buffer.
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
  stopifnot(id %in% names(vector))

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
#' @param vector `sf`/`stars`/`SpatVector`/`SpatRaster` object
#' @param raster `SpatRaster` object
#' @returns Reprojected object in the same class as \code{vector}
#' @author Insang Song
#' @examples
#' # NOT TO RUN
#' @importFrom sf st_transform
#' @importFrom terra project
#' @importFrom terra crs
#' @export
reproject_b2r <-
  function(vector,
           raster) {
    detected_vec <- dep_check(vector)
    switch(detected_vec,
           sf = sf::st_transform(vector, terra::crs(raster)),
           terra = terra::project(vector, terra::crs(raster)))
  }


#' Calculate Sum of Exponentially Decaying Contributions (SEDC) covariates
#' @param point_from `SpatVector` object. Locations where
#'  the sum of SEDCs are calculated.
#' @param point_to `SpatVector` object. Locations where each SEDC is calculated.
#' @param id character(1). Name of the unique id field in `point_to`.
#' @param sedc_bandwidth numeric(1).
#' Distance at which the source concentration is reduced to
#'  `exp(-3)` (approximately -95 %)
#' @param threshold numeric(1). For computational efficiency,
#'  the nearest points in threshold will be selected.
#'  Default is \code{2 * sedc_bandwidth}.
#' @param target_fields character(varying). Field names in characters.
#' @returns data.frame (tibble) object with input field names with
#'  a suffix \code{"_sedc"} where the sums of EDC are stored.
#'  Additional attributes are attached for the EDC information.
#'    - attr(result, "sedc_bandwidth"): the bandwidth where
#'  concentration reduces to approximately five percent
#'    - attr(result, "sedc_threshold"): the threshold distance
#'  at which emission source points are excluded beyond that
#' @note Distance calculation is done with terra functions internally.
#'  Thus, the function internally converts sf objects in
#'  \code{point_*} arguments to terra.
#'  The threshold should be carefully chosen by users.
#' @author Insang Song
#' @references
#' * [Messier KP, Akita Y, & Serre ML. (2012). Integrating Address Geocoding, Land Use Regression, and Spatiotemporal Geostatistical Estimation for Groundwater Tetrachloroethylene. _Environmental Science \& Technology_ 46(5), 2772-2780.](https://dx.doi.org/10.1021/es203152a)
#' * Wiesner C. (n.d.). [Euclidean Sum of Exponentially Decaying Contributions Tutorial](https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html)
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
    target_fields = NULL
  ) {
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
      warning(
        sprintf(
          "There are %d fields with the same name.
The result may not be accurate.\n",
          length(cn_overlap)
        )
      )
    }
    point_from$from_id <- len_point_from
    # select egrid_v only if closer than 3e5 meters from each aqs
    point_from_buf <-
      terra::buffer(
        point_from,
        width = threshold,
        quadsegs = 90
      )
    point_to <- point_to[point_from_buf, ]
    point_to$to_id <- len_point_to

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
          list(sedc = ~sum(w_sedc * ., na.rm = TRUE))
        )
      ) |>
      dplyr::ungroup()

    attr(near_from_to, "sedc_bandwidth") <- sedc_bandwidth
    attr(near_from_to, "sedc_threshold") <- threshold

    return(near_from_to)
  }


#' Area weighted summary using two polygon sf or SpatVector objects
#' @param poly_in A sf/SpatVector object at weighted means will be calculated.
#' @param poly_weight A sf/SpatVector object from
#'  which weighted means will be calculated.
#' @param id_poly_in character(1).
#'  The unique identifier of each polygon in `poly_in`
#' @return A data.frame with all numeric fields of area-weighted means.
#' @description When `poly_in` and `poly_weight` are different classes,
#'  `poly_weight` will be converted to the class of `poly_in`.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' # package
#' library(sf)
#' sf_use_s2(FALSE)
#' # run
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' nc <- sf::st_transform(nc, 5070)
#' pp <- sf::st_sample(nc, size = 300)
#' pp <- sf::st_as_sf(pp)
#' pp[["id"]] <- seq(1, nrow(pp))
#' sf::st_crs(pp) <- "EPSG:5070"
#' ppb <- sf::st_buffer(pp, nQuadSegs=180, dist = units::set_units(20, 'km'))
#'
#' system.time({ppb_nc_aw <- summarize_aw(ppb, nc, 'id')})
#' summary(ppb_nc_aw)
#' #### Example of summarize_aw ends ####
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
#' @export
summarize_aw <-
  function(
    poly_in = NULL,
    poly_weight = NULL,
    id_poly_in = "ID"
  ) {
    if (!any(
      methods::is(poly_in, "sf"),
      methods::is(poly_weight, "sf"),
      methods::is(poly_in, "SpatVector"),
      methods::is(poly_weight, "SpatVector")
    )) {
      stop("Inputs have invalid classes.\n")
    }
    ## distinguish numeric and nonnumeric columns
    index_numeric <-
      grep("(integer|numeric)", unlist(sapply(poly_weight, class)))

    summarize_aw_terra <-
      function(
        poly_in = NULL,
        poly_weight = NULL,
        id_poly_in = id_poly_in
      ) {
        poly_intersected <- terra::intersect(poly_in, poly_weight)
        poly_intersected[["area_segment_"]] <-
          terra::expanse(poly_intersected)
        poly_intersected <- data.frame(poly_intersected) |>
          dplyr::group_by(!!rlang::sym(id_poly_in)) |>
          dplyr::summarize(
            dplyr::across(
              dplyr::where(is.numeric),
              ~stats::weighted.mean(., w = area_segment_)
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
            poly_weight[, index_numeric],
            poly_in, extensive = FALSE
          )
        ),
      terra =
        summarize_aw_terra(
          poly_in, poly_weight[, index_numeric],
          id_poly_in = id_poly_in
        )
    )

  }


