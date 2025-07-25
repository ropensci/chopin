#' Kernel functions
#' @family Macros for calculation
#' @keywords internal
#' @param kernel Kernel type. One of
#' `"uniform"`, `"quartic"`, `"triweight"`, and `"epanechnikov"`
#' @param d Distance
#' @param bw Bandwidth of a kernel
#' @returns numeric. Kernel weights.
#' @references https://github.com/JanCaha/SpatialKDE
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



# Subfunction: extract at buffers with kernel weight
#' Post-extraction kernel weighting
#' @keywords internal
#' @noRd
.kernel_weighting <- function(
  x_ras,
  y_vec,
  id,
  extracted,
  kernel_func = stats::weighted.mean,
  kernel = NULL,
  bandwidth = NULL,
  max_cells = 2e7,
  ...
) {
  if (dep_check(y_vec) == "sf") {
    y_vec <- dep_switch(y_vec)
  }
  if (!grepl("point", terra::geomtype(y_vec))) {
    cli::cli_warn(
      paste0(
        "Point geometries are acceptable for kernel weighting.\n",
        "Convert to points... (Note: inside = TRUE is applied)"
      )
    )
    y_vec <- terra::centroids(y_vec, inside = TRUE)
  }

  name_surf_val <-
    ifelse(terra::nlyr(x_ras) == 1,
           "value", names(x_ras))

  # convert to data.frame
  coords_df <- as.data.frame(y_vec, geom = "XY")
  # apply strict order
  coords_df <-
    coords_df[, grep(sprintf("^(%s|%s|%s)", id, "x", "y"), names(coords_df))]
  names(coords_df)[grep("(x|y)", names(coords_df))] <- c("xorig", "yorig")

  # for linter purpose
  xorig <- NULL
  yorig <- NULL
  xdest <- NULL
  ydest <- NULL
  pairdist <- NULL
  w_kernel <- NULL
  coverage_fraction <- NULL

  # post-processing
  if (!is.data.frame(extracted)) {
    extracted <- do.call(rbind, extracted)
  }
  names(extracted)[grep("(x|y)", names(extracted))] <- c("xdest", "ydest")
  extracted_summary <-
    extracted |>
    dplyr::left_join(coords_df, by = id) |>
    # averaging with kernel weights
    dplyr::mutate(
      pairdist = terra::distance(
        x = cbind(xorig, yorig),
        y = cbind(xdest, ydest),
        pairwise = TRUE,
        lonlat = terra::is.lonlat(y_vec)
      ),
      w_kernel = kernelfunction(pairdist, bandwidth, kernel),
      w_kernelarea = w_kernel * coverage_fraction
    ) |>
    dplyr::group_by(!!rlang::sym(id)) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(name_surf_val),
        ~kernel_func(., w = w_kernelarea)
      )
    ) |>
    dplyr::ungroup()
  # restore the original identifier
  colnames(extracted_summary)[1] <- id
  return(extracted_summary)
}
# TODO: look into the behavior of kernel_func when w is not an explicit
#   argument.

#' @title Extract summarized values from raster with generic polygons
#' @param x SpatRaster
#' @param y sf/SpatVector/character(1) file path.
#' @param id character(1). Name of unique identifier field.
#' @param func character(1)/function. supported function names or functions
#'   taking `x` and `w` in `exactextractr::exact_extract`
#' @param extent numeric. Passed to .check_vector
#' @param radius numeric(1). Buffer radius.
#' @param out_class character(1). "sf" or "terra"
#' @param kernel character(1). Name of kernel functions [kernelfunction]
#' @param kernel_func function. Kernel weight summary function.
#' @param bandwidth numeric(1). Kernel bandwidth. Should match the linear unit
#'   of the coordinate reference system of x.
#' @param max_cells integer(1). Total number of cells fetched in
#'   `exactextractr::exact_extract`
#' @param .standalone logical(1). Whether or not running standalone mode.
#'   `TRUE` will apply internal input check functions, whereas
#'   `FALSE` will let `par_*` functions will check inputs.
#' @keywords internal
#' @noRd
.extract_at <- function(
  x = NULL,
  y = NULL,
  id = NULL,
  func = "mean",
  extent = NULL,
  radius = NULL,
  out_class = "sf",
  kernel = NULL,
  kernel_func = stats::weighted.mean,
  bandwidth = NULL,
  max_cells = NULL,
  .standalone = TRUE,
  ...
) {
  x <-
    .check_raster(
      x,
      extent = extent
    )

  if (.standalone) {
    y <-
      .check_vector(
        y,
        extent = extent,
        out_class = out_class,
        subject_id = id
      )
    # reproject polygons to raster's crs
    y <- reproject_to_raster(vector = y, raster = x)
    if (dep_check(y) == "terra") {
      y <- dep_switch(y)
    }
  }

  if (!is.null(radius)) {
    ygeom <- tolower(as.character(sf::st_geometry_type(y)))
    if (!all(grepl("point", ygeom))) {
      cli::cli_warn(
        "Buffer is set with non-point geometries."
      )
    }
    y <- sf::st_buffer(y, radius, nQuadSegs = 90L)
  }
  iskernel <- !is.null(kernel)

  extracted <-
    exactextractr::exact_extract(
      x = x,
      y = y,
      fun = if (iskernel) NULL else func,
      force_df = TRUE,
      stack_apply = !iskernel,
      append_cols = if (iskernel) NULL else id,
      include_cols = if (iskernel) id else NULL,
      progress = FALSE,
      include_area = iskernel,
      include_xy = iskernel,
      max_cells_in_memory = max_cells
    )

  if (iskernel) {
    stopifnot(!is.null(bandwidth))
    cli::cli_inform(
      sprintf("Kernel function [ %s ] is applied to calculate weights...",
              kernel)
    )
    extracted <-
      .kernel_weighting(
        x_ras = x,
        y_vec = y,
        id = id,
        extracted = extracted,
        kernel = kernel,
        kernel_func = kernel_func,
        bandwidth = bandwidth
      )
  }
  return(extracted)
}


## extract_at ####
#' Extract raster values with point buffers or polygons
#'
#' @family Macros for calculation
#' @details Inputs are preprocessed in different ways depending on the class.
#'   * Vector inputs in `y`: `sf` is preferred, thus character and `SpatVector`
#'     inputs will be converted to `sf` object. If `radius` is not NULL,
#'     `sf::st_buffer` is used to generate circular buffers as subsequent
#'     raster-vector overlay is done with `exactextractr::exact_extract`.
#'   * Raster input in `x`: `SpatRaster` is preferred. If the input is not
#'    `SpatRaster`, it will be converted to `SpatRaster` object.
#' @param x `SpatRaster` object or file path(s) with extensions
#' that are GDAL-compatible. When multiple file paths are used, the rasters
#' must have the same extent and resolution.
#' @param y `sf`/`SpatVector` object or file path.
#' @param id character(1). Unique identifier of each point.
#' @param func function taking one numeric vector argument.
#'   Default is `"mean"` for all supported signatures in arguments
#'  `x` and `y`.
#' @param extent numeric(4) or SpatExtent. Extent of clipping vector.
#'  It only works with `points` of character(1) file path.
#' @param radius numeric(1). Buffer radius.
#' @param out_class character(1). Output class. One of `sf` or `terra`.
#' @param kernel character(1). Name of a kernel function
#' One of `"uniform"`, `"triweight"`, `"quartic"`, and `"epanechnikov"`
#' @param kernel_func function.
#'   Kernel function to apply to the extracted values.
#'   Default is [`stats::weighted.mean()`]
#' @param bandwidth numeric(1). Kernel bandwidth.
#' @param max_cells integer(1). Maximum number of cells in memory.
#' @param .standalone logical(1). Default is `TRUE`, which means that
#'   the function will be executed in a standalone mode.
#'   When using this function in `par_*` functions,
#'   set this to `FALSE`.
#' @param ... Placeholder.
#' @returns A data.frame object with summarized raster values with
#'  respect to the mode (polygon or buffer) and the function.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' rastpath <- file.path(tempdir(), "test.tif")
#'
#' nc <- terra::vect(ncpath)
#' nc <- terra::project(nc, "EPSG:5070")
#' rrast <- terra::rast(nc, nrow = 300, ncol = 660)
#' terra::values(rrast) <- rgamma(1.98e5, 4, 2)
#' rpnt <- terra::spatSample(rrast, 16L, as.points = TRUE)
#' rpnt$pid <- sprintf("ID-%02d", seq(1, 16))
#'
#' extract_at(rrast, rpnt, "pid", "mean", radius = 1000)
#' extract_at(rrast, nc, "NAME", "mean")
#' extract_at(rrast, ncpath, "NAME", "mean")
#' # Using SpatRaster object
#' suppressWarnings(
#'   extract_at(
#'     rrast, ncpath, "NAME", "mean",
#'     kernel = "epanechnikov",
#'     bandwidth = 1e5
#'   )
#' )
#' # Using raster path
#' terra::writeRaster(rrast, rastpath, overwrite = TRUE)
#' suppressWarnings(
#'   extract_at(
#'     rastpath, ncpath, "NAME", "mean",
#'     kernel = "epanechnikov",
#'     bandwidth = 1e5
#'   )
#' )
#' @export
setGeneric("extract_at", function(x, y, ...) standardGeneric("extract_at"))



#' @rdname extract_at
#' @export
setMethod(
  "extract_at",
  signature(
    x = "SpatRaster",
    y = "sf"
  ),
  function(
    x = NULL,
    y = NULL,
    id = NULL,
    func = "mean",
    extent = NULL,
    radius = NULL,
    out_class = "sf",
    kernel = NULL,
    kernel_func = stats::weighted.mean,
    bandwidth = NULL,
    max_cells = 3e+07,
    .standalone = TRUE,
    ...
  ) {
    .extract_at(
      x = x, y = y, id = id, func = func,
      extent = extent,
      radius = radius,
      out_class = out_class,
      kernel = kernel,
      kernel_func = kernel_func,
      bandwidth = bandwidth,
      max_cells = max_cells,
      .standalone = .standalone
    )
  }
)


#' @rdname extract_at
#' @export
setMethod(
  "extract_at",
  signature(
    x = "character",
    y = "character"
  ),
  function(
    x = NULL,
    y = NULL,
    id = NULL,
    func = "mean",
    extent = NULL,
    radius = NULL,
    out_class = "sf",
    kernel = NULL,
    kernel_func = stats::weighted.mean,
    bandwidth = NULL,
    max_cells = 3e+07,
    .standalone = TRUE,
    ...
  ) {
    .extract_at(
      x = x, y = y, id = id, func = func,
      extent = extent,
      radius = radius,
      out_class = out_class,
      kernel = kernel,
      kernel_func = kernel_func,
      bandwidth = bandwidth,
      max_cells = max_cells,
      .standalone = .standalone
    )
  }
)


#' @rdname extract_at
#' @export
setMethod(
  "extract_at",
  signature(
    x = "SpatRaster",
    y = "character"
  ),
  function(
    x = NULL,
    y = NULL,
    id = NULL,
    func = "mean",
    extent = NULL,
    radius = NULL,
    out_class = "sf",
    kernel = NULL,
    kernel_func = stats::weighted.mean,
    bandwidth = NULL,
    max_cells = 3e+07,
    .standalone = TRUE,
    ...
  ) {
    .extract_at(
      x = x, y = y, id = id, func = func,
      extent = extent,
      radius = radius,
      out_class = out_class,
      kernel = kernel,
      kernel_func = kernel_func,
      bandwidth = bandwidth,
      max_cells = max_cells,
      .standalone = .standalone
    )
  }
)

#' @rdname extract_at
#' @export
setMethod(
  "extract_at",
  signature(
    x = "SpatRaster",
    y = "SpatVector"
  ),
  function(
    x = NULL,
    y = NULL,
    id = NULL,
    func = "mean",
    extent = NULL,
    radius = NULL,
    out_class = "sf",
    kernel = NULL,
    kernel_func = stats::weighted.mean,
    bandwidth = NULL,
    max_cells = 3e+07,
    .standalone = TRUE,
    ...
  ) {
    .extract_at(
      x = x, y = y, id = id, func = func,
      extent = extent,
      radius = radius,
      out_class = out_class,
      kernel = kernel,
      kernel_func = kernel_func,
      bandwidth = bandwidth,
      max_cells = max_cells,
      .standalone = .standalone
    )
  }
)


#' @rdname extract_at
#' @export
setMethod(
  "extract_at",
  signature(
    x = "character",
    y = "sf"
  ),
  function(
    x = NULL,
    y = NULL,
    id = NULL,
    func = "mean",
    extent = NULL,
    radius = NULL,
    out_class = "sf",
    kernel = NULL,
    kernel_func = stats::weighted.mean,
    bandwidth = NULL,
    max_cells = 3e+07,
    .standalone = TRUE,
    ...
  ) {
    .extract_at(
      x = x, y = y, id = id, func = func,
      extent = extent,
      radius = radius,
      out_class = out_class,
      kernel = kernel,
      kernel_func = kernel_func,
      bandwidth = bandwidth,
      max_cells = max_cells,
      .standalone = .standalone
    )
  }
)



#' @rdname extract_at
#' @export
setMethod(
  "extract_at",
  signature(
    x = "character",
    y = "SpatVector"
  ),
  function(
    x = NULL,
    y = NULL,
    id = NULL,
    func = "mean",
    extent = NULL,
    radius = NULL,
    out_class = "sf",
    kernel = NULL,
    kernel_func = stats::weighted.mean,
    bandwidth = NULL,
    max_cells = 3e+07,
    .standalone = TRUE,
    ...
  ) {
    .extract_at(
      x = x, y = y, id = id, func = func,
      extent = extent,
      radius = radius,
      out_class = out_class,
      kernel = kernel,
      kernel_func = kernel_func,
      bandwidth = bandwidth,
      max_cells = max_cells,
      .standalone = .standalone
    )
  }
)


# nolint start
#' Calculate Sum of Exponentially Decaying Contributions (SEDC) covariates
#' @family Macros for calculation
#' @details
#' The SEDC is specialized in vector to vector summary of covariates
#' with exponential decay. Decaying slope will be defined by `sedc_bandwidth`,
#' where the concentration of the source is reduced to $\\exp(-3)$
#' (approximately 5 \%). This function is useful when users a proper theory
#' of the attenuating concentration with the distance from the sources.
#' It can be thought of as a fixed bandwidth kernel weighted sum of covariates,
#' which encapsulates three steps:
#'   * Calculate the distance between each source and target points.
#'   * Calculate the weight of each source point with the exponential decay.
#'   * Summarize the weighted covariates.
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
#' @returns data.frame object with input field names with
#'  a suffix `"_sedc"` where the sums of EDC are stored.
#'  Additional attributes are attached for the EDC information.
#'    - `attr(result, "sedc_bandwidth")`: the bandwidth where
#'  concentration reduces to approximately five percent
#'    - `attr(result, "sedc_threshold")`: the threshold distance
#'  at which emission source points are excluded beyond that
#' @note Distance calculation is done with `terra` functions internally.
#'  Thus, the function internally converts `sf` objects in
#'  `point_*` arguments to `terra`. Please note that any `NA` values
#'  in the input will be ignored in SEDC calculation.
#' @author Insang Song
#' @references
#' * Messier KP, Akita Y, Serre ML. (2012). Integrating Address Geocoding, Land Use Regression, and Spatiotemporal Geostatistical Estimation for Groundwater Tetrachloroethylene. _Environmental Science & Technology_ 46(5), 2772-2780.(\doi{10.1021/es203152a})
#' * Wiesner C. (n.d.). Euclidean Sum of Exponentially Decaying Contributions Tutorial.
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
#' suppressWarnings(
#'   summarize_sedc(pnt_from, pnt_to, "NAME", 1e5, 2e5, vals)
#' )
#' @importFrom dplyr as_tibble left_join summarize mutate group_by all_of
#' @importFrom dplyr across ungroup
#' @importFrom terra nearby distance buffer
#' @importFrom rlang sym
#' @export
# nolint end
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
      .check_vector(
        point_from,
        extent = extent_from,
        subject_id = id,
        out_class = "terra"
      )
    point_to <-
      .check_vector(
        point_to,
        extent = extent_to,
        subject_id = NULL,
        out_class = "terra"
      )

    # define sources, set SEDC exponential decay range
    len_point_from <- seq_len(nrow(point_from))
    len_point_to <- seq_len(nrow(point_to))

    cn_overlap <- intersect(names(point_from), names(point_to))
    if (length(cn_overlap) > 0) {
      cli::cli_warn(
        sprintf(
          "There are %d fields with the same name.\n%s",
          length(cn_overlap),
          "The result may be inaccurate.\n"
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


## summarize_aw ####
#' Area weighted summary using two polygon objects
#' @rdname summarize_aw
#' @name summarize_aw
#' @family Macros for calculation
#' @param x A sf/SpatVector object or file path of polygons detectable
#'   with GDAL driver at weighted means will be calculated.
#' @param y A sf/SpatVector object or file path of polygons from
#'  which weighted means will be calculated.
#' @param target_fields character. Field names to calculate area-weighted.
#' @param id_x character(1).
#'  The unique identifier of each polygon in `x`.
#'  Default is `"ID"`.
#' @param fun function(1)/character(1).
#'   The function to calculate the weighted summary.
#'   Default is [`stats::weighted.mean`]. The function must have a `w`
#'   argument. If both `x` and `y` are `sf`, it should be one of
#'   `c("sum", "mean")`. It will determine `extensive` argument in
#'   [`sf::st_interpolate_aw`].
#' @param extent numeric(4) or SpatExtent object. Extent of clipping `x`.
#' It only works with `x` of character(1) file path.
#' See [`terra::ext`] for more details. Coordinate systems should match.
#' @param out_class character(1). "sf" or "terra". Output class.
#' @param ... Additional arguments depending on class of `x` and `y`.
#' @returns A data.frame with all numeric fields of area-weighted means.
#' @description When `x` and `y` are different classes,
#'  `poly_weight` will be converted to the class of `x`.
#' @note `x` and `y` classes should match.
#'   If `x` and `y` are characters, they will be
#'   read as `sf` objects.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' lastpar <- par(mfrow = c(1, 1))
#' # package
#' library(sf)
#' options(sf_use_s2 = FALSE)
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' nc <- sf::st_transform(nc, "EPSG:5070")
#' pp <- sf::st_sample(nc, size = 300)
#' pp <- sf::st_as_sf(pp)
#' pp[["id"]] <- seq(1, nrow(pp))
#' sf::st_crs(pp) <- "EPSG:5070"
#' ppb <- sf::st_buffer(pp, nQuadSegs=180, dist = units::set_units(20, "km"))
#'
#' suppressWarnings(
#'   ppb_nc_aw <-
#'     summarize_aw(
#'       ppb, nc, c("BIR74", "BIR79"),
#'       "id", fun = "sum"
#'     )
#' )
#' summary(ppb_nc_aw)
#'
#' # terra examples
#' library(terra)
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(ncpath)
#' pp <- terra::spatSample(nc, size = 300)
#' pp[["id"]] <- seq(1, nrow(pp))
#' ppb <- terra::buffer(pp, 20000)
#'
#' suppressWarnings(
#'   ppb_nc_aw <-
#'     summarize_aw(
#'       ppb, nc, c("BIR74", "BIR79"), "id",
#'       fun = sum
#'     )
#' )
#' summary(ppb_nc_aw)
#' par(lastpar)
#' @export
setGeneric("summarize_aw", function(x, y, ...) standardGeneric("summarize_aw"))

#' @rdname summarize_aw
#' @importFrom rlang sym
#' @importFrom dplyr where group_by summarize across ungroup
#' @importFrom terra intersect expanse area
#' @importFrom stats weighted.mean
#' @export
setMethod("summarize_aw", signature(x = "SpatVector", y = "SpatVector"),
  function(
    x,
    y,
    target_fields = NULL,
    id_x = "ID",
    fun = stats::weighted.mean,
    extent = NULL,
    ...
  ) {

    x <-
      .check_vector(x, extent = extent, input_id = id_x, out_class = "terra")
    y <-
      .check_vector(y, extent = extent, input_id = NULL, out_class = "terra")

    x <- x[, id_x]
    if (!is.null(target_fields)) {
      y <- y[, target_fields]
    }

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
  }
)

#' @rdname summarize_aw
#' @importFrom rlang sym
#' @importFrom dplyr where group_by summarize across ungroup
#' @importFrom terra intersect expanse area
#' @importFrom stats weighted.mean
#' @export
setMethod("summarize_aw", signature(x = "character", y = "character"),
  function(
    x,
    y,
    target_fields = NULL,
    id_x = "ID",
    fun = stats::weighted.mean,
    out_class = "terra",
    extent = NULL,
    ...
  ) {

    x <-
      .check_vector(
        x, extent = extent, subject_id = id_x, out_class = out_class
      )
    y <-
      .check_vector(y, extent = extent, out_class = out_class)

    x <- x[, id_x]
    if (!is.null(target_fields)) {
      y <- y[, target_fields]
    }

    area_norm <- terra::expanse(y[1, ])
    poly_intersected <- terra::intersect(x, y)
    poly_intersected[["area_segment_"]] <-
      terra::expanse(poly_intersected) / area_norm
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
  }
)


#' @rdname summarize_aw
#' @importFrom sf st_interpolate_aw
#' @export
setMethod("summarize_aw", signature(x = "sf", y = "sf"),
  function(
    x,
    y,
    target_fields = NULL,
    id_x = "ID",
    fun = NULL,
    extent = NULL,
    ...
  ) {
    fun <- match.arg(fun, c("mean", "sum"))
    extensive <- (fun == "sum")
    x <- .check_vector(x, extent = extent, input_id = id_x, out_class = "sf")
    y <- .check_vector(y, extent = extent, input_id = NULL, out_class = "sf")

    x <- x[, id_x]
    if (!is.null(target_fields)) {
      y <- y[, target_fields]
    }

    poly_intersected <- sf::st_interpolate_aw(y, x, extensive = extensive)
    return(poly_intersected)
  }
)
