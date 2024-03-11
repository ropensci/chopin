# Generated from chopin_rmarkdown_litr.rmd: do not edit by hand

#' Get a set of computational grids
#' @family Parallelization
#' @param input sf or Spat* object.
#' @param mode character(1). Mode of region construction.
#'  One of
#' * `"grid"` (simple grid regardless of the number of features in each grid)
#' * `"density"` (clustering-based varying grids),
#' * `"grid_advanced"` (merging adjacent grids with
#'  smaller number of features than `grid_min_features`).
#'  The argument `grid_min_features` should be specified.
#' * `"grid_quantile"` (x and y quantiles): an argument `quantiles` should
#' be specified.
#' @param nx integer(1). The number of grids along x-axis.
#' @param ny integer(1). The number of grids along y-axis.
#' @param grid_min_features integer(1). A threshold to merging adjacent grids
#' @param padding numeric(1). A extrusion factor to make buffer to
#'  clip actual datasets. Depending on the length unit of the CRS of input.
# nolint start
#' @param unit character(1). The length unit for padding (optional).
#'  units::set_units is used for padding when sf object is used.
#'  See [units package vignette (web)](https://cran.r-project.org/web/packages/units/vignettes/measurement_units_in_R.html)
#'  for the list of acceptable unit forms.
#' @param quantiles numeric. Quantiles for `grid_quantile` mode.
# nolint end
#' @param ... arguments passed to the internal function
#' @returns A list of two,
#'  * \code{original}: exhaustive and non-overlapping
#'  grid polygons in the class of input
#'  * \code{padded}: a square buffer of each polygon in
#'  \code{original}. Used for computation.
#' @description Using input points, the bounding box is split to
#'  the predefined numbers of columns and rows.
#'  Each grid will be buffered by the radius.
#' @seealso [par_cut_coords], [par_merge_grid]
#' @author Insang Song
#' @examples
#' # data
#' library(sf)
#' ncpath <- system.file("shape/nc.shp", package = "sf")
#' nc <- read_sf(ncpath)
#' nc <- st_transform(nc, "EPSG:5070")
#' # run: nx and ny should strictly be integers
#' # In the example below, nx is 12L, not 12.
#' nc_comp_region <-
#'   par_make_gridset(
#'     nc,
#'     mode = "grid",
#'     nx = 12L, ny = 8L,
#'     padding = 10000)
#' par(mfcol = c(1, 2))
#' plot(nc_comp_region$original)
#' plot(nc_comp_region$padded)
#' @importFrom sf st_crs
#' @importFrom sf st_set_crs
#' @importFrom terra crs
#' @importFrom terra set.crs
#' @importFrom terra buffer
#' @export
par_make_gridset <-
  function(
      input,
      mode = c("grid", "grid_advanced", "density", "grid_quantile"),
      nx = 10L,
      ny = 10L,
      grid_min_features = 30L,
      padding = NULL,
      unit = NULL,
      quantiles = NULL,
      ...) {
    mode <- match.arg(mode)

    if (!all(
      is.integer(nx),
      is.integer(ny)
    )
    ) {
      stop("nx, ny must be integer.\n")
    }
    if (!is.numeric(padding)) {
      message("padding should be numeric.
We try converting padding to numeric...\n")
      padding <- as.numeric(padding)
      if (any(inherits(padding, "try-error"), is.na(padding))) {
        stop("padding is not convertible to numeric or converted to NA.\n")
      }
    }

    # valid unit compatible with units::set_units?
    grid_reg <-
      switch(mode,
        grid = par_make_grid(points_in = input, ncutsx = nx, ncutsy = ny),
        grid_advanced =
        par_merge_grid(
          points_in = input,
          par_make_grid(input, nx, ny),
          grid_min_features = grid_min_features
        ),
        grid_quantile = par_cut_coords(
          x = input,
          y = NULL,
          quantiles = quantiles
        ),
        density = simpleError("density method is under development.\n")
      )

    # type_grid_reg <- dep_check(grid_reg)
    if (dep_check(grid_reg) == "sf") {
      grid_reg <- sf::st_set_crs(grid_reg, sf::st_crs(input))
      grid_reg_conv <- dep_switch(grid_reg)
    } else {
      grid_reg <- terra::set.crs(grid_reg, terra::crs(input))
      grid_reg_conv <- grid_reg
    }

    grid_reg_pad <-
      # switch(type_grid_reg,
      #        sf =
      #        sf::st_buffer(grid_reg,
      #                      dist = padding,
      #                      endCapStyle = "SQUARE",
      #                      joinStyle = "MITRE"),
            #  terra =
      terra::buffer(
        grid_reg_conv,
        width = padding,
        capstyle = "square",
        joinstyle = "mitre"
      )
    if (dep_check(grid_reg) != dep_check(grid_reg_pad)) {
      grid_reg_pad <- dep_switch(grid_reg_pad)
    }
    grid_results <-
      list(original = grid_reg,
           padded = grid_reg_pad)
    return(grid_results)

  }

#' @title Generate grid polygons
#' @family Parallelization
#' @description Returns a sf object that includes x- and y- index
#' by using two inputs ncutsx and ncutsy, which are x- and
#' y-directional splits, respectively.
#' @param points_in `sf` or `SpatVector` object. Target points of computation.
#' @param ncutsx integer(1). The number of splits along x-axis.
#' @param ncutsy integer(1). The number of splits along y-axis.
#' @returns A `sf` or `SpatVector` object of computation grids with
#' unique grid id (CGRIDID).
#' @note Grids are generated based on the extent of `points_in` first,
#' then exhaustive grids will be filtered by the intersection between
#' these and `points_in`. Thus, the number of generated grids may be
#' smaller than `ncutsx * ncutsy`.
#' @author Insang Song
#' @examples
#' library(sf)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' nc_path <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(nc_path)
#' nc_rp <- terra::spatSample(nc, 1000)
#' nc_gr <- par_make_grid(nc_rp, 10L, 6L)
#'
#' plot(nc_rp)
#' plot(nc_gr, add = TRUE)
#' @importFrom terra rast
#' @importFrom terra as.polygons
#' @importFrom sf st_as_sf
#' @importFrom sf st_make_grid
#' @export
par_make_grid <-
  function(
    points_in = NULL,
    ncutsx = NULL,
    ncutsy = NULL
  ) {
    package_detected <- dep_check(points_in)

    grid_out <-
      switch(package_detected,
        sf = sf::st_make_grid(points_in, n = c(ncutsx, ncutsy)) |>
          as.data.frame() |>
          sf::st_as_sf(),
        terra = terra::rast(points_in, nrows = ncutsy, ncols = ncutsx) |>
          terra::as.polygons()
      )
    # grid select
    grid_out <- grid_out[points_in, ]

    grid_out$CGRIDID <- seq(1, nrow(x = grid_out))
    return(grid_out)
  }


#' Quantile definition
#' @family Helper functions
#' @param steps integer(1). The number of quantiles.
#' @returns numeric vector of quantiles.
#' @examples
#' par_def_q(5L)
#' @export
par_def_q <- function(steps = 4L) {
  if (steps < 2L) {
    stop("steps should be greater than 1.")
  }
  quantiles <- seq(0, 1, length.out = steps + 1)
  return(quantiles)
}


#' @title Partition coordinates into quantile polygons
#' @note This function is only for two-dimensional points.
#' @family Parallelization
#' @param x numeric/sf/SpatVector. x-coordinates (if numeric).
#' @param y numeric. y-coordinates.
#' @param quantiles numeric vector. Quantiles.
#' @returns A `SpatVector` object with field `CGRIDID`.
#' @examples
#' library(terra)
#' random_points <-
#'   data.frame(x = runif(1000, 0, 100), y = runif(1000, 0, 100))
#' quantiles <- par_def_q(4L)
#' qpoly <- par_cut_coords(random_points$x, random_points$y, quantiles)
#' clustered_points <-
#'   data.frame(x = rgamma(1000, 1, 1), y = rgamma(1000, 4, 1))
#' qpoly_c <- par_cut_coords(clustered_points$x, clustered_points$y, quantiles)
#' par(mfcol = c(1, 2))
#' plot(qpoly)
#' plot(qpoly_c)
#' par(mfcol = c(1, 1))
#' cvect <- terra::vect(clustered_points, geom = c("x", "y"))
#' plot(cvect)
#' plot(qpoly_c, add = TRUE, col = "transparent", border = "red")
#' qcv <- intersect(cvect, qpoly_c)
#' table(qcv$CGRIDID)
#' sum(table(qcv$CGRIDID)) # should be 1000
#' @importFrom methods is
#' @importFrom sf st_coordinates
#' @importFrom terra crds
#' @importFrom terra ext
#' @importFrom terra as.polygons
#' @importFrom stats setNames
#' @importFrom stats quantile
#' @export
par_cut_coords <- function(x = NULL, y = NULL, quantiles) {
  if (any(methods::is(x, "sf"), methods::is(x, "SpatVector"))) {
    coord <- if (methods::is(x, "sf")) sf::st_coordinates else terra::crds
    detectgeom <-
      if (methods::is(x, "sf")) sf::st_geometry_type else terra::geomtype
    center <- if (methods::is(x, "sf")) sf::st_centroid else terra::centroids
    if (any(grepl("polygon", tolower(unique(detectgeom(x)))))) {
      x <- center(x)
    }

    invect <- coord(x)
    x <- invect[, 1]
    y <- invect[, 2]
  }
  if (!all.equal(length(x), length(y))) {
    stop("x and y should have the same length.")
  }
  x_quantiles <- stats::quantile(x, probs = quantiles)
  y_quantiles <- stats::quantile(y, probs = quantiles)

  # these lines are rounding quantiles between
  # the minimum and the maximum (exclusive) to the nearest 4th decimal place
  x_quantiles[-c(1, length(x_quantiles))] <-
    sapply(
      x_quantiles[-c(1, length(x_quantiles))],
      function(x) round(x, 4L - ceiling(log10(abs(x) - as.integer(x))))
    )
  y_quantiles[-c(1, length(y_quantiles))] <-
    sapply(
      y_quantiles[-c(1, length(y_quantiles))],
      function(x) round(x, 4L - ceiling(log10(abs(x) - as.integer(x))))
    )

  xy_quantiles <- expand.grid(
    x = x_quantiles,
    y = y_quantiles
  )

  # leveraging the auto-sorting factor levels and
  # ll-rr combinations for terra::ext, then convert to polygons
  xy_quantiles$xindx <- as.integer(factor(xy_quantiles$x))
  xy_quantiles$yindx <- as.integer(factor(xy_quantiles$y))
  xy_quantiles_next <- xy_quantiles
  xy_quantiles$xurindx <- xy_quantiles$xindx + 1
  xy_quantiles$yurindx <- xy_quantiles$yindx + 1
  xy_quantiles_next <-
    stats::setNames(xy_quantiles_next, c("xur", "yur", "xurindx", "yurindx"))
  xy_quantiles <-
    merge(xy_quantiles, xy_quantiles_next, by = c("xurindx", "yurindx"))
  exts <- mapply(
    function(xur, yur, x, y) {
      terra::as.polygons(terra::ext(c(x, xur, y, yur)))
    },
    xy_quantiles$xur,
    xy_quantiles$yur,
    xy_quantiles$x,
    xy_quantiles$y,
    SIMPLIFY = TRUE
  )
  xy_poly <- Reduce(rbind, exts)
  xy_poly$CGRIDID <- seq(1, nrow(xy_poly))

  return(xy_poly)
}


#' @title Merge adjacent grid polygons with given rules
#' @family Parallelization
#' @description Merge boundary-sharing (in "Rook" contiguity) grids with
#'  fewer target features than the threshold.
#'  This function strongly assumes that the input
#'  is returned from the par_make_grid,
#'  which has `"CGRIDID"` as the unique id field.
#' @author Insang Song
#' @param points_in `sf` or `SpatVector` object. Target points of computation.
#' @param grid_in `sf` or `SpatVector` object.
#' The grid generated by [`par_make_grid`].
#' @param grid_min_features integer(1). Threshold to merge adjacent grids.
#' @returns A `sf` or `SpatVector` object of computation grids.
#' @examples
#' \dontrun{
#' library(sf)
#' library(igraph)
#' ligrary(dplyr)
#' dg <- sf::st_as_sfc(st_bbox(c(xmin = 0, ymin = 0, xmax = 8e5, ymax = 6e5)))
#' sf::st_crs(dg) <- 5070
#' dgs <- sf::st_as_sf(st_make_grid(dg, n = c(20, 15)))
#' dgs$CGRIDID <- seq(1, nrow(dgs))
#'
#' dg_sample <- sf::st_sample(dg, kappa = 5e-9, mu = 15,
#' scale = 15000, type = "Thomas")
#' sf::st_crs(dg_sample) <- sf::st_crs(dg)
#' dg_merged <- par_merge_grid(sf::st_as_sf(dg_sample), dgs, 100)
#' plot(dg_merged$geometry)
#' #### NOT RUN ####
#' }
#' @references
#' Polsby, D. D., & Popper, F. J. (1991). The Third Criterion: Compactness as a Procedural Safeguard Against Partisan Gerrymandering. _Yale Law & Policy Review_, 9(2), 301–353. [Link](http://hdl.handle.net/20.500.13051/17448)
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr n
#' @importFrom sf st_relate
#' @importFrom sf st_length
#' @importFrom sf st_cast
#' @importFrom sf st_intersects
#' @importFrom sf st_as_sf
#' @importFrom sf st_area
#' @importFrom rlang sym
#' @importFrom igraph graph_from_edgelist
#' @importFrom igraph mst
#' @importFrom igraph components
#' @importFrom utils combn
#' @export
par_merge_grid <-
  function(
    points_in = NULL,
    grid_in = NULL,
    grid_min_features = NULL
  ) {
    package_detected <- dep_check(points_in)
    if (package_detected == "terra") {
      points_in <- sf::st_as_sf(points_in)
      grid_in <- sf::st_as_sf(grid_in)
    }

    n_points_in_grid <- lengths(sf::st_intersects(grid_in, points_in))
    grid_self <- sf::st_relate(grid_in, grid_in, pattern = "2********")
    grid_rook <- sf::st_relate(grid_in, grid_in, pattern = "F***1****")
    grid_rooks <- mapply(c, grid_self, grid_rook, SIMPLIFY = FALSE)
    grid_lt_threshold <- (n_points_in_grid < grid_min_features)

    # does the number of points per grid exceed minimum threshold?
    if (sum(grid_lt_threshold) < 2) {
      stop(
        sprintf(
          "Threshold is too low. Please try higher threshold.\n
        min # points in grids: %d, your threshold: %d\n",
          min(n_points_in_grid), grid_min_features
        )
      )
    }
    grid_lt_threshold <- seq(1, nrow(grid_in))[grid_lt_threshold]

    # This part does not work as expected.
    # Should investigate edge list and actual row index of the grid object;
    identified <- lapply(grid_rooks,
                         function(x) sort(x[which(x %in% grid_lt_threshold)]))
    identified <- identified[grid_lt_threshold]
    identified <- unique(identified)
    identified <- identified[sapply(identified, length) > 1]

    # Minimum spanning tree
    identified_graph <-
      lapply(identified, function(x) t(utils::combn(x, 2))) |>
      Reduce(f = rbind, x = _) |>
      unique() |>
      apply(X = _, 2, as.character) |>
      igraph::graph_from_edgelist(el = _, directed = 0) |>
      igraph::mst() |>
      igraph::components()

    identified_graph_member <- identified_graph$membership

    merge_idx <- as.integer(names(identified_graph_member))
    merge_member <- split(merge_idx, identified_graph_member)
    merge_member_label <-
      unlist(lapply(merge_member, function(x) paste(x, collapse = "_")))
    merge_member_label <- merge_member_label[identified_graph_member]

    # sf object manipulation
    grid_out <- grid_in
    grid_out[["CGRIDID"]][merge_idx] <- merge_member_label

    grid_out <- grid_out |>
      dplyr::group_by(!!rlang::sym("CGRIDID")) |>
      dplyr::summarize(n_merged = dplyr::n()) |>
      dplyr::ungroup()

    ## Polsby-Popper test for shape compactness
    par_merge_gridd <- grid_out[which(grid_out$n_merged > 1), ]
    par_merge_gridd_area <- as.numeric(sf::st_area(par_merge_gridd))
    par_merge_gridd_perimeter <-
      as.numeric(sf::st_length(sf::st_cast(par_merge_gridd, "LINESTRING")))
    par_merge_gridd_pptest <-
      (4 * pi * par_merge_gridd_area) / (par_merge_gridd_perimeter ^ 2)

    # pptest value is bounded [0,1];
    # 0.3 threshold is groundless at this moment,
    # possibly will make it defined by users.
    if (max(unique(identified_graph_member)) > floor(0.1 * nrow(grid_in)) ||
          any(par_merge_gridd_pptest < 0.3)) {
      message("The reduced computational regions have too complex shapes.
      Consider increasing thresholds or using the original grids.\n")
    }

    return(grid_out)
}

