# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' Get a set of computational regions
#' 
#' @param input sf or Spat* object.
#' @param mode character(1). Mode of region construction.
#'  One of "grid" (simple grid regardless of
#'  the number of features in each grid),
#'  "density" (clustering-based varying grids),
#'  "grid_advanced" (merging adjacent grids with
#'  smaller number of features than grid_min_features). 
#' @param nx integer(1). The number of grids along x-axis.
#' @param ny integer(1). The number of grids along y-axis.
#' @param grid_min_features integer(1). A threshold to merging adjacent grids
#' @param padding numeric(1). A extrusion factor to make buffer to
#'  clip actual datasets. Depending on the length unit of the CRS of input.
#' @param unit character(1). The length unit for padding (optional).
#'  units::set_units is used for padding when sf object is used.
#'  See [units package vignette (web)]
#' (https://cran.r-project.org/web/packages/units/vignettes/measurement_units_in_R.html)
#'  for the list of acceptable unit forms.
#' @param ... arguments passed to the internal function
#' @return A list of two,
#'   \code{original}: exhaustive and non-overlapping
#'  grid polygons in the class of input
#'   \code{padded}: a square buffer of each polygon in
#'  \code{original}. Used for computation.
#' @description Using input points, the bounding box is split to
#'  the predefined numbers of columns and rows.
#'  Each grid will be buffered by the radius.
#' @author Insang Song
#' @examples
#' # data
#' library(sf)
#' ncpath <- system.file("shape/nc.shp", package = "sf")
#' nc <- read_sf(ncpath)
#' nc <- st_transform(nc, "EPSG:5070")
#' # run
#' # nc_comp_region <- get_computational_regions(nc, nx = 12, ny = 8)
#' 
#' @export
get_computational_regions <-
  function(
      input,
      mode = c("grid", "grid_advanced", "density"),
      nx = 10L,
      ny = 10L,
      grid_min_features = 30L,
      padding = NULL,
      unit = NULL,
      ...) {

    stopifnot("Argument mode should be one of
      'grid', 'grid_advanced', or 'density'.\n" =
                mode %in% c("grid", "grid_advanced", "density"))
    stopifnot("Ensure that nx, ny, and grid_min_features are
      all integer.\n" = {
                        all(is.integer(nx),
                            is.integer(ny),
                            is.integer(grid_min_features))})
    stopifnot("padding should be numeric.
      We convert padding to numeric...\n" = {
                                            is.numeric(padding)})
    # valid unit compatible with units::set_units?
    grid_reg <-
      switch(mode,
        grid = sp_index_grid(points_in = input, ncutsx = nx, ncutsy = ny),
        grid_advanced = grid_merge(
                                  points_in = input,
                                  sp_index_grid(input, nx, ny),
                                  grid_min_features = grid_min_features),
        density = simpleError("density method is under development.\n")
      )

    type_grid_reg <- check_packbound(grid_reg)
    grid_reg_pad <-
      switch(type_grid_reg,
             sf =
             sf::st_buffer(grid_reg,
                           dist = padding,
                           endCapStyle = "SQUARE"),
             terra =
             terra::buffer(grid_reg,
                           width = padding,
                           capstyle = "square"))
    grid_results <-
      list(original = grid_reg,
           padded = grid_reg_pad)
    return(grid_results)

  }

#' @title sp_index_grid: Generate grid polygons
#' @description Returns a sf object that includes x- and y- index
#'  by using two inputs ncutsx and ncutsy, which are x- and
#'  y-directional splits, respectively.
#' @author Insang Song
#' @param points_in sf or SpatVector object. Target points of computation.
#' @param ncutsx integer(1). The number of splits along x-axis.
#' @param ncutsy integer(1). The number of splits along y-axis.
#' @return A sf or SpatVector object of computation grids with
#'  unique grid id (CGRIDID).
#' @export
sp_index_grid <-
  function(
    points_in,
    ncutsx,
    ncutsy) {
  package_detected <- check_packbound(points_in)

  sp_index_grid_sf <- function(points_in, ncutsx, ncutsy) {
    grid1 <- sf::st_make_grid(points_in, n = c(ncutsx, ncutsy)) |>
      as.data.frame() |>
      sf::st_as_sf()
    grid1 <- grid1[points_in, ]
    return(grid1)
  }
  sp_index_grid_terra <- function(points_in, ncutsx, ncutsy) {
    grid1 <- terra::rast(points_in, nrows = ncutsy, ncols = ncutsx)
    grid1 <- terra::as.polygons(grid1)
    grid1 <- grid1[points_in,]
    return(grid1)
  }
  grid_out <-
    switch(package_detected,
           sf = sp_index_grid_sf(points_in, ncutsx, ncutsy),
           terra = sp_index_grid_terra(points_in, ncutsx, ncutsy))

  grid_out$CGRIDID <- seq(1, nrow(x = grid_out))
  return(grid_out)
}


#' @title grid_merge: Merge grid polygons with given rules
#' @description Merge boundary-sharing (in "Rook" contiguity) grids with
#'  fewer target features than the threshold.
#'  This function strongly assumes that the input
#'  is returned from the sp_index_grid,
#'  which has 'CGRIDID' as the unique id field.
#' @author Insang Song
#' @param points_in sf or SpatVector object. Target points of computation.
#' @param grid_in sf or SpatVector object. The grid generated by sp_index_grid
#' @param grid_min_features integer(1). Threshold to merge adjacent grids.
#' @return A sf or SpatVector object of computation grids.
#' @examples
#' # library(sf)
#' # library(igraph)
#' # ligrary(dplyr)
#' # dg <- sf::st_as_sfc(st_bbox(c(xmin = 0, ymin = 0, xmax = 8e5, ymax = 6e5)))
#' # sf::st_crs(dg) <- 5070
#' # dgs <- sf::st_as_sf(st_make_grid(dg, n = c(20, 15)))
#' # dgs$CGRIDID <- seq(1, nrow(dgs))
#' #
#' # dg_sample <- st_sample(dg, kappa = 5e-9, mu = 15,
#' # scale = 20000, type = "Thomas")
#' # sf::st_crs(dg_sample) <- sf::st_crs(dg)
#' # dg_merged <- grid_merge(sf::st_as_sf(sss), dgs, 100)
#'
#' #### NOT RUN ####
#' @export
grid_merge <- function(points_in, grid_in, grid_min_features) {
  package_detected <- check_packbound(points_in)
  if (package_detected == "terra") {
    points_in <- sf::st_as_sf(points_in)
    grid_in <- sf::st_as_sf(grid_in)
  }

  n_points_in_grid <- lengths(sf::st_intersects(grid_in, points_in))
  grid_self <- sf::st_relate(grid_in, grid_in, pattern = "2********")
  grid_rook <- sf::st_relate(grid_in, grid_in, pattern = "F***1****")
  grid_rooks <- mapply(c, grid_self, grid_rook, SIMPLIFY = FALSE)
  grid_lt_threshold <- (n_points_in_grid < grid_min_features)
  stopifnot("Threshold is too low. Please try higher threshold.\n" =
    sum(grid_lt_threshold) != 0)
  grid_lt_threshold <- seq(1, nrow(grid_in))[grid_lt_threshold]

  # This part does not work as expected.
  # Should investigate edge list and actual row index of the grid object; 
  identified <- lapply(grid_rooks, \(x) sort(x[which(x %in% grid_lt_threshold)]))
  identified <- identified[grid_lt_threshold]
  identified <- unique(identified)
  identified <- identified[sapply(identified, length) > 1]

  identified_graph <- lapply(identified, \(x) t(utils::combn(x, 2))) |>
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
    unlist(lapply(merge_member, \(x) paste(x, collapse = "_")))
  merge_member_label <- merge_member_label[identified_graph_member]

  # sf object manipulation
  grid_out <- grid_in
  grid_out[["CGRIDID"]][merge_idx] <- merge_member_label

  grid_out <- grid_out |>
    dplyr::group_by(!!rlang::sym("CGRIDID")) |>
    dplyr::summarize(n_merged = dplyr::n()) |>
    dplyr::ungroup() 

  ## polsby-popper test for shape compactness
  grid_merged <- grid_out[which(grid_out$n_merged > 1),]
  grid_merged_area <- as.numeric(sf::st_area(grid_merged))
  grid_merged_perimeter <-
    as.numeric(sf::st_length(sf::st_cast(grid_merged, "LINESTRING")))
  grid_merged_pptest <-
    (4 * pi * grid_merged_area) / (grid_merged_perimeter ^ 2)

  # pptest value is bounded [0,1];
  # 0.3 threshold is groundless at this moment,
  # possibly will make it defined by users.
  if (max(unique(identified_graph_member)) > floor(0.1 * nrow(grid_in)) ||
   any(grid_merged_pptest < 0.3)) {
    message("The reduced computational regions have too complex shapes.
     Consider increasing thresholds or using the original grids.\n")
  }

  return(grid_out)

}



