# nolint start
#' @name chopin-package
#' @aliases chopin
#' @title Computation of spatial data by hierarchical and objective partitioning of inputs for parallel processing
#' @description
#' The `chopin` package provides a set of functions to compute on divided
#'  geospatial data.
#' @keywords internal
## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @section Basic functionalities:
#' * Distribute `terra`, `sf`, and `chopin` functions to parallel workers set by `future`
#' * Set parallelization strategies based on artificial grids, equal-size clusters, hierarchy, and multiple raster files
#' * Convenience functions for raster-vector overlay and weighted summary from vector dataset
#'
#' @section `chopin` workflow:
#'
#' * The simplest way of parallelizing generic geospatial computation is to start from `par_pad_*` functions to `par_grid`, or running `par_hierarchy`, or `par_multirasters` functions at once.
#'
#' ```r
#' library(chopin)
#' library(terra)
#' library(sf)
#' library(collapse)
#' library(dplyr)
#' library(future)
#' library(future.mirai)
#' library(future.apply)
#' ```
#'
#' **Example data**
#'
#' * North Carolinian counties and a raster of elevation data are used as example data.
#'
#' ```r
#' nccnty_path <- system.file("extdata", "nc_hierarchy.gpkg", package = "chopin")
#' ncelev_path <-
#'   system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
#' nccnty <- terra::vect(nccnty_path)
#' ncelev <- terra::rast(ncelev_path)
#' ```
#'
#' * To demonstrate chopin functions, we generate 10,000 random points in North Carolina
#'
#' ```r
#' ncsamp <-
#'   terra::spatSample(
#'     nccnty,
#'     1e4L
#'   )
#' ncsamp$pid <- 1:nrow(ncsamp)
#' ```
#'
#' **Creating grids**
#'
#' * The example below will generate a regular grid from the random point data.
#'
#' ```r
#' ncgrid <- par_pad_grid(ncsamp, mode = "grid", nx = 4L, ny = 2L, padding = 10000)
#' plot(ncgrid$original)
#' ```
#'
#'
#' **Extracting values from raster**
#' * Since all `par_*` functions operate on `future` backends, users should define the future plan before running the functions. `multicore` plan supports `terra` objects which may lead to faster computation, but it is not supported in Windows. An alternative is `future.mirai`'s `mirai_multisession` plan, which is supported in many platforms and generally faster than plain future multisession plan.
#' * `workers` argument should be defined with an integer value to specify the number of threads to be used.
#'
#' ```r
#' future::plan(future.mirai::mirai_multisession, workers = 2L)
#' ```
#'
#' * Then we dispatch multiple `extract_at` runs on the grid polygons.
#' * Before we proceed, the terra object should be converted to sf object.
#'
#' ```r
#' pg <-
#'   par_grid(
#'     grids = ncgrid,
#'     pad_y = FALSE,
#'     .debug = TRUE,
#'     fun_dist = extract_at,
#'     x = ncelev_path,
#'     y = sf::st_as_sf(ncsamp),
#'     id = "pid",
#'     radius = 1e4,
#'     func = "mean"
#'   )
#' ```
#'
#' **Hierarchical processing**
#' * Here we demonstrate hierarchical processing of the random points using census tract polygons.
#'
#' ```r
#' nccnty <- sf::st_read(nccnty_path, layer = "county")
#' nctrct <- sf::st_read(nccnty_path, layer = "tracts")
#' ```
#'
#' * The example below will parallelize summarizing mean elevation at 10 kilometers circular buffers of random sample points by the first five characters of census tract unique identifiers, which are county codes.
#' * This example demonstrates the hierarchy can be defined from any given polygons if the unique identifiers are suitably formatted for defining the hierarchy.
#'
#' ```r
#' px <-
#'   par_hierarchy(
#'     # from here the par_hierarchy-specific arguments
#'     regions = nctrct,
#'     regions_id = "GEOID",
#'     length_left = 5,
#'     pad = 10000,
#'     pad_y = FALSE,
#'     .debug = TRUE,
#'     # from here are the dispatched function definition
#'     # for parallel workers
#'     fun_dist = extract_at,
#'     # below should follow the arguments of the dispatched function
#'     x = ncelev,
#'     y = sf::st_as_sf(ncsamp),
#'     id = "pid",
#'     radius = 1e4,
#'     func = "mean"
#'   )
#' ```
#'
#' **Multiraster processing**
#'
#' * Here we demonstrate multiraster processing of the random points using multiple rasters.
#'
#' ```r
#' ncelev <-
#'   system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
#' ncelev <- terra::rast(ncelev)
#' tdir <- tempdir(check = TRUE)
#' terra::writeRaster(ncelev, file.path(tdir, "test1.tif"), overwrite = TRUE)
#' terra::writeRaster(ncelev, file.path(tdir, "test2.tif"), overwrite = TRUE)
#' terra::writeRaster(ncelev, file.path(tdir, "test3.tif"), overwrite = TRUE)
#' terra::writeRaster(ncelev, file.path(tdir, "test4.tif"), overwrite = TRUE)
#' terra::writeRaster(ncelev, file.path(tdir, "test5.tif"), overwrite = TRUE)
#' rasts <- list.files(tdir, pattern = "tif$", full.names = TRUE)
#'
#' pm <-
#'   par_multirasters(
#'     filenames = rasts,
#'     fun_dist = extract_at,
#'     x = NA,
#'     y = sf::st_as_sf(ncsamp)[1:500, ],
#'     id = "pid",
#'     radius = 1e4,
#'     func = "mean",
#'     .debug = TRUE
#'   )
#' ```
## usethis namespace: end
"_PACKAGE"
#nolint end
