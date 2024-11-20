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
#' * Distribute `terra`, `sf`, and `chopin` functions to parallel workers set by `future` or `mirai`
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
#'
#' @section Function selection guide for `par_*()`:
#' We provide two flowcharts to help users choose the right function for
#' parallel processing. The raster-oriented flowchart is for users who
#' want to start with raster data, and the vector-oriented flowchart
#' is for users with large vector data.
#'
#' In **raster-oriented selection**, we suggest four factors to consider:
#'
#' - Number of raster files: for multiple files, `par_multirasters` is recommended. When there are multiple rasters that share the same extent and resolution, consider stacking the rasters into multilayer SpatRaster object by calling `terra::rast(filenames)`.
#' - Raster resolution: We suggest 100 meters as a threshold. Rasters with resolution coarser than 100 meters and a few layers would be better for the direct call of `exactextractr::exact_extract()`.
#' - Raster extent: Using `SpatRaster` in `exactextractr::exact_extract()` is often minimally affected by the raster extent.
#' - Memory size: `max_cells_in_memory` argument value of `exactextractr::exact_extract()`, raster resolution, and the number of layers in `SpatRaster` are multiplicatively related to the memory usage.
#'
#' For **vector-oriented selection**, we suggest three factors to consider:
#' - Number of features: When the number of features is over 100,000, consider using `par_grid` or `par_hierarchy` to split the data into smaller chunks.
#' - Hierarchical structure: If the data has a hierarchical structure, consider using `par_hierarchy` to parallelize the operation.
#' - Data grouping: If the data needs to be grouped in similar sizes, consider using `par_pad_balanced` or `par_pad_grid` with `mode = "grid_quantile"`.
#'
#'
#' @section Caveats:
#' **Why parallelization is slower than the ordinary function run?**
#' Parallelization may underperform when the datasets are too small
#' to take advantage of divide-and-compute approach, where
#' parallelization overhead is involved. Overhead here refers to
#' the required amount of computational resources for transferring
#' objects to multiple processes. Since the demonstrations above
#' use quite small datasets, the advantage of parallelization was not
#' as noticeable as it was expected. Should a large amount of
#' data (spatial/temporal resolution or number of files,
#' for example) be processed, users could find the efficiency of this
#' package. A vignette in this package demonstrates use cases
#' extracting various climate/weather datasets.
#'
#' **Notes on data restrictions**
#'
#' `chopin` works best with **two-dimensional** (**planar**) geometries.
#' Users should disable `s2` spherical geometry mode in `sf` by setting
#' `sf::sf_use_s2(FALSE)`.
#' Running any `chopin` functions at spherical or three-dimensional
#' (e.g., including M/Z dimensions) geometries
#' may produce incorrect or unexpected results.
#'
## usethis namespace: end
"_PACKAGE"
#nolint end
