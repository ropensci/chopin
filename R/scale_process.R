# Generated from chopin_rmarkdown_litr.rmd: do not edit by hand

#' Parallelization error fallback
#' @family Parallelization
#' @param err Error status or message.
#' @param fun function.
#' @param debug logical(1). Print error messages (`TRUE`) or not (`FALSE`)
#' @returns data.frame with one column
#' @note This function assumes that the `fun` has an argument named
#' `"id"`.
#' @author Insang Song
#' @examples
#' err <- simpleError("No input.")
#' par_fallback(err, extract_at, debug = TRUE)
#' @export
par_fallback <-
  function(
    err = NULL,
    fun = NULL,
    debug = FALSE
  ) {
    if (debug) {
      print(err)
    }
    fallback <- data.frame(ID = NA)
    fun_args <- formals(fun)
    indx <- grepl("id", names(fun_args))
    if (any(indx)) {
      detected_id <- fun_args[indx]
    } else {
      detected_id <- "id"
    }
    colnames(fallback)[1] <- detected_id
    return(fallback)
  }


#' @title Process a given function in the entire or partial computational grids
#' @family Parallelization
#' @description
#' [future::multicore], [future::multisession], [future::cluster]
#' with [doParallel::registerDoParallel] will parallelize the work
#' in each grid. For details of the terminology in \code{future} package,
#' refer to \link[future]{plan}. This function assumes that
#' users have one raster file and a sizable and spatially distributed
#' target locations. Each thread will process
#' the nearest integer of $|N_g| / |N_t|$ grids
#' where $|N_g|$ denotes the number of grids and $|N_t|$ denotes
#' the number of threads.
#' @note In dynamic dots (\code{...}), the first and second
#' arguments should be the \code{fun_dist} arguments where
#' sf/SpatVector objects are accepted.
#' Virtually any sf/terra functions that accept two arguments
#' can be put in \code{fun_dist}, but please be advised that
#' some spatial operations do not necessarily give the
#' exact result from what would have been done single-thread.
#' For example, distance calculated through this function may return the
#' lower value than actual because the computational region was reduced.
#' This would be the case especially where the target features
#' are spatially sparsely distributed.
#' @param grids sf/SpatVector object. Computational grids.
#'  It takes a strict assumption that the grid input is
#'  an output of \code{par_make_gridset}
#' @param grid_target_id character(1) or numeric(2).
#'  Default is NULL. If NULL, all grid_ids are used.
#'  \code{"id_from:id_to"} format or
#'  \code{c(unique(grid_id)[id_from], unique(grid_id)[id_to])}
#' @param debug logical(1). Prints error messages
#' if there were any errors during the calculation.
#' @param fun_dist `sf`, `terra` or `chopin` functions.
#' @param ... Arguments passed to the argument \code{fun_dist}.
#' The **second** place should get a vector or raster dataset from which
#' you want to extract or calculate values. For example, a raster dataset
#' when vector-raster overlay is performed.
#' @returns a data.frame object with computation results.
#'  For entries of the results, consult the function used in
#'  \code{fun_dist} argument.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' \dontrun{
#' ncpath <- system.file("shape/nc.shp", package = "sf")
#' ncpoly <- terra::vect(ncpath) |>
#'   terra::project("EPSG:5070")
#' ncpnts <-
#'   readRDS(
#'     system.file("extdata/nc_random_point.rds", package = "chopin")
#'   )
#' ncpnts <- terra::vect(ncpnts)
#' ncpnts <- terra::project(ncpnts, "EPSG:5070")
#' ncelev <-
#'   terra::unwrap(
#'     readRDS(system.file("extdata/nc_srtm15_otm.rds", package = "chopin"))
#'   )
#' terra::crs(ncelev) <- "EPSG:5070"
#' names(ncelev) <- c("srtm15")
#'
#' ncsamp <-
#'   terra::spatSample(
#'     terra::ext(ncelev),
#'     1e4L,
#'     lonlat = FALSE,
#'     as.points = TRUE
#'   )
#' ncsamp$kid <- sprintf("K-%05d", seq(1, nrow(ncsamp)))
#' nccompreg <-
#'   par_make_gridset(
#'     input = ncpnts,
#'     mode = "grid",
#'     nx = 6L,
#'     ny = 4L,
#'     padding = 3e4L
#'   )
#' res <-
#'   par_grid(
#'     grids = nccompreg,
#'     grid_target_id = NULL,
#'     fun_dist = extract_at_buffer,
#'     points = ncpnts,
#'     surf = ncelev,
#'     qsegs = 90L,
#'     radius = 5e3L,
#'     id = "pid"
#'   )
#' }
#' @import future
#' @importFrom future.apply future_lapply
#' @importFrom rlang inject
#' @importFrom rlang !!!
#' @importFrom dplyr bind_rows
#' @importFrom sf sf_use_s2
#' @export
par_grid <-
  function(
    grids,
    grid_target_id = NULL,
    debug = FALSE,
    fun_dist,
    ...
  ) {
    if (is.character(grid_target_id) && !grepl(":", grid_target_id)) {
      stop("Character grid_target_id should be in a form of 'startid:endid'.\n")
    }
    if (is.numeric(grid_target_id)) {
      if (length(grid_target_id) != 2) {
        stop(
          "Numeric grid_target_id should be in a form of c(startid, endid).\n"
        )
      }
      grid_target_ids <- unlist(grids$original[["CGRIDID"]])[grid_target_id]
    }
    # subset using grids and grid_id
    if (is.null(grid_target_id)) {
      grid_target_ids <- unlist(grids$original[["CGRIDID"]])
    }
    if (is.character(grid_target_id)) {
      grid_id_parsed <- strsplit(grid_target_id, ":", fixed = TRUE)[[1]]
      grid_target_ids <-
        c(which(unlist(grids$original[["CGRIDID"]]) == grid_id_parsed[1]),
          which(unlist(grids$original[["CGRIDID"]]) == grid_id_parsed[2]))
    }

    grids_target <-
      grids$original[grid_target_ids %in% unlist(grids$original[["CGRIDID"]]), ]
    grids_target_list <- split(grids_target, unlist(grids_target[["CGRIDID"]]))

    results_distributed <-
      future.apply::future_lapply(
        grids_target_list,
        function(grid) {
          sf::sf_use_s2(FALSE)

          run_result <- tryCatch({
            args_input <- list(...)
            ## Strongly assuming that
            # the first is "at", the second is "from"
            args_input[[1]] <-
              args_input[[1]][grid, ]
            if (methods::is(args_input[[2]], "SpatVector")) {
              gpad_in <- grids$padded[grids$padded$CGRIDID == grid$CGRIDID, ]
              args_input[[2]] <- args_input[[2]][gpad_in, ]
            }
            if (!"id" %in% names(formals(fun_dist))) {
              args_input$id <- NULL
            }

            res <- rlang::inject(fun_dist(!!!args_input))
            cat(
              sprintf(
                "Your input function was successfully run at CGRIDID: %s\n",
                as.character(unlist(grid[["CGRIDID"]]))
              )
            )

            if (!is.data.frame(res)) {
              res <- as.data.frame(res)
            }

            return(res)
          },
          error = function(e) {
            par_fallback(e, fun_dist, debug)

          })

          return(run_result)
        },
        future.seed = TRUE,
        future.packages = c("terra", "sf", "dplyr", "chopin", "exactextractr")
      )
    results_distributed <- do.call(dplyr::bind_rows, results_distributed)

    return(results_distributed)
  }


#' @title Process a given function using a hierarchy in input data
#' @family Parallelization
#' @description "Hierarchy" refers to a system,
#'  which divides the entire study region into multiple subregions.
#'  It is oftentimes reflected in an area code system
#'  (e.g., FIPS for US Census geographies, HUC-4, -6, -8, etc.).
#' [future::multicore], [future::multisession], [future::cluster]
#' with [doParallel::registerDoParallel] will parallelize the work
#' in each grid. For details of the terminology in \code{future} package,
#'  refer to \link[future]{plan}.
#'  This function assumes that users have one raster file and
#'  a sizable and spatially distributed target locations.
#'  Each thread will process the number of lower level features
#'  in each higher level feature. Please be advised that
#'  accessing the same file simultaneously with
#'  multiple processes may result in errors.
#' @note In dynamic dots (\code{...}), the first and second
#' arguments should be the \code{fun_dist} arguments where
#' `sf`/`SpatVector` objects are accepted.
#' Virtually any `sf`/`terra` functions that accept two arguments
#' can be put in \code{fun_dist}, but please be advised that
#' some spatial operations do not necessarily give the
#' exact result from what would have been done single-thread.
#' For example, distance calculated through this function may return the
#' lower value than actual because the computational region was reduced.
#' This would be the case especially where the target features
#' are spatially sparsely distributed.
#' @param regions sf/SpatVector object.
#'  Computational regions. Only polygons are accepted.
#' @param split_level character(nrow(regions)) or character(1).
#'  The regions will be split by the common level value.
#'  The level should be higher than the original data level.
#'  A field name with the higher level information is also accepted.
#' @param debug logical(1). Prints error messages
#' if there were any errors during the calculation.
#' @param fun_dist sf, terra, or chopin functions.
#' @param ... Arguments passed to the argument \code{fun_dist}.
#' The **second** place should get a vector or raster dataset from which
#' you want to extract or calculate values. For example, a raster dataset
#' when vector-raster overlay is performed.
#' @returns a data.frame object with computation results.
#'  For entries of the results, consult the function used in
#'  \code{fun_dist} argument.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#' library(chopin)
#' library(future)
#' library(doFuture)
#' sf::sf_use_s2(FALSE)
#' plan(multicore)
#' registerDoFuture()
#' 
#' ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
#' nccnty <- terra::vect(ncpath, layer = "county")
#' nctrct <- sf::st_read(ncpath, layer = "tracts")
#' nctrct <- terra::vect(nctrct)
#' ncelev <-
#'   terra::unwrap(
#'     readRDS(
#'       system.file("extdata/nc_srtm15_otm.rds", package = "chopin")
#'     )
#'   )
#' terra::crs(ncelev) <- "EPSG:5070"
#' names(ncelev) <- c("srtm15")
#'
#' ncsamp <-
#'   terra::spatSample(
#'     terra::ext(ncelev),
#'     1e4L,
#'     lonlat = FALSE,
#'     as.points = TRUE
#'   )
#' ncsamp$kid <- sprintf("K-%05d", seq(1, nrow(ncsamp)))
#' res <-
#'   par_hierarchy(
#'     regions = nccnty,
#'     split_level = "GEOID",
#'     fun_dist = extract_at_poly,
#'     polys = nctrct,
#'     surf = ncelev,
#'     id = "GEOID",
#'     func = "mean"
#'   )
#' )
#' }
#' @import future
#' @importFrom future.apply future_lapply
#' @importFrom rlang inject
#' @importFrom rlang !!!
#' @importFrom dplyr bind_rows
#' @importFrom sf sf_use_s2
#' @export
par_hierarchy <-
  function(
    regions,
    split_level = NULL,
    debug = FALSE,
    fun_dist,
    ...
  ) {

    if (!any(length(split_level) == 1, length(split_level) == nrow(regions))) {
      stop("The length of split_level is not valid.")
    }
    split_level <-
      ifelse(length(split_level) == nrow(regions),
             split_level,
             unlist(regions[[split_level]]))

    regions_list <- base::split(split_level, split_level)

    results_distributed <-
      future.apply::future_lapply(
        regions_list,
        function(subregion) {
          sf::sf_use_s2(FALSE)
          run_result <-
            tryCatch(
              {
                # TODO: padded subregion to deal with
                # edge cases; how to determine padding?
                subregion <-
                  regions[startsWith(split_level, subregion)]
                args_input <- list(...)
                ## Strongly assuming that
                # the first is "at", the second is "from"
                args_input[[1]] <-
                  args_input[[1]][subregion, ]
                if (!"id" %in% names(formals(fun_dist))) {
                  args_input$id <- NULL
                }

                res <- rlang::inject(fun_dist(!!!args_input))
                if (!is.data.frame(res)) {
                  res <- as.data.frame(res)
                }
                return(res)
              },
              error =
              function(e) {
                par_fallback(e, fun_dist, debug)
              }
            )
          return(run_result)
        },
        future.seed = TRUE,
        future.packages = c("terra", "sf", "dplyr", "rlang",
                            "chopin", "future", "exactextractr")
      )
    results_distributed <- do.call(dplyr::bind_rows, results_distributed)

    return(results_distributed)
  }




#' @title Process a given function over multiple large rasters
#' @family Parallelization
#' @description Large raster files usually exceed the memory capacity in size.
#'  Cropping a large raster into a small subset even consumes
#'  a lot of memory and adds processing time.
#'  This function leverages `terra` `SpatRaster` proxy
#'  to distribute computation jobs over multiple cores.
#'  It is assumed that users have multiple large raster files
#'  in their disk, then each file path is assigned to a thread.
#'  Each thread will directly read raster values from
#'  the disk using C++ pointers that operate in terra functions.
#'  For use, it is strongly recommended to use vector data with
#'  small and confined spatial extent for computation to avoid
#'  out-of-memory error. For this, users may need
#'  to make subsets of input vector objects in advance.
#' @param filenames character(n). A vector or list of
#'  full file paths of raster files. n is the total number of raster files.
#' @param debug logical(1). Prints error messages
#' if there were any errors during the calculation.
#' @param fun_dist sf, terra, or chopin functions.
#' @param ... Arguments passed to the argument \code{fun_dist}.
#' The **second** place should get a vector or raster dataset from which
#' you want to extract or calculate values. For example, a raster dataset
#' when vector-raster overlay is performed.
#' @returns a data.frame object with computation results.
#'  For entries of the results,
#'  consult the function used in \code{fun_dist} argument.
#' @author Insang Song \email{geoissong@@gmail.com}
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#' library(chopin)
#' library(future)
#' library(doFuture)
#' sf::sf_use_s2(FALSE)
#' plan(multicore)
#' registerDoFuture()
#'
#' ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
#' nccnty <- terra::vect(ncpath, layer = "county")
#' ncelev <-
#'   terra::unwrap(
#'     readRDS(
#'       system.file("extdata/nc_srtm15_otm.rds", package = "chopin")
#'     )
#'   )
#' terra::crs(ncelev) <- "EPSG:5070"
#' names(ncelev) <- c("srtm15")
#' tdir <- tempdir(check = TRUE)
#' terra::writeRaster(ncelev, file.path(tdir, "test1.tif"), overwrite = TRUE)
#' terra::writeRaster(ncelev, file.path(tdir, "test2.tif"), overwrite = TRUE)
#' terra::writeRaster(ncelev, file.path(tdir, "test3.tif"), overwrite = TRUE)
#' testfiles <- list.files(tdir, pattern = "tif$", full.names = TRUE)
#'
#' res <- par_multirasters(
#'   filenames = testfiles,
#'   fun_dist = extract_at_poly,
#'   polys = nccnty,
#'   surf = ncelev,
#'   id = "GEOID",
#'   func = "mean"
#' )
#' }
#' @import future
#' @importFrom future.apply future_lapply
#' @import doFuture
#' @importFrom terra rast
#' @export
par_multirasters <-
  function(
    filenames,
    debug = FALSE,
    fun_dist,
    ...
  ) {

    file_list <- split(filenames, filenames)
    results_distributed <-
      future_lapply(
        file_list,
        function(path) {
          run_result <-
            try({
              args_input <- list(...)
              vect_target_tr <- any_class_args(args_input, "SpatVector")
              vect_target_sf <- any_class_args(args_input, "sf")
              vect_target <- (vect_target_tr | vect_target_sf)
              vect_ext <- args_input[vect_target]
              vect_ext <- terra::ext(vect_ext[[1]])

              rast_target <- which(any_class_args(args_input, "SpatRaster"))
              args_input[[rast_target]] <-
                terra::rast(x = path, win = vect_ext)
              if (!"id" %in% names(formals(fun_dist))) {
                args_input$id <- NULL
              }

              res <- rlang::inject(fun_dist(!!!args_input))
              if (!is.data.frame(res)) res <- as.data.frame(res)
              res$base_raster <- path
              return(res)
            }
            )
          if (inherits(run_result, "try-error")) {
            par_fallback(run_result, fun_dist, debug)
          }
        },
        future.seed = TRUE,
        future.packages =
        c("terra", "sf", "dplyr", "rlang",
          "chopin", "future",
          "exactextractr")
      )
    results_distributed <- do.call(dplyr::bind_rows, results_distributed)

    return(results_distributed)
  }



