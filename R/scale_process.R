#' Parallelization error fallback
#' @family Parallelization
#' @param err Error status or message.
#' @param fun function.
#' @param inputid character(1). ID of the computational region.
#'   For example, `par_pad_grid` output should have `"CGRIDID"`,
#'   which will be included in the outcome data.frame.
#'   If the function does not have an argument containing `"id"`,
#'   the output will have a column named `"chopin_domain_id"`.
#' @returns data.frame with domain id and error message.
#' @note This function assumes that the `fun` has an argument containing
#' `"id"`.
#' @author Insang Song
#' @examples
#' err <- simpleError("No input.")
#' par_fallback(err, extract_at, inputid = 1)
#' @keywords internal
par_fallback <-
  function(
    err = NULL,
    fun = NULL,
    inputid = NULL
  ) {
    fallback <- matrix(NA, nrow = length(inputid), ncol = 1)
    fallback <- as.data.frame(fallback)
    fun_args <- formals(fun)
    indx <- grep("id", names(fun_args))
    detected_id <- "CGRIDID"
    if (length(indx) != 0) {
      detected_id <- "chopin_domain_id"
    }
    fallback$error_message <- paste(unlist(err), collapse = " ")
    fallback[, 1] <- inputid
    colnames(fallback)[1] <- detected_id
    return(fallback)
  }


#' @title Process a given function in the entire or partial computational grids
#' @family Parallelization
#' @description
#' [future::multicore], [future::multisession], [future::cluster]
#' will parallelize the work in each grid.
#' For details of the terminology in \code{future} package,
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
#'  an output of \code{par_pad_grid}.
#'  If missing or NULL is entered, `par_pad_grid` is internally
#'  called. In this case, the **first** element of the ellipsis argument
#'  `...` is considered `input` in `par_pad_grid` and `mode` is fixed
#'  as "grid"`. See [par_pad_grid()] for details.
#' @param grid_target_id character(1) or numeric(2).
#'  Default is NULL. If NULL, all grid_ids are used.
#'  \code{"id_from:id_to"} format or
#'  \code{c(unique(grid_id)[id_from], unique(grid_id)[id_to])}
#' @param debug logical(1). Default is `FALSE`. Otherwise,
#'   if a unit computation fails, the error message and the `CGRIDID`
#'   value where the error occurred will be included in the output.
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
#'   par_pad_grid(
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
#' @importFrom future.apply future_lapply
#' @importFrom rlang inject
#' @importFrom rlang !!!
#' @importFrom dplyr bind_rows
#' @importFrom collapse rowbind
#' @importFrom sf sf_use_s2
#' @importFrom cli cli_abort cli_inform
#' @export
par_grid <-
  function(
    grids,
    grid_target_id = NULL,
    debug = FALSE,
    fun_dist,
    ...
  ) {
    # grid generation if grids is NULL
    if (is.null(grids) || missing(grids)) {
      ellipsis <- list(...)
      if (any(names(ellipsis) == "mode")) {
        ellipsis[["mode"]] <- NULL
      }
      ellipsis$input <- ellipsis[[1]]
      grids <-
        rlang::inject(
          par_pad_grid(
            mode = "grid",
            !!!ellipsis
          )
        )
    }

    # grid id selection check
    grid_target_ids <- unlist(grids$original[["CGRIDID"]])
    if (is.numeric(grid_target_id)) {
      if (length(grid_target_id) != 2) {
        cli::cli_abort(
          "Numeric grid_target_id should be in a form of c(startid, endid).\n"
        )
      }
      grid_target_ids <- seq(grid_target_id[1], grid_target_id[2], by = 1)
    }
    if (is.character(grid_target_id)) {
      # subset using grids and grid_id
      if (grepl(":", grid_target_id)) {
        grid_id_parsed <- strsplit(grid_target_id, ":", fixed = TRUE)[[1]]
        grid_id_parsed <- as.numeric(grid_id_parsed)
        grid_target_ids <-
          seq(grid_id_parsed[1], grid_id_parsed[2], by = 1)
      } else {
        cli::cli_abort("grid_target_id should be formed 'startid:endid'.\n")
      }
    }
    grids_target_in <-
      grids$original[grid_target_ids, ]
    grids_target_list <-
      base::split(grids_target_in, unlist(grids_target_in[["CGRIDID"]]))

    # results_distributed <-
    #   future.apply::future_lapply(
    #     grids_target_list,
    #     function(grid) {
          sf::sf_use_s2(FALSE)
          grid <-
            tryCatch(
              terra::vect(grid),
              error = function(e) grid
            )

          args_input <- list(...)
          run_result <- tryCatch({
            ## Strongly assuming that
            # the first is "at", the second is "from"
            if (is.character(args_input[[1]])) {
              args_input$extent <- terra::ext(grid)
            } else {
              if (dep_check(grid) != dep_check(args_input[[1]])) {
                grid <- dep_switch(grid)
              }
              grid <- reproject_std(grid, terra::crs(args_input[[1]]))
              args_input[[1]] <-
                args_input[[1]][grid, ]
            }
            if (methods::is(args_input[[2]], "SpatVector")) {
              gpad_in <- grids$padded[grids$padded$CGRIDID %in% grid$CGRIDID, ]
              args_input[[2]] <- args_input[[2]][gpad_in, ]
            }
            if (!"id" %in% names(formals(fun_dist))) {
              args_input$id <- NULL
            }

            res <- rlang::inject(fun_dist(!!!args_input))
            cli::cli_inform(
              sprintf(
                "Your input function was successfully run at CGRIDID: %s\n",
                as.character(unlist(grid[["CGRIDID"]]))
              )
            )

            try(res <- as.data.frame(res))
            return(res)
          },
          error = function(e) {
            if (debug) {
              par_fallback(e, fun_dist, inputid = grid$CGRIDID)
            } else {
              return(NULL)
            }
          })

    #       return(run_result)
    #     },
    #     future.seed = TRUE,
    #     future.packages = c("chopin", "dplyr", "sf", "terra", "rlang"),
    #     future.globals = FALSE,
    #     future.scheduling = 2
    #   )
    # results_distributed <-
    #   results_distributed[!vapply(results_distributed, is.null, logical(1))]
    # results_distributed <- collapse::rowbind(results_distributed, fill = TRUE)
    # return(results_distributed)
    results <- Map(function(x) .backend_collector(x), results)
    results <-
      results[!vapply(results, is.null, logical(1))]
    results <- collapse::rowbind(results, fill = TRUE)
    return(results)

  }


#' @title Process a given function using a hierarchy in input data
#' @family Parallelization
#' @description "Hierarchy" refers to a system,
#'  which divides the entire study region into multiple subregions.
#'  It is oftentimes reflected in an area code system
#'  (e.g., FIPS for US Census geographies, HUC-4, -6, -8, etc.).
#'  [future::multicore], [future::multisession], [future::cluster]
#'  will parallelize the work by splitting lower level features into
#'  several higher level feature group.
#'  For details of the terminology in \code{future} package,
#'  refer to \link[future]{plan}.
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
#' @param regions_id character(nrow(regions)) or character(1).
#'  The regions will be split by the common level value.
#'  The level should be higher than the original data level.
#'  A field name with the higher level information is also accepted.
#' @param unit_id character(1). Default is NULL.
#'   If NULL, the lower level units will be split by the intersection
#'   between a higher level region and lower level units.
#'   Otherwise, the **first** element of the ellipsis argument
#'   `...` is used to split the lower level units.
#' @param debug logical(1). Default is `FALSE`
#'   If a unit computation fails, the error message and the `regions_id`
#'   value where the error occurred will be included in the output.
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
#' registerDoFuture()
#' plan(multicore)
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
#'     regions_id = "GEOID",
#'     fun_dist = extract_at_poly,
#'     polys = nctrct,
#'     surf = ncelev,
#'     id = "GEOID",
#'     func = "mean"
#'   )
#' )
#' }
#' @importFrom future.apply future_lapply
#' @importFrom rlang inject
#' @importFrom rlang !!!
#' @importFrom collapse rowbind
#' @importFrom sf sf_use_s2
#' @importFrom cli cli_abort cli_inform
#' @export
par_hierarchy <-
  function(
    regions,
    regions_id = NULL,
    unit_id = NULL,
    debug = FALSE,
    fun_dist,
    ...
  ) {

    if (!length(regions_id) %in% c(1, nrow(regions))) {
      cli::cli_abort("The length of regions_id is not valid.")
    }

    regions_idn <-
      if (length(regions_id) == nrow(regions)) {
        regions_id
      } else {
        unique(unname(unlist(regions[[regions_id]])))
      }
    regions_list <- as.list(regions_idn)

    results <- regions_list
    # results_distributed <-
      # future.apply::future_lapply(
      #   regions_list,
      #   function(subregion) {
          sf::sf_use_s2(FALSE)
      for (i in seq_len(length(regions_list))) {
        args_input <- list(...)
        results[[i]] <-
          .backend_worker(
            {
            tryCatch(
              {
                # TODO: padded subregion to deal with
                # edge cases; how to determine padding?
                subregion_in <-
                  regions[startsWith(regions_idn, regions_list[[i]]), ]
                
                ## Strongly assuming that
                # the first is "at", the second is "from"
                if (is.null(unit_id)) {
                  args_input[[1]] <-
                    args_input[[1]][subregion_in, ]
                } else {
                  ain1 <- args_input[[1]]
                  uid <- unname(unlist(ain1[[unit_id]]))
                  ain11 <- ain1[grep(paste0("^", subregion), uid), ]
                  args_input[[1]] <- ain11
                }
                if (!"id" %in% names(formals(fun_dist))) {
                  args_input$id <- NULL
                }

                res <- rlang::inject(fun_dist(!!!args_input))
                res <- try(as.data.frame(res))
                return(res)
              },
              error =
              function(e) {
                if (debug) {
                  par_fallback(
                    e, fun_dist,
                    inputid = subregion
                  )
                } else {
                  return(NULL)
                }
              }
            )
            }, environment()
          )
        cli::cli_inform(
          c(i = sprintf("Your input function at %s is dispatched.\n", subregion))
        )
      }
          # return(run_result)
      #   },
      #   future.seed = TRUE,
      #   future.packages = c("chopin", "dplyr", "sf", "terra", "rlang"),
      #   future.globals = TRUE,
      #   future.scheduling = 2
      # )
    # results_distributed <-
    #   results_distributed[!vapply(results_distributed, is.null, logical(1))]
    # results_distributed <- collapse::rowbind(results_distributed, fill = TRUE)
    results <- Map(function(x) .backend_collector(x), results)

    results <-
      results[!vapply(results, is.null, logical(1))]
    results <- collapse::rowbind(results, fill = TRUE)

    return(results)
  }




#' @title Process a given function over multiple large rasters
#' @family Parallelization
#' @description Large raster files usually exceed the memory capacity in size.
#'  This function can be helpful to process heterogenous raster files with
#'  homogeneous summary functions. Heterogenous raster files refer to
#'  rasters with different spatial extents and resolutions.
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
#' @param debug logical(1). Default is `FALSE`.
#'   If a unit computation fails, the error message and the file path
#'   where the error occurred will be included in the output.
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
#' registerDoFuture()
#' plan(multicore)
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
#' @importFrom future future
#' @importFrom terra rast
#' @importFrom mirai mirai call_mirai
#' @importFrom rlang inject
#' @importFrom collapse rowbind
#' @importFrom cli cli_inform
#' @export
par_multirasters <-
  function(
    filenames,
    debug = FALSE,
    fun_dist,
    ...
  ) {

    file_list <- filenames#split(filenames, filenames)
    results <- vector("list", length = length(filenames))
    args_input <- list(...)

    # results_distributed <-
    #   future.apply::future_lapply(
    #     file_list,
    #     function(path) {
          # run_result <-
    for (i in seq_along(filenames)) {
      results[[i]] <-
        .backend_worker(
          {
            tryCatch({
              args_input <- args_input
              debug <- debug
              filenames <- filenames
              vect_target <- vapply(args_input, inherits, logical(1), c("SpatVector", "sf"))
              # vect_target_tr <- any_class_args(args_input, "SpatVector")
              # vect_target_sf <- any_class_args(args_input, "sf")
              # vect_target <- (vect_target_tr | vect_target_sf)
              
              # if (!all(vect_target)) {
              #   vect_ext <- terra::vect()
              # } else {
              #   vect_ext <- args_input[vect_target]
              #   vect_ext <- terra::ext(vect_ext[[1]])
              # }

              rast_target <- vapply(args_input, inherits, logical(1), "SpatRaster")
              rast_target <- which(rast_target)
              #rast_target <- which(any_class_args(args_input, "SpatRaster"))
              args_input$surf <-#[[rast_target]] <-
                terra::rast(x = file_list[i])
              if (!"id" %in% names(formals(fun_dist))) {
                args_input$id <- NULL
              }

              res <- rlang::inject(fun_dist(!!!args_input))
              try(res <- as.data.frame(res))
              res$base_raster <- filenames[i]
              return(res)
            }, error = function(e) {
              data.frame(base_raster = filenames[i], error_message = paste(unlist(e), collapse = " "))
              #if (debug) {
                #par_fallback(e, fun_dist, inputid = filenames[i])
              #} else {
              #  return(NULL)
              #}
            }
          )
          }, environment()
        )
        cli::cli_inform(c(i = sprintf("Your input function at %s is dispatched.\n", filenames[i])))
    }
    # return(results)
    # TODO: mirai-centered approach; future should call value()
    #results_l <- results
    results <- Map(function(x) .backend_collector(x), results)
    # return(results_l)
    results <-
      results[!vapply(results, is.null, logical(1))]
    results <- collapse::rowbind(results, fill = TRUE)
    return(results)
          # if (inherits(run_result, "try-error")) {
          # }
      #   },
      #   future.seed = TRUE,
      #   future.packages =
      #   c("chopin", "dplyr", "sf", "terra", "rlang"),
      #   future.globals = FALSE,
      #   future.scheduling = 2
      # )
    # results_distributed <-
    #   results_distributed[!vapply(results_distributed, is.null, logical(1))]
    # results_distributed <- collapse::rowbind(results_distributed, fill = TRUE)
    # return(results_distributed)
  }
