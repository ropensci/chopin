#' Process a given function in the entire or partial computational grids
#' @family Parallelization
#' @description
#' [future::multicore], [future::multisession], [future::cluster]
#' will parallelize the work in each grid.
#' For details of the terminology in `future` package,
#' refer to [`future::plan`]. This function assumes that
#' users have one raster file and a sizable and spatially distributed
#' target locations. Each thread will process
#' the nearest integer of $|N_g| / |N_t|$ grids
#' where $|N_g|$ denotes the number of grids and $|N_t|$ denotes
#' the number of threads.
#' @note In dynamic dots (`...`), `fun_dist` arguments should include
#' x and y where sf/terra class objects or file paths are accepted.
#' Virtually any sf/terra functions that accept two arguments
#' can be put in `fun_dist`, but please be advised that
#' some spatial operations do not necessarily give the
#' exact result from what would have been done single-thread.
#' For example, distance calculated through this function may return the
#' lower value than actual because the computational region was reduced.
#' This would be the case especially where the target features
#' are spatially sparsely distributed.
#' @param grids List of two sf/SpatVector objects. Computational grids.
#'  It takes a strict assumption that the grid input is
#'  an output of `par_pad_grid``.
#' @param fun_dist `sf`, `terra` or `chopin` functions.
#'   This function should have `x` and `y` arguments.
#' @param ... Arguments passed to the argument `fun_dist`.
#' @param pad_y logical(1). Whether to filter y with the padded grid.
#'  Should be TRUE when x is where the values are calculated.
#'  Default is `FALSE`. In the reverse case, like `terra::extent` or
#'  `exactextractr::exact_extract`, the raster (x) should be scoped
#'   with the padded grid.
#' @param .debug logical(1). Default is `FALSE`. Otherwise,
#'   if a unit computation fails, the error message and the `CGRIDID`
#'   value where the error occurred will be included in the output.
#' @returns a data.frame object with computation results.
#'  For entries of the results, consult the documentation of the function put
#'  in `fun_dist` argument.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' ncpath <- system.file("shape/nc.shp", package = "sf")
#' ncpoly <- sf::st_read(ncpath)
#' ncpnts <-
#'   readRDS(
#'     system.file("extdata/nc_random_point.rds", package = "chopin")
#'   )
#' ncelev <-
#'     readRDS(system.file("extdata/nc_srtm15_otm.tif", package = "chopin"))
#'
#' res <-
#'   par_grid(
#'     grids = nccompreg,
#'     fun_dist = extract_at,
#'     y = ncpnts,
#'     x = ncelev,
#'     qsegs = 90L,
#'     radius = 5e3L,
#'     id = "pid"
#'   )
#' @seealso
#'  [`future::multisession`] [`future::multicore`] [`future::cluster`]
#'  [`future.mirai::mirai_multisession`] [`future::plan`] [`par_map_args`]
#' @importFrom future.apply future_lapply
#' @importFrom rlang inject !!!
#' @importFrom dplyr bind_rows
#' @importFrom collapse rowbind
#' @importFrom sf sf_use_s2
#' @importFrom cli cli_abort cli_inform
#' @importFrom methods getPackageName
#' @export
par_grid <-
  function(
    grids,
    fun_dist,
    ...,
    pad_y = FALSE,
    .debug = FALSE
  ) {
    sf::sf_use_s2(FALSE)

    if (inherits(grids[[1]], "SpatVector")) {
      grids <- Map(sf::st_as_sf, grids)
    }

    # grid id selection check
    grids_target_in <- grids$original
    grids_target_list <-
      split(grids_target_in, unlist(grids_target_in[["CGRIDID"]]))

    # initiate an index list
    results <- as.list(seq_along(grids_target_list))

    # is the function sf?
    fun_parent <- methods::getPackageName(environment(fun_dist))
    is_sf_parent <- (fun_parent == "sf")

    # is the function extract_at?
    is_extract_at <- as.character(substitute(fun_dist))
    is_extract_at <- any(endsWith(is_extract_at, "extract_at"))

    # clean additional arguments
    args_input <- list(...)
    if (fun_parent == "chopin" && is.null(args_input$.standalone)) {
      args_input$.standalone <- FALSE
    }
    if (!"id" %in% names(formals(fun_dist))) {
      args_input$id <- NULL
    }

    # get hints from the inputs
    crs_x <- .check_character(args_input$x)
    peek_x <- try(.check_character(args_input$x), silent = TRUE)
    peek_y <- try(.check_character(args_input$y), silent = TRUE)

    # Main parallelization
    results <-
      future.apply::future_lapply(results, function(i) {
        # inside each parallel job, feel free to use terra functions
        # technically we do not export terra objects, rather calling
        # terra functions directly to make objects from scratch in
        # parallel workers.
        options(sf_use_s2 = FALSE)
        tryCatch({
          grid_in <- grids_target_list[[i]]

          grid_in <- reproject_std(grid_in, attr(crs_x, "crs"))
          gpad_in <- grids$padded[grids$padded$CGRIDID %in% grid_in$CGRIDID, ]
          class(gpad_in)

          args_input$x <-
            .par_screen(
              type = peek_x,
              input = args_input$x,
              input_id = NULL,
              out_class = if (is_sf_parent) "sf" else "terra",
              .window = if (pad_y) grid_in else gpad_in
            )
          print(args_input$x)
          args_input$y <-
            .par_screen(
              type = peek_y,
              input = args_input$y,
              input_id = NULL,
              out_class = if (is_sf_parent || is_extract_at) "sf" else "terra",
              .window = if (pad_y) gpad_in else grid_in
            )
          print(args_input$y)
          res <- rlang::inject(fun_dist(!!!args_input))
          cli::cli_alert_info(
            sprintf(
              "Task at CGRIDID: %s is successfully dispatched.\n",
              as.character(unlist(grid_in[["CGRIDID"]]))
            )
          )

          res <- try(as.data.frame(res), silent = TRUE)
          return(res)
        },
        error = function(e) {
          if (.debug) {
            grid_in <- grids_target_list[[i]]
            data.frame(
              CGRIDID = grid_in[["CGRIDID"]],
              error_message = paste(unlist(e), collapse = " ")
            )
          } else {
            return(NULL)
          }
        })
      },
      future.seed = TRUE
      )

    # remove NULL
    results <-
      results[!vapply(results, is.null, logical(1))]

    # Bind rows
    results <- collapse::rowbind(results, fill = TRUE)

    return(results)
  }


#' @title Process a given function using a hierarchy in input data
#' @family Parallelization
#' @description "Hierarchy" refers to a system,
#'  which divides the entire study region into multiple subregions.
#'  It is oftentimes reflected in an area code system
#'  (e.g., FIPS for US Census geographies, HUC-4, -6, -8, etc.).
#'  [`future::multisession`], [`future::multicore`], [`future::cluster`],
#'  [`future.mirai::mirai_multisession`] in [`future::plan`]
#'  will parallelize the work by splitting lower level features into
#'  several higher level feature group.
#'  For details of the terminology in `future` package,
#'  refer to [`future::plan`].
#'  Each thread will process the number of lower level features
#'  in each higher level feature. Please be advised that
#'  accessing the same file simultaneously with
#'  multiple processes may result in errors.
#' @note In dynamic dots (`...`), `fun_dist` arguments should include
#' x and y where sf/terra class objects or file paths are accepted.
#' Virtually any sf/terra functions that accept two arguments
#' can be put in `fun_dist`, but please be advised that
#' some spatial operations do not necessarily give the
#' exact result from what would have been done single-thread.
#' For example, distance calculated through this function may return the
#' lower value than actual because the computational region was reduced.
#' This would be the case especially where the target features
#' are spatially sparsely distributed.
#' @param regions `sf`/`SpatVector` object.
#'  Computational regions. Only polygons are accepted.
#' @param regions_id character(nrow(regions)) or character(1).
#'  The regions will be split by the common level value.
#'  The level should be higher than the original data level.
#'  A field name with the higher level information is also accepted.
#' @param fun_dist `sf`, `terra`, or `chopin` functions.
#'   This function should have `x` and `y` arguments.
#' @param ... Arguments passed to the argument \code{fun_dist}.
#' The **second** place should get a vector or raster dataset from which
#' you want to extract or calculate values. For example, a raster dataset
#' when vector-raster overlay is performed.
#' @param pad_y logical(1). Whether to filter y with the padded grid.
#'  Should be TRUE when x is where the values are calculated.
#'  Default is `FALSE`. In the flipped case, like `terra::extent` or
#'  `exactextractr::exact_extract`, the raster (x) should be scoped
#'   with the padded grid.
#' @param .debug logical(1). Default is `FALSE`
#'   If a unit computation fails, the error message and the `regions_id`
#'   value where the error occurred will be included in the output.
#' @returns a data.frame object with computation results.
#'  For entries of the results, consult the function used in
#'  \code{fun_dist} argument.
#' @seealso
#'  [`future::multisession`] [`future::multicore`] [`future::cluster`]
#'  [`future.mirai::mirai_multisession`] [`future::plan`] [`par_map_args`]
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' library(terra)
#' library(sf)
#' library(future)
#' library(future.mirai)
#' sf::sf_use_s2(FALSE)
#' future::plan(future.mirai::mirai_multisession, workers = 2)
#'
#' ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
#' nccnty <- sf::st_read(ncpath, layer = "county")
#' nctrct <- sf::st_read(ncpath, layer = "tracts")
#' ncelev <-
#'   system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
#'
#' ncsamp <-
#'   sf::st_sample(
#'     nccnty,
#'     size = 1e4L
#'   )
#' ncsamp <- sf::st_as_sf(ncsamp)
#' ncsamp$kid <- sprintf("K-%05d", seq_len(nrow(ncsamp)))
#' res <-
#'   par_hierarchy(
#'     regions = nccnty,
#'     regions_id = "GEOID",
#'     fun_dist = extract_at,
#'     y = nctrct,
#'     x = ncelev,
#'     id = "GEOID",
#'     func = "mean"
#'   )
#' )
#' @importFrom future.apply future_lapply
#' @importFrom rlang inject !!!
#' @importFrom collapse rowbind
#' @importFrom sf sf_use_s2
#' @importFrom cli cli_abort cli_inform
#' @export
par_hierarchy <-
  function(
    regions,
    regions_id = NULL,
    fun_dist,
    ...,
    pad_y = FALSE,
    .debug = FALSE
  ) {
    args_input <- list(...)
    if (!"id" %in% names(formals(fun_dist))) {
      args_input$id <- NULL
    }

    # get hints from the inputs
    crs_x <- .check_character(args_input$x)
    peek_x <- try(.check_character(args_input$x), silent = TRUE)
    peek_y <- try(.check_character(args_input$y), silent = TRUE)

    if (!length(regions_id) %in% c(1, nrow(regions))) {
      cli::cli_abort("The length of regions_id is not valid.")
    }

    # Region ID cleaning to get unique high-level IDs
    # what if regions refers to a path string?
    regions_idn <-
      if (length(regions_id) == nrow(regions)) {
        unique(regions_id)
      } else {
        unique(unlist(regions[[regions_id]], use.names = FALSE))
      }
    regions_list <- as.list(regions_idn)

    ## Main parallelization
    results <-
      future.apply::future_lapply(
        seq_along(regions_list),
        function(i) {
          options(sf_use_s2 = FALSE)

          result <-
            tryCatch(
              {
                subregion_in <-
                  regions[startsWith(regions_idn, regions_list[[i]]), ]
                query_id <-
                  unlist(subregion_in[[regions_id]], use.names = FALSE)

                # interpret the function input x and y
                args_input$x <-
                  .par_screen(
                    type = peek_x,
                    input = args_input$x,
                    input_id = NULL,
                    out_class = "terra",
                    .window = NULL
                  )
                args_input$y <-
                  .par_screen(
                    type = peek_y,
                    input = args_input$y,
                    input_id = NULL,
                    out_class = "terra",
                    .window = NULL
                  )

                if (pad_y) {
                  data_id <-
                    unlist(args_input$x[[regions_id]], use.names = FALSE)
                  args_input$x <-
                    args_input$x[startsWith(data_id, query_id), ]
                } else {
                  data_id <-
                    unlist(args_input$y[[regions_id]], use.names = FALSE)
                  args_input$y <-
                    args_input$y[startsWith(data_id, query_id), ]
                }

                # reproject the y to the crs of x
                args_input$y <-
                  reproject_std(args_input$y, attr(crs_x, "crs"))

                res <- rlang::inject(fun_dist(!!!args_input))
                res <- try(as.data.frame(res), silent = TRUE)
                cli::cli_alert_info(
                  sprintf("Your input function at %s is dispatched.\n",
                          query_id)
                )

                return(res)
              },
              error =
              function(e) {
                if (.debug) {
                  data.frame(
                    regions_id = regions_list[[i]],
                    error_message = paste(unlist(e), collapse = " ")
                  )
                } else {
                  return(NULL)
                }
              }
            )
          return(result)
        },
        future.seed = TRUE
      )

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
#'  This function leverages `terra` `SpatRaster`
#'  to distribute computation jobs over multiple threads.
#'  It is assumed that users have multiple large raster files
#'  in their disk, then each file path is assigned to a thread.
#'  Each thread will directly read raster values from
#'  the disk using C++ pointers that operate in terra functions.
#'  For use, it is strongly recommended to use vector data with
#'  small and confined spatial extent for computation to avoid
#'  out-of-memory error. `y` argument in `fun_dist` will be used as-is.
#'  That means no preprocessing or subsetting will be
#'  applied. Please be aware of the spatial extent and size of the
#'  inputs.
#' @param filenames character. A vector or list of
#'  full file paths of raster files. n is the total number of raster files.
#' @param fun_dist sf, terra, or chopin functions.
#'   This function should have `x` and `y` arguments and `x` is a
#'   raster object.
#' @param ... Arguments passed to the argument `fun_dist`.
#' @param .debug logical(1). Default is `FALSE`. If `TRUE` and
#'   a unit computation fails, the error message and the file path
#'   where the error occurred will be included in the output.
#' @returns a data.frame object with computation results.
#'  For entries of the results,
#'  consult the function used in `fun_dist` argument.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @seealso
#'  [`future::multisession`] [`future::multicore`] [`future::cluster`]
#'  [`future.mirai::mirai_multisession`] [`future::plan`] [`par_map_args`]
#'
#' @examples
#' library(terra)
#' library(sf)
#' library(future)
#' library(future.mirai)
#' sf::sf_use_s2(FALSE)
#' future::plan(future.mirai::mirai_multisession, workers = 2)
#'
#' ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
#' nccnty <- sf::st_read(ncpath, layer = "county")
#' ncelev <-
#'   system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
#' tdir <- tempdir(check = TRUE)
#' terra::writeRaster(ncelev, file.path(tdir, "test1.tif"), overwrite = TRUE)
#' terra::writeRaster(ncelev, file.path(tdir, "test2.tif"), overwrite = TRUE)
#' terra::writeRaster(ncelev, file.path(tdir, "test3.tif"), overwrite = TRUE)
#' testfiles <- list.files(tdir, pattern = "tif$", full.names = TRUE)
#'
#' res <- par_multirasters(
#'   filenames = testfiles,
#'   fun_dist = extract_at,
#'   x = ncelev,
#'   y = nccnty,
#'   id = "GEOID",
#'   func = "mean"
#' )
#' @importFrom future future
#' @importFrom terra rast
#' @importFrom mirai mirai call_mirai
#' @importFrom rlang inject !!!
#' @importFrom collapse rowbind
#' @importFrom cli cli_inform
#' @export
par_multirasters <-
  function(
    filenames,
    fun_dist,
    ...,
    .debug = FALSE
  ) {

    file_list <- filenames
    file_iter <- as.list(seq_along(file_list))
    args_input <- list(...)

    # get hints from the inputs
    crs_x <- .check_character(filenames[1])
    # peek_x <- try(.check_character(args_input$x), silent = TRUE)
    # peek_y <- try(.check_character(args_input$y), silent = TRUE)

    results <-
      future.apply::future_lapply(
        file_iter,
        function(i) {
          options(sf_use_s2 = FALSE)
          result <-
            tryCatch({
              if (!"id" %in% names(formals(fun_dist))) {
                args_input$id <- NULL
              }

              # interpret the function input x and y
              args_input$x <-
                .par_screen(
                  type = "raster",
                  input = filenames[i],
                  input_id = NULL,
                  out_class = "terra",
                  .window = NULL
                )
              args_input$y <-
                .par_screen(
                  type = "vector",
                  input = args_input$y,
                  input_id = NULL,
                  out_class = "terra",
                  .window = NULL
                )
              args_input$y <- reproject_std(args_input$y, attr(crs_x, "crs"))

              res <- rlang::inject(fun_dist(!!!args_input))
              cli::cli_alert_info(
                sprintf(
                  "Your input function at %s is dispatched.\n", filenames[i]
                )
              )
              res <- try(as.data.frame(res), silent = TRUE)
              res$base_raster <- filenames[i]
              return(res)
            }, error = function(e) {
              if (.debug) {
                data.frame(
                  base_raster = filenames[i],
                  error_message = paste(unlist(e), collapse = " ")
                )
              } else {
                return(NULL)
              }
            }
            )
          return(result)
        },
        future.seed = TRUE
      )
    args_input <- NULL

    results <-
      results[!vapply(results, is.null, logical(1))]
    results <- collapse::rowbind(results, fill = TRUE)
    return(results)

  }


#' Prescreen input data for parallelization
#'
#' This function takes input object and type character to ingest
#' the input object to return the object in the desired class.
#' @param type character(1). "raster" or "vector".
#' @param input object. Input object.
#' @param input_id character(1). Default is NULL. If NULL, the function
#'  will not check the object with an ID column.
#' @param out_class character(1). Default is NULL, but should be one of
#'   `c("sf", "terra")`. Default is "terra".
#' @param .window numeric(4)/SpatExtent/st_bbox object. Loading window.
#' @keywords internal
.par_screen <- function(
  type,
  input,
  input_id = NULL,
  out_class = "terra",
  .window = NULL
) {
  # type check
  if (inherits(type, "try-error")) {
    type <- datamod(input)
  }
  match.arg(type, c("vector", "raster"))

  if (type == "raster") {
    scr <- .check_raster(input = input, extent = .window)
  } else {
    scr <- .check_vector(
      input = input, input_id = input_id, extent = .window,
      out_class = out_class
    )
  }
  return(scr)

}


#' Map arguments to the desired names
#' @family Helper functions
#' @description This function maps the arguments of a target function
#' to the desired names. Users will use a named list `name_match` to
#' standardize the argument names, at least x and y, to the target function.
#' This function is particularly useful to parallelize functions for spatial
#' data outside `sf` and `terra` packages that do not have arguments
#' named x and/or y. `par_*` functions could detect such functions by
#' wrapping nonstandardized functions to parallelize the computation.
#' @param fun The target function to be called.
#' @param name_match A named list of arguments to be mapped.
#' @param ... Arguments to be passed to the target function.
#' @returns The result of the target function.
#' @examples
#' # Example target function
#' example_fun <- function(x, y, z = 1) {
#'   return(c(x = x, y = y, z = z))
#' }
#'
#' # Example usage of map_args_xy
#' result <- map_args_xy(fun = example_fun,
#'                       name_match = list(a = "x", b = "y"),
#'                       a = 10, b = 20, z = 5)
#' print(result)
#' @export
par_map_args <- function(fun, name_match = list(), ...) {
  # Capture the calling arguments, excluding 'fun' and 'name_match'
  args <- as.list(match.call(expand.dots = TRUE))[-c(1, 2, 3)]

  # Modify argument names based on 'name_match'
  names(args) <-
    ifelse(
      names(args) %in% names(name_match),
      name_match[names(args)],
      names(args)
    )

  # Call the target function 'fun' with modified arguments
  do.call(fun, args)
}
