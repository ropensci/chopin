#' Parallelize spatial computation over the computational grids
#' @family Parallelization
#' @description
#' [future::multicore], [future::multisession], [future::cluster]
#' [future.mirai::mirai_multisession] in [future::plan] will parallelize
#' the work in each grid. For details of the terminology in `future` package,
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
#'  `exactextractr::exact_extract`, the raster (x) extent should be set
#'   with the padded grid.
#' @param .debug logical(1). Default is `FALSE`. Otherwise,
#'   if a unit computation fails, the error message and the `CGRIDID`
#'   value where the error occurred will be included in the output.
#' @returns a data.frame object with computation results.
#'  For entries of the results, consult the documentation of the function put
#'  in `fun_dist` argument.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' library(sf)
#' library(future)
#' library(future.mirai)
#' plan(mirai_multisession, workers = 2)
#' ncpath <- system.file("shape/nc.shp", package = "sf")
#' ncpoly <- sf::st_read(ncpath)
#' # sf object
#' # sf object
#' ncpnts <-
#'   sf::st_sample(ncpoly, 2000)
#' # file path
#' rrast <- terra::rast(nccnty, nrow = 1000, ncol = 2200)
#' ncr <- terra::rasterize(nc, rrast)
#' terra::values(rrast) <- rgamma(2.2e6, 4, 2)
#'
#' # Using raster path
#' rastpath <- file.path(tempdir(), "ncelev.tif")
#' terra::writeRaster(rrast, rastpath, overwrite = TRUE)
#'
# generate grids
#' nccompreg <-
#'   chopin::par_pad_grid(
#'     input = ncpnts,
#'     mode = "grid",
#'     nx = 4L,
#'     ny = 2L,
#'     padding = 5e3L
#'   )
#' res <-
#'   par_grid(
#'     grids = nccompreg,
#'     fun_dist = extract_at,
#'     x = rastpath,
#'     y = ncpnts,
#'     qsegs = 90L,
#'     radius = 5e3L,
#'     id = "pid"
#'   )
#' @seealso
#'  [`future::multisession`], [`future::multicore`], [`future::cluster`],
#'  [`future.mirai::mirai_multisession`], [`future::plan`], [`par_convert_f`]
#' @importFrom future.apply future_lapply
#' @importFrom rlang inject !!!
#' @importFrom collapse rowbind
#' @importFrom sf sf_use_s2
#' @importFrom cli cli_abort cli_alert_info
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
    funname <- as.character(substitute(fun_dist))
    # is the function extract_at?
    is_extract_at <- any(endsWith(funname, "extract_at"))
    funname <- funname[length(funname)]
    pkgname <- try(.check_package(funname), silent = TRUE)

    # parallel worker will take terra class objects
    # if chopin function is used
    class_vec <-
      if (pkgname == "chopin") {
        if (is_extract_at) {
          "sf"
        } else {
          "terra"
        }
      } else {
        pkgname
      }

    # clean additional arguments
    args_input <- list(...)
    if (funname == "chopin" && is.null(args_input$.standalone)) {
      args_input$.standalone <- FALSE
    }

    # Track spatraster file path
    args_input$x <- .check_par_spatraster(args_input$x)
    args_input$y <- .check_par_spatraster(args_input$y)
    # get hints from the inputs on data model
    peek_x <- try(.check_character(args_input$x), silent = TRUE)
    peek_y <- try(.check_character(args_input$y), silent = TRUE)
    if (inherits(peek_x, "try-error")) {
      crs_x <- terra::crs(args_input$x)
    } else {
      crs_x <- .check_character(args_input$x)
      crs_x <- attr(crs_x, "crs")
    }

    # class identity check
    .check_align_fxy(pkgname, args_input$x, args_input$y)

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

          grid_in <- reproject_std(grid_in, crs_x)
          gpad_in <- grids$padded[grids$padded$CGRIDID %in% grid_in$CGRIDID, ]

          args_input$x <-
            .par_screen(
              type = peek_x,
              input = args_input$x,
              input_id = NULL,
              out_class = class_vec,
              .window = if (pad_y) grid_in else gpad_in
            )

          args_input$y <-
            .par_screen(
              type = peek_y,
              input = args_input$y,
              input_id = NULL,
              out_class = class_vec,
              .window = if (pad_y) gpad_in else grid_in
            )

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


# nolint start
#' Parallelize spatial computation by hierarchy in input data
#' @family Parallelization
#' @description "Hierarchy" refers to a system,
#'  which divides the entire study region into multiple subregions.
#'  It is oftentimes reflected in an area code system
#'  (e.g., FIPS for US Census geographies and
#'  Nomenclature of Territorial Units for Statistics (NUTS), etc.).
#'  [`future::multisession`], [`future::multicore`], [`future::cluster`],
#'  [`future.mirai::mirai_multisession`] in [`future::plan`]
#'  will parallelize the work by splitting lower level features into
#'  several higher level feature group.
#'  For details of the terminology in `future` package,
#'  please refer to [`future::plan`] documentation.
#'  Each thread will process the number of lower level features
#'  in each higher level feature. Please be advised that
#'  accessing the same file simultaneously with
#'  multiple processes may result in errors.
#' @details 
#' In dynamic dots (`...`), `fun_dist` arguments should include
#'   x and y where sf/terra class objects or file paths are accepted.
#'   Hierarchy is interpreted by the `regions_id` argument first.
#'   `regions_id` is assumed to be a field name in the `x` or `y` argument
#'   object. It is expected that `regions` represents the higher level
#'   boundaries and `x` or `y` in `fun_dist` is the lower level boundaries.
#'   However, if that is not the case, with `trim` argument, the function
#'   will generate the higher level codes from `regions_id` by extracting
#'   left-t
#'   Whether `x` or `y` is searched is determined by `pad_y` value.
#'   `pad_y = TRUE` will make the function attempt to find `regions_id`
#'   in `x`, whereas `pad_y = FALSE` will look for `regions_id` at
#'   `y`. If the `regions_id` doesn't exist in `x` or `y`, the function
#'   will utilize spatial relationship (intersects) to filter the data.
#'   Note that dispatching computation by subregions based on the spatial
#'   relationship may lead to a slight discrepancy in the result. For
#'   example, if the higher and lower level features are not perfectly
#'   aligned, there may be some features that are not included or duplicated
#'   in the subregions. The function will alert the user if spatial relation-
#'   ship is used to filter the data.
#'
#' @note
#' Virtually any sf/terra functions that accept two arguments
#' can be put in `fun_dist`, but please be advised that
#' some spatial operations do not necessarily give the
#' exact result from what would have been done with single thread.
#' For example, distance calculated through this function may return the
#' lower value than actual because the computational region was reduced.
#' This would be the case especially where the target features
#' are spatially sparsely distributed.
#' @param regions `sf`/`SpatVector` object.
#'  Computational regions. Only polygons are accepted.
#' @param regions_id character(1). Name of unique ID field in `regions`.
#'  The regions will be split by the common level value.
#' @param length_left integer(1). Length of the first characters of
#'  the `regions_id` values. Default is NULL, which will not manipulate
#'  the `regions_id` values. If the number of characters is not
#'  consistent (for example, numerics), the function will alert the user.
#' @param fun_dist `sf`, `terra`, or `chopin` functions.
#'   This function should have `x` and `y` arguments.
#' @param pad numeric(1). Padding distance for each subregion defined
#'  by `regions_id` or trimmed `regions_id` values.
#'  in linear unit of coordinate system. Default is 0, which means
#'  each subregion is used as is. If the value is greater than 0,
#'  the subregion will be buffered by the value. The padding distance will
#'  be applied to `x` (`pad_y = FALSE`) or `y` (`pad_y = TRUE`) to filter
#'  the data.
#' @param pad_y logical(1). Whether to filter y with the padded grid.
#'  Should be TRUE when x is where the values are calculated.
#'  Default is `FALSE`. In the reverse case, like `terra::extent` or
#'  `exactextractr::exact_extract`, the raster (x) should be scoped
#'   with the padded grid.
#' @param ... Arguments passed to the argument `fun_dist`.
#' @param .debug logical(1). Default is `FALSE`
#'   If a unit computation fails, the error message and the `regions_id`
#'   value where the error occurred will be included in the output.
#' @returns a data.frame object with computation results.
#'  For entries of the results, consult the function used in
#'  \code{fun_dist} argument.
#' @seealso
#'  [`future::multisession`], [`future::multicore`], [`future::cluster`],
#'  [`future.mirai::mirai_multisession`], [`future::plan`], [`par_convert_f`]
#' @author Insang Song \email{geoissong@@gmail.com}
#' @examples
#' library(terra)
#' library(sf)
#' library(future)
#' library(future.mirai)
#' options(sf_use_s2 = FALSE)
#' future::plan(future.mirai::mirai_multisession, workers = 2)
#'
#' nccnty <- sf::st_read(
#'   system.file("shape/nc.shp", package = "sf")
#' )
#' nccnty <- sf::st_transform(nccnty, "EPSG:9311")
#'
#' nccntygrid <- sf::st_make_grid(nccnty, n = c(200, 100))
#' nccntygrid <- sf::st_as_sf(nccntygrid)
#' nccntygrid$GEOID <- sprintf("%05d", seq_len(nrow(nccntygrid)))
#' nccntygrid <- sf::st_intersection(nccntygrid, nccnty)
#'
#' rrast <- terra::rast(nccnty, nrow = 1000, ncol = 2200)
#' ncr <- terra::rasterize(nc, rrast)
#' terra::values(rrast) <- rgamma(2.2e6, 4, 2)
#'
#' # Using raster path
#' rastpath <- file.path(tempdir(), "ncelev.tif")
#' terra::writeRaster(rrast, rastpath, overwrite = TRUE)
#'
#' ncsamp <-
#'   sf::st_sample(
#'     nccnty,
#'     size = 1e4L
#'   )
#' # sfc to sf
#' ncsamp <- sf::st_as_sf(ncsamp)
#' # assign ID
#' ncsamp$kid <- sprintf("K-%05d", seq_len(nrow(ncsamp)))
#' res <-
#'   par_hierarchy(
#'     regions = nccnty,
#'     regions_id = "GEOID",
#'     fun_dist = extract_at,
#'     y = nccntygrid,
#'     x = rastpath,
#'     id = "GEOID",
#'     func = "mean"
#'   )
#' @importFrom future.apply future_lapply
#' @importFrom rlang inject !!!
#' @importFrom collapse rowbind
#' @importFrom sf sf_use_s2
#' @importFrom cli cli_abort cli_alert_info
#' @importFrom stats var
#' @export
par_hierarchy <-
  function(
    regions,
    regions_id = NULL,
    length_left = NULL,
    pad = 0,
    pad_y = FALSE,
    fun_dist,
    ...,
    .debug = FALSE
  ) {
    args_input <- list(...)

    # is the function sf?
    funname <- as.character(substitute(fun_dist))
    # is the function extract_at?
    is_extract_at <- any(endsWith(funname, "extract_at"))
    funname <- funname[length(funname)]
    pkgname <- try(.check_package(funname), silent = TRUE)

    # parallel worker will take terra class objects
    # if chopin function is used
    class_vec <-
      if (pkgname == "chopin") {
        if (is_extract_at) {
          "sf"
        } else {
          "terra"
        }
      } else {
        pkgname
      }

    # Track spatraster file path
    args_input$x <- .check_par_spatraster(args_input$x)
    args_input$y <- .check_par_spatraster(args_input$y)
    # get hints from the inputs
    peek_x <- try(.check_character(args_input$x), silent = TRUE)
    peek_y <- try(.check_character(args_input$y), silent = TRUE)
    if (inherits(peek_x, "try-error")) {
      crs_x <- terra::crs(args_input$x)
    } else {
      crs_x <- .check_character(args_input$x)
      crs_x <- attr(crs_x, "crs")
    }

    if (length(regions_id) != 1) {
      cli::cli_abort("The length of regions_id is not valid.")
    }

    # class identity check
    .check_align_fxy(pkgname, args_input$x, args_input$y)

    # Region ID cleaning to get unique high-level IDs
    # what if regions refers to a path string?
    # vectorize the regions_id
    vec_regions_id <- unlist(regions[[regions_id]], use.names = FALSE)

    if (is.null(length_left)) {
      cli::cli_alert_info(
        sprintf(
          "%s is used to stratify the process.",
          regions_id
        )
      )
      regions_idn <- unique(vec_regions_id)
    } else {
      cli::cli_alert_info(
        sprintf(
          paste0(
            "Substring is extracted from the left for level definition. ",
            "First %d characters are used to stratify the process."
          ),
          length_left
        )
      )
      check_nchar <- nchar(vec_regions_id)
      if (var(check_nchar) != 0) {
        cli::cli_alert_warning(
          paste0(
            "The regions_id values are in different lengths. ",
            "substr may not work properly."
          )
        )
      }
      regions_idn <-
        unique(substr(vec_regions_id, 1, length_left))
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
                # subregion header string retrieval
                region_i <- regions_list[[i]]
                regions_ids <- vec_regions_id
                
                # subregion object
                subregion_in <-
                  regions[startsWith(regions_ids, region_i), ]
                # padding if necessary
                # can be expanded to other classes in common packages
                # but it elongates the function and lint failure
                if (inherits(subregion_in, "sf")) {
                  subregion_inb <- sf::st_buffer(subregion_in, pad)
                } else {
                  subregion_inb <- terra::buffer(subregion_in, pad)
                }

                # interpret the function input x and y
                args_input$x <-
                  .par_screen(
                    type = peek_x,
                    input = args_input$x,
                    input_id = NULL,
                    out_class = class_vec,
                    .window = NULL
                  )
                args_input$y <-
                  .par_screen(
                    type = peek_y,
                    input = args_input$y,
                    input_id = NULL,
                    out_class = class_vec,
                    .window = NULL
                  )

                # Here we use twofold approach to filter the data
                # 1. If pad_y is TRUE, y is filtered with:
                #   1a. the string prefix if the same field `regions_id`
                #       exists in y
                #   1b. Otherwise, it uses the padded subregion
                # I believe there would be a succinct and sophisticated
                # way, but this is the most straightforward way.
                # 2. If pad_y is FALSE, x is filtered with:
                #   2a and 2b: ditto as 1a and 1b but x replaces y
                if (pad_y) {
                  # aligning the CRS
                  args_input$y <-
                    reproject_std(args_input$y, crs_x)
                  # if the same regions_id present in the x
                  if (regions_id %in% names(args_input$x)) {
                    args_input$x <-
                      args_input$x[
                        startsWith(
                          unlist(args_input$x[[regions_id]], use.names = FALSE),
                          region_i
                        ),
                      ]
                  } else {
                    cli::cli_alert_info(
                      paste0(
                        "The regions_id is not found in the x object.",
                        " Spatial relationship is used to filter x."
                      )
                    )
                    args_input$x <- .intersect(args_input$x, subregion_in)
                  }
                  args_input$y <- .intersect(args_input$y, subregion_inb)
                } else {
                  args_input$y <-
                    reproject_std(args_input$y, crs_x)
                  if (regions_id %in% names(args_input$y)) {
                    args_input$y <-
                      args_input$y[
                        startsWith(
                          unlist(args_input$y[[regions_id]], use.names = FALSE),
                          region_i
                        ),
                      ]
                  } else {
                    cli::cli_alert_info(
                      paste0(
                        "The regions_id is not found in the x object.",
                        " Spatial relationship is used to filter y."
                      )
                    )
                    args_input$y <- .intersect(args_input$y, subregion_in)
                  }
                  args_input$x <- .intersect(args_input$x, subregion_inb)
                }

                # Main dispatch
                res <- rlang::inject(fun_dist(!!!args_input))
                res <- try(as.data.frame(res), silent = TRUE)
                cli::cli_alert_info(
                  sprintf("Your input function at %s is dispatched.\n",
                          region_i)
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
# nolint end


#' @title Parallelize spatial computation over multiple raster files
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
#' @param fun_dist terra or chopin functions that accept `SpatRaster`
#'   object in an argument. In particular, `x` and `y` arguments
#'   should be present and `x` should be a `SpatRaster`.
#' @param ... Arguments passed to the argument `fun_dist`.
#' @param .debug logical(1). Default is `FALSE`. If `TRUE` and
#'   a unit computation fails, the error message and the file path
#'   where the error occurred will be included in the output.
#' @returns a data.frame object with computation results.
#'  For entries of the results,
#'  consult the function used in `fun_dist` argument.
#' @author Insang Song \email{geoissong@@gmail.com}
#' @seealso
#'  [`future::multisession`], [`future::multicore`], [`future::cluster`],
#'  [`future.mirai::mirai_multisession`], [`future::plan`], [`par_convert_f`]
#'
#' @examples
#' library(terra)
#' library(sf)
#' library(future)
#' library(future.mirai)
#' sf::sf_use_s2(FALSE)
#' future::plan(future.mirai::mirai_multisession, workers = 2)
#'
#' nccnty <- sf::st_read(
#'   system.file("shape/nc.shp", package = "sf")
#' )
#' nccnty <- sf::st_transform(nccnty, "EPSG:9311")
#'
#' nccntygrid <- sf::st_make_grid(nccnty, n = c(200, 100))
#' nccntygrid <- sf::st_as_sf(nccntygrid)
#' nccntygrid$GEOID <- sprintf("%05d", seq_len(nrow(nccntygrid)))
#' nccntygrid <- sf::st_intersection(nccntygrid, nccnty)
#'
#' rrast <- terra::rast(nc, nrow = 1000, ncol = 2200)
#' ncr <- terra::rasterize(nc, rrast)
#' terra::values(rrast) <- rgamma(2.2e6, 4, 2)
#'
#' tdir <- tempdir(check = TRUE)
#' testpath1 <- file.path(tdir, "test1.tif")
#' testpath2 <- file.path(tdir, "test2.tif")
#' terra::writeRaster(rrast, testpath1, overwrite = TRUE)
#' terra::writeRaster(rrast, testpath2, overwrite = TRUE)
#' testfiles <- list.files(tdir, pattern = "tif$", full.names = TRUE)
#'
#' res <- par_multirasters(
#'   filenames = testfiles,
#'   fun_dist = extract_at,
#'   x = testpath1,
#'   y = nccnty,
#'   id = "GEOID",
#'   func = "mean"
#' )
#' @importFrom future.apply future_lapply
#' @importFrom terra rast
#' @importFrom rlang inject !!!
#' @importFrom collapse rowbind
#' @importFrom cli cli_inform cli_alert_info
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

    # is the function sf?
    funname <- as.character(substitute(fun_dist))
    # is the function extract_at?
    is_extract_at <- any(endsWith(funname, "extract_at"))
    funname <- funname[length(funname)]
    pkgname <- try(.check_package(funname), silent = TRUE)

    # parallel worker will take terra class objects
    # if chopin function is used
    class_vec <-
      if (pkgname == "chopin") {
        if (is_extract_at) {
          "sf"
        } else {
          "terra"
        }
      } else {
        pkgname
      }

    # Unlike other par_* functions, raster paths are not
    # tracked by the function since the raster file paths
    # are required to be passed as an argument to each parallel worker.
    # y class identification
    peek_y <- try(.check_character(args_input$y), silent = TRUE)

    # get hints from the inputs
    crs_x <- .check_character(filenames[1])

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
                  out_class = class_vec,
                  .window = NULL
                )
              args_input$y <-
                .par_screen(
                  type = peek_y,
                  input = args_input$y,
                  input_id = NULL,
                  out_class = class_vec,
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

    results <- results[!vapply(results, is.null, logical(1))]
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
  # if (inherits(type, "try-error")) {
  #   type <- datamod(input)
  # }
  match.arg(type, c("vector", "raster"))

  if (type == "raster") {
    scr <-
      .check_raster(
        input = input,
        extent = .window
      )
  } else {
    scr <-
      .check_vector(
        input = input,
        input_id = input_id,
        extent = .window,
        out_class = out_class
      )
  }
  return(scr)

}


#' Map arguments to the desired names
#' @description This function maps the arguments of a target function
#' to the desired names. Users will use a named list `name_match` to
#' standardize the argument names, at least x and y, to the target function.
#' This function is particularly useful to parallelize functions for spatial
#' data outside `sf` and `terra` packages that do not have arguments
#' named x and/or y. `par_*` functions could detect such functions by
#' wrapping nonstandardized functions to parallelize the computation.
#' @param fun A function to map arguments.
#' @param arg_map named character vector.
#'   `c(x = "a", y = "i")` will map `a` and `i` in `fun` to
#'   `x` and `y`, respectively.
#' @note `arg_map` should be defined carefully according to the characteristics
#'   of `fun`. After mapping `x` and `y`, the resultant function will fail
#'   if there remain arguments without default. It is recommended to map all
#'   the arguments in `fun` to avoid any side effects.
#' @returns Function with arguments mapped.
#' @examples
#' cov_map <- arg_mapping <- c(x = "a", y = "b", z = "c", w = "d")
#' # Example original function
#' f1 <- function(a, b, c, d) {
#'   return(a + b + c + d)
#' }
#' # Mapping of new argument names to original argument names
#' arg_mapping <- c(x = "a", y = "b", z = "c", w = "d")
#' f2 <- par_convert_f(f1, arg_mapping)
#'
#' # demonstrate f2 with the mapped arguments
#' f2(x = 1, y = 2, z = -1, w = 10)
#' @export
par_convert_f <- function(fun, arg_map) {

  # Create a new function with the mapped arguments
  new_fun <- function(...) {
    # Capture the arguments passed to the new function
    args_in <- list(...)

    # Initialize an empty list for mapped arguments
    mapped_args <- list()

    # Loop through each argument in args_in
    for (arg_name in names(args_in)) {
      if (arg_name %in% names(arg_map)) {
        # If the argument name is in arg_map, map it
        mapped_args[[arg_map[[arg_name]]]] <- args_in[[arg_name]]
      } else {
        # Otherwise, keep the original argument name
        mapped_args[[arg_name]] <- args_in[[arg_name]]
      }
    }

    # Call the original function with the mapped arguments
    do.call(fun, mapped_args)
  }
  return(new_fun)
}
