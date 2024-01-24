# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title Process a given function in the entire or partial computational grids
#'
#' @description Currently only accepting \link[future]{multicore} setting
#'  (single node, single process, and multiple threads).
#'  For details of the terminology in \code{future} package,
#'  refer to \link[future]{plan}. This function assumes that
#'  users have one raster file and a sizable and spatially distributed
#'  target locations. Each thread will process ceiling(|Ng|/|Nt|) grids
#'  where |Ng| denotes the number of grids and |Nt| denotes
#'  the number of threads.
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
#'  an output of \code{get_computational_regions}
#' @param grid_target_id character(1) or numeric(2).
#'  Default is NULL. If NULL, all grid_ids are used.
#'  \code{"id_from:id_to"} format or
#'  \code{c(unique(grid_id)[id_from], unique(grid_id)[id_to])}
#' @param debug logical(1). Prints error messages
#' if there were any errors during the calculation.
#' @param fun_dist sf, terra or chopin functions.
#' @param ... Arguments passed to the argument \code{fun_dist}.
#' @returns a data.frame object with computation results.
#'  For entries of the results, consult the function used in
#'  \code{fun_dist} argument.
#' @author Insang Song \email{geoissong@@gmail.com}
#'
#' @examples
#' library(future)
#' plan(multicore, workers = 4)
#' # Does not run ...
#' # distribute_process_grid()
#' @import future
#' @importFrom future.apply future_lapply
#' @importFrom rlang inject
#' @importFrom rlang `!!!`
#' @export
distribute_process_grid <-
  function(
      grids,
      grid_target_id = NULL,
      debug = FALSE,
      fun_dist,
      ...) {
    if (is.character(grid_target_id) && !grepl(":", grid_target_id)) {
      stop("Character grid_target_id should be in a form of 'startid:endid'.\n")
    }
    if (is.numeric(grid_target_id)) {
      if (length(grid_target_id) != 2) {
        stop("Numeric grid_target_id should be in a form of c(startid, endid).\n")
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
        \(grid) {
          sf::sf_use_s2(FALSE)

          run_result <- tryCatch({
            args_input <- list(...)
            # args_fun <- formals(fun_dist)
            ## Strongly assuming that
            # the first is "at", the second is "from"
            args_input[[1]] <-
              args_input[[1]][grid, ]
            if (class(args_input[[2]])[1] == "SpatVector") {
              args_input[[2]] <-
                args_input[[2]][
                  grids$padded[grids$padded$CGRIDID == grid$CGRIDID, ], ]
            }
            if (!"id" %in% names(formals(fun_dist))) {
              args_input$id <- NULL
            }

            res <- rlang::inject(fun_dist(!!!args_input))
            cat(sprintf("Your input function was 
            successfully run at CGRIDID: %s\n",
              as.character(unlist(grid[["CGRIDID"]]))))

            if (!is.data.frame(res)) {
              res <- as.data.frame(res)
            }

            return(res)
          },
          error = function(e) {
            if (debug) print(e)
            fallback <- data.frame(ID = NA)
            if ("id" %in% names(formals(fun_dist))) {
              detected_id <- list(...)
              detected_id <- detected_id$id
            } else {
              detected_id <- "id"
            }
            colnames(fallback)[1] <- detected_id
            return(fallback)
          })

          return(run_result)
        },
        future.seed = TRUE,
        future.packages = c("terra", "sf", "dplyr", "chopin", "exactextractr"))
    results_distributed <- do.call(dplyr::bind_rows, results_distributed)

    return(results_distributed)
}


#' @title Process a given function using a hierarchy in input data
#'
#' @description "Hierarchy" refers to a system,
#'  which divides the entire study region into multiple subregions.
#'  It is oftentimes reflected in an area code system
#'  (e.g., FIPS for US Census geographies, HUC-4, -6, -8, etc.).
#'  Currently only accepting \link[future]{multicore} setting
#'  (single node, single process, and multiple threads).
#'  For details of the terminology in \code{future} package,
#'  refer to \link[future]{plan}.
#'  This function assumes that users have one raster file and
#'  a sizable and spatially distributed target locations.
#'  Each thread will process ceiling(|Ng|/|Nt|) grids where
#'  |Ng| denotes the number of grids and |Nt| denotes
#'  the number of threads. Please be advised that
#'  accessing the same file simultaneously with
#'  multiple processes may result in errors.
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
#' @returns a data.frame object with computation results.
#'  For entries of the results, consult the function used in
#'  \code{fun_dist} argument.
#' @author Insang Song \email{geoissong@@gmail.com}
#'
#' @examples
#' library(future)
#' plan(multicore, workers = 4)
#' # Does not run ...
#' # distribute_process_hierarchy()
#' @import future
#' @importFrom future.apply future_lapply
#' @importFrom rlang inject
#' @importFrom rlang `!!!`
#' @export
distribute_process_hierarchy <-
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
      future_lapply(
                    regions_list,
                    \(subregion) {
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

                                  res <-
                                    rlang::inject(fun_dist(!!!args_input))
                                  if (!is.data.frame(res)) {
                                    res <- as.data.frame(res)
                                  }
                                  return(res)
                                },
                                error =
                                function(e) {
                                  if (debug) print(e)
                                  fallback <- data.frame(ID = NA)
                                  if ("id" %in% names(formals(fun_dist))) {
                                    detected_id <- list(...)
                                    detected_id <- detected_id$id
                                  } else {
                                    detected_id <- "id"
                                  }
                                  colnames(fallback)[1] <- detected_id
                                  return(fallback)
                                })
                      return(run_result)
                    },
                    future.seed = TRUE,
                    future.packages = c("terra", "sf", "dplyr",
                                        "chopin", "future", "exactextractr"))
    results_distributed <- do.call(dplyr::bind_rows, results_distributed)

    return(results_distributed)
  }




#' @title Process a given function over multiple large rasters
#'
#' @description Large raster files usually exceed the memory capacity in size.
#'  Cropping a large raster into a small subset even consumes
#'  a lot of memory and adds processing time.
#'  This function leverages terra SpatRaster proxy
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
#' @returns a data.frame object with computation results.
#'  For entries of the results,
#'  consult the function used in \code{fun_dist} argument.
#' @author Insang Song \email{geoissong@@gmail.com}
#'
#' @examples
#' library(future)
#' plan(multicore, workers = 4)
#' # Does not run ...
#' # distribute_process_multirasters()
#' @import future
#' @import future.apply
#' @import doFuture
#' @export
distribute_process_multirasters <- function(
  filenames,
  debug = FALSE,
  fun_dist,
  ...) {
  # par_fun <- list(...)
  # detected_id <- grep("^id", names(par_fun), value = TRUE)
  # detected_id <- par_fun[[detected_id]]
  # if (is.null(detected_id)) {
  #   detected_id <- "ID"
  # }

  # if (any(sapply(filenames, \(x) !file.exists(x)))) {
  #   warning("One or many of files do not exist in provided file paths. Outputs would be invalid.\n")
  # }

  file_list <- split(filenames, filenames)
  results_distributed <-
    future_lapply(
      file_list,
      function(path) {
        run_result <-
          try({
            args_input <- list(...)
            vect_target_tr <- detect_class(args_input, "SpatVector")
            vect_target_sf <- detect_class(args_input, "sf")
            vect_target <- (vect_target_tr | vect_target_sf)
            vect_ext <- args_input[vect_target]
            vect_ext <- terra::ext(vect_ext[[1]])

            rast_target <- which(detect_class(args_input, "SpatRaster"))
            args_input[[rast_target]] <- rast_short(rasterpath = path, win = vect_ext)
            if (!"id" %in% names(formals(fun_dist))) args_input$id <- NULL

            res <- rlang::inject(fun_dist(!!!args_input))
            if (!is.data.frame(res)) res <- as.data.frame(res)
            res$base_raster <- path

            res
          }
          )
        if (inherits(run_result, "try-error")) {
          if (debug) {
            message(attr(run_result, "condition")$message)
          }
          fallback <- data.frame(ID = NA)
          if ("id" %in% names(formals(fun_dist))) {
            detected_id <- list(...)
            detected_id <- detected_id$id
          }
          colnames(fallback)[1] <- detected_id
          run_result <- fallback
        }
        return(run_result)
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



