# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title Process a given function in the entire or partial computational grids
#' 
#' @description Currently only accepting \link[future]{multicore} setting (single node, single process, and multiple threads). For details of the terminology in \code{future} package, refer to \link[future]{plan}. This function assumes that users have one raster file and a sizable and spatially distributed target locations. Each thread will process ceiling(|Ng|/|Nt|) grids where |Ng| denotes the number of grids and |Nt| denotes the number of threads.
#' @param grids sf/SpatVector object. Computational grids. It takes a strict assumption that the grid input is an output of \code{get_computational_regions}
#' @param grid_target_id character(1) or numeric(2). Default is NULL. If NULL, all grid_ids are used. \code{"id_from:id_to"} format or \code{c(unique(grid_id)[id_from], unique(grid_id)[id_to])}
#' @param fun function supported in scomps.
#' @param ... Arguments passed to the argument \code{fun}.
#' @return a data.frame object with computation results. For entries of the results, consult the function used in \code{fun} argument.
#' @author Insang Song \email{geoissong@@gmail.com}
#' 
#' @examples 
#' library(future)
#' plan(multicore, workers = 4)
#' # Does not run ...
#' # distribute_process()
#' @import future
#' @import progressr
#' @export
distribute_process <- function(
  grids, 
  grid_target_id = NULL,
  fun,
  ...) {
  if (is.character(grid_target_id) && !grepl(":", grid_target_id)) {
    stop("Character grid_target_id should be in a form of 'startid:endid'.\n")
  }
  if (is.numeric(grid_target_id) && length(grid_target_id) != 2) {
    stop("Numeric grid_target_id should be in a form of c(startid, endid).\n")
  }
  # subset using grids and grid_id
  if (is.null(grid_target_id)) {
    grid_target_ids <- unlist(grids[["CGRIDID"]])
  }
  if (is.character(grid_target_id)) {
    grid_id_parsed <- strsplit(grid_target_id, ":", fixed = TRUE)[[1]]
    grid_target_ids <- c(which(unique(grids[["CGRIDID"]]) == grid_id_parsed[1]),
                    which(unique(grids[["CGRIDID"]]) == grid_id_parsed[2]))
  }
  if (is.numeric(grid_target_id)) {
    grid_target_ids <- unique(grids[["CGRIDID"]])[grid_target_id]
  }
  par_fun <- list(...)

  progressr::handlers(global = TRUE)
  grids_target <- grids[grid_target_ids %in% unlist(grids[["CGRIDID"]]),]
  grids_target_list <- split(grids_target, unlist(grids_target[["CGRIDID"]]))

  pgrs <- progressr::progressor(along = nrow(grids_target))

  results_distributed <- future.apply::future_lapply(
    grids_target_list,
    \(x) {
      sf::sf_use_s2(FALSE)
      
      run_result <- tryCatch({
        res <- fun(..., grid_ref = x)
        cat(sprintf("Your input function %s was successfully run at CGRIDID: %s\n",
          paste0(quote(fun)), as.character(unlist(x[["CGRIDID"]]))))
        pgrs(print(""))
        return(res)
      },
      error = function(e) return(data.frame(ID = NA)))
      pgrs(print(""))
      return(run_result)
    },
    future.seed = TRUE,
    future.packages = c("terra", "sf", "dplyr", "scomps", "future"))
  results_distributed <- do.call(dplyr::bind_rows, results_distributed)
  results_distributed <- results_distributed[!is.na(results_distributed[["ID"]]),]

  # post-processing
  detected_id <- grep("^id", names(par_fun), value = TRUE)
  detected_point <- grep("^points", names(par_fun), value = TRUE)
  names(results_distributed)[1] <- par_fun[[detected_id]]
  results_distributed[[detected_id]] <- 
    get(par_fun[[detected_point]])[[par_fun[[detected_id]]]][unlist(results_distributed[[detected_id]])]

  return(results_distributed)
  
  return(results_distributed)
}


#' @title Process a given function using a hierarchy in input data
#' 
#' @description "Hierarchy" refers to a system, which divides the entire study region into multiple subregions. It is oftentimes reflected in an area code system (e.g., FIPS for US Census geographies, HUC-4, -6, -8, etc.). Currently only accepting \link[future]{multicore} setting (single node, single process, and multiple threads). For details of the terminology in \code{future} package, refer to \link[future]{plan}. This function assumes that users have one raster file and a sizable and spatially distributed target locations. Each thread will process ceiling(|Ng|/|Nt|) grids where |Ng| denotes the number of grids and |Nt| denotes the number of threads. Please be advised that accessing the same file simultaneously with multiple processes may result in errors.
#' @param regions sf/SpatVector object. Computational regions. Only polygons are accepted.
#' @param split_level character(nrow(regions)) or character(1). The regions will be split by the common level value. The level should be higher than the original data level. A field name with the higher level information is also accepted.
#' @param fun function supported in scomps.
#' @param ... Arguments passed to the argument \code{fun}.
#' @return a data.frame object with computation results. For entries of the results, consult the function used in \code{fun} argument.
#' @author Insang Song \email{geoissong@@gmail.com}
#' 
#' @examples 
#' library(future)
#' plan(multicore, workers = 4)
#' # Does not run ...
#' # distribute_process()
#' @import future
#' @import progressr
#' @export
distribute_process_hierarchy <- function(
  regions, 
  split_level = NULL,
  fun,
  ...) {
  progressr::handlers(global = TRUE)
  par_fun <- list(...)

  if (!any(length(split_level) == 1, length(split_level) == nrow(regions))) {
    stop("The length of split_level is not valid.")
  }
  split_level <- ifelse(length(split_level) == nrow(regions),
    split_level,
    unlist(regions[[split_level]]))

  pgrs <- progressr::progressor(along = seq_len(split_level))
  regions_list <- split(regions, split_level)

  results_distributed <- future.apply::future_lapply(
    regions_list,
    \(x) {
      sf::sf_use_s2(FALSE)
      
      run_result <- tryCatch({
        res <- fun(..., grid_ref = x)
        pgrs()
        return(res)
      },
      error = function(e) return(data.frame(ID = NA)))
      pgrs(print(""))
      return(run_result)
    },
    future.seed = TRUE,
    future.packages = c("terra", "sf", "dplyr", "scomps", "future"))
  results_distributed <- do.call(dplyr::bind_rows, results_distributed)
  results_distributed <- results_distributed[!is.na(results_distributed[["ID"]]),]
  
  # post-processing
  detected_id <- grep("^id", names(par_fun), value = TRUE)
  detected_point <- grep("^points", names(par_fun), value = TRUE)
  names(results_distributed)[1] <- par_fun[[detected_id]]
  results_distributed[[detected_id]] <- 
    get(par_fun[[detected_point]])[[par_fun[[detected_id]]]][unlist(results_distributed[[detected_id]])]

  return(results_distributed)
}

