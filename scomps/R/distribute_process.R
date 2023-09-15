# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title Process a given function in the entire or partial computational grids (under construction)
#' 
#' @description Should 
#' @param grids sf/SpatVector object. Computational grids.
#' @param grid_id character(1) or numeric(2). Default is NULL. If NULL, all grid_ids are used. "id_from:id_to" format or c(unique(grid_id)[id_from], unique(grid_id)[id_to])
#' @param fun function supported in scomps. 
#' @param ... Arguments passed to fun.
#' @return a data.frame object with mean value
#' @author Insang Song \email{geoissong@@gmail.com}
#' 
#' @export
distribute_process <- function(grids, grid_id = NULL, fun, ...) {
    require(future.apply)
    
    # subset using grids and grid_id
    if (!is.null(grid_id)) {
        if (is.character(grid_id)) {
            grid_id_parsed = strsplit(grid_id, ":", fixed = TRUE)[[1]]
            grid_ids = c(which(unique(grids[["CGRIDID"]]) == grid_id_parsed[1]),
                         which(unique(grids[["CGRIDID"]]) == grid_id_parsed[2]))
        }
        if (is.numeric(grid_id)) {
            grid_ids = unique(grids[["CGRIDID"]])[grid_id]
        }
    }
    grids_target = grids[grid_ids,]
    grids_target_list = split(grids_target, grids_target[["CGRIDID"]])

    results_distributed = future.apply::future_lapply(
        \(x, ...) {
            fun(...)
        }, grids_target_list, points_cutslist, SIMPLIFY = FALSE,
        future.seed = TRUE)
    results_distributed = do.call(rbind, results_distributed)
    return(results_distributed)
}

