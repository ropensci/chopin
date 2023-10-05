# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

#' @title Create integer indices for grid
#' @description Returns a tibble object that includes x- and y- index by using two inputs ncutsx and ncutsy, which are x- and y-directional splits, respectively.
#' @author Insang Song
#' @param points_in sf object.
#' @param ncutsx integer(1). The number of splits along x-axis.
#' @param ncutsy integer(1). The number of splits along y-axis.
#' @export
sp_indexing <- function(points_in, ncutsx, ncutsy) {
  # pts <- data.table(pnts)
  points_in <- points_in |>
    dplyr::mutate(or_id = seq(1, dim(points_in)[1]))

  range_x <- range(points_in$x)
  limits_x <- (range_x[1] + seq(0, ncutsx) * (range_x[2] - range_x[1]) / ncutsx)
  range_y <- range(points_in$y)
  limits_y <- (range_y[1] + seq(0, ncutsy) * (range_y[2] - range_y[1]) / ncutsy)

  points_in_cut <- points_in |>
    dplyr::mutate(
      xcut = as.integer(cut(x, ncutsx, labels = seq(1, ncutsx))),
      ycut = as.integer(cut(y, ncutsy, labels = seq(1, ncutsy)))
    )

  return(points_in_cut)
}
