#' Regular grid points in the mainland United States at 1km spatial resolution
#' @family Dataset
#' @format A data frame with 8,092,995 rows and three variables:
#' \describe{
#' \item{site_id}{Unique point identifier. Arbitrarily generated.}
#' \item{lon}{Longitude}
#' \item{lat}{Latitude}
#' }
#' @note Coordinates are in EPSG:5070 (Conus Albers Equal Area)
#' @source Mainland United States polygon was obtained from
#' the US Census Bureau.
#' @examples
#' data("prediction_grid", package = "chopin")
"prediction_grid"



#' Mildly clustered points in North Carolina, United States
#' @family Dataset
#' @format A data frame with 2,304 rows and two variables:
#' \describe{
#' \item{X}{X coordinate}
#' \item{Y}{Y coordinate}
#' }
#' @note Coordinates are in EPSG:5070 (Conus Albers Equal Area)
#' @source sf package data `nc`
#' @examples
#' data("ncpoints", package = "chopin")
"ncpoints"
