# Unexported function examples (copied)

## check.R ####
#' @examples
#' \donttest{
#' library(sf)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' nc_path <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc_sf <- sf::st_read(nc_path)
#' dep_check(nc_sf)
#' nc_vect <- terra::vect(nc_sf)
#' dep_check(nc_vect)
#' }


#' @examples
#' \donttest{
#' library(sf)
#' library(stars)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' ## generate a random raster
#' ras_rand <- terra::rast(nrow = 30, ncol = 30)
#' terra::values(ras_rand) <- runif(900)
#' stars_rand <- dep_switch(ras_rand)
#' stars_rand
#' inherits(sf_rand, "stars") # TRUE
#' # should return stars object
#'
#' vec_rand <- terra::spatSample(ras_rand, size = 10L, as.points = TRUE)
#' sf_rand <- dep_switch(vec_rand)
#' inherits(sf_rand, "sf") # TRUE
#' sf_rand
#' # should return sf object
#' }


#' @examples
#' \donttest{
#' library(sf)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' nc_path <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc_sf <- sf::st_read(nc_path)
#' datamod(nc_sf)
#'
#' ra_path <- system.file("ex/elev.tif", package = "terra")
#' ra <- terra::rast(ra_path)
#' datamod(ra)
#' }


#' @examples
#' \donttest{
#' library(sf)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' base_crs <- "EPSG:5070"
#' nc_path <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc_sf <- sf::st_read(nc_path)
#' reproject_std(nc_sf, base_crs)
#'
#' nc_vect <- terra::vect(nc_sf)
#' reproject_std(nc_vect, base_crs)
#' }



#' @examples
#' \donttest{
#' library(terra)
#' library(sf)
#' options(sf_use_s2 = FALSE)
#'
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' elev <- system.file("ex/elev.tif", package = "terra")
#' nc <- terra::vect(ncpath)
#' elev <- terra::rast(elev)
#' reproject_to_raster(nc, elev)
#' }



#' @examples
#' \donttest{
#' # Check a SpatVector object
#' ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(ncpath)
#' extent <- c(-80, -77, 35, 36)
#' .check_vector(input = nc, extent = extent, input_id = "FIPS")
#'
#' # Check a sf object
#' ncsf <- sf::st_read(ncpath)
#' .check_vector(input = ncsf, extent = extent, input_id = "FIPS")
#'
#' # Check a character object
#' .check_vector(
#'   input = ncpath,
#'   extent = extent,
#'   out_class = "terra",
#'   input_id = "FIPS"
#' )
#' }



#' @examples
#' \donttest{
#' .check_raster(system.file("extdata/nc_srtm15_otm.tif", package = "chopin"))
#' }


## gridding.R ####
#' @examples
#' \donttest{
#' library(sf)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' nc_path <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc <- terra::vect(nc_path)
#' nc_rp <- terra::spatSample(nc, 1000)
#' nc_gr <- par_make_grid(nc_rp, 10L, 6L)
#'
#' plot(nc_rp)
#' plot(nc_gr, add = TRUE)
#' }


#' @examples
#' \donttest{
#' library(terra)
#' library(anticlust)
#' data(ncpoints, package = "chopin")
#'
#' ncp <- terra::vect(
#'   ncpoints, geom = c("X", "Y"),
#'   keepgeom = FALSE, crs = "EPSG:5070"
#' )
#'
#' # 2,304 points / 12 = 192 points per cluster
#' ncpbal <- par_make_balanced(ncp, 12)
#' ncpbal
#' }


#' @examples
#' \donttest{
#' par_def_q(5L)
#' }


#' @examples
#' \donttest{
#' library(terra)
#'
#' random_points <-
#'   data.frame(x = runif(1000, 0, 100), y = runif(1000, 0, 100))
#' quantiles <- seq(0, 1, length.out = 5L)
#' qpoly <- par_cut_coords(random_points$x, random_points$y, quantiles)
#' clustered_points <-
#'   data.frame(x = rgamma(1000, 1, 1), y = rgamma(1000, 4, 1))
#'
#' qpoly_c <- par_cut_coords(clustered_points$x, clustered_points$y, quantiles)
#'
#' par(mfcol = c(1, 2))
#' plot(qpoly)
#' plot(qpoly_c)
#' par(mfcol = c(1, 1))
#'
#' cvect <- terra::vect(clustered_points, geom = c("x", "y"))
#' plot(cvect)
#' plot(qpoly_c, add = TRUE, col = "transparent", border = "red")
#'
#' qcv <- intersect(cvect, qpoly_c)
#' table(qcv$CGRIDID)
#' sum(table(qcv$CGRIDID)) # should be 1000
#' }


## preprocessing.R ####
#' @examples
#' \donttest{
#' library(sf)
#' library(terra)
#' options(sf_use_s2 = FALSE)
#'
#' nc_path <- system.file("gpkg/nc.gpkg", package = "sf")
#' nc_sf <- sf::st_read(nc_path)
#' nc_sf <- sf::st_transform(nc_sf, "EPSG:5070")
#' get_clip_ext(nc_sf, 2.5e4)
#' nc_vect <- terra::vect(nc_sf)
#' get_clip_ext(nc_vect, 2.5e4)
#' }

