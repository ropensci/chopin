# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand  
testthat::test_that("What package does the input object belong?",
{
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))
  bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
  bcsd_stars <- stars::read_stars(bcsd_path)

  packbound_stars <- check_packbound(bcsd_stars)
  sprast_bcsd <- terra::rast(bcsd_path)
  packbound_terra <- check_packbound(sprast_bcsd)

  testthat::expect_equal(packbound_stars, "sf")
  testthat::expect_equal(packbound_terra, "terra")
})


testthat::test_that("What package does the input object belong?",
{
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))
  bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
  bcsd_stars <- stars::read_stars(bcsd_path)

  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)

  datatype_stars <- check_datatype(bcsd_stars)
  datatype_sf <- check_datatype(nc)

  testthat::expect_equal(datatype_stars, "raster")
  testthat::expect_equal(datatype_sf, "vector")
})


testthat::test_that("Format is well converted",
{
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  # starts from sf/stars
  bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
  bcsd_stars <- stars::read_stars(bcsd_path)
  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)

  stars_bcsd_tr <- switch_packbound(bcsd_stars)
  sf_nc_tr <- switch_packbound(nc)

  testthat::expect_equal(check_packbound(stars_bcsd_tr), "terra")
  testthat::expect_equal(check_packbound(sf_nc_tr), "terra")

  stars_bcsd_trb <- switch_packbound(stars_bcsd_tr)
  sf_nc_trb <- switch_packbound(sf_nc_tr)

  testthat::expect_equal(check_packbound(stars_bcsd_trb), "sf")
  testthat::expect_equal(check_packbound(sf_nc_trb), "sf")

})



testthat::test_that("Clip extent is set properly", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncpath <- system.file("shape/nc.shp", package = "sf")
  suppressWarnings(nc <- sf::read_sf(ncpath) |>
    sf::st_transform("EPSG:5070") |>
    sf::st_centroid())

  radius <- 1e4L

  (nc_ext_sf <- set_clip_extent(nc, radius))

  nct <- terra::vect(nc)
  (nc_ext_terra <- set_clip_extent(nct, radius))

  (proper_xmin <- sf::st_bbox(nc)[1] - (1.1 * radius))

  testthat::expect_s3_class(nc_ext_sf, "sfc")
  testthat::expect_s4_class(nc_ext_terra, "SpatExtent")

  nc_ext_sf_1 <- sf::st_bbox(nc_ext_sf)[1]
  nc_ext_terra_1 <- nc_ext_terra[1]

  testthat::expect_equal(nc_ext_sf_1, proper_xmin)
  testthat::expect_equal(nc_ext_terra_1, proper_xmin)

})


testthat::test_that("Raster is read properly with a window.", {
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))
  bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")

  ext_numeric <- c(-84, -82, 34, 36) # unnamed
  testthat::expect_error(rast_short(bcsd_path, ext_numeric[1:3]))
  testthat::expect_error(rast_short(bcsd_path, ext_numeric))

  names(ext_numeric) <- c("xmin", "xmax", "ymin", "ymax")
  rastshort_num <- rast_short(bcsd_path, ext_numeric)
  testthat::expect_s4_class(rastshort_num, "SpatRaster")

  ext_terra <- terra::ext(ext_numeric)
  rastshort_terra <- rast_short(bcsd_path, ext_terra)
  testthat::expect_s4_class(rastshort_terra, "SpatRaster")

})



testthat::test_that("input extent is converted to a polygon", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  mainland_vec <- c(xmin = -128, xmax = -62, ymin = 25, ymax = 52)
  mainland_box <- extent_to_polygon(mainland_vec, output_class = "sf")
  mainland_box_t <- extent_to_polygon(mainland_vec, output_class = "terra")
  mainland_vec_un <- unname(mainland_vec)
  
  testthat::expect_s3_class(mainland_box, "sf")
  # terra Spat* objects are s4 class...
  testthat::expect_s4_class(mainland_box_t, "SpatVector")
  testthat::expect_error(
    extent_to_polygon(mainland_vec_un, output_class = "sf")
  )
})


testthat::test_that("check_crs is working as expected", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))
  ncpath <- system.file("shape/nc.shp", package = "sf")
  nc <- sf::read_sf(ncpath)
  nct <- terra::vect(nc)
  crs_checked1 <- check_crs(nc)
  dummy <- character(0)
  crs_checked2 <- check_crs(nct)

  testthat::expect_equal(crs_checked1, sf::st_crs(nc))
  testthat::expect_equal(crs_checked2, terra::crs(nct))
  testthat::expect_error(check_crs(dummy))

})


testthat::test_that("nc data is within the mainland US", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))
  ncpath <- system.file("shape/nc.shp", package = "sf")
  nc <- sf::read_sf(ncpath)
  nc <- sf::st_transform(nc, "EPSG:4326")
  mainland_vec <- c(xmin = -128, xmax = -62, ymin = 25, ymax = 52)
  mainland_box <- extent_to_polygon(mainland_vec, output_class = "sf")
  within_res <- check_within_reference(nc, mainland_box)
  testthat::expect_equal(within_res, TRUE)
})


testthat::test_that("Processes are properly spawned and compute", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncpath <- system.file("shape/nc.shp", package = "sf")
  ncpoly <- terra::vect(ncpath) |>
    terra::project("EPSG:5070")
  ncpnts <- readRDS("../testdata/nc_random_point.rds")
  ncpnts <- terra::vect(ncpnts)
  ncelev <- terra::rast("../testdata/nc_srtm15_otm.tif")


  nccompreg <- scomps::get_computational_regions(input = ncpoly, mode = 'grid', nx=6L, ny=4L, padding=3e4L)
  
  
  res <- scomps::distribute_process(grids = nccompreg, grid_target_id = NULL,#grid_id = "CGRIDID",
    fun = scomps::extract_with_buffer, points = terra::vect(ncpnts), qsegs = 90L, surf = ncelev, radius = 5e3L, id = "pid")

  testthat::expect_equal(!any(is.null(res)), TRUE)
})



