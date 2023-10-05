# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand
testthat::test_that("What package does the input object belong?", {
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


testthat::test_that("What package does the input object belong?", {
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


testthat::test_that("Format is well converted", {
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
  testthat::expect_error(extent_to_polygon(mainland_vec_un, output_class = "sf"))
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
