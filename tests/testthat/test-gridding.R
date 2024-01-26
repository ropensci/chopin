# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

testthat::test_that("Grid split is well done.", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  # starts from sf/stars
  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")

  testthat::expect_no_error(
    get_computational_regions(nc, mode = "grid", padding = 3e4L)
  )
  ncgrid <- get_computational_regions(nc, mode = "grid", padding = 3e4L)
  testthat::expect_s3_class(ncgrid$original, "sf")

  nctr <- terra::vect(nc)
  testthat::expect_no_error(
    get_computational_regions(nctr, mode = "grid", padding = 3e4L)
  )
  ncgridtr <- get_computational_regions(nctr, mode = "grid", padding = 3e4L)
  testthat::expect_s4_class(ncgridtr$original, "SpatVector")

})


testthat::test_that("Grid merge is well done.", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("igraph")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))
  withr::local_seed(20231121)

  nc <- system.file("shape/nc.shp", package = "sf")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")
  nctr <- terra::vect(nc)
  ncp <- readRDS(testthat::test_path("..", "testdata", "nc_random_point.rds"))
  ncp <- sf::st_transform(ncp, "EPSG:5070")
  ncrp <- sf::st_as_sf(sf::st_sample(nc, 1000L))

  gridded <-
    get_computational_regions(ncrp,
                              mode = "grid",
                              nx = 8L, ny = 5L,
                              padding = 1e4L)
  # suppress warnings for "all sub-geometries for which ..."
  testthat::expect_warning(grid_merge(ncrp, gridded$original, 25L))

  ncptr <- terra::vect(ncrp)
  griddedtr <-
    get_computational_regions(ncptr,
                              mode = "grid",
                              nx = 8L, ny = 5L,
                              padding = 1e4L)
  testthat::expect_warning(grid_merge(ncptr, griddedtr$original, 25L))

})

