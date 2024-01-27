# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand  
testthat::test_that("Kernel functions work okay", {
  testthat::expect_error(kernelfunction(10, 100, "hyperbolic"))
  testthat::expect_no_error(kernelfunction(10, 100, "uniform"))
  testthat::expect_no_error(kernelfunction(10, 100, "quartic"))
  testthat::expect_no_error(kernelfunction(10, 100, "triweight"))
  testthat::expect_no_error(kernelfunction(10, 100, "epanechnikov"))
})


testthat::test_that("SEDC are well calculated.", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("testthat")
  withr::local_options(list(sf_use_s2 = FALSE))

  # read and generate data
  ncpath <- system.file("shape/nc.shp", package = "sf")
  ncpoly <- terra::vect(ncpath) |>
    terra::project("EPSG:5070")
  ncpnts <-
    readRDS(testthat::test_path("..", "testdata", "nc_random_point.rds"))
  ncpnts <- terra::vect(ncpnts)
  ncpnts <- terra::project(ncpnts, "EPSG:5070")
  ncrand <- terra::spatSample(ncpoly, 250L)
  ncrand$pollutant1 <- stats::rgamma(250L, 1, 0.01)
  ncrand$pollutant2 <- stats::rnorm(250L, 30, 4)
  ncrand$pollutant3 <- stats::rbeta(250L, 0.5, 0.5)

  polnames <- paste0("pollutant", 1:3)

  testthat::expect_no_error(
    sedc_calc <-
      summarize_sedc(ncpnts, ncrand, "pid", 3e4L, 5e4L, polnames)
  )
  testthat::expect_s3_class(sedc_calc, "data.frame")

  testthat::expect_equal(
    sum(paste0(polnames, "_sedc") %in% names(sedc_calc)),
    length(polnames)
  )
  testthat::expect_true(!is.null(attr(sedc_calc, "sedc_bandwidth")))
  testthat::expect_true(!is.null(attr(sedc_calc, "sedc_threshold")))

  ncpnts <-
    readRDS(testthat::test_path("..", "testdata", "nc_random_point.rds"))
  ncpnts <- sf::st_transform(ncpnts, "EPSG:5070")
  ncrandsf <- sf::st_as_sf(ncrand)

  testthat::expect_no_error(
    summarize_sedc(ncpnts, ncrandsf, "pid", 3e4L, 5e4L, polnames)
  )

  ncpnts2 <- ncpnts
  ncpnts2$FIPS <- rpois(nrow(ncpnts2), 20)
  testthat::expect_warning(
    summarize_sedc(ncpnts2, ncrandsf, "pid", 3e4L, 5e4L, polnames)
  )
})


