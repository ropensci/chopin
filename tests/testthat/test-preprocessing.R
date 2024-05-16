# Generated from chopin_rmarkdown_litr.rmd: do not edit by hand


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

    stars_bcsd_tr <- dep_switch(bcsd_stars)
    sf_nc_tr <- dep_switch(nc)

    testthat::expect_equal(dep_check(stars_bcsd_tr), "terra")
    testthat::expect_equal(dep_check(sf_nc_tr), "terra")

    stars_bcsd_trb <- dep_switch(stars_bcsd_tr)
    sf_nc_trb <- dep_switch(sf_nc_tr)

    testthat::expect_equal(dep_check(stars_bcsd_trb), "sf")
    testthat::expect_equal(dep_check(sf_nc_trb), "sf")

    testthat::expect_error(dep_check(list(1, 2)))
    testthat::expect_error(dep_check(matrix(c(1, 2), nrow = 2, byrow = TRUE)))
  }
)



testthat::test_that("Clip extent is set properly", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncpath <- system.file("shape/nc.shp", package = "sf")
  suppressWarnings({
    nc <- sf::read_sf(ncpath) |>
      sf::st_transform("EPSG:5070") |>
      sf::st_centroid()
  })

  radius <- 1e4L

  (nc_ext_sf <- get_clip_ext(nc, radius))

  nct <- terra::vect(nc)
  (nc_ext_terra <- get_clip_ext(nct, radius))

  (proper_xmin <- sf::st_bbox(nc)[1] - (1.1 * radius))

  testthat::expect_s3_class(nc_ext_sf, "sfc")
  testthat::expect_s4_class(nc_ext_terra, "SpatExtent")

  nc_ext_sf_1 <- sf::st_bbox(nc_ext_sf)[1]
  nc_ext_terra_1 <- nc_ext_terra[1]

  testthat::expect_equal(nc_ext_sf_1, proper_xmin)
  testthat::expect_equal(nc_ext_terra_1, proper_xmin)

})
