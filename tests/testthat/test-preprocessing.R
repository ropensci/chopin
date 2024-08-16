testthat::test_that("Format is well converted",
  {
    # testthat::skip_on_ci()
    withr::local_package("stars")
    withr::local_package("terra")
    withr::local_options(list(sf_use_s2 = FALSE))

    # starts from sf/stars
    bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
    bcsd_stars <- stars::read_stars(bcsd_path)
    nc <- system.file(package = "sf", "shape/nc.shp")
    nc <- sf::read_sf(nc)

    stars_bcsd_tr <- chopin:::dep_switch(bcsd_stars)
    sf_nc_tr <- chopin:::dep_switch(nc)

    testthat::expect_equal(chopin:::dep_check(stars_bcsd_tr), "terra")
    testthat::expect_equal(chopin:::dep_check(sf_nc_tr), "terra")

    stars_bcsd_trb <- chopin:::dep_switch(stars_bcsd_tr)
    sf_nc_trb <- chopin:::dep_switch(sf_nc_tr)

    testthat::expect_equal(chopin:::dep_check(stars_bcsd_trb), "sf")
    testthat::expect_equal(chopin:::dep_check(sf_nc_trb), "sf")

    testthat::expect_error(chopin:::dep_check(list(1, 2)))
    testthat::expect_error(chopin:::dep_check(matrix(c(1, 2), nrow = 2, byrow = TRUE)))
  }
)


testthat::test_that("Clip extent is set properly", {
  # testthat::skip_on_ci()
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

  (nc_ext_sf <- chopin:::get_clip_ext(nc, radius))

  nct <- terra::vect(nc)
  (nc_ext_terra <- chopin:::et_clip_ext(nct, radius))

  (proper_xmin <- sf::st_bbox(nc)[1] - (1.1 * radius))

  testthat::expect_s3_class(nc_ext_sf, "sfc")
  testthat::expect_s4_class(nc_ext_terra, "SpatExtent")

  nc_ext_sf_1 <- sf::st_bbox(nc_ext_sf)[1]
  nc_ext_terra_1 <- nc_ext_terra[1]

  testthat::expect_equal(nc_ext_sf_1, proper_xmin)
  testthat::expect_equal(nc_ext_terra_1, proper_xmin)

})


testthat::test_that("Vector inputs are clipped by clip_vec_ext", {
  # testthat::skip_on_ci()
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
  nccnty <- terra::vect(
    ncpath, layer = "county",
    query = "SELECT * FROM county WHERE GEOID IN (37063, 37183)"
  )
  nctrct <- terra::vect(ncpath, layer = "tracts")

  ncp <- readRDS(system.file("extdata/nc_random_point.rds", package = "chopin"))
  ncp <- sf::st_transform(ncp, "EPSG:5070")
  ncpt <- terra::vect(ncp)

  ncpt <- ncpt[nccnty, ]

  # terra-terra
  testthat::expect_no_error(
    suppressWarnings(
      cl_terra <-
        clip_vec_ext(
          x = ncpt,
          radius = 3e4L,
          y = nctrct
        )
    )
  )
  testthat::expect_s4_class(cl_terra, "SpatVector")

  # sf-sf
  ncp <- sf::st_as_sf(ncpt)
  nccntysf <- sf::st_as_sf(nccnty)
  nctrct <- sf::st_as_sf(nctrct)
  testthat::expect_no_error(
    suppressWarnings(
      cl_sf <-
        clip_vec_ext(
          x = ncp,
          radius = 3e4L,
          y = nctrct
        )
    )
  )
  testthat::expect_s3_class(cl_sf, "sf")

  # sf-terra
  testthat::expect_no_error(
    suppressWarnings(
      clip_vec_ext(
        x = ncpt,
        radius = 3e4L,
        y = sf::st_as_sf(nctrct)
      )
    )
  )

  testthat::expect_error(
    clip_vec_ext(
      x = NULL, radius = 3e4L, y = nctrct
    )
  )

})


testthat::test_that("Clip by extent works without errors", {
  # testthat::skip_on_ci()
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  # starts from sf/stars
  ncelev <-
    system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
  ncelev <- terra::rast(ncelev)
  terra::crs(ncelev) <- "EPSG:5070"
  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  ncp <-
    readRDS(
      system.file("extdata/nc_random_point.rds", package = "chopin")
    )
  ncp_terra <- terra::vect(ncp)

  testthat::expect_no_error(clip_ras_ext(ncelev, ncp, 30000L))
  testthat::expect_no_error(clip_ras_ext(ncelev, ncp_terra, 30000L))
  testthat::expect_error(clip_ras_ext(ncelev, ncp_terra, NULL))
})
