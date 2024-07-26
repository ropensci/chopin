## should be fixed ####
testthat::test_that("extract_at runs well", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_options(list(sf_use_s2 = FALSE))

  # starts from sf/stars
  ncp <- readRDS(system.file("extdata/nc_random_point.rds", package = "chopin"))
  ncp <- sf::st_transform(ncp, "EPSG:5070")
  ncp <- terra::vect(ncp)
  nccnty <- system.file("shape/nc.shp", package = "sf")
  nccnty <- sf::st_read(nccnty)
  nccnty <- sf::st_transform(nccnty, "EPSG:5070")
  nccntytr <- terra::vect(nccnty)
  ncelev <-
    terra::rast(system.file("extdata/nc_srtm15_otm.tif", package = "chopin"))

  nccnty4326 <- sf::st_transform(nccnty, "EPSG:4326")
  testthat::expect_no_error(reproject_to_raster(nccnty4326, ncelev))

  # test two modes
  ncexpoly <-
    extract_at(
      ncelev,
      nccntytr,
      "FIPS"
    )
  testthat::expect_s3_class(ncexpoly, "data.frame")

  testthat::expect_warning(
  testthat::expect_warning(
    testthat::expect_message(
      testthat::expect_message(
        extract_at(
          ncelev,
          nccnty,
          "FIPS",
          radius = 100,
          kernel = "epanechnikov",
          func = stats::weighted.mean,
          bandwidth = 1.25e4L
        )
      )
    )
  )
  )

  withr::with_envvar(c("CHOPIN_FORCE_CROP" = "TRUE"),
    testthat::expect_no_error(
      extract_at(
        ncelev,
        ncp,
        "pid",
        radius = 1e4L
      )
    )
  )

  testthat::expect_error(
    nullret <-
      extract_at(
        ncelev,
        matrix(runif(100, 2e6, 3e6), 50, 2, TRUE),
        "pid",
        radius = 1e4L
      )
  )

  testthat::expect_warning(
    testthat::expect_message(
      testthat::expect_message(
        ncexbuffkern <-
          extract_at(
            ncelev,
            ncp,
            "pid",
            kernel = "epanechnikov",
            func = stats::weighted.mean,
            bandwidth = 1.25e4L,
            radius = 1e4L
          )
      )
    )
  )


})


testthat::test_that("Character input works", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_options(list(sf_use_s2 = FALSE))

  # starts from sf/stars
  ncp <- system.file("extdata/nc_random_point.rds", package = "chopin") |>
    readRDS()
  ncpfile <- file.path(tempdir(), "ncp.shp")
  sf::st_write(ncp, ncpfile, append = FALSE)

  nccnty <- system.file("shape/nc.shp", package = "sf")
  ncelev <- system.file("extdata/nc_srtm15_otm.tif", package = "chopin")

  testthat::expect_no_error(
    extract_at(ncelev, ncpfile, "pid", radius = 1e4L)
  )
  testthat::expect_warning(
    extract_at(ncelev, ncpfile, "pid", radius = 1e4L,
               kernel = "epanechnikov", func = stats::weighted.mean,
               bandwidth = 1.25e4L)
  )
  testthat::expect_no_error(
    extract_at(ncelev, nccnty, "FIPS")
  )

})

testthat::test_that("summarize_aw works as expected.", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("units")
  withr::local_package("dplyr")
  withr::local_package("testthat")
  withr::local_options(list(sf_use_s2 = FALSE))

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 5070)
  pp <- sf::st_sample(nc, size = 300)
  pp <- sf::st_as_sf(pp)
  pp[["id"]] <- seq(1, nrow(pp))
  sf::st_crs(pp) <- "EPSG:5070"
  flds <- c("BIR74", "SID74", "BIR79", "SID79")
  ppb <- sf::st_buffer(pp, nQuadSegs = 180, dist = units::set_units(20, "km"))

  testthat::expect_warning(
    system.time({
      ppb_nc_aw <- summarize_aw(ppb, nc, target_fields = flds, "id")
    }),
    "st_interpolate_aw assumes attributes are constant or uniform over areas of x"
  )
  testthat::expect_true(inherits(ppb_nc_aw, "data.frame"))

  # terra
  ppb_t <- terra::vect(ppb)
  nc_t <- terra::vect(nc)
  testthat::expect_no_error(
    system.time({
      ppb_nc_aw <- summarize_aw(ppb_t, nc_t, target_fields = flds, "id")
    })
  )
  testthat::expect_s3_class(ppb_nc_aw, "data.frame")

  # error case
  testthat::expect_error(summarize_aw(as.list(ppb_t), nc, fld, "id"))
  testthat::expect_error(summarize_aw(ppb_t, list(1, 3), fld, "id"))
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
    readRDS(system.file("extdata/nc_random_point.rds", package = "chopin"))
  ncpnts <- terra::vect(ncpnts)
  ncpnts <- terra::project(ncpnts, "EPSG:5070")
  ncrand <- terra::spatSample(ncpoly, 250L)
  ncrand$pollutant1 <- stats::rgamma(250L, 1, 0.01)
  ncrand$pollutant2 <- stats::rnorm(250L, 30, 4)
  ncrand$pollutant3 <- stats::rbeta(250L, 0.5, 0.5)

  polnames <- paste0("pollutant", 1:3)
  testthat::expect_no_error(
    summarize_sedc(ncpnts, ncrand, "pid", 3e4L, NULL, polnames)
  )
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
    readRDS(system.file("extdata/nc_random_point.rds", package = "chopin"))
  ncpnts <- sf::st_transform(ncpnts, "EPSG:5070")
  ncrandsf <- sf::st_as_sf(ncrand)

  testthat::expect_no_error(
    summarize_sedc(ncpnts, ncrandsf, "pid", 3e4L, 5e4L, polnames)
  )

  ncpnts2 <- ncpnts
  ncpnts2$FIPS <- as.character(rpois(nrow(ncpnts2), 20))
  testthat::expect_warning(
    summarize_sedc(ncpnts2, ncrandsf, "pid", 3e4L, 5e4L, polnames)
  )
})

testthat::test_that("Kernel functions work okay", {
  testthat::expect_error(kernelfunction(10, 100, "hyperbolic"))
  testthat::expect_no_error(kernelfunction(10, 100, "uniform"))
  testthat::expect_no_error(kernelfunction(10, 100, "quartic"))
  testthat::expect_no_error(kernelfunction(10, 100, "triweight"))
  testthat::expect_no_error(kernelfunction(10, 100, "epanechnikov"))
})
