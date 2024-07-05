
testthat::test_that("Vector inputs are clipped by clip_vec_ext", {
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
          pnts = ncpt,
          radius = 3e4L,
          target_input = nctrct
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
          pnts = ncp,
          radius = 3e4L,
          target_input = nctrct
        )
    )
  )
  testthat::expect_s3_class(cl_sf, "sf")

  # sf-terra
  testthat::expect_no_error(
    suppressWarnings(
      clip_vec_ext(
        pnts = ncpt,
        radius = 3e4L,
        target_input = sf::st_as_sf(nctrct)
      )
    )
  )

  testthat::expect_error(
    clip_vec_ext(
      pnts = NULL, radius = 3e4L, target_input = nctrct
    )
  )

})


testthat::test_that("Clip by extent works without errors", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  # starts from sf/stars
  ncelev <-
    terra::unwrap(
      readRDS(
        system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
      )
    )
  terra::crs(ncelev) <- "EPSG:5070"
  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  ncp <-
    readRDS(
      system.file("extdata/nc_random_point.rds", package = "chopin")
    )
  ncp_terra <- terra::vect(ncp)

  testthat::expect_no_error(clip_ras_ext(ncp, 30000L, ncelev))
  testthat::expect_no_error(clip_ras_ext(ncp_terra, 30000L, ncelev))
  testthat::expect_error(clip_ras_ext(ncp_terra, NULL, ncelev))
})




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
  ncelev <- readRDS(
    system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
  )
  ncelev <- terra::unwrap(ncelev)

  nccnty4326 <- sf::st_transform(nccnty, "EPSG:4326")
  testthat::expect_no_error(reproject_to_raster(nccnty4326, ncelev))

  # test two modes
  testthat::expect_no_error(
    ncexpoly <-
      extract_at(
        nccntytr,
        ncelev,
        "FIPS",
        mode = "polygon"
      )
  )

  withr::with_envvar(c("CHOPIN_FORCE_CROP" = "TRUE"),
    testthat::expect_no_error(
      extract_at(
        nccntytr,
        ncelev,
        "FIPS",
        mode = "polygon"
      )
    )
  )

  testthat::expect_no_error(
    ncexbuff <-
      extract_at(ncp,
        ncelev,
        "pid",
        mode = "buffer",
        radius = 1e4L
      )
  )
  testthat::expect_no_error(
    extract_at(st_as_sf(ncp),
      ncelev,
      "pid",
      mode = "buffer",
      radius = 1e4L
    )
  )
  withr::with_envvar(c("CHOPIN_FORCE_CROP" = "TRUE"),
    testthat::expect_no_error(
      extract_at(ncp,
        ncelev,
        "pid",
        mode = "buffer",
        radius = 1e4L
      )
    )
  )

  testthat::expect_error(
    extract_at(matrix(runif(100, 2e6, 3e6), 50, 2, TRUE),
      ncelev,
      "pid",
      mode = "buffer",
      radius = 1e4L
    )
  )

  testthat::expect_no_error(
    ncexbuffkern <-
      extract_at_buffer(
        ncp,
        ncelev,
        "pid",
        kernel = "epanechnikov",
        func = stats::weighted.mean,
        bandwidth = 1.25e4L,
        radius = 1e4L
      )
  )
  withr::with_envvar(c("CHOPIN_FORCE_CROP" = "TRUE"),
    testthat::expect_no_error(
      extract_at_buffer(
        ncp,
        ncelev,
        "pid",
        kernel = "epanechnikov",
        func = stats::weighted.mean,
        bandwidth = 1.25e4L,
        radius = 1e4L
      )
    )
  )

  testthat::expect_no_error(
    ncexbuffkern <-
      extract_at(ncp,
        ncelev,
        "pid",
        mode = "buffer",
        kernel = "epanechnikov",
        func = stats::weighted.mean,
        bandwidth = 1.25e4L,
        radius = 1e4L
      )
  )


  # errors
  testthat::expect_error(
    extract_at(nccntytr,
               ncelev,
               "FIPS",
               mode = "whatnot")
  )
  testthat::expect_error(
    extract_at_buffer(nccntytr,
               list(1),
               "FIPS",
               radius = 1e4)
  )
  testthat::expect_error(
    extract_at(nccntytr,
               ncelev,
               "GEOID",
               mode = "polygon")
  )
  testthat::expect_error(
    extract_at(nccntytr,
               ncelev,
               1,
               mode = "buffer",
               radius = 1e4L)
  )
  testthat::expect_error(
    extract_at_buffer(as.list(ncp),
                      ncelev,
                      id = "GEOID",
                      radius = 1e4L)
  )
  testthat::expect_error(
    extract_at_buffer(
      sf::st_as_sf(ncp),
      ncelev,
      id = 1,
      radius = 1e4L
    )
  )
  testthat::expect_error(
    extract_at_buffer(
      sf::st_as_sf(ncp),
      ncelev,
      id = "FIPS",
      mode = "buffer",
      radius = "Ibidem"
    )
  )
  testthat::expect_error(
    extract_at_buffer(
      sf::st_as_sf(ncp),
      ncelev,
      "FIPS",
      radius = "Ibidem"
    )
  )
  testthat::expect_error(
    extract_at_buffer(
      ncp,
      ncelev,
      "pid",
      kernel = "epanechnikov",
      func = "mean",
      bandwidth = 1.25e4L,
      radius = 1e4L,
      qsegs = 3 + 2i
    )
  )


  testthat::expect_no_error(
    extract_at_poly(
      sf::st_as_sf(nccntytr),
      ncelev,
      id = "FIPS"
    )
  )
  testthat::expect_error(
    extract_at_poly(
      as.list(nccntytr),
      ncelev,
      id = "FIPS"
    )
  )
  testthat::expect_error(
    extract_at_poly(
      nccntytr,
      list(NA),
      id = "FIPS"
    )
  )
  testthat::expect_error(
    extract_at_poly(
      nccntytr,
      matrix(rnorm(100), 10, 10),
      id = "FIPS"
    )
  )
  testthat::expect_error(
    extract_at_poly(
      nccntytr,
      ncelev,
      id = 2
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
  sf::st_write(ncp, ncpfile)

  nccnty <- system.file("shape/nc.shp", package = "sf")
  ncelev <- system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
  ncelev <- terra::unwrap(ncelev)

  testthat::expect_no_error(
    extract_at(ncpfile, ncelev, "pid", mode = "buffer", radius = 1e4L)
  )
  testthat::expect_no_error(
    extract_at(ncpfile, ncelev, "pid", mode = "buffer", radius = 1e4L,
               kernel = "epanechnikov", func = stats::weighted.mean,
               bandwidth = 1.25e4L)
  )
  testthat::expect_no_error(
    extract_at(nccnty, ncelev, "FIPS", mode = "polygon")
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

  testthat::expect_no_error(
    system.time({
      ppb_nc_aw <- summarize_aw(ppb, nc, target_fields = flds, "id")
    })
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

  # auto convert formats
  testthat::expect_no_error(
    system.time({
      ppb_nc_aw <- summarize_aw(ppb_t, nc, target_fields = flds, "id")
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
