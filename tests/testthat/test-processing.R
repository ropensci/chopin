
testthat::test_that("extract_at -- character-character inputs", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_options(list(sf_use_s2 = FALSE))

  # starts from sf/stars
  data("ncpoints", package = "chopin")
  ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
  ncp$pid <- seq_len(nrow(ncp))
  ncp <- terra::vect(ncp)

  nccnty <- sf::st_read(nccntypath)
  nccnty <- sf::st_transform(nccnty, "EPSG:5070")
  nccntytr <- terra::vect(nccnty)

  ## Resampled SRTM data in NC
  ras <- terra::rast(nccnty, nrow = 1000, ncol = 2200)
  terra::values(ras) <- rgamma(2.2e6, 4, 2)

  # Using raster path
  ncelev <- terra::rast(ncelevpath)

  nccnty4326 <- sf::st_transform(nccnty, "EPSG:4326")
  testthat::expect_no_error(chopin:::reproject_to_raster(nccnty4326, ncelev))

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


testthat::test_that("extract_at -- SpatRaster-character inputs", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_options(list(sf_use_s2 = FALSE))

  # starts from sf/stars
  nccnty <- sf::st_read(nccntypath)
  nccnty <- sf::st_transform(nccnty, "EPSG:5070")
  nccntyp <- suppressWarnings(
    sf::st_centroid(nccnty, of_largest_polygon = TRUE)
  )
  nccntypath <- file.path(tempdir(check = TRUE), "nc5070.gpkg")
  suppressWarnings(
    sf::st_write(nccntyp, nccntypath, delete_dsn = TRUE)
  )

  # test two modes
  ncexpoly <-
    extract_at(
      ncelev,
      nccnty,
      "FIPS"
    )
  testthat::expect_s3_class(ncexpoly, "data.frame")

  testthat::expect_warning(
    testthat::expect_message(
      testthat::expect_message(
        extract_at(
          x = ncelev,
          y = nccntypath,
          id = "FIPS",
          radius = 100L,
          kernel = "epanechnikov",
          func = stats::weighted.mean,
          bandwidth = 1.25e4L
        )
      )
    )
  )

})


testthat::test_that("extract_at -- character-sf inputs", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_options(list(sf_use_s2 = FALSE))

  nccnty <- sf::st_read(nccntypath)

  # test two modes
  ncexpoly <-
    chopin::extract_at(
      ncelevpath,
      nccnty,
      "FIPS"
    )
  testthat::expect_s3_class(ncexpoly, "data.frame")

  testthat::expect_warning(
    testthat::expect_warning(
      testthat::expect_message(
        testthat::expect_message(
          chopin::extract_at(
            ncelevpath,
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

})


testthat::test_that("extract_at -- character-SpatVector inputs", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_options(list(sf_use_s2 = FALSE))

  nccnty <- system.file("shape/nc.shp", package = "sf")
  nccnty <- terra::vect(nccnty)
  nccnty <- terra::project(nccnty, "EPSG:5070")

  ## Resampled SRTM data in NC
  ras <- terra::rast(nccnty, nrow = 1000, ncol = 2200)
  terra::values(ras) <- rgamma(2.2e6, 4, 2)

  # Using raster path
  ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
  terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
  ncelev <- terra::rast(ncelevpath)

  # test two modes
  ncexpoly <-
    chopin::extract_at(
      x = ncelevpath,
      y = nccnty,
      id = "FIPS",
      extent = NULL
    )
  testthat::expect_s3_class(ncexpoly, "data.frame")

  testthat::expect_warning(
    testthat::expect_warning(
      testthat::expect_message(
        testthat::expect_message(
          chopin::extract_at(
            ncelevpath,
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

})

## .extract_at tests ####
testthat::test_that(".extract_at + character inputs without kernel weighting", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_options(list(sf_use_s2 = FALSE))

  testthat::expect_no_error(
    chopin:::.extract_at(ncelevpath, nccntypath, "FIPS", max_cells = 3e7)
  )

})


testthat::test_that(".extract_at + terra inputs without kernel weighting", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncpoly <- sf::st_read(nccntypath)
  ncpoly <- sf::st_transform(ncpoly, "EPSG:5070")

  terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
  ncelev <- terra::rast(ncelevpath)

  cnty <- terra::vect(nccntypath)
  elev <- terra::rast(ncelev)
  testthat::expect_no_error(
    chopin:::.extract_at(ncelev, cnty, "FIPS", max_cells = 3e7)
  )

  testthat::expect_warning(
    chopin:::.extract_at(ncelev, cnty, "FIPS", radius = 1e3, max_cells = 3e7),
    "Buffer is set with non-point geometries."
  )

})


testthat::test_that(".extract_at + terra/sf inputs without kernel weighting", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncpoly <- sf::st_read(nccntypath)
  ncpoly <- sf::st_transform(ncpoly, "EPSG:5070")

  # Using raster path
  terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
  ncelev <- terra::rast(ncelevpath)

  cnty <- sf::st_read(nccntypath)

  testthat::expect_warning(
    chopin:::.extract_at(ncelev, cnty, "FIPS", radius = 1e3, max_cells = 3e7),
    "Buffer is set with non-point geometries."
  )

})

testthat::test_that(".extract_at + character inputs with kernel weighting", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncpoly <- sf::st_read(nccntypath)
  ncpoly <- sf::st_transform(ncpoly, "EPSG:5070")
  nccnty <- ncpoly

  # Using raster path
  ncelev <- terra::rast(ncelevpath)

  cnty <- terra::vect(nccntypath)
  cntycent <- terra::centroids(cnty)

  # polygon input + kernel + no bandwidth: error
  testthat::expect_error(
    chopin:::.extract_at(
      ncelev,
      nccnty,
      id = "FIPS",
      kernel = "epanechnikov",
      bandwidth = NULL,
      max_cells = 3e7
    )
  )

  # polygon input + kernel + bandwidth: warning (converted to point)
  testthat::expect_message(
    testthat::expect_warning(
      chopin:::.extract_at(
        ncelev,
        nccnty,
        id = "FIPS",
        kernel = "epanechnikov",
        bandwidth = 3000,
        max_cells = 3e7
      )
    )
  )

  # point input + no kernel + no bandwidth: error
  testthat::expect_error(
    chopin:::.extract_at(
      ncelev,
      cntycent,
      id = "FIPS",
      kernel = NULL,
      max_cells = 3e7
    )
  )

  # point input + kernel + no bandwidth: error
  testthat::expect_error(
    chopin:::.extract_at(
      ncelev,
      cntycent,
      id = "FIPS",
      kernel = "epanechnikov",
      bandwidth = NULL,
      max_cells = 3e7
    )
  )

  # point input + radius + kernel + bandwidth: message
  testthat::expect_warning(
    testthat::expect_message(
      chopin:::.extract_at(
        ncelev,
        cntycent,
        radius = 1000,
        id = "FIPS",
        kernel = "epanechnikov",
        bandwidth = 3000,
        max_cells = 3e7
      )
    )
  )
})

## .kernel_weighting tests ####
testthat::test_that(".kernel_weighting works", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_package("exactextractr")
  withr::local_options(list(sf_use_s2 = FALSE))

  # Using raster path
  ncelev <- terra::rast(ncelevpath)

  cnty <- sf::st_read(nccntypath)
  cnty <- sf::st_transform(cnty, terra::crs(ncelev))

  extr <- exactextractr::exact_extract(
    ncelev,
    cnty,
    force_df = TRUE,
    include_cols = "FIPS",
    include_area = TRUE,
    include_xy = TRUE,
    progress = FALSE
  )

  # polygon input + kernel: error
  testthat::expect_warning(
    chopin:::.kernel_weighting(
      x_ras = ncelev,
      y_vec = cnty,
      id = "FIPS",
      extracted = extr,
      kernel = "epanechnikov",
      bandwidth = 1000
    )
  )

})



## .check_character tests ####
testthat::test_that("Character input works", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_package("dplyr")
  withr::local_package("rlang")
  withr::local_options(list(sf_use_s2 = FALSE))

  # starts from sf/stars
  ncp <- readRDS(rppath)
  ncpfile <- file.path(tempdir(), "ncp.shp")
  sf::st_write(ncp, ncpfile, append = FALSE)

  ncpoly <- sf::st_read(nccntypath)
  ncpoly <- sf::st_transform(ncpoly, "EPSG:5070")

  # Using raster path
  ncelev <- terra::rast(ncelevpath)

  testthat::expect_no_error(
    extract_at(ncelevpath, ncpfile, "pid", radius = 1e4L)
  )
  testthat::expect_warning(
    extract_at(ncelevpath, ncpfile, "pid", radius = 1e4L,
               kernel = "epanechnikov", func = stats::weighted.mean,
               bandwidth = 1.25e4L)
  )
  testthat::expect_no_error(
    extract_at(ncelevpath, nccntypath, "FIPS")
  )

})



testthat::test_that("summarize_aw works -- character-character", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("units")
  withr::local_package("dplyr")
  withr::local_package("testthat")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncpath <- system.file("shape/nc.shp", package = "sf")
  nc <- sf::st_read(ncpath)
  nc <- sf::st_transform(nc, 5070)
  nctemppath <- file.path(tempdir(), "ncr.shp")
  sf::st_write(nc, nctemppath, append = FALSE)
  pp <- sf::st_sample(nc, size = 300)
  pp <- sf::st_as_sf(pp)
  pp[["id"]] <- seq(1, nrow(pp))
  sf::st_crs(pp) <- "EPSG:5070"
  flds <- c("BIR74", "SID74", "BIR79", "SID79")
  ppb <- sf::st_buffer(pp, nQuadSegs = 180, dist = units::set_units(20, "km"))
  ppbpath <- file.path(tempdir(), "ppb.shp")
  sf::st_write(ppb, ppbpath, append = FALSE)

  system.time({
    ppb_nc_aw <- summarize_aw(ppbpath, nctemppath, target_fields = flds, "id")
  })
  testthat::expect_true(inherits(ppb_nc_aw, "data.frame"))

})


testthat::test_that("summarize_aw works -- sf-sf", {
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

})

testthat::test_that("summarize_aw works -- terra-terra", {
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

})

testthat::test_that("summarize_aw works -- error cases", {
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

  # error case
  testthat::expect_error(summarize_aw(as.list(ppb_t), nc, fld, "id"))
  testthat::expect_error(summarize_aw(ppb_t, list(1, 3), fld, "id"))
})


# SEDC tests ####
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
  ncpnts <- readRDS(rppath)
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

  ncpnts <- readRDS(rppath)
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



testthat::test_that("SEDC warning message with multiple fields overlapped", {
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
    readRDS(rppath)
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

  ncpnts$overlap <- 1L
  ncrand$overlap <- 100L

  testthat::expect_warning(
    sedc_calc <-
      summarize_sedc(ncpnts, ncrand, "pid", 3e4L, 5e4L, polnames),
    "There are 1 fields with the same name. The result may be inaccurate."
  )
  testthat::expect_s3_class(sedc_calc, "data.frame")

  testthat::expect_equal(
    sum(paste0(polnames, "_sedc") %in% names(sedc_calc)),
    length(polnames)
  )
  testthat::expect_true(!is.null(attr(sedc_calc, "sedc_bandwidth")))
  testthat::expect_true(!is.null(attr(sedc_calc, "sedc_threshold")))

})

# Kernel functions ####
testthat::test_that("Kernel functions work", {
  testthat::expect_error(chopin:::kernelfunction(10, 100, "hyperbolic"))
  testthat::expect_no_error(chopin:::kernelfunction(10, 100, "uniform"))
  testthat::expect_no_error(chopin:::kernelfunction(10, 100, "quartic"))
  testthat::expect_no_error(chopin:::kernelfunction(10, 100, "triweight"))
  testthat::expect_no_error(chopin:::kernelfunction(10, 100, "epanechnikov"))
})
