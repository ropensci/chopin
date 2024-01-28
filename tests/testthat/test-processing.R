# Generated from chopin_rmarkdown_litr.rmd: do not edit by hand

testthat::test_that("Vector inputs are clipped by clip_vec_ext", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncpath <- testthat::test_path("..", "testdata", "nc_hierarchy.gpkg")
  nccnty <- terra::vect(
    ncpath, layer = "county",
    query = "SELECT * FROM county WHERE GEOID IN (37063, 37183)"
  )
  nctrct <- terra::vect(ncpath, layer = "tracts")

  ncp <- readRDS(testthat::test_path("..", "testdata", "nc_random_point.rds"))
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
  ncelev <- terra::unwrap(readRDS("../testdata/nc_srtm15_otm.rds"))
  terra::crs(ncelev) <- "EPSG:5070"
  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  ncp <- readRDS("../testdata/nc_random_point.rds")
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
  ncp <- readRDS(testthat::test_path("..", "testdata", "nc_random_point.rds"))
  ncp <- sf::st_transform(ncp, "EPSG:5070")
  ncp <- terra::vect(ncp)
  nccnty <- system.file("shape/nc.shp", package = "sf")
  nccnty <- sf::st_read(nccnty)
  nccnty <- sf::st_transform(nccnty, "EPSG:5070")
  nccntytr <- terra::vect(nccnty)
  ncelev <- readRDS(testthat::test_path("..", "testdata", "nc_srtm15_otm.rds"))
  ncelev <- terra::unwrap(ncelev)

  nccnty4326 <- sf::st_transform(nccnty, "EPSG:4326")
  testthat::expect_no_error(reproject_b2r(nccnty4326, ncelev))

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
                 "GEOID",
                 mode = "whatnot")
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
    extract_at(as.list(ncp),
                 ncelev,
                 "GEOID",
                 mode = "buffer",
                 radius = 1e4L)
  )
  testthat::expect_error(
    extract_at(sf::st_as_sf(ncp),
                 ncelev,
                 "GEOID",
                 mode = "buffer",
                 radius = 1e4L)
  )
  testthat::expect_error(
    extract_at(sf::st_as_sf(ncp),
                 ncelev,
                 1,
                 mode = "buffer",
                 radius = "Ibidem")
  )
  testthat::expect_error(
    extract_at_buffer(ncp,
                      ncelev,
                      "pid",
                      kernel = "epanechnikov",
                      func = stats::weighted.mean,
                      bandwidth = 1.25e4L,
                      radius = 1e4L,
                      qsegs = 3 + 2i)
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
  ppb <- sf::st_buffer(pp, nQuadSegs = 180, dist = units::set_units(20, "km"))

  testthat::expect_no_error(
    system.time({ppb_nc_aw <- summarize_aw(ppb, nc, "id")})
  )
  expect_s3_class(ppb_nc_aw, "sf")

  # terra
  ppb_t <- terra::vect(ppb)
  nc_t <- terra::vect(nc)
  testthat::expect_no_error(
    system.time({ppb_nc_aw <- summarize_aw(ppb_t, nc_t, "id")})
  )
  expect_s3_class(ppb_nc_aw, "data.frame")

  # auto convert formats
  testthat::expect_no_error(
    system.time({ppb_nc_aw <- summarize_aw(ppb_t, nc, "id")})
  )
  expect_s3_class(ppb_nc_aw, "data.frame")

  # error case
  testthat::expect_error(summarize_aw(as.list(ppb_t), nc, "id"))
})



