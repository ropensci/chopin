
testthat::test_that("par_grid", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("future.mirai")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  # Reading data
  ## NC counties polygon
  ncpath <- system.file("shape/nc.shp", package = "sf")
  ncpoly <- terra::vect(ncpath) %>%
    terra::project("EPSG:5070")

  ## Bundled random points in NC
  ncpnts <-
    readRDS(system.file("extdata/nc_random_point.rds", package = "chopin"))
  ncpnts <- terra::vect(ncpnts)
  ncpnts <- terra::project(ncpnts, "EPSG:5070")

  ## Resampled SRTM data in NC
  ncelevpath <- system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
  ncelev <- terra::rast(ncelevpath)

  ## Random points in NC
  ncsamp <-
    terra::spatSample(
      terra::ext(ncelev),
      1e4L,
      lonlat = FALSE,
      as.points = TRUE
    )
  ncsamp <- terra::set.crs(ncsamp, "EPSG:5070")
  ncsamp$kid <- sprintf("K-%05d", seq(1, nrow(ncsamp)))

  tdir <- tempdir()
  target_file <- "ncrandpnts.gpkg"
  test_fullpath <- file.path(tdir, target_file)
  suppressWarnings(
    terra::writeVector(ncsamp, test_fullpath, overwrite = TRUE)
  )

  nccompreg <-
    par_pad_grid(
      input = ncpnts,
      mode = "grid",
      nx = 6L,
      ny = 4L,
      padding = 3e4L
    )
  res <-
    suppressWarnings(
      par_grid(
        grids = nccompreg,
        fun_dist = extract_at,
        x = ncelev,
        y = sf::st_as_sf(ncpnts),
        qsegs = 90L,
        radius = 5e3L,
        id = "pid",
        .debug = FALSE,
        .standalone = FALSE
      )
    )


  # check: sf <-> terra conversion changes coordinate precision?
  # this result omits 2 points which are exactly on the boundary.
  testthat::expect_no_error({
    resstr <-
      suppressWarnings(
        par_grid(
          grids = NULL,
          fun_dist = extract_at,
          points = test_fullpath,
          surf = ncelev,
          qsegs = 90L,
          radius = 5e3L,
          id = "kid",
          nx = 6L,
          ny = 4L,
          padding = 3e4L
        )
      )
  })

  ncpntsf <- sf::st_as_sf(ncpnts)
  testthat::expect_no_error(
    resk <-
      suppressWarnings(
        par_grid(
          grids = nccompreg,
          grid_target_id = NULL,
          fun_dist = extract_at_buffer,
          points = ncpntsf,
          surf = ncelev,
          qsegs = 90L,
          radius = 5e3L,
          id = "pid"
        )
      )
  )
  testthat::expect_error(
    suppressWarnings(
      par_grid(
        grids = nccompreg,
        grid_target_id = "1/10",
        fun_dist = extract_at_buffer,
        points = ncpnts,
        surf = ncelev,
        qsegs = 90L,
        radius = 5e3L,
        id = "pid"
      )
    )
  )

  testthat::expect_error(
    suppressWarnings(
      par_grid(
        grids = nccompreg,
        grid_target_id = c(1, 100, 125),
        fun_dist = extract_at_buffer,
        points = ncpnts,
        surf = ncelev,
        qsegs = 90L,
        radius = 5e3L,
        id = "pid"
      )
    )
  )


  testthat::expect_no_error(
    suppressWarnings(
      par_grid(
        grids = nccompreg,
        grid_target_id = "1:10",
        fun_dist = extract_at_buffer,
        points = ncpnts,
        surf = ncelev,
        qsegs = 90L,
        radius = 5e3L,
        id = "pid"
      )
    )
  )


  testthat::expect_no_error(
    suppressWarnings(
      par_grid(
        grids = nccompreg,
        grid_target_id = c(1, 3),
        fun_dist = extract_at_buffer,
        points = ncpnts,
        surf = ncelev,
        qsegs = 90L,
        radius = 5e3L,
        id = "pid"
      )
    )
  )

  testthat::expect_true(is.list(nccompreg))
  testthat::expect_s4_class(nccompreg$original, "SpatVector")
  testthat::expect_s3_class(res, "data.frame")

  testthat::expect_no_error(
    suppressWarnings(
      resnas <-
        par_grid(
          grids = nccompreg,
          grid_target_id = "1:10",
          fun_dist = extract_at_buffer,
          debug = TRUE,
          points = ncpnts,
          surf = ncelev,
          qsegs = 90L,
          radius = -5e3L,
          id = "pid"
        )
    )
  )

  testthat::expect_s3_class(resnas, "data.frame")
  testthat::expect_true("error_message" %in% names(resnas))

  testthat::expect_no_error(
    suppressWarnings(
      resnas0 <-
        par_grid(
          grids = nccompreg,
          grid_target_id = "1:10",
          fun_dist = terra::nearest,
          x = ncpnts,
          y = ncsamp,
          id = "pid"
        )
    )
  )
  testthat::expect_no_error(
    suppressWarnings(
      resnasz <-
        par_grid(
          grids = nccompreg,
          grid_target_id = NULL,
          fun_dist = terra::nearest,
          x = ncpnts,
          y = ncsamp,
          id = "pid"
        )
    )
  )
})



testthat::test_that(
  "Processes are properly spawned and compute over hierarchy", {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("future")
    withr::local_package("future.apply")
    withr::local_package("dplyr")
    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.resolve.recursive = 2L
      )
    )

    ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
    nccnty <- sf::st_read(ncpath, layer = "county")
    # nccnty <- terra::vect(ncpath, layer = "county")
    nctrct <- sf::st_read(ncpath, layer = "tracts")
    # nctrct <- terra::vect(nctrct)
    ncelev <-
      terra::unwrap(
        readRDS(
          system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
        )
      )
    terra::crs(ncelev) <- "EPSG:5070"
    names(ncelev) <- c("srtm15")

    ncsamp <-
      terra::spatSample(
        terra::ext(ncelev),
        1e4L,
        lonlat = FALSE,
        as.points = TRUE
      )
    ncsamp$kid <- sprintf("K-%05d", seq(1, nrow(ncsamp)))
    ncsamp <- terra::set.crs(ncsamp, "EPSG:5070")

    future::plan(future::sequential)
    testthat::expect_no_error(
      res <-
        suppressWarnings(
          par_hierarchy(
            regions = nccnty,
            regions_id = "GEOID",
            fun_dist = extract_at_poly,
            polys = nctrct,
            surf = ncelev,
            id = "GEOID",
            func = "mean"
          )
        )
    )
    testthat::expect_no_error(
      residb <-
        suppressWarnings(
          par_hierarchy(
            regions = nccnty,
            regions_id = "GEOID",
            unit_id = "GEOID",
            debug = TRUE,
            fun_dist = extract_at_poly,
            polys = sf::st_as_sf(nctrct),
            surf = ncelev,
            id = "GEOID",
            func = "mean"
          )
        )
    )
    testthat::expect_true(is.data.frame(residb))
    testthat::expect_no_error(
      residc <-
        suppressWarnings(
          par_hierarchy(
            regions = nccnty,
            regions_id = unlist(nccnty[["GEOID"]]),
            unit_id = "GEOID",
            debug = TRUE,
            fun_dist = extract_at_poly,
            polys = nctrct,
            surf = ncelev,
            id = "GEOID",
            func = "mean"
          )
        )
    )
    testthat::expect_true(is.data.frame(residc))

    testthat::expect_error(
      suppressWarnings(
        par_hierarchy(
          regions = nccnty,
          regions_id = c(1, 2, 3),
          fun_dist = extract_at_poly,
          polys = nctrct,
          surf = ncelev,
          id = "GEOID",
          func = "mean"
        )
      )
    )

    testthat::expect_s3_class(res, "data.frame")
    testthat::expect_equal(!any(is.na(unlist(res))), TRUE)

    # straightforward error case
    # invalid usage of fun_dist
    # halted at the first error
    testthat::expect_error(
      suppressWarnings(
        resnas <-
          par_hierarchy(
            regions = nccnty,
            regions_id = "GEOID",
            fun_dist = terra::nearest,
            polys = nctrct,
            surf = ncelev
          )
      )
    )

    testthat::expect_no_error(
      suppressWarnings(
        resnasx <-
          par_hierarchy(
            regions = nccnty,
            debug = TRUE,
            regions_id = "GEOID",
            fun_dist = extract_at_buffer,
            points = sf::st_centroid(nctrct),
            surf = ncelev,
            id = "GEOID",
            radius = 1e3L
          )
      )
    )

    testthat::expect_no_error(
      suppressWarnings(
        resnasz <-
          par_hierarchy(
            regions = nccnty,
            debug = TRUE,
            regions_id = "GEOID",
            fun_dist = terra::nearest,
            x = nctrct,
            y = ncsamp
          )
      )
    )
  }
)


testthat::test_that("generic function should be parallelized properly", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  # main test
  pnts <- readRDS(
    system.file("extdata/nc_random_point.rds", package = "chopin")
  )
  pnts <- terra::vect(pnts)
  rd1 <-
    terra::vect(system.file("extdata/ncroads_first.gpkg", package = "chopin"))

  pnts <- terra::project(pnts, "EPSG:5070")
  rd1 <- terra::project(rd1, "EPSG:5070")
  # expect

  nccompreg <-
    par_pad_grid(
      input = pnts,
      mode = "grid",
      nx = 6L,
      ny = 4L,
      padding = 5e4L
    )
  future::plan(future::multicore, workers = 6L)
  testthat::expect_no_error(
    res <-
      suppressWarnings(
        par_grid(
          grids = nccompreg,
          fun_dist = terra::nearest,
          debug = TRUE,
          x = pnts,
          y = rd1
        )
      )
  )
  dd <- terra::extract(nccompreg$original, pnts)
  ddt <- table(dd$CGRIDID)
  nnullgrid <- (6L * 4L) - length(ddt)

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), nrow(pnts) + nnullgrid)

  testthat::expect_no_error(
    res_nodebug <-
      suppressWarnings(
        par_grid(
          grids = nccompreg,
          fun_dist = terra::nearest,
          debug = FALSE,
          x = pnts,
          y = rd1
        )
      )
  )
  testthat::expect_s3_class(res_nodebug, "data.frame")
  testthat::expect_equal(nrow(res_nodebug), nrow(pnts))

})


testthat::test_that(
  "Processes are properly spawned and compute over multirasters",
  {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("future")
    withr::local_package("mirai")
    withr::local_package("future.apply")
    withr::local_package("dplyr")
    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.plan = "multisession",
        future.resolve.recursive = 2L
      )
    )

    ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
    nccnty <- terra::vect(ncpath, layer = "county")
    ncelev <-
      system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
    # terra::unwrap(
    #   readRDS(
    #     system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
    #   )
    # )
    terra::crs(ncelev) <- "EPSG:5070"
    names(ncelev) <- c("srtm15")
    tdir <- tempdir(check = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test1.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test2.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test3.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test4.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test5.tif"), overwrite = TRUE)

    testfiles <- list.files(tdir, pattern = "tif$", full.names = TRUE)
    testthat::expect_no_error(
      res <- par_multirasters(
        filenames = testfiles,
        fun_dist = extract_at_poly,
        debug = FALSE,
        polys = ncpath,
        surf = ncelev,
        id = "GEOID",
        func = "mean",
        radius = 50000L
      )
    )
    testthat::expect_s3_class(res, "data.frame")
    testthat::expect_true(!anyNA(res))

    testthat::expect_no_error(
      res <- par_multirasters(
        filenames = testfiles,
        fun_dist = terra::extract,
        y = nccnty,
        x = ncelev,
        fun = mean
      )
    )

    testfiles_corrupted <- c(testfiles, "/home/runner/fallin.tif")
    testthat::expect_condition(
      resnas <- par_multirasters(
        filenames = testfiles_corrupted,
        debug = TRUE,
        fun_dist = extract_at_poly,
        polys = nccnty,
        surf = ncelev,
        id = "GEOID",
        func = "mean"
      )
    )

    testthat::expect_s3_class(resnas, "data.frame")
    testthat::expect_equal(
      nrow(resnas), nrow(nccnty) * (length(testfiles_corrupted) - 1) + 1
    )
    testthat::expect_true(anyNA(resnas))

    testthat::expect_no_error(
      dough <- par_multirasters(
        filenames = testfiles,
        fun_dist = terra::extract,
        y = nccnty,
        x = ncelev,
        ID = TRUE,
        fun = "mean"
      )
    )
    testthat::expect_s3_class(dough, "data.frame")

    # error case
    future::plan(future::sequential)
    testthat::expect_condition(
      nut <- par_multirasters(
        filenames = testfiles_corrupted,
        debug = TRUE,
        fun_dist = extract_at_poly,
        polys = nccnty,
        surf = ncelev,
        id = "GEOID",
        func = "mean"
      )
    )
    testthat::expect_s3_class(nut, "data.frame")

  }
)
