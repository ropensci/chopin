# testthat::skip_on_os()

testthat::test_that("par_grid -- plain mode with raster path", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("future.mirai")
  withr::local_package("dplyr")
  withr::local_options(
    list(sf_use_s2 = FALSE,
         future.plan = "mirai_multisession")
  )
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
  testthat::expect_no_error({
    res <-
      suppressWarnings(
        par_grid(
          grids = nccompreg,
          fun_dist = extract_at,
          x = ncelevpath,
          y = sf::st_as_sf(ncpnts),
          qsegs = 90L,
          radius = 5e3L,
          id = "pid",
          .debug = FALSE
        )
      )
  })

  testthat::expect_no_error({
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
          .debug = FALSE
        )
      )
  })

})


testthat::test_that("par_grid -- grid_advanced mode", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("future.mirai")
  withr::local_package("dplyr")
  withr::local_options(
    list(sf_use_s2 = FALSE,
         future.plan = "mirai_multisession")
  )
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

  testthat::expect_message(
    testthat::expect_message(
      testthat::expect_message(
        testthat::expect_message(
          ppg <-
            par_pad_grid(
              input = ncsamp,
              mode = "grid_advanced",
              nx = 15L,
              ny = 10L,
              padding = 3e4L,
              grid_min_features = 100L,
              merge_max = 5L
            ),
          paste0(
            "The merged polygons have too complex shapes.\n",
            "Increase threshold or use the original grids."
          )
        ),
        "Switch sf class to terra..."
      ),
      "Switch terra class to sf..."
    ),
    "Switch terra class to sf..."
  )

  testthat::expect_message(
    ppg2 <-
      par_pad_grid(
        input = ncsamp,
        mode = "grid_advanced",
        nx = 15L,
        ny = 10L,
        padding = 3e4L,
        grid_min_features = 50L,
        merge_max = 5L
      ),
    paste0(
      "No grid to merge."
    )
  )

  # run with grid_advanced mode grids
  # check: sf <-> terra conversion changes coordinate precision?
  # this result omits 2 points which are exactly on the boundary.
  testthat::expect_no_error({
    resstr <-
      par_grid(
        grids = ppg,
        fun_dist = extract_at,
        x = ncelev,
        y = sf::st_as_sf(ncsamp),
        qsegs = 90L,
        radius = 5e3L,
        id = "kid",
        .standalone = FALSE,
        .debug = TRUE
      )
  })

})

testthat::test_that("par_grid -- grid_quantile mode", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("future.mirai")
  withr::local_package("dplyr")
  withr::local_package("chopin")
  withr::local_options(
    list(sf_use_s2 = FALSE,
         future.plan = "mirai_multisession")
  )
  # Reading data
  ## NC counties polygon
  ncpath <- system.file("shape/nc.shp", package = "sf")
  ncpoly <- terra::vect(ncpath) %>%
    terra::project("EPSG:5070")

  ## Bundled random points in NC
  ncpnts <-
    readRDS(system.file("extdata/nc_random_point.rds", package = "chopin"))
  ncpntssf <- sf::st_transform(ncpnts, "EPSG:5070")


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

  testthat::expect_no_error(
    ppg <-
      par_pad_grid(
        input = ncsamp,
        mode = "grid_quantile",
        quantiles = seq(0, 1, length.out = 5L),
        padding = 3e4L
      )
  )

  resq <-
    suppressWarnings(
      par_grid(
        grids = ppg,
        fun_dist = extract_at,
        y = ncpntssf,
        x = ncelev,
        qsegs = 90L,
        radius = 5e3L,
        id = "pid"
      )
    )

  testthat::expect_s3_class(resq, "data.frame")
})


testthat::test_that("par_grid -- par_pad_balanced", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("future.mirai")
  withr::local_package("dplyr")
  withr::local_package("chopin")
  withr::local_options(
    list(sf_use_s2 = FALSE,
         future.plan = "mirai_multisession",
         rlib_message_verbosity = "warning")
  )

  # Reading data
  ## NC counties polygon
  ncpath <- system.file("shape/nc.shp", package = "sf")
  ncpoly <- terra::vect(ncpath) %>%
    terra::project("EPSG:5070")

  ## Bundled random points in NC
  ncpnts <-
    readRDS(system.file("extdata/nc_random_point.rds", package = "chopin"))
  ncpntssf <- sf::st_transform(ncpnts, "EPSG:5070")
  ncpntst <- terra::vect(ncpntssf)

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
  target_ncpnts <- "ncpnts.gpkg"
  test_pntspath <- file.path(tdir, target_ncpnts)
  suppressWarnings(
    terra::writeVector(ncpntst, test_pntspath, overwrite = TRUE)
  )

  ncsamp <- ncsamp[seq_len(1000), ]
  testthat::expect_no_error(
    ppg <-
      par_pad_balanced(
        points_in = ncsamp,
        ngroups = 10,
        padding = 1e4L
      )
  )

  testthat::expect_error(
    par_grid(
      grids = ppg,
      fun_dist = nearest,
      x = ncsamp,
      y = ncpntst,
      pad_y = TRUE,
      .debug = TRUE
    ),
    "All terra inputs detected. Please replace x and y to file paths to proceed."
  )

  testthat::expect_no_error(
    par_grid(
      grids = ppg,
      fun_dist = nearest,
      x = test_fullpath,
      y = test_pntspath,
      pad_y = TRUE,
      .debug = TRUE
    )
  )
})


testthat::test_that(
  "par_hierarchy -- raster path input and spatraster fallback ", {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("future")
    withr::local_package("future.apply")
    withr::local_package("future.mirai")
    withr::local_package("dplyr")
    withr::local_package("chopin")
    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.resolve.recursive = 2L,
        future.plan = "mirai_multisession"
      )
    )

    ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
    nccnty <- sf::st_read(ncpath, layer = "county")
    nctrct <- sf::st_read(ncpath, layer = "tracts")
    ncelevpath <- system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
    ncelev <- terra::rast(ncelevpath)

    ncsamp <-
      terra::spatSample(
        terra::ext(ncelev),
        1e4L,
        lonlat = FALSE,
        as.points = TRUE
      )
    ncsamp$kid <- sprintf("K-%05d", seq(1, nrow(ncsamp)))
    ncsamp <- terra::set.crs(ncsamp, "EPSG:5070")

    testthat::expect_no_error(
      residb <-
        par_hierarchy(
          regions = nccnty,
          regions_id = "GEOID",
          .debug = TRUE,
          fun_dist = extract_at,
          y = nctrct,
          x = ncelevpath,
          id = "GEOID",
          func = "mean"
        )
    )
    testthat::expect_true(is.data.frame(residb))

    # SpatRaster input will be converted to file path
    testthat::expect_message(
      testthat::expect_message(
        residb2 <-
          par_hierarchy(
            regions = nccnty,
            regions_id = "GEOID",
            .debug = TRUE,
            fun_dist = extract_at,
            y = nctrct,
            x = ncelev,
            id = "GEOID",
            func = "mean"
          ),
        "Input is not a character."
      ),
      paste0("SpatRaster class input is detected.\n",
             "Attempt to track the data source file path...\n")
    )
    testthat::expect_true(is.data.frame(residb2))

  })


testthat::test_that("par_hierarchy: multicore-SpatRaster input", {
  testthat::skip_on_os("windows")

  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("dplyr")
  withr::local_package("chopin")
  withr::local_options(
    list(
      sf_use_s2 = FALSE,
      future.resolve.recursive = 2L
    )
  )
  future::plan(future::multicore, workers = 2L)

  ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
  nccnty <- sf::st_read(ncpath, layer = "county")
  nctrct <- sf::st_read(ncpath, layer = "tracts")
  ncelevpath <- system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
  ncelev <- terra::rast(ncelevpath)

  ncsamp <-
    terra::spatSample(
      terra::ext(ncelev),
      1e4L,
      lonlat = FALSE,
      as.points = TRUE
    )
  ncsamp$kid <- sprintf("K-%05d", seq(1, nrow(ncsamp)))
  ncsamp <- terra::set.crs(ncsamp, "EPSG:5070")

  testthat::expect_no_error(
    residc <-
      par_hierarchy(
        regions = nccnty,
        regions_id = unlist(nccnty[["GEOID"]]),
        .debug = TRUE,
        fun_dist = extract_at,
        y = nctrct,
        x = ncelev,
        id = "GEOID",
        func = "mean"
      )
  )
  testthat::expect_true(is.data.frame(residc))

  # regions_id is neither length 1 nor length of regions
  testthat::expect_error(
    reshsing <-
      par_hierarchy(
        regions = nccnty,
        regions_id = c(1, 2, 3),
        fun_dist = extract_at,
        y = nctrct,
        x = ncelev,
        id = "GEOID",
        func = "mean"
      ),
    "The length of regions_id is not valid."
  )

  future::plan(future::sequential)
})


testthat::test_that("par_hierarchy: multicore-SpatRaster input", {
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
