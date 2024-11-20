### par_grid tests ####
testthat::test_that("par_grid_mirai -- plain mode with raster path", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("mirai")
  withr::local_package("dplyr")
  withr::local_package("chopin")
  withr::local_options(
    list(sf_use_s2 = FALSE)
  )
  mirai::daemons(4, dispatcher = "process")
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
        par_grid_mirai(
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
        par_grid_mirai(
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
  mirai::daemons(0)
})


### par_hierarchy tests ####
testthat::test_that(
  "par_hierarchy_mirai -- raster path input and spatraster fallback ", {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("mirai")
    withr::local_package("dplyr")
    withr::local_package("chopin")

    withr::local_options(
      list(
        sf_use_s2 = FALSE
      )
    )
    mirai::daemons(4, dispatcher = "process")
    withr::local_seed(202407)

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
        par_hierarchy_mirai(
          regions = nccnty,
          regions_id = "GEOID",
          .debug = TRUE,
          fun_dist = extract_at,
          y = nctrct,
          x = ncelevpath,
          id = "GEOID",
          func = "mean",
          pad_y = FALSE,
          .standalone = FALSE
        )
    )
    testthat::expect_true(is.data.frame(residb))

    # SpatRaster input will be converted to file path
    testthat::expect_message(
      testthat::expect_message(
        residb2 <-
          par_hierarchy_mirai(
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

    mirai::daemons(0)
  }
)


testthat::test_that("par_hierarchy_mirai: define level by substring", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("mirai")
  withr::local_package("dplyr")
  withr::local_package("chopin")

  withr::local_options(
    list(
      sf_use_s2 = FALSE
    )
  )
  mirai::daemons(4, dispatcher = "process")
  withr::local_seed(202407)

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

  # use length_left to substring GEOID
  testthat::expect_no_error(
    residc <-
      par_hierarchy_mirai(
        regions = nctrct,
        regions_id = "GEOID",
        length_left = 5L,
        .debug = TRUE,
        fun_dist = extract_at,
        y = nctrct,
        x = ncelev,
        id = "GEOID",
        func = "mean"
      )
  )
  testthat::expect_true(is.data.frame(residc))

  # bare integers with different lengths: warning message
  nctrct$qid <- seq_len(nrow(nctrct))
  testthat::expect_message(
    residc <-
      par_hierarchy_mirai(
        regions = nctrct,
        regions_id = "qid",
        length_left = 2L,
        .debug = TRUE,
        fun_dist = extract_at,
        y = nctrct,
        x = ncelev,
        id = "GEOID",
        func = "mean"
      )
  )

})


testthat::test_that("generic function should be parallelized properly", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("mirai")
  withr::local_package("dplyr")
  withr::local_package("chopin")

  withr::local_options(
    list(
      sf_use_s2 = FALSE
    )
  )

  mirai::daemons(4L, dispatcher = "process")

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
  testthat::expect_error(
    res <-
      suppressWarnings(
        par_grid_mirai(
          grids = nccompreg,
          fun_dist = nearest,
          .debug = TRUE,
          x = pnts,
          y = rd1
        )
      ),
    "terra inputs detected in both x and y. Please replace x and y to file paths to proceed."
  )
  mirai::daemons(0L)
})


testthat::test_that(
  "par_multirasters_mirai -- character filenames, character y",
  {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("mirai")
    withr::local_package("dplyr")
    withr::local_package("chopin")
    withr::local_options(
      list(
        sf_use_s2 = FALSE
      )
    )
    mirai::daemons(4, dispatcher = "process")
    ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
    nccnty <- sf::st_read(ncpath, layer = "county")
    nccnty <- terra::vect(nccnty)
    ncelev <-
      system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
    ncelev <- terra::rast(ncelev)
    tdir <- tempdir(check = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test1.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test2.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test3.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test4.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test5.tif"), overwrite = TRUE)

    testfiles <- list.files(tdir, pattern = "tif$", full.names = TRUE)
    testthat::expect_no_error(
      suppressWarnings(
        res <- par_multirasters_mirai(
          filenames = testfiles,
          fun_dist = extract_at,
          .debug = TRUE,
          y = ncpath,
          x = ncelev,
          id = "GEOID",
          func = "mean",
          radius = 5000L
        )
      )
    )
    testthat::expect_s3_class(res, "data.frame")
    testthat::expect_true(!anyNA(res))

    mirai::daemons(0)
  }
)


testthat::test_that(
  "par_multirasters_mirai -- terra function dispatch",
  {
    testthat::skip_on_os("windows")

    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("mirai")
    withr::local_package("dplyr")
    withr::local_package("chopin")
    withr::local_options(
      list(
        sf_use_s2 = FALSE
      )
    )
    mirai::daemons(4, dispatcher = "process")
    ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
    nccnty <- terra::vect(ncpath, layer = "county")
    ncelev <-
      system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
    ncelev <- terra::rast(ncelev)
    tdir <- tempdir(check = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test1.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test2.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test3.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test4.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test5.tif"), overwrite = TRUE)

    testfiles <- list.files(tdir, pattern = "tif$", full.names = TRUE)
    testthat::expect_no_error(
      suppressWarnings(
        res <- par_multirasters_mirai(
          filenames = testfiles,
          .debug = TRUE,
          fun_dist = extract,
          y = ncpath,
          x = ncelev,
          fun = mean
        )
      )
    )
    mirai::daemons(0)

  }
)



testthat::test_that(
  "Processes are properly spawned and compute over multirasters",
  {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("mirai")
    withr::local_package("dplyr")
    withr::local_package("chopin")

    withr::local_options(
      list(
        sf_use_s2 = FALSE
      )
    )
    mirai::daemons(4, dispatcher = "process")
    ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
    nccnty <- sprintf("GPKG:%s:%s", ncpath, "county")
    ncelev <-
      system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
    ncelev <- terra::rast(ncelev)
    tdir <- tempdir(check = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test1.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test2.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test3.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test4.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test5.tif"), overwrite = TRUE)

    testfiles <- list.files(tdir, pattern = "tif$", full.names = TRUE)

    testfiles_corrupted <- c(testfiles, "/home/runner/fallin.tif")

    # suppressWarnings: suppressing multilayer gpkg read warnings
    suppressWarnings(
      resnas <- par_multirasters_mirai(
        filenames = testfiles_corrupted,
        .debug = TRUE,
        fun_dist = extract_at,
        y = nccnty,
        x = ncelev,
        id = "GEOID",
        func = "mean"
      )
    )

    testthat::expect_s3_class(resnas, "data.frame")
    testthat::expect_equal(
      nrow(resnas), 100L * (length(testfiles_corrupted) - 1) + 1
    )
    testthat::expect_true("error_message" %in% names(resnas))
    testthat::expect_true(anyNA(resnas))

    # error case: function loading with ::
    testthat::expect_no_error(
      suppressWarnings(
        nut <- par_multirasters_mirai(
          filenames = testfiles_corrupted,
          .debug = TRUE,
          fun_dist = terra::extract,
          y = nccnty,
          x = ncelev,
          ID = TRUE,
          fun = mean
        )
      )
    )

    testthat::expect_s3_class(nut, "data.frame")
    mirai::daemons(0)

  }
)


testthat::test_that(
  "par_multirasters: sf y",
  {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("mirai")
    withr::local_package("dplyr")
    withr::local_package("chopin")

    withr::local_options(
      list(
        sf_use_s2 = FALSE
      )
    )
    mirai::daemons(4, dispatcher = "process")
    ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
    nccnty <- sprintf("GPKG:%s:%s", ncpath, "county")
    suppressWarnings(nccnty <- sf::st_read(nccnty))
    ncelev <-
      system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
    ncelev <- terra::rast(ncelev)
    tdir <- tempdir(check = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test1.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test2.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test3.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test4.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test5.tif"), overwrite = TRUE)

    testfiles <- list.files(tdir, pattern = "tif$", full.names = TRUE)
    testfiles_corrupted <- c(testfiles, "/home/runner/fallin.tif")

    testthat::expect_no_error(
      dough <- par_multirasters_mirai(
        filenames = testfiles,
        fun_dist = extract,
        y = nccnty,
        x = ncelev,
        ID = TRUE,
        fun = mean
      )
    )
    testthat::expect_s3_class(dough, "data.frame")
    testthat::expect_true(!anyNA(dough))
    testthat::expect_equal(nrow(dough), nrow(nccnty) * length(testfiles))

    # error case
    nut <- par_multirasters_mirai(
      filenames = testfiles_corrupted,
      .debug = TRUE,
      fun_dist = extract_at,
      y = nccnty,
      x = ncelev,
      id = "GEOID",
      func = "mean"
    )

    testthat::expect_s3_class(nut, "data.frame")
    testthat::expect_true("error_message" %in% names(nut))
    testthat::expect_true(sum(!is.na(nut$error_message)) == 1L)

    mirai::daemons(0)
  }
)
