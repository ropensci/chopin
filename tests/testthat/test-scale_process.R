### .par_screen tests ####
testthat::test_that(".par_screen -- vector", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  # withr::local_package("chopin")
  withr::local_options(
    list(sf_use_s2 = FALSE)
  )
  withr::with_dir(testthat::test_path("../.."), devtools::load_all())
  # Reading data
  ## NC counties polygon
  ncpath <- system.file("shape/nc.shp", package = "sf")
  ncpoly <- terra::vect(ncpath) %>%
    terra::project("EPSG:5070")

  scr_terra <-
    .par_screen(
      type = "vector",
      input = ncpath,
      input_id = "FIPS",
      out_class = "terra"
    )

  scr_sf <-
    .par_screen(
      type = "vector",
      input = ncpath,
      input_id = "FIPS",
      out_class = "sf"
    )

  testthat::expect_s3_class(scr_sf, "sf")
  testthat::expect_s4_class(scr_terra, "SpatVector")

  # sf into terra
  scr_sfterra <-
    .par_screen(
      type = "vector",
      input = scr_sf,
      input_id = "FIPS",
      out_class = "terra"
    )
  # sf (as is)
  scr_sfsf <-
    .par_screen(
      type = "vector",
      input = scr_sf,
      input_id = "FIPS",
      out_class = "sf"
    )
  testthat::expect_s3_class(scr_sfsf, "sf")
  testthat::expect_s4_class(scr_sfterra, "SpatVector")

  # terra into sf
  scr_terrasf <-
    .par_screen(
      type = "vector",
      input = scr_terra,
      input_id = "FIPS",
      out_class = "sf"
    )
  # terra (as is)
  scr_terraterra <-
    .par_screen(
      type = "vector",
      input = scr_terra,
      input_id = "FIPS",
      out_class = "terra"
    )

  testthat::expect_s3_class(scr_terrasf, "sf")
  testthat::expect_s4_class(scr_terraterra, "SpatVector")

})


testthat::test_that(".par_screen -- raster", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  # withr::local_package("chopin")
  withr::local_options(
    list(sf_use_s2 = FALSE)
  )
  withr::with_dir(testthat::test_path("../.."), devtools::load_all())
  # Reading data
  ## NC counties polygon
  bundleras <- system.file("ex/elev.tif", package = "terra")

  scr_terra <-
    .par_screen(
      type = "raster",
      input = bundleras,
      input_id = NULL,
      out_class = "terra"
    )

  # in "raster" mode, out_class = "sf" is ignored.
  scr_sf <-
    .par_screen(
      type = "raster",
      input = bundleras,
      input_id = NULL,
      out_class = "sf"
    )

  testthat::expect_s4_class(scr_sf, "SpatRaster")
  testthat::expect_s4_class(scr_terra, "SpatRaster")

  # input_id is ignored
  scr_trid <-
    .par_screen(
      type = "raster",
      input = bundleras,
      input_id = "FIPS",
      out_class = "terra"
    )
  testthat::expect_s4_class(scr_trid, "SpatRaster")

})



### par_grid tests ####
testthat::test_that("par_grid -- plain mode with raster path", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("future.mirai")
  withr::local_package("dplyr")
  # withr::local_package("chopin")
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
  # withr::local_package("chopin")
  withr::local_options(
    list(sf_use_s2 = FALSE,
         future.plan = "mirai_multisession")
  )
  withr::local_seed(202407)
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
  # withr::local_package("chopin")
  withr::local_options(
    list(sf_use_s2 = FALSE,
         future.plan = "mirai_multisession")
  )
  withr::local_seed(202407)

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
  # withr::local_package("chopin")
  withr::local_options(
    list(sf_use_s2 = FALSE,
         future.plan = "mirai_multisession",
         rlib_message_verbosity = "warning")
  )
  withr::local_seed(202407)

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
    paste0(
      "terra inputs detected in both x and y. ",
      "Please replace x and y to file paths to proceed."
    )
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



### par_hierarchy tests ####
testthat::test_that(
  "par_hierarchy -- raster path input and spatraster fallback ", {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("future")
    withr::local_package("future.apply")
    withr::local_package("future.mirai")
    withr::local_package("dplyr")
    # withr::local_package("chopin")

    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.resolve.recursive = 2L,
        future.plan = "mirai_multisession"
      )
    )
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
        par_hierarchy(
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

  }
)


testthat::test_that("par_hierarchy: multicore-SpatRaster input", {
  testthat::skip_on_os("windows")

  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("dplyr")
  # withr::local_package("chopin")
  withr::local_options(
    list(
      sf_use_s2 = FALSE,
      future.resolve.recursive = 2L,
      future.plan = future::multicore(workers = 2L)
    )
  )
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

  testthat::expect_error(
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
      ),
    "The length of regions_id is not valid."
  )

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

})


testthat::test_that("par_hierarchy: multicore-generic function dispatch", {
  testthat::skip_on_os("windows")
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("dplyr")
  # withr::local_package("chopin")

  withr::local_options(
    list(
      sf_use_s2 = FALSE,
      future.resolve.recursive = 2L
    )
  )

  ncpath <- system.file("extdata/nc_hierarchy.gpkg", package = "chopin")
  nccnty <- terra::vect(ncpath, layer = "county")
  nctrct <- sf::st_read(ncpath, layer = "tracts")
  nctrct <- terra::vect(nctrct)
  ncelevpath <- system.file("extdata/nc_srtm15_otm.tif", package = "chopin")
  ncelev <- terra::rast(ncelevpath)
  ncroad <- system.file("extdata/ncroads_first.gpkg", package = "chopin")
  ncsamp <-
    terra::spatSample(
      terra::ext(ncelev),
      1e4L,
      lonlat = FALSE,
      as.points = TRUE
    )
  ncsamp$kid <- sprintf("K-%05d", seq(1, nrow(ncsamp)))
  ncsamp <- terra::set.crs(ncsamp, "EPSG:5070")

  nctrctc <- terra::centroids(nctrct)
  ncroadv <- terra::vect(ncroad)

  future::plan(future::multicore(workers = 2L))
  # no errors since 100km buffer is enough to capture
  # nearest road for coastal tracts
  resnas0 <-
    par_hierarchy(
      regions = nccnty,
      regions_id = "GEOID",
      pad_y = TRUE,
      pad = 50000,
      .debug = TRUE,
      fun_dist = nearest,
      x = nctrctc,
      y = ncroadv
    )

  # no errors since 100km buffer is enough to capture
  # nearest road for coastal tracts
  resnas <-
    par_hierarchy(
      regions = nccnty,
      regions_id = "GEOID",
      pad_y = TRUE,
      pad = 100000,
      .debug = TRUE,
      fun_dist = nearest,
      x = nctrctc,
      y = ncroadv
    )

  # resnas0 and resnas must have different #rows
  testthat::expect_true(nrow(resnas) > nrow(resnas0))

  # regions are sf object
  nctrcc <- terra::centroids(nctrct)
  testthat::expect_no_error(
    resnasx <-
      par_hierarchy(
        regions = sf::st_as_sf(nccnty),
        regions_id = "GEOID",
        pad_y = FALSE,
        fun_dist = extract_at,
        x = ncelev,
        y = nctrcc,
        id = "GEOID",
        radius = 1e3L,
        .standalone = FALSE,
        .debug = TRUE
      )
  )

  testthat::expect_no_error(
    suppressWarnings(
      resnasz <-
        par_hierarchy(
          regions = nccnty,
          .debug = TRUE,
          pad_y = TRUE,
          regions_id = "GEOID",
          fun_dist = nearest,
          x = ncsamp,
          y = nctrct
        )
    )
  )
  future::plan(future::sequential)
})


testthat::test_that("par_hierarchy: define level by substring", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("future.mirai")
  withr::local_package("dplyr")
  # withr::local_package("chopin")

  withr::local_options(
    list(
      sf_use_s2 = FALSE,
      future.resolve.recursive = 2L,
      future.plan = "mirai_multisession"
    )
  )
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
      par_hierarchy(
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
      par_hierarchy(
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
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("dplyr")
  # withr::local_package("chopin")

  withr::local_options(
    list(
      sf_use_s2 = FALSE,
      future.resolve.recursive = 2L
    )
  )

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
  future::plan(future.mirai::mirai_multisession, workers = 4L)
  testthat::expect_error(
    res <-
      suppressWarnings(
        par_grid(
          grids = nccompreg,
          fun_dist = nearest,
          .debug = TRUE,
          x = pnts,
          y = rd1
        )
      ),
    "terra inputs detected in both x and y. Please replace x and y to file paths to proceed."
  )

})


testthat::test_that(
  "par_multirasters -- character filenames, character y",
  {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("future")
    withr::local_package("future.mirai")
    withr::local_package("future.apply")
    withr::local_package("dplyr")
    # withr::local_package("chopin")
    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.plan = "mirai_multisession",
        future.resolve.recursive = 2L
      )
    )
    future::plan(future.mirai::mirai_multisession, workers = 2L)
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
        res <- par_multirasters(
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

    future::plan(future::sequential)
  }
)


testthat::test_that(
  "par_multirasters -- terra function dispatch",
  {
    testthat::skip_on_os("windows")

    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("future")
    withr::local_package("future.mirai")
    withr::local_package("future.apply")
    withr::local_package("dplyr")
    # withr::local_package("chopin")
    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.resolve.recursive = 2L
      )
    )
    future::plan(future::multicore, workers = 2L)
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
        res <- par_multirasters(
          filenames = testfiles,
          .debug = TRUE,
          fun_dist = extract,
          y = ncpath,
          x = ncelev,
          fun = mean
        )
      )
    )
    future::plan(future::sequential)

  }
)



testthat::test_that(
  "Processes are properly spawned and compute over multirasters",
  {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("future")
    withr::local_package("future.mirai")
    withr::local_package("future.apply")
    withr::local_package("dplyr")
    # withr::local_package("chopin")

    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.plan = "mirai_multisession",
        future.resolve.recursive = 2L
      )
    )
    future::plan(future.mirai::mirai_multisession, workers = 2L)
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
      resnas <- par_multirasters(
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
      nut <- par_multirasters(
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
    future::plan(future::sequential)

  }
)


testthat::test_that(
  "par_multirasters: sf y",
  {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("future")
    withr::local_package("future.mirai")
    withr::local_package("future.apply")
    withr::local_package("dplyr")
    # withr::local_package("chopin")

    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.plan = "mirai_multisession",
        future.resolve.recursive = 2L
      )
    )
    future::plan(future.mirai::mirai_multisession, workers = 2L)
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
      dough <- par_multirasters(
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
    testthat::expect_warning(
      nut <- par_multirasters(
        filenames = testfiles_corrupted,
        .debug = TRUE,
        fun_dist = extract_at,
        y = nccnty,
        x = ncelev,
        id = "GEOID",
        func = "mean"
      )
    )
    testthat::expect_s3_class(nut, "data.frame")
    testthat::expect_true("error_message" %in% names(nut))
    testthat::expect_true(sum(!is.na(nut$error_message)) == 1L)

    future::plan(future::sequential)
  }
)
