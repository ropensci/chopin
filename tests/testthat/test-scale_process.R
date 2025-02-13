### .par_screen tests ####
testthat::test_that(".par_screen -- vector", {
  testthat::skip_on_ci()
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_package("chopin")
  withr::local_options(
    list(sf_use_s2 = FALSE)
  )
  # Reading data
  ## NC counties polygon
  ncpath <- system.file("shape/nc.shp", package = "sf")
  ncpoly <- terra::vect(ncpath) %>%
    terra::project("EPSG:5070")

  scr_terra <-
    chopin:::.par_screen(
      type = "vector",
      input = ncpath,
      input_id = "FIPS",
      out_class = "terra"
    )

  scr_sf <-
    chopin:::.par_screen(
      type = "vector",
      input = ncpath,
      input_id = "FIPS",
      out_class = "sf"
    )

  testthat::expect_s3_class(scr_sf, "sf")
  testthat::expect_s4_class(scr_terra, "SpatVector")

  # sf into terra
  scr_sfterra <-
    chopin:::.par_screen(
      type = "vector",
      input = scr_sf,
      input_id = "FIPS",
      out_class = "terra"
    )
  # sf (as is)
  scr_sfsf <-
    chopin:::.par_screen(
      type = "vector",
      input = scr_sf,
      input_id = "FIPS",
      out_class = "sf"
    )
  testthat::expect_s3_class(scr_sfsf, "sf")
  testthat::expect_s4_class(scr_sfterra, "SpatVector")

  # terra into sf
  scr_terrasf <-
    chopin:::.par_screen(
      type = "vector",
      input = scr_terra,
      input_id = "FIPS",
      out_class = "sf"
    )
  # terra (as is)
  scr_terraterra <-
    chopin:::.par_screen(
      type = "vector",
      input = scr_terra,
      input_id = "FIPS",
      out_class = "terra"
    )

  testthat::expect_s3_class(scr_terrasf, "sf")
  testthat::expect_s4_class(scr_terraterra, "SpatVector")

})


testthat::test_that(".par_screen -- raster", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("dplyr")
  withr::local_package("chopin")
  withr::local_options(
    list(sf_use_s2 = FALSE)
  )
  # Reading data
  ## NC counties polygon
  bundleras <- system.file("ex/elev.tif", package = "terra")

  scr_terra <-
    chopin:::.par_screen(
      type = "raster",
      input = bundleras,
      input_id = NULL,
      out_class = "terra"
    )

  # in "raster" mode, out_class = "sf" is ignored.
  scr_sf <-
    chopin:::.par_screen(
      type = "raster",
      input = bundleras,
      input_id = NULL,
      out_class = "sf"
    )

  testthat::expect_s4_class(scr_sf, "SpatRaster")
  testthat::expect_s4_class(scr_terra, "SpatRaster")

  # input_id is ignored
  scr_trid <-
    chopin:::.par_screen(
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
    readRDS(rppath)
  ncpnts <- terra::vect(ncpnts)
  ncpnts <- terra::project(ncpnts, "EPSG:5070")

  ## Simulated raster: 500 by 1100
  ras <- terra::rast(ncpoly, nrow = 500, ncol = 1100)
  terra::values(ras) <- rgamma(5.5e5, 4, 2)

  # Using raster path
  ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
  terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
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
  future::plan(future::sequential)
})


testthat::test_that("par_grid -- grid_advanced mode", {
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
  withr::local_seed(202407)
  # Reading data
  nccnty <- sf::st_read(
    system.file("shape/nc.shp", package = "sf")
  )[seq_len(30L), ]
  nccnty <- sf::st_transform(nccnty, "EPSG:5070")
  nccntygrid <- sf::st_make_grid(nccnty, n = c(200, 100))
  nccntygrid <- sf::st_as_sf(nccntygrid)
  nccntygrid$GEOID <- sprintf("%05d", seq_len(nrow(nccntygrid)))
  suppressWarnings(
    nccntygrid <- sf::st_intersection(nccntygrid, nccnty)
  )

  ## Generated random points in NC
  data("ncpoints", package = "chopin")
  ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
  ncp$pid <- seq_len(nrow(ncp))
  ncpnts <- terra::vect(ncp)

  ## Simulated raster: 500 by 1100
  ras <- terra::rast(nccnty, nrow = 500, ncol = 1100)
  terra::values(ras) <- rgamma(5.5e5, 4, 2)

  # Using raster path
  ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
  terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
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

  testthat::expect_no_error(
    ppg2 <-
      par_pad_grid(
        input = ncsamp,
        mode = "grid_advanced",
        nx = 15L,
        ny = 10L,
        padding = 3e4L,
        grid_min_features = 50L,
        merge_max = 5L
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

  future::plan(future::sequential)
  mirai::daemons(0)
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
  withr::local_seed(202407)

  # Reading data
  ## NC counties polygon
  ncpath <- system.file("shape/nc.shp", package = "sf")
  ncpoly <- terra::vect(ncpath) %>%
    terra::project("EPSG:5070")

  nccnty <- sf::st_read(
    system.file("shape/nc.shp", package = "sf")
  )[seq_len(30L), ]
  nccnty <- sf::st_transform(nccnty, "EPSG:5070")
  nccntygrid <- sf::st_make_grid(nccnty, n = c(200, 100))
  nccntygrid <- sf::st_as_sf(nccntygrid)
  nccntygrid$GEOID <- sprintf("%05d", seq_len(nrow(nccntygrid)))
  suppressWarnings(
    nccntygrid <- sf::st_intersection(nccntygrid, nccnty)
  )

  ## Generated random points in NC
  data("ncpoints", package = "chopin")
  ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
  ncp$pid <- seq_len(nrow(ncp))
  ncpnts <- terra::vect(ncp)

  ## Simulated raster: 500 by 1100
  ras <- terra::rast(nccnty, nrow = 500, ncol = 1100)
  terra::values(ras) <- rgamma(5.5e5, 4, 2)

  # Using raster path
  ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
  terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
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
        y = ncp,
        x = ncelev,
        qsegs = 90L,
        radius = 5e3L,
        id = "pid"
      )
    )

  testthat::expect_s3_class(resq, "data.frame")
  future::plan(future::sequential)
  mirai::daemons(0)
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
         rlib_message_verbosity = "warning")
  )
  future::plan(future.mirai::mirai_multisession, workers = 2L)
  withr::local_seed(202407)

  # Reading data
  ## NC counties polygon
  ncpath <- system.file("shape/nc.shp", package = "sf")
  ncpoly <- terra::vect(ncpath) %>%
    terra::project("EPSG:5070")

  nccnty <- sf::st_read(
    system.file("shape/nc.shp", package = "sf")
  )[seq_len(30L), ]
  nccnty <- sf::st_transform(nccnty, "EPSG:5070")
  nccntygrid <- sf::st_make_grid(nccnty, n = c(200, 100))
  nccntygrid <- sf::st_as_sf(nccntygrid)
  nccntygrid$GEOID <- sprintf("%05d", seq_len(nrow(nccntygrid)))
  suppressWarnings(
    nccntygrid <- sf::st_intersection(nccntygrid, nccnty)
  )

  ## Generated random points in NC
  data("ncpoints", package = "chopin")
  ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
  ncp$pid <- seq_len(nrow(ncp))
  ncpnts <- terra::vect(ncp)

  ## Simulated raster: 500 by 1100
  ras <- terra::rast(nccnty, nrow = 500, ncol = 1100)
  terra::values(ras) <- rgamma(5.5e5, 4, 2)

  # Using raster path
  ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
  terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
  ncelev <- terra::rast(ncelevpath)
  ncpntst <- ncpnts


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

  future::plan(future::sequential)
  mirai::daemons(0)
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
    withr::local_package("chopin")

    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.resolve.recursive = 2L
      )
    )
    future::plan(future.mirai::mirai_multisession, workers = 2L)
    withr::local_seed(202407)

    nccnty <- sf::st_read(
      system.file("shape/nc.shp", package = "sf")
    )[seq_len(30L), ]
    nccnty <- sf::st_transform(nccnty, "EPSG:5070")
    nccntygrid <- sf::st_make_grid(nccnty, n = c(200, 100))
    nccntygrid <- sf::st_as_sf(nccntygrid)
    nccntygrid$GEOID <- sprintf("%05d", seq_len(nrow(nccntygrid)))
    suppressWarnings(
      nccntygrid <- sf::st_intersection(nccntygrid, nccnty)
    )

    ## Generated random points in NC
    data("ncpoints", package = "chopin")
    ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
    ncp$pid <- seq_len(nrow(ncp))
    ncpnts <- terra::vect(ncp)

    ## Simulated raster: 500 by 1100
    ras <- terra::rast(nccnty, nrow = 500, ncol = 1100)
    terra::values(ras) <- rgamma(5.5e5, 4, 2)

    # Using raster path
    ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
    terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
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
          regions_id = "FIPS",
          .debug = TRUE,
          fun_dist = extract_at,
          y = nccntygrid,
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
            regions_id = "FIPS",
            .debug = TRUE,
            fun_dist = extract_at,
            y = nccntygrid,
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

    future::plan(future::sequential)
    mirai::daemons(0)

  }
)


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
      future.resolve.recursive = 2L,
      future.plan = future::multicore(workers = 2L)
    )
  )
  withr::local_seed(202407)

  nccnty <- sf::st_read(
    system.file("shape/nc.shp", package = "sf")
  )
  nccnty <- sf::st_transform(nccnty, "EPSG:5070")
  nccntygrid <- sf::st_make_grid(nccnty, n = c(200, 100))
  nccntygrid <- sf::st_as_sf(nccntygrid)
  nccntygrid$GEOID <- sprintf("%05d", seq_len(nrow(nccntygrid)))
  suppressWarnings(
    nccntygrid <- sf::st_intersection(nccntygrid, nccnty)
  )

  ## Generated random points in NC
  data("ncpoints", package = "chopin")
  ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
  ncp$pid <- seq_len(nrow(ncp))
  ncpnts <- terra::vect(ncp)

  ## Resampled SRTM data in NC
  ras <- terra::rast(nccnty, nrow = 1000, ncol = 2200)
  terra::values(ras) <- rgamma(2.2e6, 4, 2)

  # Using raster path
  ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
  terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
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
        y = nccntygrid,
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
        y = nccntygrid,
        x = ncelev,
        id = "GEOID",
        func = "mean"
      ),
    "The length of regions_id is not valid."
  )

  future::plan(future::sequential)
  mirai::daemons(0)
})


# testthat::test_that("par_hierarchy: multicore-generic function dispatch", {
#   testthat::skip_on_os("windows")
#   withr::local_package("terra")
#   withr::local_package("sf")
#   withr::local_package("future")
#   withr::local_package("future.apply")
#   withr::local_package("dplyr")
#   withr::local_package("chopin")

#   withr::local_options(
#     list(
#       sf_use_s2 = FALSE,
#       future.resolve.recursive = 2L
#     )
#   )
#   withr::local_seed(202502)
#   future::plan(future::sequential)
#   future::plan(future::multicore(workers = 2L))

#   nccnty <- sf::st_read(
#     system.file("shape/nc.shp", package = "sf")
#   )
#   nccnty <- sf::st_transform(nccnty, "EPSG:5070")
#   nccntygrid <- sf::st_make_grid(nccnty, n = c(200, 100))
#   nccntygrid <- sf::st_as_sf(nccntygrid)
#   nccntygrid$GEOID <- sprintf("%05d", seq_len(nrow(nccntygrid)))
#   suppressWarnings(
#     nccntygrid <- sf::st_intersection(nccntygrid, nccnty)
#   )

#   ## Generated random points in NC
#   data("ncpoints", package = "chopin")
#   ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
#   ncp$pid <- seq_len(nrow(ncp))
#   ncpnts <- terra::vect(ncp)

#   ## Resampled SRTM data in NC
#   ras <- terra::rast(nccnty, nrow = 1000, ncol = 2200)
#   terra::values(ras) <- rgamma(2.2e6, 4, 2)

#   # Using raster path
#   ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
#   terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
#   ncelev <- terra::rast(ncelevpath)
#   ncroad <- terra::vect(roadpath)
#   ncsamp <-
#     terra::spatSample(
#       terra::ext(ncelev),
#       1e4L,
#       lonlat = FALSE,
#       as.points = TRUE
#     )
#   ncsamp$kid <- sprintf("K-%05d", seq(1, nrow(ncsamp)))
#   ncsamp <- terra::set.crs(ncsamp, "EPSG:5070")

#   nctrctc <- terra::centroids(terra::vect(nccntygrid))
#   ncroadv <- ncroad

#   # no errors since 100km buffer is enough to capture
#   # nearest road for coastal tracts
#   resnas0 <-
#     par_hierarchy(
#       regions = nccnty,
#       regions_id = "FIPS",
#       pad_y = TRUE,
#       pad = 50000,
#       .debug = TRUE,
#       fun_dist = nearest,
#       x = nctrctc,
#       y = ncroadv
#     )

#   testthat::skip_on_os("windows")

#   # no errors since 100km buffer is enough to capture
#   # nearest road for coastal tracts
#   resnas <-
#     par_hierarchy(
#       regions = nccnty,
#       regions_id = "FIPS",
#       pad_y = TRUE,
#       pad = 100000,
#       .debug = TRUE,
#       fun_dist = nearest,
#       x = nctrctc,
#       y = ncroadv
#     )

#   # resnas0 and resnas must have different #rows
#   testthat::expect_true(nrow(resnas) > nrow(resnas0))

#   testthat::skip_on_os("windows")
#   # regions are sf object
#   nctrcc <- terra::centroids(terra::vect(nccntygrid))
#   testthat::expect_no_error(
#     resnasx <-
#       par_hierarchy(
#         regions = sf::st_as_sf(nccnty),
#         regions_id = "FIPS",
#         pad_y = FALSE,
#         fun_dist = extract_at,
#         x = ncelev,
#         y = nctrcc,
#         id = "GEOID",
#         radius = 1e3L,
#         .standalone = FALSE,
#         .debug = TRUE
#       )
#   )

#   testthat::skip_on_os("windows")
#   testthat::expect_no_error(
#     suppressWarnings(
#       resnasz <-
#         par_hierarchy(
#           regions = nccnty,
#           .debug = TRUE,
#           pad_y = TRUE,
#           regions_id = "FIPS",
#           fun_dist = nearest,
#           x = ncsamp,
#           y = terra::vect(nccntygrid)
#         )
#     )
#   )
#   future::plan(future::sequential)
#   mirai::daemons(0)
# })


testthat::test_that("par_hierarchy: define level by substring", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("mirai")
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
  withr::local_seed(202407)

  nccnty <- sf::st_read(
    system.file("shape/nc.shp", package = "sf")
  )
  nccnty <- sf::st_transform(nccnty, "EPSG:5070")
  nccntygrid <- sf::st_make_grid(nccnty, n = c(200, 100))
  nccntygrid <- sf::st_as_sf(nccntygrid)
  nccntygrid$GEOID <- sprintf("%05d", seq_len(nrow(nccntygrid)))
  suppressWarnings(
    nccntygrid <- sf::st_intersection(nccntygrid, nccnty)
  )

  ## Generated random points in NC
  data("ncpoints", package = "chopin")
  ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
  ncp$pid <- seq_len(nrow(ncp))
  ncpnts <- terra::vect(ncp)

  ## Resampled SRTM data in NC
  ras <- terra::rast(nccnty, nrow = 1000, ncol = 2200)
  terra::values(ras) <- rgamma(2.2e6, 4, 2)

  # Using raster path
  ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
  terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
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
        regions = nccntygrid,
        regions_id = "FIPS",
        length_left = 5L,
        .debug = TRUE,
        fun_dist = extract_at,
        y = nccntygrid,
        x = ncelev,
        id = "GEOID",
        func = "mean"
      )
  )
  testthat::expect_true(is.data.frame(residc))

  # bare integers with different lengths: warning message
  nccntygrid$qid <- seq_len(nrow(nccntygrid))
  testthat::expect_message(
    residc <-
      par_hierarchy(
        regions = nccntygrid,
        regions_id = "qid",
        length_left = 2L,
        .debug = TRUE,
        fun_dist = extract_at,
        y = nccntygrid,
        x = ncelev,
        id = "GEOID",
        func = "mean"
      )
  )

  future::plan(future::sequential)
  mirai::daemons(0)
})



testthat::test_that("generic function should be parallelized properly", {
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

  # main test
  ## Generated random points in NC
  data("ncpoints", package = "chopin")
  ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
  ncp$pid <- seq_len(nrow(ncp))
  pnts <- terra::vect(ncp)

  rd1 <- terra::vect(roadpath)

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

  future::plan(future::sequential)
  mirai::daemons(0)
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
    withr::local_package("chopin")
    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.resolve.recursive = 2L
      )
    )
    future::plan(future.mirai::mirai_multisession, workers = 2L)


    nccnty <- sf::st_read(
      system.file("shape/nc.shp", package = "sf")
    )
    nccnty <- sf::st_transform(nccnty, "EPSG:5070")
    ncpath <- file.path(tempdir(check = TRUE), "nccnty.gpkg")
    sf::st_write(nccnty, ncpath, "nccnty", delete_dsn = TRUE)

    ## Generated random points in NC
    data("ncpoints", package = "chopin")
    ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
    ncp$pid <- seq_len(nrow(ncp))
    ncpnts <- terra::vect(ncp)

    ## Simulated raster: 500 by 1100
    ras <- terra::rast(nccnty, nrow = 500, ncol = 1100)
    terra::values(ras) <- rgamma(5.5e5, 4, 2)

    # Using raster path
    ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
    terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
    ncelev <- terra::rast(ncelevpath)

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
    mirai::daemons(0)
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
    withr::local_package("chopin")
    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.resolve.recursive = 2L
      )
    )
    future::plan(future::multicore, workers = 2L)

    nccnty <- sf::st_read(
      system.file("shape/nc.shp", package = "sf")
    )
    nccnty <- sf::st_transform(nccnty, "EPSG:5070")
    ncpath <- file.path(tempdir(check = TRUE), "nccnty.gpkg")
    sf::st_write(nccnty, ncpath, "nccnty", delete_dsn = TRUE)

    ## Generated random points in NC
    data("ncpoints", package = "chopin")
    ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
    ncp$pid <- seq_len(nrow(ncp))
    ncpnts <- terra::vect(ncp)

    ## Simulated raster: 500 by 1100
    ras <- terra::rast(nccnty, nrow = 500, ncol = 1100)
    terra::values(ras) <- rgamma(5.5e5, 4, 2)

    # Using raster path
    ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
    terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
    ncelev <- terra::rast(ncelevpath)

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
    mirai::daemons(0)
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
    withr::local_package("chopin")

    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.plan = "mirai_multisession",
        future.resolve.recursive = 2L
      )
    )
    future::plan(future.mirai::mirai_multisession, workers = 2L)

    nccnty <- sf::st_read(
      system.file("shape/nc.shp", package = "sf")
    )
    nccnty <- sf::st_transform(nccnty, "EPSG:5070")
    ncpath <- file.path(tempdir(check = TRUE), "nccnty.gpkg")
    sf::st_write(nccnty, ncpath, "nccnty", delete_dsn = TRUE)

    ## Generated random points in NC
    data("ncpoints", package = "chopin")
    ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
    ncp$pid <- seq_len(nrow(ncp))
    ncpnts <- terra::vect(ncp)

    ## Resampled SRTM data in NC
    ras <- terra::rast(nccnty, nrow = 1000, ncol = 2200)
    terra::values(ras) <- rgamma(2.2e6, 4, 2)

    # Using raster path
    ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
    terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
    ncelev <- terra::rast(ncelevpath)

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
    mirai::daemons(0)
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
    withr::local_package("chopin")

    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.plan = "mirai_multisession",
        future.resolve.recursive = 2L
      )
    )
    future::plan(future.mirai::mirai_multisession, workers = 2L)


    nccnty <- sf::st_read(
      system.file("shape/nc.shp", package = "sf")
    )
    nccnty <- sf::st_transform(nccnty, "EPSG:5070")
    ncpath <- file.path(tempdir(check = TRUE), "nccnty.gpkg")
    sf::st_write(nccnty, ncpath, "nccnty", delete_dsn = TRUE)

    ## Generated random points in NC
    data("ncpoints", package = "chopin")
    ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
    ncp$pid <- seq_len(nrow(ncp))
    ncpnts <- terra::vect(ncp)

    ## Simulated raster: 500 by 1100
    ras <- terra::rast(nccnty, nrow = 500, ncol = 1100)
    terra::values(ras) <- rgamma(5.5e5, 4, 2)

    # Using raster path
    ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
    terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
    ncelev <- terra::rast(ncelevpath)

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
    mirai::daemons(0)
  }
)


# par_map_args tests ####
testthat::test_that("par_convert_f works", {
  example_fun <- function(x, y, z = 1) {
    return(c(x = x, y = y, z = z))
  }

  # Example usage of map_args_xy
  testthat::expect_no_error(
    resultfoo <-
      par_convert_f(
        fun = example_fun,
        arg_map = c(a = "x", b = "y")
      )
  )

  testthat::expect_true(is.function(resultfoo))
  result <- resultfoo(a = 10, b = 20, z = 5)
  testthat::expect_true(is.vector(result))
})
