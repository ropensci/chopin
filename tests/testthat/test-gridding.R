# Generated from chopin_rmarkdown_litr.rmd: do not edit by hand

testthat::test_that("Quantile cut tests", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  rv <- terra::vect(matrix(rpois(100, 8), ncol = 2))
  rs <- sf::st_as_sf(rv)

  testthat::expect_no_error(
    par_cut_coords(rv, NULL, par_def_q(4L))
  )
  testthat::expect_no_error(
    par_cut_coords(rs, NULL, par_def_q(4L))
  )

  # numeric cases
  randpoints <- data.frame(
    x = runif(1000, 0, 100),
    y = runif(1000, 0, 100)
  )
  testthat::expect_no_error(
    quantiles <- par_def_q(4L)
  )
  testthat::expect_equal(length(quantiles), 5)
  testthat::expect_error(
    par_def_q(1L)
  )

  testthat::expect_no_error(
    par_cut_coords(randpoints$x, randpoints$y, quantiles)
  )
  testthat::expect_error(
    par_cut_coords(randpoints$x, randpoints$y[seq(1, 100)], quantiles)
  )

  testthat::expect_equal(
    par_cut_coords(randpoints$x, randpoints$y, quantiles) |>
      nrow(),
    16
  )

  testthat::expect_error(
    par_cut_coords(randpoints$x, c(1, 0, 4), quantiles)
  )

  # polygon case
  ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
  nc <- sf::st_read(ncpath)
  testthat::expect_warning(
    testthat::expect_warning(
      par_cut_coords(nc, NULL, par_def_q(3L))
    )
  )

})



testthat::test_that("Grid split is well done.", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  # starts from sf/stars
  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")

  testthat::expect_no_error(
    par_make_gridset(nc, mode = "grid", padding = 3e4L)
  )
  ncgrid <- par_make_gridset(nc, mode = "grid", padding = 3e4L)
  testthat::expect_s3_class(ncgrid$original, "sf")

  nctr <- terra::vect(nc)
  testthat::expect_no_error(
    par_make_gridset(nctr, mode = "grid", padding = 3e4L)
  )
  ncgridtr <- par_make_gridset(nctr, mode = "grid", padding = 3e4L)
  testthat::expect_s4_class(ncgridtr$original, "SpatVector")

  testthat::expect_error(
    par_make_gridset(nctr, mode = "grid", nx = 3.6, ny = 10L, padding = 3e4L)
  )
  testthat::expect_error(
    suppressWarnings(
      par_make_gridset(nctr, mode = "grid", nx = 4L, ny = 10L, padding = "july")
    )
  )

  ncp <- readRDS(system.file("extdata/nc_random_point.rds", package = "chopin"))
  ncp <- sf::st_transform(ncp, "EPSG:5070")
  ncrp <- sf::st_as_sf(sf::st_sample(nc, 1000L))

  # Points
  testthat::expect_warning(
    par_make_gridset(
      ncp,
      mode = "grid_advanced",
      padding = 3e4L,
      grid_min_features = 20L
    )
  )
  # Points
  testthat::expect_no_error(
    par_make_gridset(
      ncp,
      mode = "grid_quantile",
      padding = 3e4L,
      quantiles = par_def_q(5L)
    )
  )

})


testthat::test_that("Grid merge is well done.", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("igraph")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))
  withr::local_seed(202403)

  nc <- system.file("shape/nc.shp", package = "sf")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")
  nctr <- terra::vect(nc)
  ncp <- readRDS(system.file("extdata/nc_random_point.rds", package = "chopin"))
  ncp <- sf::st_transform(ncp, "EPSG:5070")
  ncrp <- sf::st_as_sf(sf::st_sample(nc, 500L))

  gridded <-
    par_make_gridset(ncrp,
                     mode = "grid",
                     nx = 8L, ny = 5L,
                     padding = 1e4L)
  # suppress warnings for "all sub-geometries for which ..."
  testthat::expect_warning(par_merge_grid(ncrp, gridded$original, 10L))
  testthat::expect_error(par_merge_grid(ncrp, gridded$original, 2L))

  ncptr <- terra::vect(ncrp)
  griddedtr <-
    par_make_gridset(ncptr,
                     mode = "grid",
                     nx = 8L, ny = 5L,
                     padding = 1e4L)
  testthat::expect_warning(par_merge_grid(ncptr, griddedtr$original, 25L))

})

