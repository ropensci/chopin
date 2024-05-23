
testthat::test_that("Balanced group tests", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("anticlust")
  withr::local_options(list(sf_use_s2 = FALSE))

  rv <- terra::vect(matrix(rnorm(1000, 1e3, 350), ncol = 2))
  rs <- sf::st_as_sf(rv)

  testthat::expect_no_error(
    par_group_balanced(rv, 10)
  )
  testthat::expect_no_error(
    par_group_balanced(rs, 10)
  )

  testthat::expect_error(
    par_group_balanced(rv, "NUMBER")
  )
  testthat::expect_error(
    par_group_balanced(rv, 1L)
  )
  testthat::expect_true(any("CGRIDID" %in% names(par_group_balanced(rv, 10))))

  # gridded
  testthat::expect_no_error(
    pgg_terra <- par_group_grid(rv, 10, 100)
  )
  testthat::expect_no_error(
    pgg_sf <- par_group_grid(rs, 10, 100)
  )
  testthat::expect_equal(length(pgg_terra), 2)
  testthat::expect_equal(length(pgg_sf), 2)
  testthat::expect_true(all(table(pgg_terra$original$CGRIDID) == 50))

  testthat::expect_error(
    par_group_grid(rv, NULL)
  )
  testthat::expect_error(
    par_group_grid(rv, 5L)
  )
  testthat::expect_no_error(
    par_group_grid(rv, 5L, "10000")
  )
  testthat::expect_error(
    suppressWarnings(par_group_grid(rv, 5L, "radius"))
  )
  testthat::expect_error(
    par_group_grid(rv, 5L, NA)
  )

})




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
    par_cut_coords(randpoints$x, randpoints$y[seq(1, 10)], quantiles)
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
  testthat::expect_no_warning(
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
  set.seed(202403)
  nc <- system.file("shape/nc.shp", package = "sf")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")
  nctr <- terra::vect(nc)
  ncp <- readRDS(system.file("extdata/nc_random_point.rds", package = "chopin"))
  ncp <- sf::st_transform(ncp, "EPSG:5070")
  ncrp <- sf::st_as_sf(sf::st_sample(nc, 1600L))

  gridded <-
    par_make_gridset(ncrp,
                     mode = "grid",
                     nx = 8L, ny = 5L,
                     padding = 1e4L)
  testthat::expect_message(par_merge_grid(ncrp, gridded$original, 10L))
  testthat::expect_message(par_merge_grid(ncrp, gridded$original, 2L))
  ncrp2 <- sf::st_as_sf(sf::st_sample(nc, 10000L))
  testthat::expect_message(par_merge_grid(ncrp2, gridded$original, 10L))

  ncptr <- terra::vect(ncrp)
  griddedtr <-
    par_make_gridset(ncptr,
                     mode = "grid",
                     nx = 8L, ny = 5L,
                     padding = 1e4L)
  testthat::expect_message(
    par_merge_grid(ncptr, griddedtr$original, 10L)
  )

  # pp test fails
  # Then expect warnings of "all sub-geometries for which ..."
  data("ncpoints", package = "chopin")
  ncptr2 <- terra::vect(ncpoints, geom = c("X", "Y"), keepgeom = TRUE)
  griddedtr2 <-
    par_make_gridset(ncptr2,
                     mode = "grid",
                     nx = 20L, ny = 12L,
                     padding = 1e4L)
  testthat::expect_message(
    gridmerged2 <- par_merge_grid(ncptr2, griddedtr2$original, 15L)
  )
  testthat::expect_s4_class(gridmerged2, "SpatVector")

  griddedtr22 <-
    par_make_gridset(ncptr2,
                     mode = "grid",
                     nx = 40L, ny = 20L,
                     padding = 1e4L)
  testthat::expect_message(
    gridmergedx <-
      par_merge_grid(
        ncptr2,
        griddedtr22$original,
        10L,
        merge_max = 10L
      )
  )

})


testthat::test_that("par_group_balanced returns the correct output", {
  # Create test data
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))
  ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
  nc <- terra::vect(ncpath)
  nc_rp <- terra::spatSample(nc, 1000)

  # Call the function
  testthat::expect_no_error(
    result <- par_group_balanced(nc_rp, n_clusters = 10)
  )
  testthat::expect_no_error(
    result <- par_group_balanced(sf::st_as_sf(nc_rp), n_clusters = 10)
  )

  # Perform assertions
  testthat::expect_true("CGRIDID" %in% names(result))
  testthat::expect_true(is.numeric(result$CGRIDID))
  testthat::expect_equal(length(unique(result$CGRIDID)), 10)

  # error cases
  testthat::expect_error(
    par_group_balanced(nc_rp, n_clusters = "vingt")
  )
  testthat::expect_error(
    par_group_balanced(nc_rp, n_clusters = 1L)
  )

})


testthat::test_that("par_group_grid returns the correct output", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("anticlust")
  withr::local_options(list(sf_use_s2 = FALSE))
  # Test case 1: Using the example from the documentation
  ncpath <- system.file("shape/nc.shp", package = "sf")
  nc <- sf::read_sf(ncpath)
  nc <- sf::st_transform(nc, "EPSG:5070")
  nc <- terra::vect(nc)
  nc_rp <- terra::spatSample(nc, 1000)
  testthat::expect_no_error(
    gridset <-
      par_group_grid(
        points_in = nc_rp,
        ngroups = 10L,
        padding = 10000
      )
  )
  testthat::expect_true("original" %in% names(gridset))
  testthat::expect_true("padded" %in% names(gridset))
  testthat::expect_s4_class(gridset$original, "SpatVector")
  testthat::expect_s4_class(gridset$padded, "SpatVector")

  # other cases
  testthat::expect_no_error(
    gridset <-
      par_group_grid(
        points_in = sf::st_as_sf(nc_rp),
        ngroups = 10L,
        padding = 10000
      )
  )
  testthat::expect_message(
    gridset2 <-
      par_group_grid(
        points_in = nc_rp,
        ngroups = 10,
        padding = "10000"
      )
  )
  testthat::expect_error(
    testthat::expect_warning(
      gridset2 <-
      par_group_grid(
        points_in = nc_rp,
        ngroups = 10,
        padding = "eintausend"
      )
    )
  )
  testthat::expect_error(
    par_group_grid(
      points_in = nc_rp
    )
  )
  testthat::expect_error(
    par_group_grid(
      points_in = nc_rp,
      ngroups = 10L
    )
  )
  testthat::expect_error(
    par_group_grid(
      points_in = nc_rp,
      ngroups = 10L,
      padding = NA
    )
  )
  testthat::expect_error(
    par_group_grid(
      points_in = nc_rp,
      ngroups = 10L
    )
  )

})
