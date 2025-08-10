## par_pad_grid ####
testthat::test_that("par_pad_grid with sf inputs", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  # starts from sf/stars
  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")

  # main tests
  ncgrid <- par_pad_grid(nc, mode = "grid", padding = 3e4L)
  testthat::expect_s3_class(ncgrid$original, "sf")

})

testthat::test_that("par_pad_grid with terra inputs", {
  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")
  nctr <- terra::vect(nc)
  ncgridtr <- par_pad_grid(nctr, mode = "grid", padding = 3e4L)
  testthat::expect_s4_class(ncgridtr$original, "SpatVector")

  # non-integer nx, ny
  testthat::expect_error(
    par_pad_grid(nctr, mode = "grid", nx = 3.6, ny = 10L, padding = 3e4L),
    "nx, ny must be integer."
  )
 
  testthat::expect_error(
    testthat::expect_warning(
      testthat::expect_message(
        par_pad_grid(nctr, mode = "grid", nx = 4L, ny = 10L, padding = "july"),
        "padding should be numeric. Try converting padding to numeric..."
      ),
      "NAs introduced by coercion"
    ),
    "padding is not convertible to numeric or converted to NA."
  )

})

testthat::test_that("par_pad_grid with other modes", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")

  data("ncpoints", package = "chopin")
  ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
  ncp$pid <- seq_len(nrow(ncp))
  ncrp <- sf::st_as_sf(sf::st_sample(nc, 1000L))

  # Points
  testthat::expect_message(
    testthat::expect_message(
      ppg_adv <-
        par_pad_grid(
          nx = 10L, ny = 10L,
          input = ncp,
          mode = "grid_advanced",
          padding = 3e4L,
          grid_min_features = 10L
        ),
      "Switch sf class to terra..."
    ),
    "Switch terra class to sf..."
  )
  testthat::expect_lt(nrow(ppg_adv$original), 100)

  # Points -- grid_quantile mode
  testthat::expect_no_error(
    par_pad_grid(
      ncp,
      mode = "grid_quantile",
      padding = 3e4L,
      quantiles = chopin:::par_def_q(5L)
    )
  )

})

## par_make_grid
testthat::test_that("par_make_grid file path input", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  nc <- system.file("shape/nc.shp", package = "sf")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")
  suppressWarnings(ncc <- sf::st_centroid(nc, of_largest_polygon = TRUE))
  tmpdir <- tempdir()
  filepath <- file.path(tmpdir, "cents.shp")
  sf::st_write(ncc, filepath, append = FALSE)
  testthat::expect_no_error(
    ncc_mg <- chopin:::par_make_grid(filepath, ncutsx = 10L, ncutsy = 10L)
  )
  testthat::expect_true(inherits(ncc_mg, "sf"))
})



## grid merge ####
testthat::test_that("Grid merging internal function too low threshold-- ", {
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

  data("ncpoints", package = "chopin")
  ncp <- sf::st_as_sf(ncpoints, coords = c("X", "Y"), crs = "EPSG:5070")
  ncrp <- sf::st_as_sf(sf::st_sample(nc, 1600L))

  gridded <-
    par_pad_grid(
      ncrp,
      mode = "grid",
      nx = 8L, ny = 5L,
      padding = 1e4L
    )

  # messaging: too low threshold for merging
  # in the case below, the grids with less than 2 points
  # are too dispersed to be merged (i.e., no rook neighbors among them)
  testthat::expect_message(
    par_merge_grid(ncrp, gridded$original, 2L),
    paste0(
      "Threshold is too low. Return the original grid.\n",
      "Please try higher threshold. your threshold: 2\n",
      "Top-10 non-zero number of points in grids: ",
      "2, 2, 5, 6, 11, 12, 15, 16, 25, 29"
    )
  )
  ncrp2 <- sf::st_as_sf(sf::st_sample(nc, 10000L))

  # points are too dense such that there are no grids to merge
  testthat::expect_message(
    par_merge_grid(ncrp2, gridded$original, 10L),
    "No grid to merge."
  )

  # terra SpatVector input
  ncptr <- terra::vect(ncrp2)
  griddedtr <-
    par_pad_grid(
      ncptr,
      mode = "grid",
      nx = 8L, ny = 5L,
      padding = 1e4L
    )
  testthat::expect_message(
    par_merge_grid(ncptr, griddedtr$original, 10L)
  )
})

testthat::test_that("Grid merging internal -- Polsby-Popper test fails.", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("igraph")
  withr::local_package("dplyr")

  # pp test fails
  # Then expect warnings of "all sub-geometries for which ..."
  data("ncpoints", package = "chopin")
  ncptr2 <- terra::vect(ncpoints, geom = c("X", "Y"), keepgeom = TRUE)

  griddedtr22 <-
    par_pad_grid(
      ncptr2,
      mode = "grid",
      nx = 40L, ny = 20L,
      padding = 1e4L
    )
  testthat::expect_message(
    {
      gridmergedx <-
        par_merge_grid(
          ncptr2,
          griddedtr22$original,
          10L,
          merge_max = 10L
        )
    },
    "The merged polygons have too complex shapes.\nIncrease threshold or use the original grids."
  )

})

testthat::test_that("par_pad_balanced -- input validity", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("anticlust")
  withr::local_options(list(sf_use_s2 = FALSE))

  # generate length of 500 vector
  rv <- terra::vect(matrix(rnorm(1000, 40, 10), ncol = 2), crs = "EPSG:4326")
  rs <- sf::st_as_sf(rv)

  # n_clusters type condition
  testthat::expect_error(
    par_pad_balanced(rv, "NUMBER", 0L),
    "n_clusters should be numeric."
  )
  # n_clusters range condition
  testthat::expect_error(
    par_pad_balanced(rv, 1L, 1000L),
    "n_clusters should be greater than 1."
  )
  # n_clusters can be nonnumeric which is convertible to numeric
  testthat::expect_message(
    par_pad_balanced(rv, 5L, "10000"),
    "padding should be numeric. Try converting padding to numeric..."
  )

  # par_make_balanced: ID assignment
  testthat::expect_true(
    "CGRIDID" %in% names(chopin:::par_make_balanced(rv, 10))
  )
})

testthat::test_that("par_pad_balanced -- output check", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("anticlust")
  withr::local_options(list(sf_use_s2 = FALSE))

  # generate length of 500 vector
  rv <- terra::vect(matrix(rnorm(1000, 40, 10), ncol = 2), crs = "EPSG:4326")
  rs <- sf::st_as_sf(rv)

  # gridded
  pgg_terra <- par_pad_balanced(rv, 10, 100)
  pgg_sf <- par_pad_balanced(rs, 10, 100)

  # #fields
  testthat::expect_equal(length(pgg_terra), 2)
  testthat::expect_equal(length(pgg_sf), 2)
  # equal partitioning
  testthat::expect_true(all(table(pgg_terra$original$CGRIDID) == 50))

})


testthat::test_that("par_make_balanced internal -- input validity", {
  # testthat::skip_on_ci()
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("anticlust")
  withr::local_options(list(sf_use_s2 = FALSE))

  testthat::expect_error(
    chopin:::par_make_balanced(rv, NULL)
  )
  testthat::expect_error(
    chopin:::par_make_balanced(rv, "five")
  )
  testthat::expect_error(
    suppressWarnings(chopin:::par_make_balanced(rv, 5L, "radius"))
  )
  testthat::expect_error(
    chopin:::par_make_balanced(rv, 5L, NA)
  )
})


testthat::test_that("Quantile cut internal tests", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  rv <- terra::vect(matrix(rpois(100, 8), ncol = 2))
  rs <- sf::st_as_sf(rv)

  # numeric cases
  randpoints <- data.frame(
    x = runif(1000, 0, 100),
    y = runif(1000, 0, 100)
  )
  N <- 4L
  quantiles <- chopin:::par_def_q(N)

  testthat::expect_equal(length(quantiles), 5)
  testthat::expect_error(
    chopin:::par_def_q(1L),
    "steps should be greater than 1."
  )

  testthat::expect_error(
    chopin:::par_cut_coords(randpoints$x, randpoints$y[seq(1, 10)], quantiles),
    "x and y should have the same length."
  )

  testthat::expect_error(
    chopin:::par_cut_coords(randpoints$x, c(1, 0, 4), quantiles),
    "x and y should have the same length."
  )

  # output should have Nquantiles * Nquantiles elements
  testthat::expect_equal(
    nrow(chopin:::par_cut_coords(randpoints$x, randpoints$y, quantiles)), N^2
  )

  # polygon case
  ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
  nc <- sf::st_read(ncpath)

  testthat::expect_warning(
    testthat::expect_warning(
      chopin:::par_cut_coords(nc, NULL, chopin:::par_def_q(3L)),
      "st_centroid assumes attributes are constant over geometries"
    ),
    "st_centroid does not give correct centroids for longitude/latitude"
  )

})


testthat::test_that("par_pad_balanced exceptions", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncpath <- system.file("shape/nc.shp", package = "sf")
  nc <- read_sf(ncpath)
  nc <- st_transform(nc, "EPSG:5070")
  suppressWarnings(
    ncc <- st_centroid(nc, of_largest_polygon = TRUE)
  )

  testthat::expect_error(
    chopin::par_pad_balanced(ncc, padding = 1300)
  )
  testthat::expect_error(
    chopin::par_pad_balanced(ncc, ngroups = 10L)
  )

  # tmpdir <- tempdir()
  # filepath <- file.path(tmpdir, "cents.shp")
  # sf::st_write(ncc, filepath, append = FALSE)
  # testthat::expect_no_error(
  #   ncc_ppb <- chopin::par_pad_balanced(filepath, ngroups = 10L, padding = 1e4)
  # )
  # testthat::expect_true(inherits(ncc_ppb[[1]], "SpatVector"))
})



# par_split_list tests ####
testthat::test_that("par_split_list returns correct output", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncpath <- system.file("shape/nc.shp", package = "sf")
  nc <- read_sf(ncpath)
  nc <- st_transform(nc, "EPSG:5070")
  nc_comp_region <-
    par_pad_grid(
      nc,
      mode = "grid",
      nx = 4L, ny = 2L,
      padding = 10000
    )

  ps <- par_split_list(nc_comp_region)

  testthat::expect_true(is.list(ps))
  testthat::expect_equal(length(ps), 8)

  # WKT mode
  nc_comp_region_wkt <-
    par_pad_grid(
      nc,
      mode = "grid",
      nx = 4L, ny = 2L,
      padding = 10000,
      return_wkt = TRUE
    )
  psw <- par_split_list(nc_comp_region_wkt)

  testthat::expect_true(is.list(psw))
  testthat::expect_equal(length(psw), 8)
  testthat::expect_true(is.character(psw[[1]]$original))


  # Create sample input
  gridlist <- list(data.frame(x = c(1, 2, 3, 4, 5),
                              y = c(1, 2, 3, 4, 5)),
                   data.frame(x = c(8, 6, 4, 0, -1),
                              y = c(7, 5, 3, 0, -1)))

  # Call the function
  result <- par_split_list(gridlist)

  # Check the output
  testthat::expect_equal(result[[1]]$original, data.frame(x = 1, y = 1))
  testthat::expect_equal(result[[1]]$padded, data.frame(x = 8, y = 7))
})

testthat::test_that("search_h3 returns correct H3 indices for sf input", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("h3r")
  withr::local_package("sf")
  withr::local_package("h3r")
  # Create a simple sf POINT
  pt <- st_sfc(st_point(c(18, -33)), crs = 4326)
  sf_pt <- st_sf(id = 1, geometry = pt)
  res <- 10L
  suppressWarnings(
    h3_idx <- search_h3(sf_pt, res = res)
  )
  testthat::expect_type(h3_idx, "character")
  testthat::expect_length(h3_idx, 1)
})


testthat::test_that("search_h3 errors for non-sf input", {
  withr::local_package("sf")
  withr::local_package("h3r")
  testthat::expect_error(search_h3(list(x = 1)),
  "input should be sf or SpatVector object")
})

testthat::test_that("par_make_h3 returns sf polygons with CGRIDID", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("h3r")
  withr::local_package("sf")
  withr::local_package("h3r")
  pt <- st_sfc(st_point(c(18, -33)), crs = 4326)
  sf_pt <- st_sf(id = 1, geometry = pt)

  # point geometry does not warn
  h3_sf <- par_make_h3(sf_pt, res = 10L)
  testthat::expect_s3_class(h3_sf, "sf")
  testthat::expect_true("CGRIDID" %in% names(h3_sf))
  testthat::expect_true(any(sf::st_geometry_type(h3_sf) == "POLYGON"))

  # polygon geometry warns
  sf_pt <- st_buffer(sf_pt, dist = units::set_units(1000, "m"))
  suppressWarnings(h3_sf <- par_make_h3(sf_pt, res = 10L))
  testthat::expect_s3_class(h3_sf, "sf")
  testthat::expect_true("CGRIDID" %in% names(h3_sf))
  testthat::expect_true(any(sf::st_geometry_type(h3_sf) == "POLYGON"))

})

testthat::test_that("par_make_dggrid returns sf polygons with CGRIDID", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("dggridR")
  withr::local_package("sf")
  withr::local_package("dggridR")
  pt <- st_sfc(st_point(c(18, -33)), crs = 4326)
  sf_pt <- st_sf(id = 1, geometry = pt)
  dg_sf <- par_make_dggrid(sf_pt, res = 8L)
  testthat::expect_s3_class(dg_sf, "sf")
  testthat::expect_true("CGRIDID" %in% names(dg_sf))
})

testthat::test_that("par_make_dggrid transforms CRS if needed", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("dggridR")
  withr::local_package("sf")
  withr::local_package("dggridR")
  pt <- st_sfc(st_point(c(18, -33)), crs = 3857)
  sf_pt <- st_sf(id = 1, geometry = pt)
  testthat::expect_message(
    par_make_dggrid(sf_pt, res = 8L),
    "Input sf object should be in WGS84"
  )
})

testthat::test_that("par_make_dggrid errors if dggridR is not installed", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("dggridR")
  withr::local_package("sf")
  withr::local_package("dggridR")
  pt <- st_sfc(st_point(c(18, -33)), crs = 4326)
  sf_pt <- st_sf(id = 1, geometry = pt)
  testthat::expect_error(
    par_make_dggrid(sf_pt, res = 8L),
    NA
  ) # Should not error if dggridR is installed
})
