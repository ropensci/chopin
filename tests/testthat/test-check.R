
testthat::test_that("Input object class is detected",
  {
    withr::local_package("stars")
    withr::local_package("terra")
    withr::local_options(list(sf_use_s2 = FALSE))
    bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
    bcsd_stars <- stars::read_stars(bcsd_path)

    packbound_stars <- dep_check(bcsd_stars)
    sprast_bcsd <- terra::rast(bcsd_path)
    packbound_terra <- dep_check(sprast_bcsd)

    testthat::expect_equal(packbound_stars, "sf")
    testthat::expect_equal(packbound_terra, "terra")

    # proxy
    ncpath <- system.file(package = "sf", "shape/nc.shp")
    nc <- terra::vect(ncpath, proxy = TRUE)
    packbound_proxy <- dep_check(nc)
    testthat::expect_equal(packbound_proxy, "terra")

  }
)


testthat::test_that("Data model detection",
  {
    withr::local_package("stars")
    withr::local_package("terra")
    withr::local_options(list(sf_use_s2 = FALSE))
    bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
    bcsd_stars <- stars::read_stars(bcsd_path)

    nc <- system.file(package = "sf", "shape/nc.shp")
    nc <- sf::read_sf(nc)

    datatype_stars <- datamod(bcsd_stars)
    datatype_sf <- datamod(nc)

    testthat::expect_equal(datatype_stars, "raster")
    testthat::expect_equal(datatype_sf, "vector")

    testthat::expect_error(datamod(list(1, 2)))
  }
)

testthat::test_that("CRS is transformed when it is not standard", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")
  nctr <- terra::vect(nc)
  terra::crs(nctr) <- "EPSG:5070"
  ncna <- nc
  sf::st_crs(ncna) <- NA
  ncnatr <- terra::vect(ncna)

  nctr_align <- reproject_std(nctr, "EPSG:4326")
  nc_align <- reproject_std(nc, "EPSG:4326")

  testthat::expect_s3_class(nc_align, "sf")
  testthat::expect_s4_class(nctr_align, "SpatVector")

  nc_align_epsg <- sf::st_crs(nc_align)$epsg
  nctr_align_epsg <- terra::crs(nctr_align, describe = TRUE)$code

  testthat::expect_equal(nc_align_epsg, 4326)
  testthat::expect_equal(nctr_align_epsg, "4326")

  terra::crs(ncnatr) <- NULL
  # error case
  testthat::expect_error(reproject_std(ncnatr, "EPSG:4326"))

})


testthat::test_that("reproject to raster: sf", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")
  nctr <- terra::vect(nc)
  terra::crs(nctr) <- "EPSG:5070"

  # make random raster
  rr <- terra::rast(matrix(runif(100), 10, 10), crs = "EPSG:4326")

  ncr <- reproject_to_raster(nc, rr)
  nctr <- reproject_to_raster(nctr, rr)

  testthat::expect_s3_class(ncr, "sf")
  testthat::expect_s4_class(nctr, "SpatVector")
})


testthat::test_that("vector validity check is cleared", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)

  testthat::expect_no_error(vect_validate(nc))

  nct <- terra::vect(nc)
  testthat::expect_no_error(vect_validate(nct))
})


testthat::test_that(".check_id throws error with non-character id", {
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))
  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  input_sf <- sf::st_read(input_char)

  testthat::expect_error(
    .check_id(input_sf, 32)
  )
})


testthat::test_that(".check_id throws error with nonexistent id", {
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))
  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  input_sf <- sf::st_read(input_char)

  testthat::expect_error(
    .check_id(input_sf, "fips"),
    "id should exist in the input object"
  )
})


testthat::test_that("check_subject performs necessary conversions", {
  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  input_sf <- sf::st_read(input_char)
  checked_sf <- check_subject(input_sf, subject_id = "FIPS")
  testthat::expect_equal(dep_check(checked_sf), "terra")

  input_vect <- terra::vect(input_sf)
  checked_vect <- check_subject(input_vect, subject_id = "FIPS")
  testthat::expect_equal(dep_check(checked_vect), "terra")

  checked_char <-
    check_subject(
      input_char, extent = NULL, subject_id = "FIPS"
    )
  testthat::expect_equal(dep_check(checked_char), "terra")

  checked_ext <-
    check_subject(
      input_char, extent = c(-80, -77, 35, 36), subject_id = "FIPS"
    )
  testthat::expect_equal(dep_check(checked_ext), "terra")
})


testthat::test_that(".check_character with non-character inputs",{
  # test for non-character input
  testthat::expect_message(
    testthat::expect_error(.check_character(3L)),
    "Input is not a character."
  )
  testthat::expect_message(
    testthat::expect_error(.check_character(11.11)),
    "Input is not a character."
  )
  testthat::expect_message(
    testthat::expect_error(.check_character(NA)),
    "Input is not a character."
  )
})

testthat::test_that(".check_character with character inputs",{
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))
  # test for non file path character input
  testthat::expect_error(
    .check_character("test")
  )
  # vector file
  ncfile <- system.file(package = "sf", "shape/nc.shp")
  vec_file <- .check_character(ncfile)
  # remove attributes for comparison
  attr(vec_file, "crs") <- NULL
  testthat::expect_equal(vec_file, "vector")

  # raster file
  elevfile <- system.file(package = "terra", "ex/elev.tif")
  ras_file <- .check_character(elevfile)
  attr(ras_file, "crs") <- NULL
  testthat::expect_equal(ras_file, "raster")
})


testthat::test_that(".check_character with sf objects", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_options(list(sf_use_s2 = FALSE))

  # sf object
  ncfile <- system.file(package = "sf", "shape/nc.shp")
  elevfile <- system.file(package = "terra", "ex/elev.tif")

  nc <- sf::read_sf(ncfile)
  elev <- stars::read_stars(elevfile)
  testthat::expect_message(
    ncsf_detected <- .check_character(nc),
    "Input is not a character."
  )
  testthat::expect_message(
    elev_detected <- .check_character(elev),
    "Input is not a character."
  )
  testthat::expect_equal(ncsf_detected, "vector")
  testthat::expect_equal(elev_detected, "raster")

})

testthat::test_that(".check_character with Spat* objects", {
  withr::local_package("terra")

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  elevfile <- system.file(package = "terra", "ex/elev.tif")

  nct <- terra::vect(nc)
  elevt <- terra::rast(elevfile)
  testthat::expect_message(
    nct_detected <- .check_character(nct),
    "Input is not a character."
  )
  testthat::expect_message(
    elevt_detected <- .check_character(elevt),
    "Input is not a character."
  )
  testthat::expect_equal(nct_detected, "vector")
  testthat::expect_equal(elevt_detected, "raster")

})


# `[` Tests ####
testthat::test_that("`[` methods in chopin -- SpatVector-bbox", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(ncfile)
  nct <- terra::vect(nc)

  nc10 <- nc[seq_len(10L), ]
  nc10box <- sf::st_bbox(nc10)
  testthat::expect_no_error(nct10 <- nct[nc10box, ])
})


testthat::test_that("`[` methods in chopin -- SpatVector-sf", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(ncfile)
  nct <- terra::vect(nc)

  nc10 <- nc[seq_len(10L), ]
  nc10box <- sf::st_as_sf(nc10)
  testthat::expect_no_error(nct10 <- nct[nc10box, ])
})


testthat::test_that("`[` methods in chopin -- SpatVector-sfc", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(ncfile)
  nct <- terra::vect(nc)

  nc10 <- nc[seq_len(10L), ]
  nc10box <- sf::st_as_sf(nc10)$geometry
  testthat::expect_no_error(nct10 <- nct[nc10box, ])
})


testthat::test_that("`[` methods in chopin -- SpatVector-SpatExtent", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(ncfile)
  nct <- terra::vect(nc)

  nc10 <- nc[seq_len(10L), ]
  nc10box <- terra::ext(sf::st_bbox(nc10))
  testthat::expect_no_error(nct10 <- nct[nc10box, ])
})


testthat::test_that(".check_vector -- SpatVector-SpatExtent", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nct <- terra::vect(ncfile)

  nc10 <- nct[seq_len(10L), ]
  nc10box <- terra::ext(nc10)
  nc10e <- .intersect_extent(nc10box)
  nct10 <- .check_vector(input = nct, extent = nc10box, out_class = "terra")

  testthat::expect_s4_class(nc10e, "SpatExtent")
  testthat::expect_s4_class(nct10, "SpatVector")
})


testthat::test_that(".check_vector -- SpatVector-SpatExtent", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nct <- terra::vect(ncfile)

  nc10 <- nct[seq_len(10L), ]
  nc10box <- terra::ext(nc10)
  nc10e <- .intersect_extent(nc10box)
  nct10 <- .check_vector(input = nct, extent = nc10box, out_class = "terra")

  testthat::expect_s4_class(nc10e, "SpatExtent")
  testthat::expect_s4_class(nct10, "SpatVector")

  nc10e <- .intersect_extent(nc10box)
  nct10 <- .check_vector(input = nct, extent = nc10box, out_class = "sf")

  testthat::expect_s3_class(nct10, "sf")

})


testthat::test_that(".check_vector -- SpatVector-SpatExtent", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nct <- terra::vect(ncfile)

  nc10 <- nct[seq_len(10L), ]
  nc10box <- terra::ext(nc10)
  nc10e <- .intersect_extent(nc10box)
  nct10 <- .check_vector(input = nct, extent = nc10box, out_class = "sf")

  testthat::expect_s3_class(nct10, "sf")

  nc10e <- .intersect_extent(nc10box)
  nct10 <- .check_vector(input = nct, extent = nc10box, out_class = "sf")

  testthat::expect_s3_class(nct10, "sf")
})