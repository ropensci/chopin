# dep_check tests ####
testthat::test_that("Input object class is detected",
  {
    # testthat::skip_on_ci()
    withr::local_package("stars")
    withr::local_package("terra")
    # withr::local_package("chopin")
    withr::local_options(list(sf_use_s2 = FALSE))
    bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
    bcsd_stars <- stars::read_stars(bcsd_path)

    packbound_stars <- chopin:::dep_check(bcsd_stars)
    sprast_bcsd <- terra::rast(bcsd_path)
    packbound_terra <- dep_check(sprast_bcsd)

    testthat::expect_equal(packbound_stars, "sf")
    testthat::expect_equal(packbound_terra, "terra")

    # proxy
    ncpath <- system.file(package = "sf", "shape/nc.shp")
    nc <- terra::vect(ncpath, proxy = TRUE)
    packbound_proxy <- chopin:::dep_check(nc)
    testthat::expect_equal(packbound_proxy, "terra")

  }
)

# datamod tests ####
testthat::test_that("Data model detection",
  {
    # testthat::skip_on_ci()
    # testthat::skip_on_covr()
    withr::local_package("stars")
    withr::local_package("terra")
    # withr::local_package("chopin")
    withr::local_options(list(sf_use_s2 = FALSE))
    bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
    bcsd_stars <- stars::read_stars(bcsd_path)

    nc <- system.file(package = "sf", "shape/nc.shp")
    nc <- sf::read_sf(nc)

    datatype_stars <- chopin:::datamod(bcsd_stars)
    datatype_sf <- chopin:::datamod(nc)

    testthat::expect_equal(datatype_stars, "raster")
    testthat::expect_equal(datatype_sf, "vector")

    testthat::expect_error(datamod(list(1, 2)))
  }
)

# reproject_std tests ####
testthat::test_that("CRS is transformed when it is not standard", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  withr::local_package("sf")
  withr::local_package("terra")
  # withr::local_package("chopin")
  withr::local_options(list(sf_use_s2 = FALSE))

  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")
  nctr <- terra::vect(nc)
  terra::crs(nctr) <- "EPSG:5070"
  ncna <- nc
  sf::st_crs(ncna) <- NA
  ncnatr <- terra::vect(ncna)

  nctr_align <- chopin:::reproject_std(nctr, "EPSG:4326")
  nc_align <- chopin:::reproject_std(nc, "EPSG:4326")

  testthat::expect_s3_class(nc_align, "sf")
  testthat::expect_s4_class(nctr_align, "SpatVector")

  nc_align_epsg <- sf::st_crs(nc_align)$epsg
  nctr_align_epsg <- terra::crs(nctr_align, describe = TRUE)$code

  testthat::expect_equal(nc_align_epsg, 4326)
  testthat::expect_equal(nctr_align_epsg, "4326")

  terra::crs(ncnatr) <- NULL
  # error case
  testthat::expect_error(chopin:::reproject_std(ncnatr, "EPSG:4326"))

})


# reproject_to_raster tests ####
testthat::test_that("reproject to raster: sf", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  withr::local_package("sf")
  withr::local_package("terra")
  # withr::local_package("chopin")
  withr::local_options(list(sf_use_s2 = FALSE))

  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")
  nctr <- terra::vect(nc)
  terra::crs(nctr) <- "EPSG:5070"

  # make random raster
  rr <- terra::rast(matrix(runif(100), 10, 10), crs = "EPSG:4326")

  ncr <- chopin:::reproject_to_raster(nc, rr)
  nctr <- chopin:::reproject_to_raster(nctr, rr)

  testthat::expect_s3_class(ncr, "sf")
  testthat::expect_s4_class(nctr, "SpatVector")
})

# vect_validate tests ####
testthat::test_that("vector validity check is cleared", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  withr::local_package("sf")
  withr::local_package("terra")
  # withr::local_package("chopin")
  withr::local_options(list(sf_use_s2 = FALSE))

  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)

  testthat::expect_no_error(chopin:::vect_validate(nc))

  nct <- terra::vect(nc)
  testthat::expect_no_error(chopin:::vect_validate(nct))
})

# .check_id tests ####
testthat::test_that(".check_id throws error with non-character id", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))
  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  input_sf <- sf::st_read(input_char)

  testthat::expect_error(
    chopin:::.check_id(input_sf, 32)
  )
})


testthat::test_that(".check_id throws error with nonexistent id", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))
  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  input_sf <- sf::st_read(input_char)

  testthat::expect_error(
    chopin:::.check_id(input_sf, "fips"),
    "id should exist in the input object"
  )
})


# .check_vector tests ####
testthat::test_that(".check_vector with sf", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  input_sf <- sf::st_read(input_char)
  checked_sf <-
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = NULL, out_class = "sf"
    )
  testthat::expect_equal(chopin:::dep_check(checked_sf), "sf")

})


testthat::test_that(".check_vector with terra", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  input_vect <- terra::vect(input_char)
  checked_vect <-
    chopin:::.check_vector(
      input_vect, input_id = "FIPS",
      extent = NULL, out_class = "terra"
    )
  testthat::expect_equal(chopin:::dep_check(checked_vect), "terra")

})


testthat::test_that(".check_vector with file path and extent", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  input_vect <- terra::vect(input_char)

  # nonexistent file path: terra/sf ingestion is tried and it
  # inherits "try-error", still available for .check_id
  # the error is thrown in .check_id
  testthat::expect_error(
    chopin:::.check_vector(
      "nonexistent.gpkg", input_id = "FIPS",
      extent = NULL, out_class = "sf"
    ),
    "id should exist in the input object"
  )

  testthat::expect_error(
    chopin:::.check_vector(
      "nonexistent.gpkg", input_id = "FIPS",
      extent = NULL, out_class = "terra"
    ),
    "id should exist in the input object"
  )

  # existing file path: terra output
  checked_char <-
    chopin:::.check_vector(
      input_char, extent = NULL, input_id = "FIPS", out_class = "terra"
    )
  testthat::expect_equal(chopin:::dep_check(checked_char), "terra")

  # existing file path: terra output with extent
  checked_ext <-
    chopin:::.check_vector(
      input_char, extent = c(-80, -77, 35, 36),
      input_id = "FIPS", out_class = "terra"
    )
  testthat::expect_equal(chopin:::dep_check(checked_ext), "terra")

  # existing file path: terra output without extent
  checked_ext <-
    chopin:::.check_vector(
      input_char, extent = NULL,
      input_id = "FIPS", out_class = "terra"
    )
  testthat::expect_equal(chopin:::dep_check(checked_ext), "terra")

})


testthat::test_that("vect_validate repairs input vector data", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_sf <-
    sf::st_as_sf(
      data.frame(
        wkt = sf::st_as_text(sf::st_polygon(
          list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))
        ))
      ),
      wkt = "wkt"
    )
  testthat::expect_true(
    sf::st_is_valid(chopin:::vect_validate(input_sf))
  )

  input_terra <- terra::vect(input_sf)
  testthat::expect_true(
    terra::is.valid(chopin:::vect_validate(input_terra))
  )
})

testthat::test_that(".intersect_extent returns the intersection extent", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_sf <- sf::st_as_sf(
    data.frame(wkt = "POLYGON ((-1 -1, 1 -1, 1 1, -1 1, -1 -1))"),
    wkt = "wkt"
  )
  extent_sf <-
    sf::st_as_text(
      sf::st_as_sfc(sf::st_bbox(c(xmin = -1, ymin = -1, xmax = 1, ymax = 1)))
    )
  ext_chopin <- chopin:::.intersect_extent(input_sf, "sf")
  ext_chopin <-
    sf::st_as_text(
      sf::st_as_sfc(sf::st_bbox(ext_chopin))
    )
  testthat::expect_equal(
    ext_chopin, extent_sf
  )

  input_terra <- terra::vect(input_sf)
  extent_terra <- terra::ext(input_terra)
  ie_trtr <- chopin:::.intersect_extent(input_terra, "terra")
  ie_trsf <- chopin:::.intersect_extent(input_terra, "sf")

  testthat::expect_s4_class(ie_trtr, "SpatExtent")
  testthat::expect_true(class(ie_trsf)[1] == "bbox")

  input_num <- c(-0.5, 1, -0.5, 1)
  testthat::expect_error(
    chopin:::.intersect_extent(input_num, "land")
  )

  testthat::expect_no_error(
    ie_sf <- chopin:::.intersect_extent(input_num, "sf")
  )
  testthat::expect_true(inherits(ie_sf, "sfc"))
  testthat::expect_no_error(
    ie_tr <- chopin:::.intersect_extent(input_num, "terra")
  )
  testthat::expect_s4_class(ie_tr, "SpatExtent")


})


testthat::test_that(".check_character with non-character inputs",{
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  # test for non-character input
  testthat::expect_message(
    testthat::expect_error(chopin:::.check_character(3L)),
    "Input is not a character."
  )
  testthat::expect_message(
    testthat::expect_error(chopin:::.check_character(11.11)),
    "Input is not a character."
  )
  testthat::expect_message(
    testthat::expect_error(chopin:::.check_character(NA)),
    "Input is not a character."
  )
})


testthat::test_that(".check_character with character inputs",{
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))
  # test for non file path character input
  testthat::expect_error(
    .check_character("test")
  )
  # vector file
  ncfile <- system.file(package = "sf", "shape/nc.shp")
  vec_file <- chopin:::.check_character(ncfile)
  # remove attributes for comparison
  attr(vec_file, "crs") <- NULL
  testthat::expect_equal(vec_file, "vector")

  # raster file
  elevfile <- system.file(package = "terra", "ex/elev.tif")
  ras_file <- chopin:::.check_character(elevfile)
  attr(ras_file, "crs") <- NULL
  testthat::expect_equal(ras_file, "raster")
})


testthat::test_that(".check_character with sf objects", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_options(list(sf_use_s2 = FALSE))

  # sf object
  ncfile <- system.file(package = "sf", "shape/nc.shp")
  elevfile <- system.file(package = "terra", "ex/elev.tif")

  nc <- sf::read_sf(ncfile)
  testthat::expect_message(
    ncsf_detected <- chopin:::.check_character(nc),
    "Input is not a character."
  )
  # is CRS correctly detected from the input path?
  testthat::expect_true(is.character(attr(ncsf_detected, "crs")))

  # remove attributes for comparison
  attr(ncsf_detected, "crs") <- NULL
  testthat::expect_equal(ncsf_detected, "vector")

  # run crs() on stars object gives errors
  # elev <- stars::read_stars(elevfile)
  # testthat::expect_message(
  #   elev_detected <- chopin:::.check_character(elev),
  #   "Input is not a character."
  # )
  # testthat::expect_true(is.na(attr(elev_detected, "crs")))
  # attr(elev_detected, "crs") <- NULL
  # testthat::expect_equal(elev_detected, "raster")


})

testthat::test_that(".check_character with Spat* objects", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  withr::local_package("terra")

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  elevfile <- system.file(package = "terra", "ex/elev.tif")

  nct <- terra::vect(ncfile)
  elevt <- terra::rast(elevfile)
  testthat::expect_message(
    nct_detected <- chopin:::.check_character(nct),
    "Input is not a character."
  )
  testthat::expect_message(
    elevt_detected <- chopin:::.check_character(elevt),
    "Input is not a character."
  )
  testthat::expect_true(is.character(attr(nct_detected, "crs")))
  testthat::expect_true(is.character(attr(elevt_detected, "crs")))

  attr(nct_detected, "crs") <- NULL
  attr(elevt_detected, "crs") <- NULL
  testthat::expect_equal(nct_detected, "vector")
  testthat::expect_equal(elevt_detected, "raster")

})


# `[` Tests ####
testthat::test_that("`[` methods in chopin -- SpatVector-bbox", {
  skip_on_ci()
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("chopin")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(ncfile)
  nct <- terra::vect(nc)

  nc10 <- nc[seq_len(10L), ]
  nc10box <- sf::st_bbox(nc10)
  testthat::expect_no_error(nct10 <- nct[nc10box, ])
})


testthat::test_that("`[` methods in chopin -- SpatVector-sf", {
  skip_on_ci()
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("chopin")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(ncfile)
  nct <- terra::vect(nc)

  nc10 <- nc[seq_len(10L), ]
  nc10box <- sf::st_as_sf(nc10)
  testthat::expect_no_error(nct10 <- nct[nc10box, ])
})


testthat::test_that("`[` methods in chopin -- SpatVector-sfc", {
  skip_on_ci()
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("chopin")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(ncfile)
  nct <- terra::vect(nc)

  nc10 <- nc[seq_len(10L), ]
  nc10box <- sf::st_as_sf(nc10)$geometry
  testthat::expect_no_error(nct10 <- nct[nc10box, ])
})


testthat::test_that("`[` methods in chopin -- SpatVector-SpatExtent", {
  skip_on_ci()
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_package("chopin")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(ncfile)
  nct <- terra::vect(nc)

  nc10 <- nc[seq_len(10L), ]
  nc10box <- terra::ext(sf::st_bbox(nc10))
  testthat::expect_no_error(nct10 <- nct[nc10box, ])
})


testthat::test_that(".check_vector -- SpatVector-SpatExtent", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nct <- terra::vect(ncfile)

  nc10 <- nct[seq_len(10L), ]
  nc10box <- terra::ext(nc10)
  nc10e <- chopin:::.intersect_extent(nc10box, NULL)
  nct10 <- chopin:::.check_vector(input = nct, extent = nc10box, out_class = "terra")

  testthat::expect_s4_class(nc10e, "SpatExtent")
  testthat::expect_s4_class(nct10, "SpatVector")
})


testthat::test_that(".check_vector -- SpatVector-SpatExtent", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nct <- terra::vect(ncfile)

  nc10 <- nct[seq_len(10L), ]
  nc10box <- terra::ext(nc10)
  nc10e <- chopin:::.intersect_extent(nc10box, NULL)
  nct10 <- chopin:::.check_vector(input = nct, extent = nc10box, out_class = "terra")

  testthat::expect_s4_class(nc10e, "SpatExtent")
  testthat::expect_s4_class(nct10, "SpatVector")

  nc10e <- chopin:::.intersect_extent(nc10box, NULL)
  nct10 <- chopin:::.check_vector(input = nct, extent = nc10box, out_class = "sf")

  testthat::expect_s3_class(nct10, "sf")

})


testthat::test_that(".check_vector -- SpatVector-SpatExtent", {
  # testthat::skip_on_ci()
  # testthat::skip_on_covr()
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nct <- terra::vect(ncfile)

  nc10 <- nct[seq_len(10L), ]
  nc10box <- terra::ext(nc10)
  nc10e <- chopin:::.intersect_extent(nc10box, NULL)
  nct10 <- chopin:::.check_vector(input = nct, extent = nc10box, out_class = "sf")

  testthat::expect_s3_class(nct10, "sf")

  nc10e <- chopin:::.intersect_extent(nc10box, NULL)
  nct10 <- chopin:::.check_vector(input = nct, extent = nc10box, out_class = "sf")

  testthat::expect_s3_class(nct10, "sf")
})


testthat::test_that(".check_vector -- character and sf", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nct <- terra::vect(ncfile)

  nctex <- terra::as.polygons(terra::ext(nct))
  nctex <- terra::set.crs(nctex, terra::crs(nct))
  nctex <- sf::st_as_sf(nctex)

  cv_chsfsf <-
    chopin:::.check_vector(ncfile, extent = nctex, out_class = "sf")
  cv_chsftr <-
    chopin:::.check_vector(ncfile, extent = nctex, out_class = "terra")

  testthat::expect_s3_class(cv_chsfsf, "sf")
  testthat::expect_s4_class(cv_chsftr, "SpatVector")
})
