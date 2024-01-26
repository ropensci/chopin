# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

testthat::test_that("What package does the input object belong?",
  {
    withr::local_package("stars")
    withr::local_package("terra")
    withr::local_options(list(sf_use_s2 = FALSE))
    bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
    bcsd_stars <- stars::read_stars(bcsd_path)

    packbound_stars <- check_packbound(bcsd_stars)
    sprast_bcsd <- terra::rast(bcsd_path)
    packbound_terra <- check_packbound(sprast_bcsd)

    testthat::expect_equal(packbound_stars, "sf")
    testthat::expect_equal(packbound_terra, "terra")
  }
)


testthat::test_that("What package does the input object belong?",
  {
    withr::local_package("stars")
    withr::local_package("terra")
    withr::local_options(list(sf_use_s2 = FALSE))
    bcsd_path <- system.file(package = "stars", "nc/bcsd_obs_1999.nc")
    bcsd_stars <- stars::read_stars(bcsd_path)

    nc <- system.file(package = "sf", "shape/nc.shp")
    nc <- sf::read_sf(nc)

    datatype_stars <- check_datatype(bcsd_stars)
    datatype_sf <- check_datatype(nc)

    testthat::expect_equal(datatype_stars, "raster")
    testthat::expect_equal(datatype_sf, "vector")
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

  testthat::expect_error(check_crs_align(nc, 4326))
  testthat::expect_error(check_crs_align(ncna, crs_standard = "EPSG:4326"))
  testthat::expect_error(check_crs_align(ncnatr, "EPSG:4326"))

  testthat::expect_no_error(check_crs_align(nc, crs_standard = "EPSG:4326"))
  testthat::expect_no_error(check_crs_align(nc, crs_standard = "EPSG:5070"))
  testthat::expect_no_error(check_crs_align(nctr, crs_standard = "EPSG:4326"))
  testthat::expect_no_error(check_crs_align(nctr, crs_standard = "EPSG:5070"))

  nctr_align <- check_crs_align(nctr, "EPSG:4326")
  nc_align <- check_crs_align(nc, "EPSG:4326")

  testthat::expect_s3_class(nc_align, "sf")
  testthat::expect_s4_class(nctr_align, "SpatVector")

  nc_align_epsg <- sf::st_crs(nc_align)$epsg 
  nctr_align_epsg <- terra::crs(nctr_align, describe = TRUE)$code

  testthat::expect_equal(nc_align_epsg, 4326)
  testthat::expect_equal(nctr_align_epsg, "4326")

  terra::crs(ncnatr) <- NULL
  # error case
  testthat::expect_error(check_crs_align(ncnatr, "EPSG:4326"))

})


testthat::test_that("vector validity check is cleared", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)

  testthat::expect_no_error(validate_and_repair_vectors(nc))

  nct <- terra::vect(nc)
  testthat::expect_no_error(validate_and_repair_vectors(nct))
})


testthat::test_that("input extent is converted to a polygon", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  mainland_vec <- c(xmin = -128, xmax = -62, ymin = 25, ymax = 52)
  mainland_box <- extent_to_polygon(mainland_vec, output_class = "sf")
  mainland_box_t <- extent_to_polygon(mainland_vec, output_class = "terra")
  mainland_vec_un <- unname(mainland_vec)

  testthat::expect_s3_class(mainland_box, "sf")
  # terra Spat* objects are s4 class...
  testthat::expect_s4_class(mainland_box_t, "SpatVector")
  # error cases
  testthat::expect_error(
    extent_to_polygon(mainland_vec_un, output_class = "sf")
  )
  testthat::expect_error(
    extent_to_polygon(mainland_vec_un, output_class = "GeoDataFrames")
  )
})


testthat::test_that("Check bbox abides.", {
  withr::local_package("sf")
  withr::local_package("stars")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  # starts from sf/stars
  nc <- system.file(package = "sf", "shape/nc.shp")
  nc <- sf::read_sf(nc)
  nc <- sf::st_transform(nc, "EPSG:5070")
  ncp <- readRDS(testthat::test_path("..", "testdata", "nc_random_point.rds"))
  ncp <- sf::st_transform(ncp, "EPSG:5070")

  testthat::expect_no_error(check_bbox(ncp, nc))
  res <- check_bbox(ncp, nc)
  testthat::expect_equal(res, TRUE)

  # error cases
  testthat::expect_no_error(check_bbox(ncp, sf::st_bbox(nc)))
})


testthat::test_that("check_crs is working as expected", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))
  ncpath <- system.file("shape/nc.shp", package = "sf")
  nc <- sf::read_sf(ncpath)
  nct <- terra::vect(nc)
  crs_checked1 <- check_crs(nc)
  dummy <- character(0)
  crs_checked2 <- check_crs(nct)

  testthat::expect_equal(crs_checked1, sf::st_crs(nc))
  testthat::expect_equal(crs_checked2, terra::crs(nct))
  testthat::expect_error(check_crs(dummy))
  ncna <- nc
  sf::st_crs(ncna) <- NA
  testthat::expect_error(check_crs(ncna))
  nctna <- nct
  terra::crs(nctna) <- ""
  testthat::expect_error(check_crs(nctna))

})


testthat::test_that("nc data is within the mainland US", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))
  ncpath <- system.file("shape/nc.shp", package = "sf")
  nc <- sf::read_sf(ncpath)
  nc <- sf::st_transform(nc, "EPSG:4326")
  mainland_vec <- c(xmin = -128, xmax = -62, ymin = 22, ymax = 52)
  mainland_box <- extent_to_polygon(mainland_vec, output_class = "sf")
  within_res <- check_within_reference(nc, mainland_box)
  testthat::expect_equal(within_res, TRUE)

  # error cases
  testthat::expect_error(check_within_reference(list(1), mainland_box))
  testthat::expect_error(check_within_reference(nc, list(1)))

})

