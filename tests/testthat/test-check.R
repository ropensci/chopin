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


# .check_id tests ####
testthat::test_that(".check_id throws error with non-character id", {
  withr::local_package("sf")
  withr::local_options(list(sf_use_s2 = FALSE))
  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  input_sf <- sf::st_read(input_char)

  testthat::expect_error(
    chopin:::.check_id(input_sf, 32)
  )
})


testthat::test_that(".check_id throws error with nonexistent id", {
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
# input = sf ####
testthat::test_that(".check_vector with sf-NULL", {

  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  input_sf <- sf::st_read(input_char)
  checked_sf <-
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = NULL, out_class = "sf"
    )
  testthat::expect_equal(chopin:::dep_check(checked_sf), "sf")

  checked_terra <-
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = NULL, out_class = "terra"
    )
  testthat::expect_equal(chopin:::dep_check(checked_terra), "terra")

  testthat::expect_error(chopin:::.check_vector(
    input = input_sf,
    input_id = "FIPS",
    extent = NULL,
    out_class = "land"
  ))
})

# input = sf ####
testthat::test_that(".check_vector with sf-numeric", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  input_sf <- sf::st_read(input_char)
  num_ext <- c(-88, -80, 33, 36)
  num_ext_bbox <- sf::st_as_sfc(sf::st_bbox(terra::ext(num_ext)))
  num_ext_bbox <- sf::st_set_crs(num_ext_bbox, sf::st_crs(input_sf))
  input_sf_sub <- input_sf[num_ext_bbox, ]

  checked_sf <-
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = num_ext, out_class = "sf"
    )
  testthat::expect_equal(chopin:::dep_check(checked_sf), "sf")
  testthat::expect_true(nrow(input_sf_sub) == nrow(checked_sf))

  checked_terra <-
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = num_ext, out_class = "terra"
    )
  testthat::expect_equal(chopin:::dep_check(checked_terra), "terra")

  testthat::expect_error(
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = num_ext, out_class = "land"
    )

  )
})


testthat::test_that(".check_vector with sf-SpatVector", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  name_filter <- c("Orange", "Durham", "Wake")
  input_sf <- sf::st_read(input_char)
  input_sfu <- sf::st_union(input_sf[input_sf$NAME %in% name_filter, ])
  input_sfu <- sf::st_transform(input_sfu, "EPSG:5070")
  input_sfu <- sf::st_bbox(sf::st_buffer(input_sfu, 2000))
  input_sfu <- terra::ext(input_sfu)

  checked_sf <-
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = input_sfu, out_class = "sf"
    )
  testthat::expect_equal(chopin:::dep_check(checked_sf), "sf")

  checked_terra <-
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = input_sfu, out_class = "terra"
    )
  testthat::expect_equal(chopin:::dep_check(checked_terra), "terra")

  testthat::expect_error(
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = input_sfu, out_class = "land"
    )
  )

})


testthat::test_that(".check_vector with sf-sf", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  name_filter <- c("Orange", "Durham", "Wake")
  input_sf <- sf::st_read(input_char)
  input_sfu <- sf::st_union(input_sf[input_sf$NAME %in% name_filter, ])
  input_sfu <- sf::st_transform(input_sfu, "EPSG:5070")
  input_sfu <- sf::st_as_sf(sf::st_buffer(input_sfu, 2000))

  checked_sf <-
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = input_sfu, out_class = "sf"
    )
  testthat::expect_equal(chopin:::dep_check(checked_sf), "sf")

  checked_terra <-
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = input_sfu, out_class = "terra"
    )
  testthat::expect_equal(chopin:::dep_check(checked_terra), "terra")

  testthat::expect_error(
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = input_sfu, out_class = "land"
    )
  )

})

testthat::test_that(".check_vector with sf-SpatVector", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  name_filter <- c("Orange", "Durham", "Wake")
  input_sf <- sf::st_read(input_char)
  input_sfu <- sf::st_union(input_sf[input_sf$NAME %in% name_filter, ])
  input_sfu <- sf::st_transform(input_sfu, "EPSG:5070")
  input_sfu <- sf::st_as_sf(sf::st_buffer(input_sfu, 2000))
  input_sfu <- terra::vect(input_sfu)

  checked_sf <-
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = input_sfu, out_class = "sf"
    )
  testthat::expect_equal(chopin:::dep_check(checked_sf), "sf")

  checked_terra <-
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = input_sfu, out_class = "terra"
    )
  testthat::expect_equal(chopin:::dep_check(checked_terra), "terra")

  testthat::expect_error(
    chopin:::.check_vector(
      input_sf, input_id = "FIPS",
      extent = input_sfu, out_class = "land"
    )
  )

})


# input = terra ####
testthat::test_that(".check_vector with terra-SpatExtent", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  name_filter <- c("Orange", "Durham", "Wake")
  input_vect <- terra::vect(input_char)
  input_sf <- sf::st_read(input_char)
  input_sfu <- sf::st_union(input_sf[input_sf$NAME %in% name_filter, ])
  input_sfu <- sf::st_transform(input_sfu, "EPSG:5070")
  input_sfu <- sf::st_as_sf(sf::st_buffer(input_sfu, 2000))
  input_sfu <- terra::vect(input_sfu)
  input_sfu <- terra::ext(input_sfu)

  checked_trextr <-
    chopin:::.check_vector(
      input_vect, input_id = "FIPS",
      extent = input_sfu, out_class = "terra"
    )
  testthat::expect_equal(chopin:::dep_check(checked_trextr), "terra")

  testthat::expect_message(
    checked_trexsf <-
      chopin:::.check_vector(
        input_vect, input_id = "FIPS",
        extent = input_sfu, out_class = "sf"
      )
  )
  testthat::expect_equal(chopin:::dep_check(checked_trexsf), "sf")

  testthat::expect_error(
    chopin:::.check_vector(
      input_vect, input_id = "FIPS",
      extent = input_sfu, out_class = "land"
    )
  )
})


testthat::test_that(".check_vector with terra-SpatVector", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  name_filter <- c("Orange", "Durham", "Wake")
  input_vect <- terra::vect(input_char)
  input_sf <- sf::st_read(input_char)
  input_sfu <- sf::st_union(input_sf[input_sf$NAME %in% name_filter, ])
  input_sfu <- sf::st_transform(input_sfu, "EPSG:5070")
  input_sfu <- sf::st_as_sf(sf::st_buffer(input_sfu, 2000))
  input_sfu <- terra::vect(input_sfu)

  checked_trextr <-
    chopin:::.check_vector(
      input_vect, input_id = "FIPS",
      extent = input_sfu, out_class = "terra"
    )
  testthat::expect_equal(chopin:::dep_check(checked_trextr), "terra")

  testthat::expect_message(
    checked_trexsf <-
      chopin:::.check_vector(
        input_vect, input_id = "FIPS",
        extent = input_sfu, out_class = "sf"
      )
  )
  testthat::expect_equal(chopin:::dep_check(checked_trexsf), "sf")

  testthat::expect_message(
    checked_trexsf <-
      chopin:::.check_vector(
        input_vect, input_id = NULL,
        extent = input_sfu, out_class = "sf"
      )
  )

  testthat::expect_error(
    chopin:::.check_vector(
      input_vect, input_id = "FIPS",
      extent = input_sfu, out_class = "land"
    )
  )
})


testthat::test_that(".check_vector with terra-sf", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  name_filter <- c("Orange", "Durham", "Wake")
  input_vect <- terra::vect(input_char)
  input_sf <- sf::st_read(input_char)
  input_sfu <- sf::st_union(input_sf[input_sf$NAME %in% name_filter, ])
  input_sfu <- sf::st_transform(input_sfu, "EPSG:5070")
  input_sfu <- sf::st_as_sf(sf::st_buffer(input_sfu, 2000))

  checked_trsftr <-
    chopin:::.check_vector(
      input_vect, input_id = "FIPS",
      extent = input_sfu, out_class = "terra"
    )
  testthat::expect_equal(chopin:::dep_check(checked_trsftr), "terra")

  testthat::expect_message(
    checked_trsfsf <-
      chopin:::.check_vector(
        input_vect, input_id = "FIPS",
        extent = input_sfu, out_class = "sf"
      )
  )
  testthat::expect_equal(chopin:::dep_check(checked_trsfsf), "sf")

  testthat::expect_message(
    checked_trsfsf <-
      chopin:::.check_vector(
        input_vect, input_id = NULL,
        extent = input_sfu, out_class = "sf"
      )
  )

  testthat::expect_error(
    chopin:::.check_vector(
      input_vect, input_id = "FIPS",
      extent = input_sfu, out_class = "land"
    )
  )
})




# input = character ####
testthat::test_that(".check_vector with file path-NULL", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

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


testthat::test_that(".check_vector with file path-NULL", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_char <- system.file("gpkg/nc.gpkg", package = "sf")

  # existing file path: terra output
  testthat::expect_message(
    checked_chrtr <-
      chopin:::.check_vector(
        input_char, extent = NULL, input_id = "FIPS", out_class = "terra"
      )
  )
  testthat::expect_equal(chopin:::dep_check(checked_chrtr), "terra")

  # existing file path: terra output with extent
  checked_chrsf <-
    chopin:::.check_vector(
      input_char, extent = NULL,
      input_id = "FIPS", out_class = "sf"
    )
  testthat::expect_equal(chopin:::dep_check(checked_chrsf), "sf")

  # invalid out_class
  testthat::expect_error(
    chopin:::.check_vector(
      input_char, extent = NULL,
      input_id = "FIPS", out_class = "land"
    )
  )

})


testthat::test_that(".check_vector with file path-numeric", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  num_ext <- c(-88, -78, 34, 36)

  # existing file path: terra output
  testthat::expect_message(
    checked_chrtr <-
      chopin:::.check_vector(
        input_char, extent = num_ext, input_id = "FIPS", out_class = "terra"
      )
  )
  testthat::expect_equal(chopin:::dep_check(checked_chrtr), "terra")

  # existing file path: terra output with extent
  checked_chrsf <-
    chopin:::.check_vector(
      input_char, extent = num_ext,
      input_id = "FIPS", out_class = "sf"
    )
  testthat::expect_equal(chopin:::dep_check(checked_chrsf), "sf")

  # invalid out_class
  testthat::expect_error(
    chopin:::.check_vector(
      input_char, extent = num_ext,
      input_id = "FIPS", out_class = "land"
    )
  )

})


testthat::test_that(".check_vector with file path-sf", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  name_filter <- c("Orange", "Durham", "Wake")
  input_vect <- terra::vect(input_char)
  input_sf <- sf::st_read(input_char)
  input_sfu <- sf::st_union(input_sf[input_sf$NAME %in% name_filter, ])
  input_sfu <- sf::st_transform(input_sfu, "EPSG:5070")
  input_sfu <- sf::st_as_sf(sf::st_buffer(input_sfu, 2000))

  # existing file path: terra output
  testthat::expect_message(
    checked_chrtr <-
      chopin:::.check_vector(
        input_char, extent = input_sfu, input_id = "FIPS", out_class = "terra"
      )
  )
  testthat::expect_equal(chopin:::dep_check(checked_chrtr), "terra")

  # existing file path: terra output with extent
  checked_chrsf <-
    chopin:::.check_vector(
      input_char, extent = input_sfu,
      input_id = "FIPS", out_class = "sf"
    )
  testthat::expect_equal(chopin:::dep_check(checked_chrsf), "sf")

  # invalid out_class
  testthat::expect_error(
    chopin:::.check_vector(
      input_char, extent = input_sfu,
      input_id = "FIPS", out_class = "land"
    )
  )

})



testthat::test_that(".check_vector with file path-SpatVector", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  input_char <- system.file("gpkg/nc.gpkg", package = "sf")
  name_filter <- c("Orange", "Durham", "Wake")
  input_vect <- terra::vect(input_char)
  input_sf <- sf::st_read(input_char)
  input_sfu <- sf::st_union(input_sf[input_sf$NAME %in% name_filter, ])
  input_sfu <- sf::st_transform(input_sfu, "EPSG:5070")
  input_sfu <- sf::st_as_sf(sf::st_buffer(input_sfu, 2000))
  input_sfu <- terra::vect(input_sfu)

  # existing file path: terra output
  testthat::expect_message(
    checked_chrtr <-
      chopin:::.check_vector(
        input_char, extent = input_sfu, input_id = "FIPS", out_class = "terra"
      )
  )
  testthat::expect_equal(chopin:::dep_check(checked_chrtr), "terra")

  # existing file path: terra output with extent
  checked_chrsf <-
    chopin:::.check_vector(
      input_char, extent = input_sfu,
      input_id = "FIPS", out_class = "sf"
    )
  testthat::expect_equal(chopin:::dep_check(checked_chrsf), "sf")

  # invalid out_class
  testthat::expect_error(
    chopin:::.check_vector(
      input_char, extent = input_sfu,
      input_id = "FIPS", out_class = "land"
    )
  )

})


# .intersect tests ####
testthat::test_that(
  ".intersect unifies different package objects and intersects", {
    withr::local_package("sf")
    withr::local_package("terra")
    withr::local_options(list(sf_use_s2 = FALSE))

    path_vec <- system.file("gpkg/nc.gpkg", package = "sf")
    vec <- sf::st_read(path_vec)
    vec <- sf::st_transform(vec, "EPSG:5070")

    ras <- terra::rast(vec, nrow = 1000, ncol = 2200)
    ncr <- terra::rasterize(vec, ras)
    terra::values(ras) <- rgamma(2.2e6, 4, 2)

    # Using raster path
    path_ras <- file.path(tempdir(check = TRUE), "ncelev.tif")
    terra::writeRaster(ras, path_ras, overwrite = TRUE)


    name_filter <- c("Orange", "Durham", "Wake")
    input_vect <- terra::vect(path_vec)
    input_sf <- sf::st_read(path_vec)
    input_sfu <- sf::st_union(input_sf[input_sf$NAME %in% name_filter, ])
    input_sfu <- sf::st_transform(input_sfu, "EPSG:5070")
    input_sfu <- sf::st_as_sf(sf::st_buffer(input_sfu, 2000))

    testthat::expect_no_error(
      rasvec <- chopin:::.intersect(x = ras, y = vec)
    )
    testthat::expect_no_error(
      rasvec <- chopin:::.intersect(x = vec, y = input_sfu)
    )
  })



# .intersect_extent tests ####
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


# .check_character ####
testthat::test_that(".check_character with non-character inputs", {
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


testthat::test_that(".check_character with character inputs", {
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


# .check_vector test duplicates: READY TO REMOVE ####
testthat::test_that(".check_vector -- SpatVector-SpatExtent", {
  withr::local_package("sf")
  withr::local_package("terra")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncfile <- system.file(package = "sf", "shape/nc.shp")
  nct <- terra::vect(ncfile)

  nc10 <- nct[seq_len(10L), ]
  nc10box <- terra::ext(nc10)
  nc10e <- chopin:::.intersect_extent(nc10box, NULL)
  nct10 <- chopin:::.check_vector(
    input = nct, extent = nc10box, out_class = "terra"
  )

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
  nc10e <- chopin:::.intersect_extent(nc10box, NULL)
  nct10 <- chopin:::.check_vector(
    input = nct, extent = nc10box, out_class = "terra"
  )

  testthat::expect_s4_class(nc10e, "SpatExtent")
  testthat::expect_s4_class(nct10, "SpatVector")

  nc10e <- chopin:::.intersect_extent(nc10box, NULL)
  nct10 <- chopin:::.check_vector(
    input = nct, extent = nc10box, out_class = "sf"
  )

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
  nc10e <- chopin:::.intersect_extent(nc10box, NULL)
  nct10 <- chopin:::.check_vector(
    input = nct, extent = nc10box, out_class = "sf"
  )

  testthat::expect_s3_class(nct10, "sf")

  nc10e <- chopin:::.intersect_extent(nc10box, NULL)
  nct10 <- chopin:::.check_vector(
    input = nct, extent = nc10box, out_class = "sf"
  )

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

# ^^^ READY TO REMOVE ^^^ ####



# .check_raster tests ####
testthat::test_that(
  ".check_raster distinguishes SpatRasterCollection from SpatRaster", {
    withr::local_package("terra")

    # neither SpatRaster nor character
    testthat::expect_error(
      chopin::.check_raster(1L)
    )

    path_ras <- system.file("ex/elev.tif", package = "terra")
    ras1 <- terra::rast(path_ras)
    ras2 <- terra::rast(path_ras)
    rascol <- terra::sprc(ras1, ras2)

    # SpatRasterCollection will abort the function call
    testthat::expect_error(
      chopin:::.check_raster(rascol)
    )

    testthat::expect_message(
      rasread <- chopin:::.check_raster(path_ras)
    )

    num_ext <- c(6.0, 6.3, 49.7, 50.0)
    testthat::expect_message(
      rassub <- chopin:::.check_raster(path_ras, num_ext)
    )
    testthat::expect_s4_class(rassub, "SpatRaster")

  }
)
