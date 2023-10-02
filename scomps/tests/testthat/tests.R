# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand  
testthat::test_that("What package does the input object belong?",
{
    withr::local_package("stars")
    withr::local_package("terra")
    withr::local_options(list(sf_use_s2 = FALSE))
    bcsd_path = system.file(package = "stars", "nc/bcsd_obs_1999.nc")
    bcsd_stars = stars::read_stars(bcsd_path)

    packbound_stars = check_packbound(bcsd_stars)
    sprast_bcsd = terra::rast(bcsd_path)
    packbound_terra = check_packbound(sprast_bcsd)

    testthat::expect_equal(packbound_stars, "sf")
    testthat::expect_equal(packbound_terra, "terra")
})


testthat::test_that("What package does the input object belong?",
{
    withr::local_package("stars")
    withr::local_package("terra")
    withr::local_options(list(sf_use_s2 = FALSE))
    data(stars_sentinel2)
    nc = system.file(package = "sf", "shape/nc.shp")
    nc = sf::read_sf(nc)

    datatype_stars = check_datatype(stars_sentinel2)
    datatype_sf = check_datatype(nc)

    testthat::expect_equal(datatype_stars, "raster")
    testthat::expect_equal(datatype_sf, "vector")
})

