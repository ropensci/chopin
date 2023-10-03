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
    bcsd_path = system.file(package = "stars", "nc/bcsd_obs_1999.nc")
    bcsd_stars = stars::read_stars(bcsd_path)

    nc = system.file(package = "sf", "shape/nc.shp")
    nc = sf::read_sf(nc)

    datatype_stars = check_datatype(bcsd_stars)
    datatype_sf = check_datatype(nc)

    testthat::expect_equal(datatype_stars, "raster")
    testthat::expect_equal(datatype_sf, "vector")
})


testthat::test_that("Format is well converted",
{
    withr::local_package("stars")
    withr::local_package("terra")
    withr::local_options(list(sf_use_s2 = FALSE))

    # starts from sf/stars
    bcsd_path = system.file(package = "stars", "nc/bcsd_obs_1999.nc")
    bcsd_stars = stars::read_stars(bcsd_path)
    nc = system.file(package = "sf", "shape/nc.shp")
    nc = sf::read_sf(nc)

    stars_bcsd_tr = switch_packbound(bcsd_stars)
    sf_nc_tr = switch_packbound(nc)

    testthat::expect_equal(check_packbound(stars_bcsd_tr), "terra")
    testthat::expect_equal(check_packbound(sf_nc_tr), "terra")

    stars_bcsd_trb = switch_packbound(stars_bcsd_tr)
    sf_nc_trb = switch_packbound(sf_nc_tr)

    testthat::expect_equal(check_packbound(stars_bcsd_trb), "sf")
    testthat::expect_equal(check_packbound(sf_nc_trb), "sf")

})



