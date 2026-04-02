roadpath <- file.path(tempdir(check = TRUE), "ncroads_first.gpkg")
roadurl <- "https://raw.githubusercontent.com/ropensci/chopin/refs/heads/main/tests/testdata/ncroads_first.gpkg"
download.file(roadurl, roadpath, mode = "wb", method = "wget")

rppath <- file.path(tempdir(check = TRUE), "nc_random_point.rds")
rpurl <- "https://raw.githubusercontent.com/ropensci/chopin/refs/heads/main/tests/testdata/nc_random_point.rds"
download.file(rpurl, rppath, mode = "wb", method = "wget")

nccntypath <- system.file("shape/nc.shp", package = "sf")
ncpoly <- sf::st_read(nccntypath)
ncpoly <- sf::st_transform(ncpoly, "EPSG:5070")

ras <- terra::rast(ncpoly, nrow = 1000, ncol = 2200)
terra::values(ras) <- rgamma(2.2e6, 4, 2)

ncelevpath <- file.path(tempdir(check = TRUE), "ncelev.tif")
terra::writeRaster(ras, ncelevpath, overwrite = TRUE)
ncelev <- terra::rast(ncelevpath)
