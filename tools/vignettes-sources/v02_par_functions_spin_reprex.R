#' ---
#' output: reprex::reprex_document
#' ---

## ----message = FALSE, warning = FALSE-----------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)


## -----------------------------------------------------------------------------
library(chopin)
library(dplyr)
library(sf)
library(terra)
library(future)
library(future.apply)
library(doFuture)
library(tigris)
options(sf_use_s2 = FALSE)
set.seed(2023, kind = "L'Ecuyer-CMRG")



## -----------------------------------------------------------------------------
ncpoly <- system.file("shape/nc.shp", package = "sf")
ncsf <- sf::read_sf(ncpoly)
ncsf <- sf::st_transform(ncsf, "EPSG:5070")
plot(sf::st_geometry(ncsf))


## ----point-generation---------------------------------------------------------
ncpoints <- sf::st_sample(ncsf, 10000)
plot(sf::st_geometry(ncpoints))

# st_sample output is st_sfc. We should convert it to sf
ncpoints <- st_as_sf(ncpoints)
ncpoints$pid <- seq(1, nrow(ncpoints))



## ----load-srtm----------------------------------------------------------------
srtm <- terra::unwrap(readRDS("../../tests/testdata/nc_srtm15_otm.tif"))
srtm
plot(srtm)
terra::crs(srtm) <- "EPSG:5070"


## ----process-single-----------------------------------------------------------
ncpoints_tr <- terra::vect(ncpoints)
system.time(
  ncpoints_srtm <-
    chopin::extract_at(
      vector = ncpoints_tr,
      raster = srtm,
      id = "pid",
      mode = "buffer",
      radius = 1e4L
    ) # 10,000 meters (10 km)
)


## ----generate-compregion------------------------------------------------------
compregions <-
  chopin::par_pad_grid(
    ncpoints_tr,
    mode = "grid",
    nx = 8L,
    ny = 5L,
    padding = 1e4L
  )

names(compregions)

oldpar <- par()
par(mfcol = c(1, 2))
plot(compregions$original, main = "Original grids")
plot(compregions$padded, main = "Padded grids")
par(oldpar)


## ----process-multithread------------------------------------------------------
plan(multicore, workers = 4L)
doFuture::registerDoFuture()

system.time(
  ncpoints_srtm_mthr <-
    chopin::par_grid(
      grids = compregions,
      grid_target_id = NULL,
      fun_dist = chopin::extract_at,
      vector = ncpoints_tr,
      raster = srtm,
      id = "pid",
      mode = "buffer",
      radius = 1e4L
    )
)



## ----check-equal-results------------------------------------------------------
ncpoints_srtm_mthr <-
  ncpoints_srtm_mthr[order(ncpoints_srtm_mthr$pid),]
all.equal(ncpoints_srtm, ncpoints_srtm_mthr)




## ----vis-results--------------------------------------------------------------
ncpoints_s <-
  merge(ncpoints, ncpoints_srtm)
ncpoints_m <-
  merge(ncpoints, ncpoints_srtm_mthr)

plot(ncpoints_s[, "mean"], main = "Single-thread")
plot(ncpoints_m[, "mean"], main = "Multi-thread")


## ----get-hierarchy------------------------------------------------------------
nc_county <- file.path("../testdata/nc_hierarchy.gpkg")
nc_county <- sf::st_read(nc_county, layer = "county")

nc_tracts <- file.path("../testdata/nc_hierarchy.gpkg")
nc_tracts <- sf::st_read(nc_tracts, layer = "tracts")

nc_county <- sf::st_transform(nc_county, "EPSG:5070")
nc_tracts <- sf::st_transform(nc_tracts, "EPSG:5070")
nc_tracts$COUNTY <-
  substr(nc_tracts$GEOID, 1, 5)


## -----------------------------------------------------------------------------
system.time(
  nc_elev_tr_single <-
    chopin::extract_at(
      vector = nc_tracts,
      raster = srtm,
      id = "GEOID",
      mode = "polygon"
    )
)


## -----------------------------------------------------------------------------
system.time(
  nc_elev_tr_distr <-
    chopin::par_hierarchy(
      regions = nc_county, # higher level geometry
      regions_id = "GEOID", # higher level unique id
      fun_dist = chopin::extract_at,
      vector = nc_tracts, # lower level geometry
      raster = srtm,
      id = "GEOID", # lower level unique id
      func = "mean"
    )
)



## ----multirasters-------------------------------------------------------------
ncpath <- "../testdata/nc_hierarchy.gpkg"
nccnty <- terra::vect(ncpath, layer = "county")
ncelev <- terra::unwrap(readRDS("../testdata/nc_srtm15_otm.tif"))
terra::crs(ncelev) <- "EPSG:5070"
names(ncelev) <- c("srtm15")
tdir <- tempdir()
terra::writeRaster(ncelev, file.path(tdir, "test1.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test2.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test3.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test4.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test5.tif"), overwrite = TRUE)

testfiles <- list.files(tempdir(), pattern = "*.tif$", full.names = TRUE)
testfiles


## ----multirasters-processing--------------------------------------------------
res <-
  chopin::par_multirasters(
    filenames = testfiles,
    fun_dist = chopin::extract_with_polygons,
    polys = nccnty,
    surf = ncelev,
    id = "GEOID",
    func = "mean"
  )

knitr::kable(head(res))


## ----generic-distribution-----------------------------------------------------
pnts <- readRDS("../testdata/nc_random_point.rds")
pnts <- terra::vect(pnts)
rd1 <- terra::vect(
  file.path("../testdata/ncroads_first.gpkg")
)

pnts <- terra::project(pnts, "EPSG:5070")
rd1 <- terra::project(rd1, "EPSG:5070")


nccompreg <-
  chopin::par_pad_grid(
    input = pnts,
    mode = "grid",
    nx = 6L,
    ny = 4L,
    padding = 3e4L
  )

future::plan(future::multicore, workers = 6L)

system.time(
res <-
  chopin::par_grid(
    grids = nccompreg,
    fun_dist = terra::nearest,
    x = pnts,
    y = rd1)
  )



system.time(
  restr <- terra::nearest(x = pnts, y = rd1)
)
knitr::kable(head(res))
knitr::kable(head(restr))

