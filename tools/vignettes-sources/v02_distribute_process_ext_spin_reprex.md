``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
```

``` r
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
```

``` r
ncpoly <- system.file("shape/nc.shp", package = "sf")
ncsf <- sf::read_sf(ncpoly)
ncsf <- sf::st_transform(ncsf, "EPSG:5070")
plot(sf::st_geometry(ncsf))
```

![](https://i.imgur.com/ImPfGXP.png)<!-- -->

``` r
ncpoints <- sf::st_sample(ncsf, 10000)
plot(sf::st_geometry(ncpoints))
```

![](https://i.imgur.com/3ZQ4uoH.png)<!-- -->

``` r

# st_sample output is st_sfc. We should convert it to sf
ncpoints <- st_as_sf(ncpoints)
ncpoints$pid <- seq(1, nrow(ncpoints))
```

``` r
srtm <- terra::unwrap(readRDS("../../tests/testdata/nc_srtm15_otm.rds"))
srtm
#> class       : SpatRaster 
#> dimensions  : 1534, 2281, 1  (nrow, ncol, nlyr)
#> resolution  : 391.5026, 391.5026  (x, y)
#> extent      : 1012872, 1905890, 1219961, 1820526  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 
#> source(s)   : memory
#> name        : file928c3830468b 
#> min value   :        -3589.291 
#> max value   :         1946.400
plot(srtm)
```

![](https://i.imgur.com/l08bz4j.png)<!-- -->

``` r
terra::crs(srtm) <- "EPSG:5070"
```

``` r
ncpoints_tr <- terra::vect(ncpoints)
system.time(
    ncpoints_srtm <-
        chopin::extract_with(
            vector = ncpoints_tr,
            raster = srtm,
            id = "pid",
            mode = "buffer",
            radius = 1e4L) # 10,000 meters (10 km)
)
#>    user  system elapsed 
#>   6.271   0.210   6.484
```

``` r
compregions <-
    chopin::get_computational_regions(
        ncpoints_tr,
        mode = "grid",
        nx = 8L,
        ny = 5L,
        padding = 1e4L
    )

names(compregions)
#> [1] "original" "padded"

oldpar <- par()
par(mfcol = c(1, 2))
plot(compregions$original, main = "Original grids")
plot(compregions$padded, main = "Padded grids")
```

![](https://i.imgur.com/c0xweeV.png)<!-- -->

``` r
par(oldpar)
```

``` r
plan(multicore, workers = 4L)
doFuture::registerDoFuture()

system.time(
    ncpoints_srtm_mthr <-
        chopin::distribute_process_grid(
            grids = compregions,
            grid_target_id = NULL,
            fun_dist = scomps::extract_with,
            vector = ncpoints_tr,
            raster = srtm,
            id = "pid",
            mode = "buffer",
            radius = 1e4L
        )
)
#> Your input function was 
#>             successfully run at CGRIDID: 1
#> Your input function was 
#>             successfully run at CGRIDID: 2
#> Your input function was 
#>             successfully run at CGRIDID: 3
#> Your input function was 
#>             successfully run at CGRIDID: 4
#> Your input function was 
#>             successfully run at CGRIDID: 5
#> Your input function was 
#>             successfully run at CGRIDID: 6
#> Your input function was 
#>             successfully run at CGRIDID: 7
#> Your input function was 
#>             successfully run at CGRIDID: 8
#> Your input function was 
#>             successfully run at CGRIDID: 9
#> Your input function was 
#>             successfully run at CGRIDID: 10
#> Your input function was 
#>             successfully run at CGRIDID: 11
#> Your input function was 
#>             successfully run at CGRIDID: 12
#> Your input function was 
#>             successfully run at CGRIDID: 13
#> Your input function was 
#>             successfully run at CGRIDID: 14
#> Your input function was 
#>             successfully run at CGRIDID: 15
#> Your input function was 
#>             successfully run at CGRIDID: 16
#> Your input function was 
#>             successfully run at CGRIDID: 17
#> Your input function was 
#>             successfully run at CGRIDID: 18
#> Your input function was 
#>             successfully run at CGRIDID: 19
#> Your input function was 
#>             successfully run at CGRIDID: 20
#> Your input function was 
#>             successfully run at CGRIDID: 21
#> Your input function was 
#>             successfully run at CGRIDID: 22
#> Your input function was 
#>             successfully run at CGRIDID: 23
#> Your input function was 
#>             successfully run at CGRIDID: 24
#> Your input function was 
#>             successfully run at CGRIDID: 25
#> Your input function was 
#>             successfully run at CGRIDID: 26
#> Your input function was 
#>             successfully run at CGRIDID: 27
#> Your input function was 
#>             successfully run at CGRIDID: 28
#> Your input function was 
#>             successfully run at CGRIDID: 29
#> Your input function was 
#>             successfully run at CGRIDID: 30
#> Your input function was 
#>             successfully run at CGRIDID: 31
#> Your input function was 
#>             successfully run at CGRIDID: 32
#> Your input function was 
#>             successfully run at CGRIDID: 33
#>    user  system elapsed 
#>   4.407   0.351   2.438
```

``` r
ncpoints_srtm_mthr <-
    ncpoints_srtm_mthr[order(ncpoints_srtm_mthr$pid),]
all.equal(ncpoints_srtm, ncpoints_srtm_mthr)
#> [1] "Attributes: < Component \"row.names\": Mean relative difference: 0.6567904 >"
#> [2] "Component \"mean\": Mean relative difference: 8.712634e-05"
```

``` r
ncpoints_s <-
    merge(ncpoints, ncpoints_srtm)
ncpoints_m <-
    merge(ncpoints, ncpoints_srtm_mthr)

plot(ncpoints_s[, "mean"], main = "Single-thread")
```

![](https://i.imgur.com/iaQHWBL.png)<!-- -->

``` r
plot(ncpoints_m[, "mean"], main = "Multi-thread")
```

![](https://i.imgur.com/fgOvOff.png)<!-- -->

``` r
nc_county <- file.path("../testdata/nc_hierarchy.gpkg")
nc_county <- sf::st_read(nc_county, layer = "county")
#> Reading layer `county' from data source 
#>   `/Users/songi2/Documents/GitHub/Scalable_GIS/tools/testdata/nc_hierarchy.gpkg' 
#>   using driver `GPKG'
#> Simple feature collection with 100 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1054155 ymin: 1341756 xmax: 1838923 ymax: 1690176
#> Projected CRS: NAD83 / Conus Albers

nc_tracts <- file.path("../testdata/nc_hierarchy.gpkg")
nc_tracts <- sf::st_read(nc_tracts, layer = "tracts")
#> Reading layer `tracts' from data source 
#>   `/Users/songi2/Documents/GitHub/Scalable_GIS/tools/testdata/nc_hierarchy.gpkg' 
#>   using driver `GPKG'
#> Simple feature collection with 2672 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1054155 ymin: 1341756 xmax: 1838923 ymax: 1690176
#> Projected CRS: NAD83 / Conus Albers

nc_county <- sf::st_transform(nc_county, "EPSG:5070")
nc_tracts <- sf::st_transform(nc_tracts, "EPSG:5070")
nc_tracts$COUNTY <-
    substr(nc_tracts$GEOID, 1, 5)
```

``` r
system.time(
    nc_elev_tr_single <- chopin::extract_with(
        vector = nc_tracts,
        raster = srtm,
        id = "GEOID",
        mode = "polygon"
    )
)
#>    user  system elapsed 
#>   1.082   0.021   1.102
```

``` r
system.time(
    nc_elev_tr_distr <-
        chopin::distribute_process_hierarchy(
            regions = nc_county, # higher level geometry
            split_level = "GEOID", # higher level unique id
            fun_dist = scomps::extract_with,
            vector = nc_tracts, # lower level geometry
            raster = srtm,
            id = "GEOID", # lower level unique id
            func = "mean"
        )
)
#>    user  system elapsed 
#>   0.023   0.013   1.317
```

``` r
ncpath <- "../testdata/nc_hierarchy.gpkg"
nccnty <- terra::vect(ncpath, layer = "county")
ncelev <- terra::unwrap(readRDS("../testdata/nc_srtm15_otm.rds"))
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
#> [1] "/var/folders/58/7rn_bn5d6k3_cxwnzdhswpz4n0z2n9/T//RtmpUJwWlN/test1.tif"
#> [2] "/var/folders/58/7rn_bn5d6k3_cxwnzdhswpz4n0z2n9/T//RtmpUJwWlN/test2.tif"
#> [3] "/var/folders/58/7rn_bn5d6k3_cxwnzdhswpz4n0z2n9/T//RtmpUJwWlN/test3.tif"
#> [4] "/var/folders/58/7rn_bn5d6k3_cxwnzdhswpz4n0z2n9/T//RtmpUJwWlN/test4.tif"
#> [5] "/var/folders/58/7rn_bn5d6k3_cxwnzdhswpz4n0z2n9/T//RtmpUJwWlN/test5.tif"
```

``` r
res <- chopin::distribute_process_multirasters(
      filenames = testfiles,
      fun_dist = extract_with_polygons,
      polys = nccnty,
      surf = ncelev,
      id = "GEOID",
      func = "mean"
    )

knitr::kable(head(res))
```

| GEOID |      mean |
|:------|----------:|
| 37037 | 136.80203 |
| 37001 | 189.76170 |
| 37057 | 231.16968 |
| 37069 |  98.03845 |
| 37155 |  41.23463 |
| 37109 | 270.96933 |

``` r
pnts <- readRDS("../testdata/nc_random_point.rds")
pnts <- terra::vect(pnts)
rd1 <- terra::vect(
    file.path("../testdata/ncroads_first.gpkg"))

pnts <- terra::project(pnts, "EPSG:5070")
rd1 <- terra::project(rd1, "EPSG:5070")


nccompreg <-
    get_computational_regions(
                              input = pnts,
                              mode = "grid",
                              nx = 6L,
                              ny = 4L,
                              padding = 3e4L)
  
future::plan(future::multicore, workers = 6L)

system.time(
res <-
  distribute_process_grid(
                          grids = nccompreg,
                          fun_dist = terra::nearest,
                          x = pnts,
                          y = rd1)
)
#> Your input function was 
#>             successfully run at CGRIDID: 1
#> Your input function was 
#>             successfully run at CGRIDID: 2
#> Your input function was 
#>             successfully run at CGRIDID: 3
#> Your input function was 
#>             successfully run at CGRIDID: 4
#> Your input function was 
#>             successfully run at CGRIDID: 5
#> Your input function was 
#>             successfully run at CGRIDID: 6
#> Your input function was 
#>             successfully run at CGRIDID: 7
#> Your input function was 
#>             successfully run at CGRIDID: 8
#> Your input function was 
#>             successfully run at CGRIDID: 9
#> Your input function was 
#>             successfully run at CGRIDID: 10
#> Your input function was 
#>             successfully run at CGRIDID: 11
#> Your input function was 
#>             successfully run at CGRIDID: 12
#> Your input function was 
#>             successfully run at CGRIDID: 13
#> Your input function was 
#>             successfully run at CGRIDID: 14
#> Your input function was 
#>             successfully run at CGRIDID: 15
#> Your input function was 
#>             successfully run at CGRIDID: 16
#> Your input function was 
#>             successfully run at CGRIDID: 17
#> Your input function was 
#>             successfully run at CGRIDID: 18
#> Your input function was 
#>             successfully run at CGRIDID: 19
#> Your input function was 
#>             successfully run at CGRIDID: 20
#>    user  system elapsed 
#>   0.366   0.265   0.173



system.time(
  restr <- terra::nearest(x = pnts, y = rd1)
)
#>    user  system elapsed 
#>   0.036   0.000   0.036
knitr::kable(head(res))
```

| from_id |  from_x |  from_y | to_id |  distance |
|--------:|--------:|--------:|------:|----------:|
|       1 | 1401370 | 1608239 |    NA | 30053.952 |
|       2 | 1332751 | 1601423 |    NA | 14346.061 |
|       3 | 1405505 | 1617417 |    NA | 35164.291 |
|       4 | 1330270 | 1598246 |    NA | 16103.851 |
|       1 | 1511056 | 1608509 |    NA | 24443.245 |
|       2 | 1537861 | 1599604 |    NA |  3509.205 |

``` r
knitr::kable(head(restr))
```

| from_id |  from_x |  from_y | to_id |  distance |
|--------:|--------:|--------:|:------|----------:|
|       1 | 1717064 | 1633983 | NA    |  9485.128 |
|       2 | 1661430 | 1585445 | NA    |  1597.318 |
|       3 | 1271148 | 1553158 | NA    | 45573.234 |
|       4 | 1519455 | 1408060 | NA    |  1668.273 |
|       5 | 1511056 | 1608509 | NA    | 24443.245 |
|       6 | 1453314 | 1448260 | NA    |  3835.407 |

<sup>Created on 2023-12-18 with [reprex v2.0.2](https://reprex.tidyverse.org)</sup>
