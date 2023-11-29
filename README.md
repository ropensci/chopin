[![test-coverage](https://github.com/Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/github/Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS/graph/badge.svg?token=IG64A3MFUA)](https://codecov.io/github/Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS)
[![R-CMD-check](https://github.com/Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/Scalable_GIS/actions/workflows/check-standard.yaml)

    
# Scalable_GIS
Scalable GIS methods for environmental and climate data analysis 


# Basic design
- Processing functions accept sf/terra classes for spatial data. Raster-vector overlay is done with `exactextractr`.
- As of version 0.1.0, this package supports three basic functions that are readily parallelized over multithread environments:
    - `extract_with`: extract raster values with point buffers or polygons.
        - `extract_with_polygons`
        - `extract_with_buffer`
    - `calculate_sedc`: calculate sums of exponentially decaying contributions
    - `aw_covariates`: area-weighted covariates based on target and reference polygons

- When processing points/polygons in parallel, the entire study area will be divided into partly overlapped grids or processed through its own hierarchy.
    - `distribute_process_grid`
    - `distribute_process_hierarchy`


# Use case
- Please refer to a small example below for extracting mean altitude values at circular point buffers and census tracts in North Carolina.

``` r
library(scomps)
library(dplyr)
library(sf)
#> Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE
library(terra)
#> terra 1.7.55
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

![](https://i.imgur.com/UIMVlFy.png)<!-- -->

``` r
ncpoints <- sf::st_sample(ncsf, 10000)
plot(sf::st_geometry(ncpoints))
```

![](https://i.imgur.com/BJUN6B0.png)<!-- -->

``` r

# st_sample output is st_sfc. We should convert it to sf
ncpoints <- st_as_sf(ncpoints)
ncpoints$pid <- seq(1, nrow(ncpoints))
```

``` r
srtm <- terra::unwrap(readRDS("./tests/testdata/nc_srtm15_otm.rds"))
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

![](https://i.imgur.com/8EJA8r2.png)<!-- -->

``` r
terra::crs(srtm) <- "EPSG:5070"
```

``` r
ncpoints_tr <- terra::vect(ncpoints)
system.time(
    ncpoints_srtm <-
        scomps::extract_with(
            vector = ncpoints_tr,
            raster = srtm,
            id = "pid",
            mode = "buffer",
            radius = 1e4L) # 10,000 meters (10 km)
)
#>    user  system elapsed 
#>   6.605   0.205   6.824
```

``` r
compregions <-
    scomps::get_computational_regions(
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

![](https://i.imgur.com/g9x7Lim.png)


``` r
plan(multicore, workers = 4L)
doFuture::registerDoFuture()

system.time(
    ncpoints_srtm_mthr <-
        scomps::distribute_process_grid(
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
#>           successfully run at CGRIDID: 1
#> Your input function was 
#>           successfully run at CGRIDID: 2
#> Your input function was 
#>           successfully run at CGRIDID: 3
#> Your input function was 
#>           successfully run at CGRIDID: 4
#> Your input function was 
#>           successfully run at CGRIDID: 5
#> Your input function was 
#>           successfully run at CGRIDID: 6
#> Your input function was 
#>           successfully run at CGRIDID: 7
#> Your input function was 
#>           successfully run at CGRIDID: 8
#> Your input function was 
#>           successfully run at CGRIDID: 9
#> Your input function was 
#>           successfully run at CGRIDID: 10
#> Your input function was 
#>           successfully run at CGRIDID: 11
#> Your input function was 
#>           successfully run at CGRIDID: 12
#> Your input function was 
#>           successfully run at CGRIDID: 13
#> Your input function was 
#>           successfully run at CGRIDID: 14
#> Your input function was 
#>           successfully run at CGRIDID: 15
#> Your input function was 
#>           successfully run at CGRIDID: 16
#> Your input function was 
#>           successfully run at CGRIDID: 17
#> Your input function was 
#>           successfully run at CGRIDID: 18
#> Your input function was 
#>           successfully run at CGRIDID: 19
#> Your input function was 
#>           successfully run at CGRIDID: 20
#> Your input function was 
#>           successfully run at CGRIDID: 21
#> Your input function was 
#>           successfully run at CGRIDID: 22
#> Your input function was 
#>           successfully run at CGRIDID: 23
#> Your input function was 
#>           successfully run at CGRIDID: 24
#> Your input function was 
#>           successfully run at CGRIDID: 25
#> Your input function was 
#>           successfully run at CGRIDID: 26
#> Your input function was 
#>           successfully run at CGRIDID: 27
#> Your input function was 
#>           successfully run at CGRIDID: 28
#> Your input function was 
#>           successfully run at CGRIDID: 29
#> Your input function was 
#>           successfully run at CGRIDID: 30
#> Your input function was 
#>           successfully run at CGRIDID: 31
#> Your input function was 
#>           successfully run at CGRIDID: 32
#> Your input function was 
#>           successfully run at CGRIDID: 33
#>    user  system elapsed 
#>   6.962   0.625   2.630
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

![](https://i.imgur.com/f1ascJ0.png)<!-- -->

``` r
plot(ncpoints_m[, "mean"], main = "Multi-thread")
```

![](https://i.imgur.com/nHqg4aK.png)<!-- -->

``` r
nc_county <- tigris::counties(state = "NC", cb = TRUE, year = 2020)

nc_tracts <- tigris::tracts(cb = TRUE, state = "NC", year = 2020)

nc_county <- sf::st_transform(nc_county, "EPSG:5070")
nc_tracts <- sf::st_transform(nc_tracts, "EPSG:5070")
nc_tracts$COUNTY <-
    paste0(nc_tracts$STATEFP, nc_tracts$COUNTYFP)
```

``` r
system.time(
    nc_elev_tr_single <- scomps::extract_with(
        vector = nc_tracts,
        raster = srtm,
        id = "GEOID",
        mode = "polygon"
    )
)
#>    user  system elapsed 
#>   0.977   0.015   0.994
```

``` r
system.time(
    nc_elev_tr_distr <-
        scomps::distribute_process_hierarchy(
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
#>   0.018   0.005   0.197
```

<sup>Created on 2023-11-29 with [reprex v2.0.2](https://reprex.tidyverse.org)</sup>