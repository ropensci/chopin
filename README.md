
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chopin

<!-- badges: start -->

[![test-coverage](https://github.com/Spatiotemporal-Exposures-and-Toxicology/chopin/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/chopin/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/github/Spatiotemporal-Exposures-and-Toxicology/chopin/graph/badge.svg?token=IG64A3MFUA)](https://codecov.io/github/Spatiotemporal-Exposures-and-Toxicology/chopin)
[![R-CMD-check](https://github.com/Spatiotemporal-Exposures-and-Toxicology/chopin/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/chopin/actions/workflows/check-standard.yaml)

<!-- badges: end -->

## Objective and target users

### What `chopin` does

-   This package automates
    [parallelization](https://en.wikipedia.org/wiki/Parallel_computing)
    in spatial operations with `chopin` functions as well as
    [sf](https://github.com/r-spatial/sf)/[terra](https://github.com/rspatial/terra)
    functions. With [GDAL](https://gdal.org)-compatible files and
    database tables, `chopin` functions help to calculate spatial
    variables from vector and raster data with no external software
    requirements.

### For whom `chopin` is useful

-   Following user groups will find this package useful to accelerate
    the covariate calculation process for further analysis and modeling:
    -   Environmental health researchers and data analysts
    -   Health geographers and spatial epidemiologists
-   We assume that users–
    -   Have basic knowledge of [geographic information system data
        models](https://r.geocompx.org/spatial-class), [spatial
        operations](https://r.geocompx.org/spatial-operations), and
        [raster-vector overlay](https://r.geocompx.org/raster-vector);
    -   Understood and planned **what they want to calculate**; and
    -   Collected **which datasets they need**

## Basic design

-   Processing functions accept
    [sf](https://github.com/r-spatial/sf)/[terra](https://github.com/rspatial/terra)’s
    classes for spatial data. Raster-vector overlay is done with
    `exactextractr`.
-   As of version 0.3.0, this package supports three basic functions
    that are readily parallelized over multithread environments:
    -   `extract_at`: extract raster values with point buffers or
        polygons.
        -   `extract_at_buffer`
        -   `extract_at_poly`
    -   `summarize_sedc`: calculate sums of [exponentially decaying
        contributions](https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html)
    -   `summarize_aw`: area-weighted covariates based on target and
        reference polygons
-   When processing points/polygons in parallel, the entire study area
    will be divided into partly overlapped grids or processed through
    its own hierarchy.
    -   `par_grid`: parallelize over artificial grid polygons that are
        generated from the maximum extent of inputs
    -   `par_hierarchy`: parallelize over hierarchy coded in identifier
        fields (for example, census blocks in each county in the US)
    -   `par_multirasters`: parallelize over multiple raster files

## Installation

-   You can install `chopin` using `pak::pak`,
    `devtools::install_github`, or `remotes::install_github`:

``` r
# install.packages("devtools")
devtools::install_github("Spatiotemporal-Exposures-and-Toxicology/chopin")
```

``` r
# install.packages("pak")
pak::pak("Spatiotemporal-Exposures-and-Toxicology/chopin")
```

``` r
# install.packages("remotes")
remotes::install_github("Spatiotemporal-Exposures-and-Toxicology/chopin")
```

## Use case

-   Please refer to a small example below for extracting mean altitude
    values at circular point buffers and census tracts in North
    Carolina.

``` r
# check and install packages to run examples
check_and_install <-
  function(pkg_name) {
    if (!require(pkg_name, character.only = TRUE)) {
      install.packages(pkg_name)
      library(pkg_name, character.only = TRUE, quietly = TRUE)
    }
  }

# chopin is loaded separately as it is only available on GitHub now
library(chopin)

# package names
pkgs <- c("dplyr", "sf", "terra", "future", "future.apply", "doFuture")
invisible(sapply(pkgs, check_and_install))
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: sf
#> Linking to GEOS 3.7.2, GDAL 3.0.4, PROJ 6.3.2; sf_use_s2() is TRUE
#> Loading required package: terra
#> terra 1.7.71
#> Loading required package: future
#> Loading required package: future.apply
#> Loading required package: doFuture
#> Loading required package: foreach

# disable spherical geometries
options(sf_use_s2 = FALSE)
# parallelization-safe random number generator
set.seed(2024, kind = "L'Ecuyer-CMRG")
```

``` r
ncpoly <- system.file("shape/nc.shp", package = "sf")
ncsf <- sf::read_sf(ncpoly)
ncsf <- sf::st_transform(ncsf, "EPSG:5070")
plot(sf::st_geometry(ncsf))
```

<img src="man/figures/README-read-nc-1.png" width="100%" />

<!--![](https://i.imgur.com/ImPfGXP.png) -->

### Generate random points in NC

-   Ten thousands random point locations were generated inside the
    counties of North Carolina.

``` r
ncpoints <- sf::st_sample(ncsf, 1e4)
ncpoints <- sf::st_as_sf(ncpoints)
ncpoints$pid <- sprintf("PID-%05d", seq(1, 1e4))
plot(sf::st_geometry(ncpoints))
```

<img src="man/figures/README-gen-ncpoints-1.png" width="100%" />

### Target raster dataset: [Shuttle Radar Topography Mission](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-srtm-1)

-   We use an elevation dataset with and a moderate spatial resolution
    (approximately 400 meters or 0.25 miles).

``` r
# data preparation
(wdir <- tempdir())
#> [1] "/tmp/Rtmp43OqGc"
path_srtm <- file.path(wdir, "nc_srtm15_otm.rds")

if (!file.exists(path_srtm)) {
  download.file(
    "https://github.com/Spatiotemporal-Exposures-and-Toxicology/chopin/raw/main/tools/testdata/nc_srtm15_otm.rds",
    path_srtm
  )
}

# terra SpatRaster objects are wrapped when exported to rds file
srtm <- terra::unwrap(readRDS(path_srtm))
terra::crs(srtm) <- "EPSG:5070"
srtm
#> class       : SpatRaster 
#> dimensions  : 1534, 2281, 1  (nrow, ncol, nlyr)
#> resolution  : 391.5026, 391.5026  (x, y)
#> extent      : 1012872, 1905890, 1219961, 1820526  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / Conus Albers (EPSG:5070) 
#> source(s)   : memory
#> name        : file928c3830468b 
#> min value   :        -3589.291 
#> max value   :         1946.400
plot(srtm)
```

<img src="man/figures/README-load-srtm-1.png" width="100%" />

<!-- ![](https://i.imgur.com/l08bz4j.png)-->

``` r
ncpoints_tr <- terra::vect(ncpoints)
system.time(
  ncpoints_srtm <-
    chopin::extract_at(
      vector = ncpoints_tr,
      raster = srtm,
      id = "pid",
      mode = "buffer",
      radius = 1e4L  # 10,000 meters (10 km)
    )
)
#>    user  system elapsed 
#>  11.031   0.360  11.424
```

### Generate regular grid computational regions

-   `chopin::par_make_gridset` takes a spatial dataset to generate
    regular grid polygons with `nx` and `ny` arguments with padding.
    Users will have both overlapping (by the degree of `radius`) and
    non-overlapping grids, both of which will be utilized to split
    locations and target datasets into sub-datasets for efficient
    processing.

``` r
compregions <-
  chopin::par_make_gridset(
    ncpoints_tr,
    mode = "grid",
    nx = 8L,
    ny = 5L,
    padding = 1e4L
  )
```

-   `compregions` is a list object with two elements named `original`
    (non-overlapping grid polygons) and `padded` (overlapping by
    `padding`). The figures below illustrate the grid polygons with and
    without overlaps.

``` r
names(compregions)
#> [1] "original" "padded"

oldpar <- par()
par(mfcol = c(1, 2))
plot(compregions$original, main = "Original grids")
plot(compregions$padded, main = "Padded grids")
```

<img src="man/figures/README-compare-compregions-1.png" width="100%" />

<!--![](https://i.imgur.com/c0xweeV.png) -->

### Parallel processing

-   Using the grid polygons, we distribute the task of averaging
    elevations at 10,000 circular buffer polygons, which are generated
    from the random locations, with 10 kilometers radius by
    `chopin::par_grid`.
-   Users always need to **register** multiple CPU threads (logical
    cores) for parallelization.
-   `chopin::par_*` functions are flexible in terms of supporting
    generic spatial operations in `sf` and `terra`, especially where two
    datasets are involved.
    -   Users can inject generic functions’ arguments (parameters) by
        writing them in the ellipsis (`...`) arguments, as demonstrated
        below:

``` r
future::plan(future::multicore, workers = 4L)
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
#> Your input function was successfully run at CGRIDID: 1
#> Your input function was successfully run at CGRIDID: 2
#> Your input function was successfully run at CGRIDID: 3
#> Your input function was successfully run at CGRIDID: 4
#> Your input function was successfully run at CGRIDID: 5
#> Your input function was successfully run at CGRIDID: 6
#> Your input function was successfully run at CGRIDID: 7
#> Your input function was successfully run at CGRIDID: 8
#> Your input function was successfully run at CGRIDID: 9
#> Your input function was successfully run at CGRIDID: 10
#> Your input function was successfully run at CGRIDID: 11
#> Your input function was successfully run at CGRIDID: 12
#> Your input function was successfully run at CGRIDID: 13
#> Your input function was successfully run at CGRIDID: 14
#> Your input function was successfully run at CGRIDID: 15
#> Your input function was successfully run at CGRIDID: 16
#> Your input function was successfully run at CGRIDID: 17
#> Your input function was successfully run at CGRIDID: 18
#> Your input function was successfully run at CGRIDID: 19
#> Your input function was successfully run at CGRIDID: 20
#> Your input function was successfully run at CGRIDID: 21
#> Your input function was successfully run at CGRIDID: 22
#> Your input function was successfully run at CGRIDID: 23
#> Your input function was successfully run at CGRIDID: 24
#> Your input function was successfully run at CGRIDID: 25
#> Your input function was successfully run at CGRIDID: 26
#> Your input function was successfully run at CGRIDID: 27
#> Your input function was successfully run at CGRIDID: 28
#> Your input function was successfully run at CGRIDID: 29
#> Your input function was successfully run at CGRIDID: 30
#> Your input function was successfully run at CGRIDID: 31
#> Your input function was successfully run at CGRIDID: 32
#> Your input function was successfully run at CGRIDID: 33
#>    user  system elapsed 
#>   8.777   1.060   5.089
```

``` r
colnames(ncpoints_srtm_mthr)[2] <- "mean_par"
ncpoints_compar <- merge(ncpoints_srtm, ncpoints_srtm_mthr)
all.equal(ncpoints_compar$mean, ncpoints_compar$mean_par)
#> [1] TRUE
```

``` r
ncpoints_s <-
    merge(ncpoints, ncpoints_srtm)
ncpoints_m <-
    merge(ncpoints, ncpoints_srtm_mthr)

plot(ncpoints_s[, "mean"], main = "Single-thread")
```

<!--![](https://i.imgur.com/iaQHWBL.png) -->

``` r
plot(ncpoints_m[, "mean"], main = "Multi-thread")
```

<!--![](https://i.imgur.com/fgOvOff.png) -->

### Parallelize geospatial computations using intrinsic data hierarchy: `chopin::par_hierarchy`

-   In real world datasets, we usually have nested/exhaustive
    hierarchies. For example, land is organized by
    administrative/jurisdictional borders where multiple levels exist.
    In the U.S. context, a state consists of several counties, counties
    are split into census tracts, and they have a group of block groups.
-   `chopin::par_hierarchy` leverages such hierarchies to parallelize
    geospatial operations, which means that a group of lower-level
    geographic units in a higher-level geography is assigned to a
    process.
-   A demonstration below shows that census tracts are grouped by their
    counties then each county will be processed in a CPU thread.

``` r
path_nchrchy <- file.path(wdir, "nc_hierarchy.gpkg")
if (!file.exists(path_nchrchy)) {
  download.file(
    "https://github.com/Spatiotemporal-Exposures-and-Toxicology/chopin/raw/main/tools/testdata/nc_hierarchy.gpkg",
    path_nchrchy
  )
}

nc_data <- path_nchrchy
nc_county <- sf::st_read(nc_data, layer = "county")
#> Reading layer `county' from data source `/tmp/Rtmp43OqGc/nc_hierarchy.gpkg' using driver `GPKG'
#> Simple feature collection with 100 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1054155 ymin: 1341756 xmax: 1838923 ymax: 1690176
#> Projected CRS: NAD83 / Conus Albers
nc_tracts <- sf::st_read(nc_data, layer = "tracts")
#> Reading layer `tracts' from data source `/tmp/Rtmp43OqGc/nc_hierarchy.gpkg' using driver `GPKG'
#> Simple feature collection with 2672 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1054155 ymin: 1341756 xmax: 1838923 ymax: 1690176
#> Projected CRS: NAD83 / Conus Albers

# reproject to Conus Albers Equal Area
nc_county <- sf::st_transform(nc_county, "EPSG:5070")
nc_tracts <- sf::st_transform(nc_tracts, "EPSG:5070")
nc_tracts$COUNTY <- substr(nc_tracts$GEOID, 1, 5)
```

``` r
# single-thread
system.time(
  nc_elev_tr_single <- chopin::extract_at(
    vector = nc_tracts,
    raster = srtm,
    id = "GEOID",
    mode = "polygon"
  )
)
#>    user  system elapsed 
#>   2.159   0.081   2.246

# hierarchical parallelization
system.time(
  nc_elev_tr_distr <-
    chopin::par_hierarchy(
      regions = nc_county, # higher level geometry
      split_level = "GEOID", # higher level unique id
      fun_dist = chopin::extract_at,
      vector = nc_tracts, # lower level geometry
      raster = srtm,
      id = "GEOID", # lower level unique id
      func = "mean"
    )
)
#>    user  system elapsed 
#>   0.054   0.016   2.579
```

### Multiple rasters

-   There is a common case of having a large group of raster files at
    which the same operation should be performed.
-   `chopin::par_multirasters` is for such cases. An example below
    demonstrates where we have five elevation raster files to calculate
    the average elevation at counties in North Carolina.

``` r
nccnty <- terra::vect(nc_data, layer = "county")
ncelev <- terra::unwrap(readRDS(path_srtm))
terra::crs(ncelev) <- "EPSG:5070"
names(ncelev) <- c("srtm15")

terra::writeRaster(ncelev, file.path(wdir, "test1.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(wdir, "test2.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(wdir, "test3.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(wdir, "test4.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(wdir, "test5.tif"), overwrite = TRUE)

# check if the raster files were exported as expected
testfiles <- list.files(wdir, pattern = "*.tif$", full.names = TRUE)
testfiles
#> [1] "/tmp/Rtmp43OqGc/test1.tif" "/tmp/Rtmp43OqGc/test2.tif"
#> [3] "/tmp/Rtmp43OqGc/test3.tif" "/tmp/Rtmp43OqGc/test4.tif"
#> [5] "/tmp/Rtmp43OqGc/test5.tif"
```

``` r
system.time(
  res <-
    par_multirasters(
      filenames = testfiles,
      fun_dist = extract_at_poly,
      polys = nccnty,
      surf = ncelev,
      id = "GEOID",
      func = "mean"
    )
)
#>    user  system elapsed 
#>   1.259   0.371   0.980
knitr::kable(head(res))
```

| GEOID |      mean | base_raster               |
|:------|----------:|:--------------------------|
| 37037 | 136.80203 | /tmp/Rtmp43OqGc/test1.tif |
| 37001 | 189.76170 | /tmp/Rtmp43OqGc/test1.tif |
| 37057 | 231.16968 | /tmp/Rtmp43OqGc/test1.tif |
| 37069 |  98.03845 | /tmp/Rtmp43OqGc/test1.tif |
| 37155 |  41.23463 | /tmp/Rtmp43OqGc/test1.tif |
| 37109 | 270.96933 | /tmp/Rtmp43OqGc/test1.tif |

<!--| GEOID |      mean |
|:------|----------:|
| 37037 | 136.80203 |
| 37001 | 189.76170 |
| 37057 | 231.16968 |
| 37069 |  98.03845 |
| 37155 |  41.23463 |
| 37109 | 270.96933 |
-->

### Parallelization of a generic geospatial operation

-   Other than `chopin` internal macros, `chopin::par_*` functions
    support generic geospatial operations.
-   An example below uses `terra::nearest`, which gets the nearest
    feature’s attributes, inside `chopin::par_grid`.

``` r
path_ncrd1 <- file.path(wdir, "ncroads_first.gpkg")
if (!file.exists(path_ncrd1)) {
  download.file(
    "https://github.com/Spatiotemporal-Exposures-and-Toxicology/chopin/raw/main/tools/testdata/ncroads_first.gpkg",
    path_ncrd1
  )
}

pnts <- sf::st_sample(ncsf, 5000)
pnts <- sf::st_as_sf(pnts)
pnts$pid <- sprintf("RPID-%04d", seq(1, 5000))
pnts <- terra::vect(pnts)
rd1 <- terra::vect(path_ncrd1)

# reproject
pnts <- terra::project(pnts, "EPSG:5070")
rd1 <- terra::project(rd1, "EPSG:5070")

# generate grids
nccompreg <-
  par_make_gridset(
    input = pnts,
    mode = "grid",
    nx = 4L,
    ny = 2L,
    padding = 5e4L
  )

# plot
plot(nccompreg$padded, border = "orange")
plot(terra::vect(ncsf), add = TRUE)
plot(rd1, col = "blue", add = TRUE)
plot(pnts, add = TRUE, cex = 0.5)
```

<img src="man/figures/README-prep-par-generic-1.png" width="100%" />

``` r
system.time(
  restr <- terra::nearest(x = pnts, y = rd1)
)
#>    user  system elapsed 
#>   0.878   0.002   0.882

# we use four threads that were configured above
system.time(
  res <-
    par_grid(
      grids = nccompreg,
      fun_dist = terra::nearest,
      x = pnts,
      y = rd1
    )
)
#> Your input function was successfully run at CGRIDID: 1
#> Your input function was successfully run at CGRIDID: 2
#> Your input function was successfully run at CGRIDID: 3
#> Your input function was successfully run at CGRIDID: 4
#> Your input function was successfully run at CGRIDID: 5
#> Your input function was successfully run at CGRIDID: 6
#> Your input function was successfully run at CGRIDID: 7
#> Your input function was successfully run at CGRIDID: 8
#>    user  system elapsed 
#>   0.745   0.186   0.388
```

``` r
resj <- merge(restr, res, by = c("from_x", "from_y"))
all.equal(resj$distance.x, resj$distance.y)
#> [1] TRUE
```

-   Users should be mindful of potential caveats in the parallelization
    of nearest feature search, which may result in no or excess distance
    depending on the distribution of the target dataset to which the
    nearest feature is searched.
    -   For example, when one wants to calculate the nearest interstate
        from rural homes with fine grids, some grids may have no
        interstates then homes in such grids will not get any distance
        to the nearest interstate.
    -   Such problems can be avoided by meticulously choosing `nx`,
        `ny`, and `padding` values in `par_make_gridset`.

### Why parallelization is slower than the ordinary function run?

-   Parallelization may underperform when the datasets are too small to
    take advantage of divide-and-compute approach, where parallelization
    overhead is involved. Overhead here refers to the required amount of
    computational resources for transferring objects to multiple
    processes.
-   Since the demonstrations above use quite small datasets, the
    advantage of parallelization was not as dramatically as it was
    expected. Should a large amount of data (spatial/temporal resolution
    or number of files, for example) be processed, users could see the
    efficiency of this package. More illustrative and truly scaled
    examples will be provided in vignettes and manuscripts in the near
    future.

#### Last edited: February 2, 2024
