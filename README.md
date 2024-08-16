
# Computation of Spatial Data by Hierarchical and Objective Partitioning of Inputs for Parallel Processing <img src="man/figures/chopin-logo.png" align="right" height="144" alt="overlapping irregular grid polygons filled with orange, green, and teal" /></a>

<!-- badges: start -->

[![cov](https://NIEHS.github.io/chopin/badges/coverage.svg)](https://github.com/NIEHS/chopin/actions)
[![R-CMD-check](https://github.com/NIEHS/chopin/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/NIEHS/chopin/actions/workflows/check-standard.yaml)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/638_status.svg)](https://github.com/ropensci/software-review/issues/638)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

## Objective and target users

### Objective

-   This package automates
    [parallelization](https://en.wikipedia.org/wiki/Parallel_computing)
    in spatial operations with `chopin` functions as well as
    [sf](https://github.com/r-spatial/sf)/[terra](https://github.com/rspatial/terra)
    functions. With [GDAL](https://gdal.org)-compatible files and
    database tables, `chopin` functions help to calculate spatial
    variables from vector and raster data with no external software
    requirements.

### For whom `chopin` is useful

-   All who need to perform geospatial operations with large datasets
    may find this package useful to accelerate the covariate calculation
    process for further analysis and modeling.
-   We assume that users–
    -   Have basic knowledge of [geographic information system data
        models](https://r.geocompx.org/spatial-class), [coordinate
        systems and
        transformations](https://doi.org/10.22224/gistbok/2023.1.2),
        [spatial operations](https://r.geocompx.org/spatial-operations),
        and [raster-vector
        overlay](https://r.geocompx.org/raster-vector);
    -   Understood and planned **what they want to calculate**; and
    -   Collected **datasets they need**

## Overview

-   Processing functions accept
    [terra](https://github.com/rspatial/terra)/[sf](https://github.com/r-spatial/sf)
    classes for spatial data. Raster-vector overlay is done with
    `exactextractr`.
-   From version 0.8.0, this package supports three basic functions that
    are readily parallelized over multithread environments:
    -   `extract_at`: extract raster values with point buffers or
        polygons with or without kernel weights
    -   `summarize_sedc`: calculate sums of [exponentially decaying
        contributions](https://mserre.sph.unc.edu/BMElab_web/SEDCtutorial/index.html)
    -   `summarize_aw`: area-weighted covariates based on target and
        reference polygons
-   When processing points/polygons in parallel, the entire study area
    will be divided into partly overlapped grids or processed through
    its own hierarchy. We suggest two flowcharts to help which function
    to use for parallel processing below. The upper flowchart is
    raster-oriented and the lower one is vector-oriented. They are
    separated but supplementary to each other. When a user follows the
    raster-oriented one, they might visit the vector-oriented flowchart
    at each end of the raster-oriented flowchart.
    -   `par_grid`: parallelize over artificial grid polygons that are
        generated from the maximum extent of inputs. `par_pad_grid` is
        used to generate the grid polygons before running this function.
    -   `par_hierarchy`: parallelize over hierarchy coded in identifier
        fields (for example, census blocks in each county in the US)
    -   `par_multirasters`: parallelize over multiple raster files
-   These functions are designed to be used with `future` and
    `future.mirai` packages to parallelize over multiple CPU threads.
    Users can choose the number of threads to be used in the
    parallelization process. Users always need to register parallel
    workers with `future` before running the three functions above.

``` r
future::plan(future.mirai::mirai_multisession, workers = 4L)
# future::multisession, future::cluster are available,
# See future.batchtools and future.callr for other options
# the number of workers are up to users' choice
```

<img src="man/figures/README-flowchart-mermaid-raster-1.png" width="100%" />

<img src="man/figures/README-flowchart-mermaid-vector-1.png" width="100%" />

## Installation

-   `chopin` can be installed using `remotes::install_github` (also
    possible with `pak::pak` or `devtools::install_github`).

``` r
rlang::check_installed("remotes")
remotes::install_github("NIEHS/chopin")
```

## Examples

-   Examples will navigate `par_grid`, `par_hierarchy`, and
    `par_multirasters` functions in `chopin` to parallelize geospatial
    operations.

``` r
# check and install packages to run examples
pkgs <- c("chopin", "dplyr", "sf", "terra", "future", "future.mirai", "mirai")
# install packages if anything is unavailable
rlang::check_installed(pkgs)

library(chopin)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.5, PROJ 9.4.0; sf_use_s2() is TRUE
library(terra)
#> terra 1.7.78
library(future)
library(future.mirai)
library(mirai)

# disable spherical geometries
sf::sf_use_s2(FALSE)
#> Spherical geometry (s2) switched off

# parallelization-safe random number generator
set.seed(2024, kind = "L'Ecuyer-CMRG")
```

### `par_grid`: parallelize over artificial grid polygons

-   Please refer to a small example below for extracting mean altitude
    values at circular point buffers and census tracts in North
    Carolina.
-   Before running code chunks below, set the cloned `chopin` repository
    as your working directory with `setwd()`

``` r
ncpoly <- system.file("shape/nc.shp", package = "sf")
ncsf <- sf::read_sf(ncpoly)
ncsf <- sf::st_transform(ncsf, "EPSG:5070")
plot(sf::st_geometry(ncsf))
```

<img src="man/figures/README-read-nc-1.png" width="100%" />

#### Generate random points in NC

-   Ten thousands random point locations were generated inside the
    counties of North Carolina.

``` r
ncpoints <- sf::st_sample(ncsf, 1e4)
ncpoints <- sf::st_as_sf(ncpoints)
ncpoints$pid <- sprintf("PID-%05d", seq(1, 1e4))
plot(sf::st_geometry(ncpoints))
```

<img src="man/figures/README-gen-ncpoints-1.png" width="100%" />

#### Target raster dataset: [Shuttle Radar Topography Mission](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-srtm-1)

-   We use an elevation dataset with and a moderate spatial resolution
    (approximately 400 meters or 0.25 miles).

``` r
# data preparation
wdir <- system.file("extdata", package = "chopin")
srtm <- file.path(wdir, "nc_srtm15_otm.tif")

# terra SpatRaster objects are wrapped when exported to rds file
srtm_ras <- terra::rast(srtm)
terra::crs(srtm_ras) <- "EPSG:5070"
srtm_ras
#> class       : SpatRaster 
#> dimensions  : 1534, 2281, 1  (nrow, ncol, nlyr)
#> resolution  : 391.5026, 391.5026  (x, y)
#> extent      : 1012872, 1905890, 1219961, 1820526  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / Conus Albers (EPSG:5070) 
#> source      : nc_srtm15_otm.tif 
#> name        :    srtm15 
#> min value   : -3589.291 
#> max value   :  1946.400
terra::plot(srtm_ras)
```

<img src="man/figures/README-load-srtm-1.png" width="100%" />

``` r
# ncpoints_tr <- terra::vect(ncpoints)
system.time(
  ncpoints_srtm <-
    chopin::extract_at(
      x = srtm,
      y = ncpoints,
      id = "pid",
      mode = "buffer",
      radius = 1e4L  # 10,000 meters (10 km)
    )
)
#> Input is a character. Attempt to read it with terra::rast...
#>    user  system elapsed 
#>   7.205   0.206   7.669
```

#### Generate regular grid computational regions

-   `chopin::par_pad_grid` takes a spatial dataset to generate regular
    grid polygons with `nx` and `ny` arguments with padding. Users will
    have both overlapping (by the degree of `radius`) and
    non-overlapping grids, both of which will be utilized to split
    locations and target datasets into sub-datasets for efficient
    processing.

``` r
compregions <-
  chopin::par_pad_grid(
    ncpoints,
    mode = "grid",
    nx = 2L,
    ny = 2L,
    padding = 1e4L
  )
#> Switch sf class to terra...
#> Switch terra class to sf...
```

-   `compregions` is a list object with two elements named `original`
    (non-overlapping grid polygons) and `padded` (overlapping by
    `padding`). The figures below illustrate the grid polygons with and
    without overlaps.

``` r
names(compregions)
#> [1] "original" "padded"

oldpar <- par()
par(mfrow = c(2, 1))
terra::plot(
  terra::vect(compregions$original),
  main = "Original grids"
)
terra::plot(
  terra::vect(compregions$padded),
  main = "Padded grids"
)
```

<img src="man/figures/README-compare-compregions-1.png" width="100%" />

#### Parallel processing

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
future::plan(future.mirai::mirai_multisession, workers = 4L)

system.time(
  ncpoints_srtm_mthr <-
    par_grid(
      grids = compregions,
      fun_dist = extract_at,
      x = srtm,
      y = ncpoints,
      id = "pid",
      radius = 1e4L,
      .standalone = FALSE
    )
)
#> ℹ Input is not a character.
#> Input is a character. Attempt to read it with terra::rast...
#> ℹ Task at CGRIDID: 1 is successfully dispatched.
#> 
#> Input is a character. Attempt to read it with terra::rast...
#> ℹ Task at CGRIDID: 2 is successfully dispatched.
#> 
#> Input is a character. Attempt to read it with terra::rast...
#> ℹ Task at CGRIDID: 3 is successfully dispatched.
#> 
#> Input is a character. Attempt to read it with terra::rast...
#> ℹ Task at CGRIDID: 4 is successfully dispatched.
#>    user  system elapsed 
#>   0.329   0.021   8.337

ncpoints_srtm <-
  extract_at(
    x = srtm,
    y = ncpoints,
    id = "pid",
    radius = 1e4L
  )
#> Input is a character. Attempt to read it with terra::rast...
```

``` r
colnames(ncpoints_srtm_mthr)[2] <- "mean_par"
ncpoints_compar <- merge(ncpoints_srtm, ncpoints_srtm_mthr)
# Are the calculations equal?
all.equal(ncpoints_compar$mean, ncpoints_compar$mean_par)
#> [1] TRUE
```

``` r
ncpoints_s <-
  merge(ncpoints, ncpoints_srtm)
ncpoints_m <-
  merge(ncpoints, ncpoints_srtm_mthr)

plot(ncpoints_s[, "mean"], main = "Single-thread", pch = 19, cex = 0.33)
```

<img src="man/figures/README-plot results-1.png" width="100%" />

``` r
plot(ncpoints_m[, "mean_par"], main = "Multi-thread", pch = 19, cex = 0.33)
```

<img src="man/figures/README-plot results-2.png" width="100%" />

### `chopin::par_hierarchy`: parallelize geospatial computations using intrinsic data hierarchy

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

#### Read data

``` r
# nc_hierarchy.gpkg includes two layers: county and tracts
path_nchrchy <- file.path(wdir, "nc_hierarchy.gpkg")

nc_data <- path_nchrchy
nc_county <- sf::st_read(nc_data, layer = "county")
#> Reading layer `county' from data source 
#>   `/tmp/RtmpkigVUL/temp_libpath3cdb0b6b55ee11/chopin/extdata/nc_hierarchy.gpkg' 
#>   using driver `GPKG'
#> Simple feature collection with 100 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1054155 ymin: 1341756 xmax: 1838923 ymax: 1690176
#> Projected CRS: NAD83 / Conus Albers
nc_tracts <- sf::st_read(nc_data, layer = "tracts")
#> Reading layer `tracts' from data source 
#>   `/tmp/RtmpkigVUL/temp_libpath3cdb0b6b55ee11/chopin/extdata/nc_hierarchy.gpkg' 
#>   using driver `GPKG'
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

#### Extract average SRTM elevations by single and multiple threads

``` r
# single-thread
system.time(
  nc_elev_tr_single <-
    chopin::extract_at(
      x = srtm,
      y = nc_tracts,
      id = "GEOID"
    )
)
#> Input is a character. Attempt to read it with terra::rast...
#>    user  system elapsed 
#>   0.910   0.003   0.916

# hierarchical parallelization
system.time(
  nc_elev_tr_distr <-
    chopin::par_hierarchy(
      regions = nc_county, # higher level geometry
      regions_id = "GEOID", # higher level unique id
      fun_dist = extract_at,
      x = srtm,
      y = nc_tracts, # lower level geometry
      id = "GEOID", # lower level unique id
      func = "mean"
    )
)
#> ℹ Input is not a character.
#> ℹ GEOID is used to stratify the process.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37037 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37001 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37057 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37069 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37155 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37109 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37027 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37063 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37145 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37115 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37151 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37131 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37013 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37159 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37051 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37153 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37093 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37025 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37029 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37169 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37031 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37005 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37139 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37193 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37003 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37083 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37163 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37189 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37173 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37011 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37045 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37125 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37067 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37077 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37185 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37137 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37033 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37107 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37075 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37073 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37161 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37187 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37007 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37135 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37049 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37195 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37061 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37087 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37081 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37099 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37097 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37091 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37149 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37165 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37085 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37105 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37017 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37039 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37035 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37177 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37113 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37143 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37095 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37071 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37101 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37015 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37167 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37079 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37129 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37147 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37141 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37179 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37121 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37133 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37065 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37119 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37199 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37197 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37023 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37191 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37059 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37111 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37183 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37053 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37103 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37041 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37021 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37157 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37117 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37089 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37127 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37009 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37019 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37123 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37181 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37175 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37171 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37043 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37055 is dispatched.
#> Input is a character. Attempt to read it with terra::rast...ℹ Your input function at 37047 is dispatched.
#>    user  system elapsed 
#>   0.258   0.073   2.005
```

### `par_multirasters`: parallelize over multiple rasters

-   There is a common case of having a large group of raster files at
    which the same operation should be performed.
-   `chopin::par_multirasters` is for such cases. An example below
    demonstrates where we have five elevation raster files to calculate
    the average elevation at counties in North Carolina.

``` r
# nccnty <- sf::st_read(nc_data, layer = "county")
ncelev <- terra::rast(srtm)
terra::crs(ncelev) <- "EPSG:5070"
names(ncelev) <- c("srtm15")
tdir <- tempdir()

terra::writeRaster(ncelev, file.path(tdir, "test1.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test2.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test3.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test4.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test5.tif"), overwrite = TRUE)

# check if the raster files were exported as expected
testfiles <- list.files(tdir, pattern = "*.tif$", full.names = TRUE)
testfiles
#> [1] "/tmp/Rtmpqu68xF/test1.tif" "/tmp/Rtmpqu68xF/test2.tif"
#> [3] "/tmp/Rtmpqu68xF/test3.tif" "/tmp/Rtmpqu68xF/test4.tif"
#> [5] "/tmp/Rtmpqu68xF/test5.tif"
```

``` r
system.time(
  res <-
    chopin::par_multirasters(
      filenames = testfiles,
      fun_dist = extract_at,
      x = ncelev,
      y = nc_county,
      id = "GEOID",
      func = "mean"
    )
)
#> ℹ Input is not a character.
#> Input is a character. Attempt to read it with terra::rast...
#> ℹ Your input function at /tmp/Rtmpqu68xF/test1.tif is dispatched.
#> 
#> Input is a character. Attempt to read it with terra::rast...
#> ℹ Your input function at /tmp/Rtmpqu68xF/test2.tif is dispatched.
#> 
#> Input is a character. Attempt to read it with terra::rast...
#> ℹ Your input function at /tmp/Rtmpqu68xF/test3.tif is dispatched.
#> 
#> Input is a character. Attempt to read it with terra::rast...
#> ℹ Your input function at /tmp/Rtmpqu68xF/test4.tif is dispatched.
#> 
#> Input is a character. Attempt to read it with terra::rast...
#> ℹ Your input function at /tmp/Rtmpqu68xF/test5.tif is dispatched.
#>    user  system elapsed 
#>   1.713   0.147   2.747
knitr::kable(head(res))
```

|      mean | base_raster               |
|----------:|:--------------------------|
| 136.80203 | /tmp/Rtmpqu68xF/test1.tif |
| 189.76170 | /tmp/Rtmpqu68xF/test1.tif |
| 231.16968 | /tmp/Rtmpqu68xF/test1.tif |
|  98.03845 | /tmp/Rtmpqu68xF/test1.tif |
|  41.23463 | /tmp/Rtmpqu68xF/test1.tif |
| 270.96933 | /tmp/Rtmpqu68xF/test1.tif |

``` r
# remove temporary raster files
file.remove(testfiles)
#> [1] TRUE TRUE TRUE TRUE TRUE
```

<!--| GEOID |      mean |
|:------|----------:|
| 37037 | 136.80203 |
| 37001 | 189.76170 |
| 37057 | 231.16968 |
| 37069 |  98.03845 |
| 37155 |  41.23463 |
| 37109 | 270.96933 |
-->

## Parallelization of a generic geospatial operation

-   Other than `chopin` internal macros, `chopin::par_*` functions
    support generic geospatial operations.
-   An example below uses `terra::nearest`, which gets the nearest
    feature’s attributes, inside `chopin::par_grid`.

``` r
path_ncrd1 <- file.path(wdir, "ncroads_first.gpkg")

# Generate 5000 random points
pnts <- sf::st_sample(nc_county, 5000)
pnts <- sf::st_as_sf(pnts)
# assign identifiers
pnts$pid <- sprintf("RPID-%04d", seq(1, 5000))
rd1 <- sf::st_read(path_ncrd1)
#> Reading layer `ncroads_first' from data source 
#>   `/tmp/RtmpkigVUL/temp_libpath3cdb0b6b55ee11/chopin/extdata/ncroads_first.gpkg' 
#>   using driver `GPKG'
#> Simple feature collection with 620 features and 4 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1152512 ymin: 1390719 xmax: 1748367 ymax: 1662294
#> Projected CRS: NAD83 / Conus Albers

# reproject
pntst <- sf::st_transform(pnts, "EPSG:5070")
rd1t <- sf::st_transform(rd1, "EPSG:5070")

# generate grids
nccompreg <-
  chopin::par_pad_grid(
    input = pntst,
    mode = "grid",
    nx = 4L,
    ny = 2L,
    padding = 5e4L
  )
#> Switch sf class to terra...
#> Switch terra class to sf...
```

-   The figure below shows the padded grids (50 kilometers), primary
    roads, and points. Primary roads will be selected by a padded grid
    per iteration and used to calculate the distance from each point to
    the nearest primary road. Padded grids and their overlapping areas
    will look different according to `padding` argument in
    `chopin::par_pad_grid`.

``` r
# plot
terra::plot(nccompreg$padded, border = "orange")
terra::plot(terra::vect(ncsf), add = TRUE)
terra::plot(rd1t, col = "blue", add = TRUE)
#> Warning in plot.sf(rd1t, col = "blue", add = TRUE): ignoring all but the first
#> attribute
terra::plot(pntst, add = TRUE, cex = 0.3)
legend(1.02e6, 1.72e6,
       legend = c("Computation grids (50km padding)", "Major roads"),
       lty = 1, lwd = 1, col = c("orange", "blue"),
       cex = 0.5)
```

<img src="man/figures/README-map-all-1.png" width="100%" />

``` r
# terra::nearest run
system.time(
  restr <- terra::nearest(x = terra::vect(pntst), y = terra::vect(rd1t))
)
#>    user  system elapsed 
#>   0.603   0.000   0.604

pnt_path <- file.path(tdir, "pntst.gpkg")
sf::st_write(pntst, pnt_path)
#> Writing layer `pntst' to data source `/tmp/Rtmpqu68xF/pntst.gpkg' using driver `GPKG'
#> Writing 5000 features with 1 fields and geometry type Point.

# we use four threads that were configured above
system.time(
  resd <-
    chopin::par_grid(
      grids = nccompreg,
      fun_dist = nearest,
      x = pnt_path,
      y = path_ncrd1,
      pad_y = TRUE
    )
)
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Task at CGRIDID: 1 is successfully dispatched.
#> 
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Task at CGRIDID: 2 is successfully dispatched.
#> 
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Task at CGRIDID: 3 is successfully dispatched.
#> 
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Task at CGRIDID: 4 is successfully dispatched.
#> 
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Task at CGRIDID: 5 is successfully dispatched.
#> 
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Task at CGRIDID: 6 is successfully dispatched.
#> 
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Task at CGRIDID: 7 is successfully dispatched.
#> 
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Input is a character. Trying to read with terra .
#> ℹ Task at CGRIDID: 8 is successfully dispatched.
#>    user  system elapsed 
#>   0.085   0.000   0.534
```

-   We will compare the results from the single-thread and multi-thread
    calculation.

``` r
resj <- merge(restr, resd, by = c("from_x", "from_y"))
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
    -   Such problems can be avoided by choosing `nx`, `ny`, and
        `padding` values in `par_pad_grid` meticulously.

### Notes on data restrictions

-   `chopin` works best with **two-dimensional** (**planar**)
    geometries. Users should disable `s2` spherical geometry mode in
    `sf` by setting. Running any `chopin` functions at spherical or
    three-dimensional (e.g., including M/Z dimensions) geometries may
    produce incorrect or unexpected results.

``` r
sf::sf_use_s2(FALSE)
```
