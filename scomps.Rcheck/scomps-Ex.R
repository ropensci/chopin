pkgname <- "scomps"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('scomps')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("aw_covariates")
### * aw_covariates

flush(stderr()); flush(stdout())

### Name: aw_covariates
### Title: Computing area weighted covariates using two polygon sf or
###   SpatVector objects
### Aliases: aw_covariates

### ** Examples

# package
library(sf)

# run
nc = sf::st_read(system.file("shape/nc.shp", package="sf"))
nc = sf::st_transform(nc, 5070)
pp = sf::st_sample(nc, size = 300)
pp = sf::st_as_sf(pp)
pp[["id"]] = seq(1, nrow(pp))
sf::st_crs(pp) = "EPSG:5070"
ppb = sf::st_buffer(pp, nQuadSegs=180, dist = units::set_units(20, 'km'))

system.time({ppb_nc_aw = aw_covariates(ppb, nc, 'id')})
summary(ppb_nc_aw)
#### Example of aw_covariates ends ####



cleanEx()
nameEx("check_crs")
### * check_crs

flush(stderr()); flush(stdout())

### Name: check_crs
### Title: Check Coordinate Reference System
### Aliases: check_crs

### ** Examples

# data
library(sf)
ncpath = system.file("shape/nc.shp", package = "sf")
nc = read_sf(ncpath)
check_crs(nc)




cleanEx()
nameEx("get_computational_regions")
### * get_computational_regions

flush(stderr()); flush(stdout())

### Name: get_computational_regions
### Title: Get a set of computational regions
### Aliases: get_computational_regions

### ** Examples

# data
library(sf)
ncpath = system.file("shape/nc.shp", package = "sf")
nc = read_sf(ncpath)
nc = st_transform(nc, "EPSG:5070")
# run
# nc_comp_region = get_computational_regions(nc, nx = 12, ny = 8)




cleanEx()
nameEx("grid_merge")
### * grid_merge

flush(stderr()); flush(stdout())

### Name: grid_merge
### Title: grid_merge: Merge grid polygons with given rules
### Aliases: grid_merge

### ** Examples

# library(sf)
# library(igraph)
# ligrary(dplyr)
# dg = sf::st_as_sfc(st_bbox(c(xmin = 0, ymin = 0, xmax = 8e5, ymax = 6e5)))
# sf::st_crs(dg) = 5070
# dgs = sf::st_as_sf(st_make_grid(dg, n = c(20, 15)))
# dgs$CGRIDID = seq(1, nrow(dgs))
#
# dg_sample = st_sample(dg, kappa = 5e-9, mu = 15, scale = 20000, type = "Thomas")
# sf::st_crs(dg_sample) = sf::st_crs(dg)
# dg_merged = grid_merge(sf::st_as_sf(sss), dgs, 100)
#### NOT RUN ####



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
