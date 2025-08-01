% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chopin-package.R
\docType{package}
\name{chopin-package}
\alias{chopin-package}
\alias{chopin}
\title{Computation of spatial data by hierarchical and objective partitioning of inputs for parallel processing}
\description{
The \code{chopin} package provides a set of functions to compute on divided
geospatial data.
}
\section{Basic functionalities}{

\itemize{
\item Distribute \code{terra}, \code{sf}, and \code{chopin} functions to parallel workers set by \code{future} or \code{mirai}
\item Set parallelization strategies based on artificial grids, equal-size clusters, hierarchy, and multiple raster files
\item Convenience functions for raster-vector overlay and weighted summary from vector dataset
}
}

\section{\code{chopin} workflow}{

\itemize{
\item The simplest way of parallelizing generic geospatial computation is to start from \verb{par_pad_*} functions to \code{par_grid}, or running \code{par_hierarchy}, or \code{par_multirasters} functions at once.
}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{library(chopin)
library(terra)
library(sf)
library(collapse)
library(dplyr)
library(future)
library(future.mirai)
library(future.apply)
lastpar <- par(mfrow = c(1, 1))
options(sf_use_s2 = FALSE)
}\if{html}{\out{</div>}}

\strong{Example data}
\itemize{
\item North Carolinian counties and a raster of elevation data are used as example data.
}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{temp_dir <- tempdir(check = TRUE)

url_nccnty <-
  paste0(
   "https://raw.githubusercontent.com/",
   "ropensci/chopin/refs/heads/main/",
   "tests/testdata/nc_hierarchy.gpkg"
 )  

url_ncelev <-
  paste0(
   "https://raw.githubusercontent.com/",
   "ropensci/chopin/refs/heads/main/",
   "tests/testdata/nc_srtm15_otm.tif"
 )

nccnty_path <- file.path(temp_dir, "nc_hierarchy.gpkg")
ncelev_path <- file.path(temp_dir, "nc_srtm15_otm.tif")

# download data
download.file(url_nccnty, nccnty_path, quiet = TRUE)
download.file(url_ncelev, ncelev_path, quiet = TRUE)
nccnty <- terra::vect(nccnty_path)
ncelev <- terra::rast(ncelev_path)
}\if{html}{\out{</div>}}
\itemize{
\item To demonstrate chopin functions, we generate 10,000 random points in North Carolina
}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{ncsamp <-
  terra::spatSample(
    nccnty,
    1e4L
  )
ncsamp$pid <- 1:nrow(ncsamp)
}\if{html}{\out{</div>}}

\strong{Creating grids}
\itemize{
\item The example below will generate a regular grid from the random point data.
}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{ncgrid <- par_pad_grid(ncsamp, mode = "grid", nx = 4L, ny = 2L, padding = 10000)
plot(ncgrid$original)
}\if{html}{\out{</div>}}

\strong{Extracting values from raster}
\itemize{
\item Since all \verb{par_*} functions operate on \code{future} backends, users should define the future plan before running the functions. \code{multicore} plan supports \code{terra} objects which may lead to faster computation, but it is not supported in Windows. An alternative is \code{future.mirai}'s \code{mirai_multisession} plan, which is supported in many platforms and generally faster than plain future multisession plan.
\item \code{workers} argument should be defined with an integer value to specify the number of threads to be used.
}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{future::plan(future.mirai::mirai_multisession, workers = 2L)
}\if{html}{\out{</div>}}
\itemize{
\item Then we dispatch multiple \code{extract_at} runs on the grid polygons.
\item Before we proceed, the terra object should be converted to sf object.
}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{pg <-
  par_grid(
    grids = ncgrid,
    pad_y = FALSE,
    .debug = TRUE,
    fun_dist = extract_at,
    x = ncelev_path,
    y = sf::st_as_sf(ncsamp),
    id = "pid",
    radius = 1e4,
    func = "mean"
  )
}\if{html}{\out{</div>}}

\strong{Hierarchical processing}
\itemize{
\item Here we demonstrate hierarchical processing of the random points using census tract polygons.
}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{nccnty <- sf::st_read(nccnty_path, layer = "county")
nctrct <- sf::st_read(nccnty_path, layer = "tracts")
}\if{html}{\out{</div>}}
\itemize{
\item The example below will parallelize summarizing mean elevation at 10 kilometers circular buffers of random sample points by the first five characters of census tract unique identifiers, which are county codes.
\item This example demonstrates the hierarchy can be defined from any given polygons if the unique identifiers are suitably formatted for defining the hierarchy.
}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{px <-
  par_hierarchy(
    # from here the par_hierarchy-specific arguments
    regions = nctrct,
    regions_id = "GEOID",
    length_left = 5,
    pad = 10000,
    pad_y = FALSE,
    .debug = TRUE,
    # from here are the dispatched function definition
    # for parallel workers
    fun_dist = extract_at,
    # below should follow the arguments of the dispatched function
    x = ncelev,
    y = sf::st_as_sf(ncsamp),
    id = "pid",
    radius = 1e4,
    func = "mean"
  )
}\if{html}{\out{</div>}}

\strong{Multiraster processing}
\itemize{
\item Here we demonstrate multiraster processing of the random points using multiple rasters.
}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{ncelev <- terra::rast(ncelev_path)
tdir <- tempdir(check = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test1.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test2.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test3.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test4.tif"), overwrite = TRUE)
terra::writeRaster(ncelev, file.path(tdir, "test5.tif"), overwrite = TRUE)
rasts <- list.files(tdir, pattern = "tif$", full.names = TRUE)

pm <-
  par_multirasters(
    filenames = rasts,
    fun_dist = extract_at,
    x = NA,
    y = sf::st_as_sf(ncsamp)[1:500, ],
    id = "pid",
    radius = 1e4,
    func = "mean",
    .debug = TRUE
  )
par(lastpar)
}\if{html}{\out{</div>}}
}

\section{Function selection guide for \verb{par_*()}}{

We provide two flowcharts to help users choose the right function for
parallel processing. The raster-oriented flowchart is for users who
want to start with raster data, and the vector-oriented flowchart
is for users with large vector data.

In \strong{raster-oriented selection}, we suggest four factors to consider:
\itemize{
\item Number of raster files: for multiple files, \code{par_multirasters} is recommended. When there are multiple rasters that share the same extent and resolution, consider stacking the rasters into multilayer SpatRaster object by calling \code{terra::rast(filenames)}.
\item Raster resolution: We suggest 100 meters as a threshold. Rasters with resolution coarser than 100 meters and a few layers would be better for the direct call of \code{exactextractr::exact_extract()}.
\item Raster extent: Using \code{SpatRaster} in \code{exactextractr::exact_extract()} is often minimally affected by the raster extent.
\item Memory size: \code{max_cells_in_memory} argument value of \code{exactextractr::exact_extract()}, raster resolution, and the number of layers in \code{SpatRaster} are multiplicatively related to the memory usage.
}

For \strong{vector-oriented selection}, we suggest three factors to consider:
\itemize{
\item Number of features: When the number of features is over 100,000, consider using \code{par_grid} or \code{par_hierarchy} to split the data into smaller chunks.
\item Hierarchical structure: If the data has a hierarchical structure, consider using \code{par_hierarchy} to parallelize the operation.
\item Data grouping: If the data needs to be grouped in similar sizes, consider using \code{par_pad_balanced} or \code{par_pad_grid} with \code{mode = "grid_quantile"}.
}
}

\section{Caveats}{

\strong{Why parallelization is slower than the ordinary function run?}
Parallelization may underperform when the datasets are too small
to take advantage of divide-and-compute approach, where
parallelization overhead is involved. Overhead here refers to
the required amount of computational resources for transferring
objects to multiple processes. Since the demonstrations above
use quite small datasets, the advantage of parallelization was not
as noticeable as it was expected. Should a large amount of
data (spatial/temporal resolution or number of files,
for example) be processed, users could find the efficiency of this
package. A vignette in this package demonstrates use cases
extracting various climate/weather datasets.

\strong{Notes on data restrictions}

\code{chopin} works best with \strong{two-dimensional} (\strong{planar}) geometries.
Users should disable \code{s2} spherical geometry mode in \code{sf} by setting
\code{sf::sf_use_s2(FALSE)}.
Running any \code{chopin} functions at spherical or three-dimensional
(e.g., including M/Z dimensions) geometries
may produce incorrect or unexpected results.
}

\seealso{
Useful links:
\itemize{
  \item \url{https://docs.ropensci.org/chopin/}
  \item \url{https://github.com/ropensci/chopin}
  \item Report bugs at \url{https://github.com/ropensci/chopin/issues}
}

}
\author{
\strong{Maintainer}: Insang Song \email{geoissong@gmail.com} (\href{https://orcid.org/0000-0001-8732-3256}{ORCID})

Authors:
\itemize{
  \item Kyle Messier (\href{https://orcid.org/0000-0001-9508-9623}{ORCID}) [contributor]
}

Other contributors:
\itemize{
  \item Alec L. Robitaille (Alec reviewed the package version 0.6.3 for rOpenSci, see <https://github.com/ropensci/software-review/issues/638>) [reviewer]
  \item Eric R. Scott (Eric reviewed the package version 0.6.3 for rOpenSci, see <https://github.com/ropensci/software-review/issues/638>) [reviewer]
}

}
\keyword{internal}
