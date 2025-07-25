% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{reproject_std}
\alias{reproject_std}
\title{Check coordinate system then reproject}
\usage{
reproject_std(input, crs_standard = "EPSG:4326")
}
\arguments{
\item{input}{Input object one of sf or terra::Spat* object}

\item{crs_standard}{character(1). A standard definition of
coordinate reference system. Default is \code{"EPSG:4326"}
Consult \href{https://epsg.io}{epsg.io} for details of other CRS.}
}
\value{
A (reprojected) \code{sf} or \code{SpatVector} object.
}
\description{
The input is checked whether its coordinate system is
present. If not, it is reprojected to the CRS specified in
\code{crs_standard}.
}
\note{
This function works well with EPSG codes.
}
\seealso{
Other Helper functions: 
\code{\link{datamod}()},
\code{\link{dep_check}()},
\code{\link{dep_switch}()},
\code{\link{get_clip_ext}()},
\code{\link{par_def_q}()},
\code{\link{reproject_to_raster}()}
}
\author{
Insang Song
}
\concept{Helper functions}
\keyword{internal}
