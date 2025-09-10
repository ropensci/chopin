library(reprex)

reprex::reprex({
library(sf)
library(h3r)

sf_use_s2(FALSE)

ncpath <- system.file("shape/nc.shp", package = "sf")
ncpoly <- sf::st_read(ncpath)
ncpoly <- sf::st_transform(ncpoly, 4326)

x <- sf::st_coordinates(ncpoly)
if (ncol(x) == 4) {
  x <- cbind(x, 1L)
}
x <- split(
  as.data.frame(x[, c(2, 1)]),
  x[, 5]
)
x <- lapply(x, as.matrix)
# the input for polygonToCells should be a
# **list** of list of matrices
x <- lapply(x, list)
searched <-
  h3r::polygonToCells(
    polygons = x,
    resolution = 4L
  )
#searched
searched_flat <- unlist(searched)
# searched_ext <-
#   h3r::gridDisk(
#     cell = searched_flat,
#     k = rep(2L, length(searched_flat))
#   )
# searched_ext <- unique(c(unlist(searched_ext), searched_flat))

ncpoly$n_h3_lv4 <- sapply(searched, length)
plot(ncpoly["n_h3_lv4"])

# ncvertex <- sfheaders::sfc_cast(ncpoly$geometry, "POLYGON")
# ncvertex2 <- sf::st_cast(ncpoly$geometry, "POLYGON")
# nc_h3idx <-
#   h3r::polygonToCells(
#     polygons = ncvertex,
#     resolution = 5L
#   )

h3list <-
  h3r::cellToBoundary(
    cell = searched_flat
  )
h3list <- Map(
  function(x) {
    sf::st_polygon(
      list(as.matrix(x[c(seq_len(nrow(x)), 1L), 2:1]))
    )
  },
  h3list
)
h3sf <- sf::st_as_sfc(
  h3list, crs = sf::st_crs(4326)
)
h3sf <- sf::st_as_sf(h3sf)
h3sf[["CGRIDID"]] <- seq_len(nrow(h3sf))

plot(ncpoly$geometry, border = "red")
plot(h3sf$x, add = TRUE, border = "blue", lwd = 2)
})




##
library(reprex)

reprex::reprex({
library(sf)
library(h3r)
library(sfheaders)
sf_use_s2(FALSE)

ncpath <- system.file("shape/nc.shp", package = "sf")
ncpoly <- sf::st_read(ncpath)
ncpoly <- sf::st_transform(ncpoly, 4326)

# case 1: list of matrices
x <- sf::st_coordinates(ncpoly)
if (ncol(x) == 4) {
  x <- cbind(x, 1L)
}
x <- split(
  as.data.frame(x[, c(2, 1)]),
  x[, 5]
)
x <- lapply(x, as.matrix)
# the input for polygonToCells should be a
# **list** of list of matrices
x <- lapply(x, list)
searched <-
  h3r::polygonToCells(
    polygons = x,
    resolution = 4L
  )
#searched
searched_flat <- sort(unlist(searched))

# case 2: sfheaders::sf_cast
y <- sfheaders::sf_cast(ncpoly, "POLYGON")
searched2 <-
  h3r::polygonToCells(
    polygons = y$geometry,
    resolution = 4L
  )
searched2_flat <- sort(unlist(searched2))

all.equal(searched_flat, searched2_flat)

searched_flat

searched2_flat

})
