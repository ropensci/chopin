# Packages
library(chopin)        # parallel grids/helpers  (https://docs.ropensci.org/chopin)
library(future)
library(future.mirai)  # lightweight parallel backend
library(sf)
library(dplyr)
library(osmdata)       # Overpass client
library(units)
library(ggplot2)
sf_use_s2(FALSE)

# Choose an area
place_name <- "Jongno-gu, Seoul, South Korea"
bb <- osmdata::getbb(place_name, format_out = "polygon")  # polygon bbox
area_poly <- st_polygon(list(bb)) |> st_sfc(crs = 4326) |> st_as_sf()

# Project to a local metric CRS for buffering/areas
# Korea: UTM 52N/51N depending on long; for Seoul long~127E -> 52N is fine
area_poly_m <- st_transform(area_poly, 32652)

# Generate many evaluation points (grid within the polygon)
# spacing ~200 m; tune as needed
grid <- st_make_grid(area_poly_m, cellsize = 200, what = "centers") |>
  st_as_sf() |>
  st_intersection(area_poly_m) |>
  mutate(pid = row_number())

nrow(grid)
# This can be several thousands for a district-sized area.


# Build Overpass query: buildings intersecting area polygon
# opq_poly <- opq(bbox = bb) |>
#   add_osm_feature(key = "building")

# # Fetch as sf polygons
# osm <- osmdata_sf(opq_poly)

# bldg <- osm$osm_polygons
# # Keep polygons with non-missing geometry
# bldg <- bldg |> st_make_valid() |> st_transform(st_crs(area_poly_m)) |>
#   st_intersection(area_poly_m)

# # Extract floors from 'building:levels' when available
# # osmdata puts tags in columns where possible; often "building.levels" or "building:levels"
# lvl_col <- intersect(names(bldg), c("building.levels","building:levels"))
# if (length(lvl_col) == 0L) {
#   warning("No explicit 'building:levels' field found. Floors will be NA; ",
#           "consider using a different area or the Overture alternative.")
#   bldg$floors <- NA_real_
# } else {
#   bldg$floors <- suppressWarnings(as.numeric(bldg[[lvl_col[1]]]))
# }

# # Compute footprint area (m^2) in projected CRS
# bldg$foot_m2 <- as.numeric(st_area(bldg))
# # Keep a lean set of columns
# bldg <- bldg |> select(floors, foot_m2)

bldg <- st_read("tools/test_osm_jongno.gpkg", layer = "buildings")
grdpnt <- st_read("tools/test_osm_jongno.gpkg", layer = "points")

# Radius for AER (meters)
radius_m <- 150  # adjust as needed

# This function will be dispatched in parallel over computational grids.
# It must accept `x` and `y` (as per chopin's contract).
aer_at_points <- function(x, y, radius, id_col = "pid", floors_col = "floors", area_col = "foot_m2") {
  # x = buildings (sf polygons with floors + footprint area in same CRS as y)
  # y = points (sf points with 'pid')
  if (!inherits(x, "sf") && !inherits(y, "sf")) {
    x <- sf::st_as_sf(x)
    y <- sf::st_as_sf(y)
  }
  # stopifnot(inherits(x, "sf"), inherits(y, "sf"))
  y <- sf::st_geometry(y)

  # Buffers around each point
  buf <- sf::st_buffer(y, radius)
  # spatial join (buildings intersecting buffers)
  j <- sf::st_intersects(buf, sf::st_geometry(x))
  # Compute
  out <- vapply(seq_along(j), function(i) {
    if (length(j[[i]]) == 0) return(NA_real_)
    sub <- x[ j[[i]], ]
    sub <- sub[ !is.na(sub[[floors_col]]) & !is.na(sub[[area_col]]), ]
    if (nrow(sub) == 0) return(NA_real_)
    aer_num <- sum(sub[[area_col]] * sub[[floors_col]] , na.rm = TRUE)
    aer_den <- pi * radius^2
    aer_num / aer_den
  }, numeric(1))

  data.frame(pid = attr(y, id_col, exact = TRUE) %||% seq_along(j),
             aer = out)
}


setGeneric(
  "par_dispatcher",
  function(x, y, ...) {
    standardGeneric("par_dispatcher")
  })
setMethod(
  "par_dispatcher",
  signature = signature(x = "sf", y = "sf"),
  function(x, y, radius, floors_col = "floors", area_col = "foot_m2") {
    # x = buildings (sf polygons with floors + footprint area in same CRS as y)
    # y = points (sf points with 'pid')
    stopifnot(inherits(x, "sf"), inherits(y, "sf"))
    y <- st_geometry(y)

    # Buffers around each point
    buf <- st_buffer(y, radius)
    # spatial join (buildings intersecting buffers)
    j <- st_intersects(buf, st_geometry(x))
    # Compute
    out <- vapply(seq_along(j), function(i) {
      if (length(j[[i]]) == 0) return(NA_real_)
      sub <- x[ j[[i]], ]
      sub <- sub[ !is.na(sub[[floors_col]]) & !is.na(sub[[area_col]]), ]
      if (nrow(sub) == 0) return(NA_real_)
      aer_num <- sum(sub[[area_col]] * sub[[floors_col]] , na.rm = TRUE)
      aer_den <- pi * radius^2
      aer_num / aer_den
    }, numeric(1))

    data.frame(pid = attr(y, "pid", exact = TRUE) %||% seq_along(j),
              aer = out)
  }
)

#' Generic dispatcher for parallel methods
#' @param fn Function name to dispatch
#' @param fun Function definition
#' @param sig_x Signature for `x` argument (default: "ANY")
#' @param sig_y Signature for `y` argument (default: "ANY")
#' @return NULL (sets the method)
#' @export
par_dispatcher <- function(fn, fun, sig_x = "ANY", sig_y = "ANY") {
  if (!isGeneric(fn)) {
    setGeneric(fn, function(x, y, ...) {
      standardGeneric(fn)
    })
  }
  setMethod(
    f = fn,
    signature = signature(x = sig_x, y = sig_y),
    definition = fun
  )
}

par_dispatcher(
  "foo",
  function(x, y, ...) {TRUE}
)

par_dispatcher <- function(fn, fun, sig_x = "ANY", sig_y = "ANY") {
  if (!isGeneric(fn)) {
    print(as.character(fn))
    setGeneric(as.character(fn), function(x, y, ...) {
      standardGeneric(as.character(fn))
    })
  }
  setMethod(
    f = as.character(fn),
    signature = signature(x = sig_x, y = sig_y),
    definition = fun
  )
}


par_dispatcher(
  fn = "getdisp",
  fun = function(x, y, radius, floors_col = "floors", area_col = "foot_m2") {
    aer_at_points(x, y, radius, floors_col, area_col)
  },
  "ANY", "ANY"
)
getdisp


# Alternative approach using bquote (cleaner syntax)
par_dispatcher_bquote <- function(fn, fun, sig_x = "ANY", sig_y = "ANY") {
  # Get the function name
  fn_name <- if (is.character(fn)) fn else as.character(substitute(fn))
  
  # Check if generic already exists
  if (!isGeneric(fn_name)) {
    # Using bquote to create the generic function with literal name
    generic_def <- eval(bquote(
      function(x, y, ...) standardGeneric(.(fn_name))
    ))
    
    setGeneric(
      name = fn_name,
      def = generic_def,
      where = .GlobalEnv
    )
    message(paste("Created S4 generic:", fn_name))
  }
  
  # Set the method
  setMethod(
    f = fn_name,
    signature = c(x = sig_x, y = sig_y),
    definition = fun
  )
  
  # invisible(getGeneric(fn_name))
}

par_dispatcher_bquote(
  fn = "getdisp",
  fun = function(x, y){1}
)
par_dispatcher_bquote(
  fn = "getdisp",
  fun = function(x, y){1}
)


# Set plan: mirai_multisession is cross-platform friendly
plan(mirai_multisession, workers = 4L)

# Run: x = buildings; y = points; push radius in dots (...).
grd <- chopin::par_pad_grid(
  bldg,
  mode = "grid",
  nx = 4L,
  ny = 1L,
  padding = 300
)
grdpnt <- grid
bldg_cast <- st_cast(bldg, "POLYGON")
radius_m <- 100

res <- par_grid_mirai(
  grids    = grd,
  fun_dist = aer_at_points,
  x        = bldg_cast,
  y        = grdpnt,
  radius   = radius_m,
  # input_id = "pid",
  .debug = TRUE
)

# Back to sequential
plan(sequential); mirai::daemons(0)


aer_at_points(grdpnt, bldg_cast, radius = radius_m)
