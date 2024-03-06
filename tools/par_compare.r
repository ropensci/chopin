# Load the required packages
library(doParallel)
library(foreach)
library(mirai)

setwd("/Volumes/songi2/projects/chopin/")

# Set the number of cores to use
# num_cores <- 16L
# # Register the parallel backend
# doParallel::registerDoParallel(cores = num_cores)

# Define the function to be executed in parallel
popplace <- readRDS("./input/populated_place_2022.rds")
bils <- list.files("input", "bil$", recursive = TRUE, full.names = TRUE)
bilssds <- terra::rast(bils[-13])


cl <- mirai::make_cluster(4L)

tictoc::tic()
doch <-
mirai::mirai(
  .expr = {
    pkgs <-
      c("chopin", "terra", "stars", "sf", "future", "doFuture", "parallelly", "tigris", "exactextractr", "tictoc")
    invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE))
    options(tigris_use_cache = TRUE, sf_use_s2 = FALSE)
    bils <- list.files("input", "bil$", recursive = TRUE, full.names = TRUE)
    bilssds <- terra::rast(bils[-13])
    feature <- readRDS("./input/populated_place_2022.rds")
    ll <- vector("list", length = length(states))

    for (k in seq_along(states)) {
        ppthis <- feature[feature$STATEFP == states[k], ]
        ppbuf <- terra::buffer(terra::vect(ppthis), width = 1e4)
        ppbuf <- sf::st_as_sf(ppbuf)

        ll[[k]] <-
        exactextractr::exact_extract(
            bilssds,
            ppbuf,
            fun = "mean",
            stack_apply = TRUE,
            force_df = TRUE,
            append_cols = "GEOID",
            max_cells_in_memory = 2.14e9
        )

    }

    return(ll)

  },
    states = unique(popplace$STATEFP)
  
)

mirai::call_mirai(doch) -> dochcal
tictoc::toc()
mirai::status(cl)

# 16: 407.5 sec
# 4: 502.979 sec


##
library(furrr)
library(future)
library(doFuture)
doFuture::registerDoFuture()
plan(multisession, workers = 10L)


tictoc::tic()
fur_ll <-
furrr::future_map(
  .x = unique(popplace$STATEFP),
  .f = function(x) {
    pkgs <-
      c("chopin")
    invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE))
    options(tigris_use_cache = TRUE, sf_use_s2 = FALSE)
    bils <- list.files("input", "bil$", recursive = TRUE, full.names = TRUE)
    bilssds <- terra::rast(bils[-13])
    feature <- readRDS("./input/populated_place_2022.rds")

      ppthis <- feature[feature$STATEFP == x, ]
      ppbuf <- terra::buffer(terra::vect(ppthis), width = 1e4)
      ppbuf <- sf::st_as_sf(ppbuf)

        exactextractr::exact_extract(
          bilssds,
          ppbuf,
          fun = "mean",
          stack_apply = TRUE,
          force_df = TRUE,
          append_cols = "GEOID",
          max_cells_in_memory = 2.14e9
        )
    },
    .options = furrr::furrr_options(scheduling = 4L)
)
tictoc::toc()
# up to 900MB / thread
# 120 sec

# lapply
library(future.apply)
tictoc::tic()
fur_lla <-
future.apply::future_lapply(
  X = unique(popplace$STATEFP),
  FUN = function(x) {
    .libPaths("~/r-libs")
    pkgs <-
      c("chopin")
    invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE))
    options(tigris_use_cache = TRUE, sf_use_s2 = FALSE)
    bils <- list.files("input", "bil$", recursive = TRUE, full.names = TRUE)
    bilssds <- terra::rast(bils[-13])
    feature <- readRDS("./input/populated_place_2022.rds")

      ppthis <- feature[feature$STATEFP == x, ]
      ppbuf <- terra::buffer(terra::vect(ppthis), width = 1e4)
      ppbuf <- sf::st_as_sf(ppbuf)

        exactextractr::exact_extract(
          bilssds,
          ppbuf,
          fun = "mean",
          stack_apply = TRUE,
          force_df = TRUE,
          append_cols = "GEOID",
          max_cells_in_memory = 2.14e9
        )
    },
    future.seed = TRUE,
    future.scheduling = 4L
)
tictoc::toc()
# 114.906 sec
# cluster, 71.8 sec, scheduling = 4L @cluster (persistent)
# 700 MB / thread