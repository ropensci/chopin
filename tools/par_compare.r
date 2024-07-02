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




doFuture::registerDoFuture()

plan(
  list(
    tweak(
      batchtools_slurm,
      template = "./tools/slurm_test/template_slurm.tmpl",
      workers = 4L,
      resources =
      list(
        memory = 6, # gb per cpu
        ncpus = 12,
        ntasks = 1,
        walltime = 600,
        email = "songi2@nih.gov",
        log.file =
        sprintf(
          "~/rtest/test_%s.log",
          strftime(Sys.time(), "%Y%m%d_%H%M%S")),
        error.file =
        sprintf(
          "~/rtest/test_%s.error",
          strftime(Sys.time(), "%Y%m%d_%H%M%S")),
        partition = "geo"
        )
      )
    ,
    multicore
  )
)

## generate 1km grid points in the southeastern US States
stt <- tigris::states(year = 2020)
targ_states <- c("NC", "SC", "GA", "FL", "AL", "MS", "TN", "LA", "AR")
stt_targ <- stt[stt$STUSPS %in% targ_states, ]
plot(stt_targ$geometry)
st_bbox(stt_targ)
stt_t <- sf::st_transform(stt_targ, "EPSG:5070")
stt_g <- sf::st_sample(stt_t, type = "regular", 1204934)
stt_g <- sf::st_as_sf(stt_g)
stt_g$pid <- seq(1, length(stt_g))
stt_gb <- sf::st_buffer(stt_g, units::set_units(20, "km"))


bils <- list.files("input", "bil$", recursive = TRUE, full.names = TRUE)


stt_gbg <-
  chopin::par_make_gridset(
    stt_g,
    mode = "grid",
    padding = 5e3,
    nx = 5L,
    ny = 5L
  )


rp <- rpois(10000, 20)
rp <- split(rp, rp)
kk %<-% future_map(.x = rp, .f = function(x) { .x }, .options = furrr_options(globals = FALSE))


system.time(
  
rr <- par_grid_furrr(
  stt_gbg,
  furrr_opt =
  furrr_options(
    scheduling = 1L,
    seed = TRUE,
    packages = c("chopin", "terra", "sf", "stars", "tictoc", "dplyr", "exactextractr"),
    globals = TRUE
  ),
  fun_dist = chopin::extract_at_buffer,
  poly = stt_g,
  surf = bils,
  id = "pid",
  func = "mean",
  radius = 5e3
)

)

pkgs <- c("chopin", "terra", "sf", "stars", "tictoc", "dplyr", "exactextractr")
invisible(sapply(pkgs, library, character.only = TRUE, quietly = TRUE))
options(tigris_use_cache = TRUE, sf_use_s2 = FALSE)

trcts <- tigris::tracts(year = 2021, state = NULL, cb = TRUE)
landscan <- terra::rast("~/Downloads/landscan-usa-2021-night-assets/landscan-usa-2021-night/landscan-usa-2021-conus-night.tif")

system.time(
  exactextractr::exact_extract(
    landscan,
    trcts,
    fun = "sum",
    stack_apply = TRUE,
    force_df = TRUE,
    append_cols = "GEOID",
    max_cells_in_memory = 2.14e9
  )
)

trcts0 <- trcts[!trcts$STUSPS %in% c("VI", "AS", "AK", "HI", "PR", "GU", "MP"), ]

tictoc::tic("4core")
  doFuture::registerDoFuture()
  plan(multicore, workers = 4L)
  grds <-
  chopin::par_make_gridset(
    trcts0,
    mode = "grid_quantile",
    quantiles = par_def_q(2L),
    padding = 1e3
  )
  grds_ext <-
  chopin::par_grid(
    grids = grds,
    fun_dist = chopin::extract_at_poly,
    poly = trcts0,
    surf = landscan,
    id = "GEOID",
    func = "sum"
  )
tictoc::toc()
