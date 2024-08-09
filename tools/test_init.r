library(terra)
library(sf)
sf_use_s2(F)
library(devtools)
library(future.mirai)
plan(mirai_multisession, workers = 2L)


## reflect changes
install(quick=T)
