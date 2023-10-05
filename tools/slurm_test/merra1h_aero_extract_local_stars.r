library(sf)
library(stars)
library(dplyr)
sf_use_s2(FALSE)
us_extent <- 
    sf::st_bbox(c(xmin = -129.5, xmax = -61.1, ymin = 19.5, ymax = 51.8))
# terra::ext(us_extent)


extract_with_buffer.flat <- function(
        points, surf, radius, id, qsegs, crs_projection = 5070, func = mean, kernel = NULL, bandwidth = NULL
    ) {
    # generate buffers
    bufs <- points |>
        sf::st_transform(crs_projection) |>
        sf::st_buffer(dist = radius, nQuadSegs = qsegs) |>
        sf::st_transform(st_crs(points))
    # crop raster
    bufs_extent <- sf::st_bbox(bufs, crs = st_crs(surf))
    # surf_cropped = sf::st_crop(surf, bufs_extent)
    surf_cropped <- surf
    name_surf_val <- names(surf)
    # extract raster values
    surf_at_bufs <- stars::st_extract(surf_cropped, bufs)
    return(surf_at_bufs)
    surf_at_bufs_summary <- 
        surf_at_bufs |> 
            st_drop_geometry() |>
            group_by(ID) |> 
            summarize(across(all_of(name_surf_val), ~mean(.x, na.rm = TRUE))) |> 
            ungroup()
    return(surf_at_bufs_summary)
}

pathappend <- "/Users/songi2/Documents/GitHub/Scalable_GIS/largedata/"
merraname <- "MERRA2_400.tavg1_2d_aer_Nx.20220820.nc4"
pointname <- "aqs-test-data.gpkg"
# pathappend = "/ddn/gs1/home/songi2/covartest/"
merra <- stars::read_ncdf(paste(pathappend, merraname, sep= ""), proxy = TRUE)
point <- sf::read_sf(paste(pathappend, pointname, sep = "")) |>
    st_transform(4326)

## I was confused with the notion of time info in SpatRaster.
# as.POSIXlt("2022-08-22 00:00:00", tz = "UTC") + 3600
# terra::time(merra) <- as.POSIXlt("2022-08-22 00:00:00", tz = "UTC") + terra::time(merra) * 60
# merra_daily = terra::tapp(merra, index = seq(1, terra::nlyr(merra)), fun = mean)

# merra_daily = terra::tapp(merra, as.Date(terra::time(merra)), fun = mean)
# merra_daily = terra::tapp(merra, "days", fun = mean)
# merra_daily = lapply(merra, \(x) tapp(x, "days", mean))
# merra_daily = do.call(c, merra_daily)
# merra_daily
merra


merra_daily <- stars::st_apply(merra, MARGIN = 1:2, FUN = mean)

# ok
# timeindex = rep(seq_along(varnames(merra)), each = 24)
# merra_daily = terra::tapp(merra, index = timeindex, fun = mean)
# merra_daily
# names(merra_daily) = varnames(merra)

# terra::writeRaster(merra_daily, "/Users/songi2/Documents/GitHub/Scalable_GIS/largedata/merra2-20220820-cropped.tif")

# merra_daily

targ_cols <- c("BCCMASS",
"BCSMASS",
"DMSCMASS",
"DMSSMASS",
"DUCMASS",
"DUSMASS",
"DUCMASS25",
"DUSMASS25",
"OCCMASS",
"OCSMASS",
"SO2CMASS",
"SO2SMASS",
"SO4CMASS",
"SO4SMASS",
"SSCMASS",
"SSSMASS",
"SSCMASS25",
"SSSMASS25")
st_crs(merra_daily_t) <- "EPSG:4326"

st_crs(merra_daily) <- st_crs(point)
merra_daily_t <- merra_daily[targ_cols][st_bbox(st_buffer(point, 3e4))]
merra_daily_t <- st_as_stars(merra_daily_t)

extracted <- extract_with_buffer.flat(point, merra_daily_t, "ID.Code", 
    radius = 20000, qsegs = 90L)

extracted
# st_crop(merra_daily_t, st_bbox(point))
write.csv(extracted, "res_merra_09182023.csv")

# sources(merra_daily) = sources(merra)
# varnames(merra_daily) = varnames(merra)
# names(merra)
# merra

# crop(merra_daily_t, ext(point |> buffer(2e4L))) |>
# names()
