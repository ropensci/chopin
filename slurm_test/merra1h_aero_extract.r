library(terra)
library(future.apply)
library(scomps)
library(dplyr)
us_extent = 
    terra::ext(c(xmin = -129.5, xmax = -61.1, ymin = 19.5, ymax = 51.8))
# terra::ext(us_extent)


extract_with_buffer.flat <- function(
        points, surf, radius, id, qsegs, func = mean, kernel = NULL, bandwidth = NULL
    ) {
    # generate buffers
    bufs = terra::buffer(points, width = radius, quadsegs = qsegs)
    # crop raster
    bufs_extent = terra::ext(bufs)
    surf_cropped = terra::crop(surf, bufs_extent)
    name_surf_val = names(surf)
    # extract raster values
    surf_at_bufs = terra::extract(surf_cropped, bufs)
    surf_at_bufs_summary = 
        surf_at_bufs |> 
            group_by(ID) |> 
            summarize(across(all_of(name_surf_val), ~mean(.x, na.rm = TRUE))) |> 
            ungroup()
    return(surf_at_bufs_summary)
}

pathappend = "/ddn/gs1/home/songi2/projects/Scalable_GIS/largedata/"
merraname = "MERRA2_400.tavg1_2d_aer_Nx.20220820.nc4"
pointname = "aqs-test-data.gpkg"
# pathappend = "/ddn/gs1/home/songi2/covartest/"
merra = terra::rast(paste(pathappend, merraname, sep= ""), win = us_extent)
point = terra::vect(paste(pathappend, pointname, sep = ""))

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

# ok
timeindex = rep(seq_along(varnames(merra)), each = 24)
merra_daily = terra::tapp(merra, index = timeindex, fun = mean)
merra_daily
names(merra_daily) = varnames(merra)

merra_daily

targ_cols = c("BCCMASS",
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

merra_daily_t = merra_daily[[targ_cols]]

extracted = extract_with_buffer.flat(point, merra_daily_t, "ID.Code", 
    radius = 2e4L, qsegs = 90L)
write.csv(extracted, "res_merra_09152023.csv")

# sources(merra_daily) = sources(merra)
# varnames(merra_daily) = varnames(merra)
# names(merra)
# merra

# crop(merra_daily_t, ext(point |> buffer(2e4L))) |>
# names()
