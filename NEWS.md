# 0.6.3
- FIX: Missing argument passing in `par_make_gridset` with `mode = "grid_advanced"`
- Vignette update: v01

# 0.6.2
- FIX: align input-output classes in `par_merge_grid`
- IMPROVE: grid_advanced mode supports the maximum of merged unit grids
- Added the balanced number mode of splitting input points (`par_group_balanced`; thanks to comments of Dr. Michael Fessler)
- Added function of the balanced mode for `par_grid` (`par_group_grid`)
- All internal `sapply` is changed to `vapply`
- Added a diagram for guiding users' choice for `par_*` functions for parallelization considering raster/vector data situations
- litr R Markdown file is moved to the archive

# 0.5.0
- `par_grid` unifies the classes of grid inputs
- `par_cut_coords`: a supplementary function effectively operating with `par_def_q` for `mode = "grid_quantile"` in `par_make_gridset`. X- and Y-coordinates (of centroids for polygon inputs) are split into quantile groups to balance the number of features in each grid; does not account for geometric complexity (i.e., number of vertices)
- Padded grid generation in `par_make_gridset` is done only by `terra::buffer`
- `extract_at*` functions get `max_cells`, which is passed to `exactextractr::exact_extract` for speedup in expense of memory pressure
- Added a new vignette on the demonstration of climate/weather data extraction

# 0.4.0
- `surf` argument in `extract_at*()` accepts file paths
- Raster cropping is now optional

# 0.3.1
- Dropped `rast_short`
- Added examples

# 0.3.0
- README.md is replaced by README.Rmd
- Documentation is clarified about the second argument in `...` in `par_*` functions