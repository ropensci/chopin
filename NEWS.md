# 0.9
- `mirai` based `par_*` functions for parallelization
- `inst/extdata` contents were moved to `tests/testdata` in 0.9.1

# 0.8
- Bumped version from 0.7.8 to 0.8.0: improving package coverage
- README.md: two mermaid plots are pre-generated as png files
- Internal `clip_*()` functions and `vect_validate()` are removed
- `par_map_args()` is renamed to `par_convert_f()`
- init.R: one-line initialization for `bbox` class detection (cf. https://github.com/r-spatial/sf/issues/2448)

# 0.7
- `.check_package` refactoring: no `library()` call is required
- Internal function .check_par_spatraster is added to main `par_*` functions
- Internal preprocessing "dot-functions" -- universally usable functions are plain S3, whereas others that are class-specific were implemented in S4
- Added
  - CITATION.cff
  - `par_*`: argument `pad_y` to enable applying padded extent to x or y (e.g., y in raster-vector overlay and x otherwise)
- Updated
  - README.Rmd: split the combined one into two mermaid figures -> MD rebuilt, removed date of last update
  - `par_*`: argument name change from debug to .debug
- Dropped
  - Custom backend for future and mirai; rolling back to `future.apply`
  - `any_class_args`, `is_within_ref`, `par_fallback`, `check_dist_incorrect`
  - `par_grid` argument: `grids_input_ids`
  - Data: SRTM (TIFF replaces RDS)
- Data preprocessing parts are separated into internal functions
- Name change
  - `check_subject` to `.check_vector`
  - `reproject_b2r` to `reproject_to_raster`
  - `vect_valid_repair` to `vect_validate`
- targets-friendly helper function: `par_split_list`
  - Vignette for `par_split_list`
- `par_*` family naming hierarchy: `par_make_*` to `par_pad_*`
  - Plain gridding (`par_make_grid`) and clustering (`par_make_balanced`) functions are internal
- Main `par_*` runners (par_grid, par_hierarchy, par_multirasters) are kept
- `extract_at` is redesigned as a S4 method
- All messaging is managed by `cli` package
- Added zzz.R for startup message
- `summarize_aw` gets generic argument names `x` and `y`
- `mirai` backend is introduced
- Internal functions are not exported
- Type check function `check_subject` is added. This function is internal.
- `vapply` argument `FUN.VALUE` fixed

# 0.6
- Fixed: `grid_target_id` in `par_grid` accepts numeric or character input to filter grids
- `par_fallback` is returning error_message field with actual error messages
- `collapse` package is added to Imports (i.e., `rowbind` function is used in place of `dplyr::bind_rows`)
- Tests were fixed following refactoring/updating `par_*` functions
- `chopin` processing functions now support file path input with extents in `par_grid`
- Fixed: Missing argument passing in `par_pad_grid` with `mode = "grid_advanced"`
- Vignette update: v01
- Fixed: align input-output classes in `par_merge_grid`
- Improved: grid_advanced mode supports the maximum of merged unit grids
- Added the balanced number mode of splitting input points (`par_pad_balanced`; thanks to comments of Dr. Michael Fessler)
- Added function of the balanced mode for `par_grid` (`par_group_grid`)
- All internal `sapply` is changed to `vapply`
- Added a diagram for guiding users' choice for `par_*` functions for parallelization considering raster/vector data situations
- litr R Markdown file is moved to the archive

# 0.5
- `par_grid` unifies the classes of grid inputs
- `par_cut_coords`: a supplementary function effectively operating with `par_def_q` for `mode = "grid_quantile"` in `par_pad_grid`. X- and Y-coordinates (of centroids for polygon inputs) are split into quantile groups to balance the number of features in each grid; does not account for geometric complexity (i.e., number of vertices)
- Padded grid generation in `par_pad_grid` is done only by `terra::buffer`
- `extract_at*` functions get `max_cells`, which is passed to `exactextractr::exact_extract` for speedup in expense of memory pressure
- Added a new vignette on the demonstration of climate/weather data extraction

# 0.4
- `surf` argument in `extract_at*()` accepts file paths
- Raster cropping is now optional

# 0.3
- Dropped `rast_short`
- Added examples
- README.md is replaced by README.Rmd
- Documentation is clarified about the second argument in `...` in `par_*` functions