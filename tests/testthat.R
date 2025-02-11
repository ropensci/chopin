# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

# https://github.com/r-lib/covr/issues/420
options(useFancyQuotes = FALSE)

library(testthat)
library(chopin)

# prepare test data
roadpath <- file.path(tempdir(check = TRUE), "ncroads_first.gpkg")
roadurl <- "https://raw.githubusercontent.com/ropensci/chopin/refs/heads/0.9.0-cran/tests/testdata/ncroads_first.gpkg"
download.file(roadurl, roadpath, mode = "wb", method = "wget")

rppath <- file.path(tempdir(check = TRUE), "nc_random_point.rds")
rpurl <- "https://raw.githubusercontent.com/ropensci/chopin/refs/heads/0.9.0-cran/tests/testdata/nc_random_point.rds"
download.file(rpurl, rppath, mode = "wb", method = "wget")

test_check("chopin")
