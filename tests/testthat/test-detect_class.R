# Generated from chopin_rmarkdown_litr.rmd: do not edit by hand

testthat::test_that("classes are detected.", {
  withr::local_package("terra")
  random_df <- data.frame(x = runif(10), y = runif(10))
  random_tv <- terra::vect(random_df, geom = c("x", "y"))
  test_args <- list(vector = random_tv,
                    func = mean,
                    pipi = pi,
                    zodiac = "Horse")
  set_detected <- any_class_args(test_args, "SpatVector")
  # test partial match
  set_detectedp <- any_class_args(test_args, "Spat")

  testthat::expect_true(is.logical(set_detected))
  testthat::expect_true(is.logical(set_detectedp))
  # both are the same
  testthat::expect_true(all.equal(set_detected, set_detectedp))

  vect_pop <- test_args[set_detected][[1]]
  testthat::expect_s4_class(vect_pop, "SpatVector")

  # does it well in a function as designed?
  downy <- function(...) {
    largs <- list(...)
    any_class_args(largs, "SpatVector")
  }
  bear <- downy(v = random_tv, f = mean, pipi = pi)

  testthat::expect_true(is.logical(bear))
  testthat::expect_true(bear[[1]] == TRUE)

})

