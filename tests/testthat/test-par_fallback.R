
testthat::test_that(
  "par_fallback works",
  {
    withr::local_package("terra")
    dc <- terra::vect("POINT (12 8)", crs = "EPSG:4326")
    dc$site_id <- "SITE1"
    rdd <- terra::rast(nrow = 10, ncol = 10, crs = "EPSG:4326")
    terra::ext(rdd) <-
      c(xmin = 10, xmax = 20, ymin = 0, ymax = 10)
    terra::values(rdd) <- runif(100L)

    foo1 <- extract_at
    foo2 <- mean
    testthat::expect_no_error(
      par_fallback(
        err = foo1(dc, rdd, id = "site_id", mode = "buffer", radius = 5e4),
        fun = foo1,
        debug = TRUE,
        1
      )
    )
    testthat::expect_error(
      par_fallback(
        err = foo1(id = "site"),
        fun = foo2,
        debug = FALSE,
        1
      )
    )
  }
)
