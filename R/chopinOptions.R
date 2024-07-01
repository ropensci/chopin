#' Internal function for setting chopin options
#' @keywords internal
#' @param chopin.backend The backend to be set. Default is "mirai".
chopinOptions <-
  function(
    chopin.backend = "mirai"
  ) {
    options(chopin.backend = chopin.backend)
  }
