#' Auto-parallelize
#' @param fun function.
#' @param x X argument of `fun`
#' @param y Y argument of `fun`
#' @param strategy character(1). Parallel processing strategy
#'   supported in `chopin`
#' @param ... Other arguments to be passed to `fun`
#' @param .backend character(1). One of "future" or "mirai"
#' @return data.frame
par_compose <-
  function(
    fun,
    x, y,
    strategy,
    ...,
    .backend = c("future", "mirai")
  ) {
    # specify the function and arguments to wrap
    # pass a list and the name of strategy
    # run the parallelization
  }
