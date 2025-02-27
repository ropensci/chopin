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
    par_func <- switcher(strategy = strategy, .backend = .backend)
    inject_args <-
      c(
        list(x = x, y = y, fun_dist = fun),
        list(...)
      )
    
    if (strategy == "grid") {
      par_pad_grid()
    }
    rlang::inject(
      par_func(!!!inject_args)
    )
  }



#' Switcher
switcher <-
  function(strategy, .backend) {
    if (.backend == "future") {
      switch(
        strategy,
        grid = par_grid,
        hierarchy = par_hierarchy,
        multirasters = par_multirasters
      )
    }
    if (.backend == "mirai") {
      switch(
        strategy,
        grid = par_grid_mirai,
        hierarchy = par_hierarchy_mirai,
        multirasters = par_multirasters_mirai
      )
    }
  }