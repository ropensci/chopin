
#' Set the backend for the chopin package
#'
#' This function sets the backend for the chopin package to be used for computation.
#'
#' @param backend The backend to be set. Default is "mirai".
#' @return None
#' @export
set_backend <- function(backend = "mirai") {
  backend_candidates <- c("mirai", "future")
  backend <- match.arg(backend, backend_candidates)
  options(chopin.backend = backend)
}

#' backend_worker function
#'
#' @description
#' This function is used to execute a specific backend based on the value of the "chopin.backend" option.
#' If the backend is set to "mirai", the function calls the mirai::mirai() function.
#' If the backend is set to "future", the function calls the future::future() function with seed and lazy options enabled.
#' If the backend is set to any other value, the function throws an error indicating an unknown backend.
#'
#' @keywords internal
#' @param ... Additional arguments to be passed to the backend function.
#' @importFrom mirai mirai
#' @importFrom future future
#' @importFrom cli cli_abort
#' @returns The result of the backend function call.
.backend_worker <-
  function(...) {
    backend <- getOption("chopin.backend")
    if (backend == "mirai") {
      mirai::mirai(...)
    } else if (backend == "future") {
      future::future(..., seed = TRUE, lazy = TRUE)
    } else {
      cli::cli_abort("Unknown backend")
    }
  }


#' Internal function for collecting parallel worker results
#' @keywords internal
#' @param worker Unresolved parallel worker
#' @importFrom mirai call_mirai
#' @importFrom future value
#' @importFrom cli cli_abort
.backend_collector <-
  function(worker) {
    if (inherits(worker, "mirai")) {
      mirai::call_mirai(worker)[["data"]]
    } else if (inherits(worker, "future")) {
      future::value(worker)
    } else {
      cli::cli_abort("Unknown worker")
    }
  }