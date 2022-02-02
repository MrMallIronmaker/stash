wrap_closure <- function(auditor, term, ..stash.closure) {
  # Not sure why these needs to be here, but sure...
  term
  ..stash.closure
  function(...) {
    auditor[[term]] <- stable_digest(..stash.closure)
    ..stash.closure(...)
  }
}

#' @importFrom rlang env empty_env
wrap_environment <- function(envir) {
  accessed <- env(empty_env())
  wrapping_env <- env(envir)
  wrapping_env$..stash_accessed <- accessed
  for(term in ls(envir)) {
    env_object <- envir[[term]]
    # if it's a value, digest the value
    # if it's a function, wrap and digest the function...
    if (typeof(env_object) == "closure") {
      wrapping_env[[term]] <- wrap_closure(accessed, term, env_object)
    }
  }
  wrapping_env
}
