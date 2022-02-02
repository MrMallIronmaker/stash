#' @importFrom vctrs fields field
stable_digest <- function(object) {
  switch(
    typeof(object),
    closure = {
      if ("..stash.closure" %in% names(fn_env(object))) {
        # if the environment of the closure has a ..stash.closure entry, then we know
        # it's a wrapped function. this means, in order to get the stable code,
        # we need to go one step deeper inthat function body...
        stable_digest(fn_env(object)$..stash.closure)
      } else {
        digest(c(names(formals(object)), as.character(body(object))), algo = "xxhash64")
      }
    },
    list = {
      if ("vctrs_rcrd" %in% class(object)) {
        stable_digest(lapply(fields(object), function(field_name) {field(object, field_name)}))
      } else {
        digest(lapply(object, stable_digest), algo = "xxhash64")
      }
    },
    double =,
    character =,
    logical = {digest(object, algo = "xxhash64")},
    { # default (typeof is something else)
      # TODO: add logger level for this value
      # cat("Implicitly digested type: ", typeof(object))
      digest(object, algo = "xxhash64")
    }
  )
}
