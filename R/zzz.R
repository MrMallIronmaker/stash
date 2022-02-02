# nocov start

#' @importFrom rlang fn_env fn_env<-
#' @importFrom digest digest
#' @importFrom logger log_info log_debug log_error log_threshold WARN
NULL

.onLoad <- function(libname, pkgname) {
  # load options
  op <- options()
  op.stash <- list(
    # TODO: make this not system-dependent?
    stash.cache_path = "~/.stash-r/cache/"
  )
  toset <- !(names(op.stash) %in% names(op))
  if(any(toset)) options(op.stash[toset])

  # set logging level of stash to WARNING
  log_threshold(WARN, namespace = 'stash')

  invisible()
}

# nocov end
