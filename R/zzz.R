.onLoad <- function(libname, pkgname) {
  op <- options()
  op.stash <- list(
    # TODO: make this not system-dependent?
    stash.cache_path = "~/.stash-r/cache/"
  )
  toset <- !(names(op.stash) %in% names(op))
  if(any(toset)) options(op.stash[toset])

  invisible()
}
