
#' @importFrom stats runif
tmp_cache_dir <- function() {
  # move this to a file in R/testthat-helpers.R if this is ever needed in multiple places.
  # i'm basically trying to get a uuid
  cache_dir <- file.path(tempdir(), digest(runif(10)))
  dir.create(cache_dir)
  cache_dir
}
