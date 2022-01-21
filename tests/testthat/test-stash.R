

tmp_cache_dir <- function() {
  # move this to a file in R/testthat-helpers.R if this is ever needed in multiple places.
  # i'm basically trying to get a uuid
  cache_dir <- file.path(tempdir(), digest::digest(runif(10)))
  dir.create(cache_dir)
  cache_dir
}

test_that("stash caches the simplest results", {
  withr::with_options(
    list(stash.cache_path = tmp_cache_dir()),
    {

      sleep_time <- 2
      error_time <- 0.2

      simple_function <- function(x) {
        Sys.sleep(sleep_time)
        mean(x)
      }

      expect_gte(system.time(do.call.stash(simple_function, list(c(1:20))))[["elapsed"]], sleep_time - error_time)
      expect_lte(system.time(do.call.stash(simple_function, list(c(1:20))))[["elapsed"]], error_time)
      expect_lte(system.time(do.call.stash(simple_function, list(c(1:20))))[["elapsed"]], error_time)
      expect_lte(system.time(do.call.stash(simple_function, list(c(1:20))))[["elapsed"]], error_time)
    }
  )
})

test_that("digest doesn't choke on rcrds", {
  x <- vctrs::new_rcrd(list(x = 1:3, y = 3:1, z = letters[1:3]))
  expect_error(stable_digest(x), NA)
})



# editing a different function in the same file then re-sourcing the file doesn't force a rerun

# refining a function in global space that a function depends on means it's recomputed

function(fake) {
  other_function <- function(x) {
    mean(x)
  }

  test_function <- function(x) {
    Sys.sleep(4)
    other_function(x)
  }
}

# functions that depend on global options are re-run when the option is checked



