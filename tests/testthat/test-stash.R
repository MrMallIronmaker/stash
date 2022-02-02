

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

# refining a function in global space that a function depends on means it's recomputed
test_that("redefining a function in a shared space implies recomputation", {
  withr::with_options(
    list(stash.cache_path = tmp_cache_dir()),
    {
      sleep_time <- 2
      error_time <- 0.2

      other_function <- function(x) {
        mean(x)
      }

      test_function <- function(x) {
        Sys.sleep(sleep_time)
        other_function(x)
      }

      # slow at first
      result_time_1 <- system.time(result_value_1 <- do.call.stash(test_function, list(c(1:20))))
      expect_gte(result_time_1[["elapsed"]], sleep_time - error_time)
      expect_equal(result_value_1, mean(1:20))

      # fast afterwards
      result_time_2 <- system.time(result_value_2 <- do.call.stash(test_function, list(c(1:20))))
      expect_lte(result_time_2[["elapsed"]], error_time)
      expect_equal(result_value_2, mean(1:20))

      # slow when function is redefined
      other_function <- function(x) {
        sd(x)
      }
      result_time_3 <- system.time(result_value_3 <- do.call.stash(test_function, list(c(1:20))))
      expect_gte(result_time_3[["elapsed"]], sleep_time - error_time)
      expect_equal(result_value_3, sd(1:20))

      # fast afterwards
      result_time_4 <- system.time(result_value_4 <- do.call.stash(test_function, list(c(1:20))))
      expect_lte(result_time_4[["elapsed"]], error_time)
      expect_equal(result_value_4, sd(1:20))

      # still fast when you switch back
      other_function <- function(x) {
        mean(x)
      }
      result_time_5 <- system.time(result_value_5 <- do.call.stash(test_function, list(c(1:20))))
      expect_lte(result_time_5[["elapsed"]], error_time)
      expect_equal(result_value_5, mean(1:20))
    }
  )
})

test_that("nested do.call.stash stashes effectively", {
  withr::with_options(
    list(stash.cache_path = tmp_cache_dir()),
    {
      sleep_time <- 2
      error_time <- 0.2

      other_function <- function(x) {
        Sys.sleep(sleep_time)
        mean(x)
      }

      test_function <- function(x) {
        Sys.sleep(sleep_time)
        do.call.stash(other_function, list(x))
      }

      # slow at first
      result_time_1 <- system.time(result_value_1 <- do.call.stash(test_function, list(c(1:20))))
      expect_gte(result_time_1[["elapsed"]], 2 * sleep_time - error_time)
      expect_equal(result_value_1, mean(1:20))

      # fast afterwards
      result_time_2 <- system.time(result_value_2 <- do.call.stash(test_function, list(c(1:20))))
      expect_lte(result_time_2[["elapsed"]], error_time)
      expect_equal(result_value_2, mean(1:20))

      # slow when function is redefined
      other_function <- function(x) {
        Sys.sleep(sleep_time)
        sd(x)
      }

      result_time_3 <- system.time(result_value_3 <- do.call.stash(test_function, list(c(1:20))))
      expect_gte(result_time_3[["elapsed"]], 2 * sleep_time - error_time)
      expect_equal(result_value_3, sd(1:20))

      # fast afterwards
      result_time_4 <- system.time(result_value_4 <- do.call.stash(test_function, list(c(1:20))))
      expect_lte(result_time_4[["elapsed"]], error_time)
      expect_equal(result_value_4, sd(1:20))

      # still fast when you switch back
      other_function <- function(x) {
        Sys.sleep(sleep_time)
        mean(x)
      }
      result_time_5 <- system.time(result_value_5 <- do.call.stash(test_function, list(c(1:20))))
      expect_lte(result_time_5[["elapsed"]], error_time)
      expect_equal(result_value_5, mean(1:20))
    }
  )
})


# TODO: functions that depend on global options are re-run when the option is checked
# TODO: editing a different function in the same file then re-sourcing the file doesn't force a rerun
