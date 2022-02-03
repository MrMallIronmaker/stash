test_that("parallel parameter", {
  withr::with_options(
    list(stash.cache_path = tmp_cache_dir()),
    {
      # so you have some function where one argument is essentially a "parallel" argument,
      # indicating many values to run at once.
      # how does one specify:
      # A) which arguments are parallel
      # B) order or name of arguments

      parallel_function <- function(x, p) {
        data.frame(
          p = p,
          result = sapply(p, function(i) {Sys.sleep(1); sum(x^i)})
        )
      }

      do.call.stash(parallel_function, list(x = 1:20, p = c(1, 2, 4, 5)), parallel = "p")
      # TODO: test case when parallel = 3, paralell = list("p", 3)

      # returns a data-frame like object, with one column/field named as "p" (first)
      time_result <- system.time(result <- do.call.stash(parallel_function, list(x = 1:20, p = 1:5), parallel = "p"))
      expect_equal(result$result, c(210, 2870, 44100, 722666, 12333300))
      expect_lte(time_result[["elapsed"]], 2)
    }
  )
})
