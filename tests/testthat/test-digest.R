test_that("digest doesn't choke on rcrds", {
  x <- vctrs::new_rcrd(list(x = 1:3, y = 3:1, z = letters[1:3]))
  expect_error(stable_digest(x), NA)
})
