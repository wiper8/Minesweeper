test_that("", {
  grid <- matrix(c(-7, 1, 0, -6, 2, 1, -6, -5, 1), nrow=3)
  status <- matrix(1, 3, 3)
  expect_equal(
    certain_core(grid, status)[[3]],
    "impossible"
  )
})
