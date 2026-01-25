source(here("src/clicker/certain_core.R"))

test_that("can_flag_all_around ajoute les flags qui sont certains autour des cases", {
  grid <- matrix(
    c(
      -2, 1, -1,
      1, 1, -1,
     0, 0, -1
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    can_flag_all_around(grid, matrix(0, nrow(grid), ncol(grid))),
    list(c(1, 1), FALSE)
  )
  
  grid <- matrix(
    c(
      -5, 1, 0, 2, -2,
      1, 2, 1, 4, -2,
      -1, -1, -5, 3, -2
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    can_flag_all_around(grid, matrix(0, nrow(grid), ncol(grid))),
    list(c(1, 5), FALSE)
  )
})
