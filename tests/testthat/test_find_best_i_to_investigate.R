source(here("src/clicker/find_best_i_to_investigate.R"))

test_that("find_best_i_to_investigate works", {
  grid <- matrix(
    c(
      0, 1, -5, 1, 0, 1, -10, -10, -10,
      0, 1, 1, 1, 1, 2, -10, -10, -10,
      0 ,0, 0, 0, 1, -5, 2, 3, -10,
      0, 0, 1, 2, 4, 3, 2, 2, -10,
      0, 0, 2, -5, -5, -5, 2, 1, -10,
      0, 0, 3, -5, -9, -8, -10, -10, -10,
      1, 1, 3, -5, 3, 2, -10, -10, -10,
      2, -5, 2, 1, 1, 1, -10, -10, -10,
      -5, 4, 3, 1, 1, 1, -10, -10, -10,
      -9, -5, -8, -9, 2, -8, -10, -10, -10,
      rep(-10, 7 * 9)
    ),
    nrow = 17,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  expect_true(
    find_best_i_to_investigate(grid, solved_around, click_order = NULL)[1] %in% c(123, 124)
  )
  expect_true(
    find_best_i_to_investigate(grid, solved_around, click_order = matrix(c(7, 6, 5, 6), nrow = 2))[1] %in% c(123, 124)
  )
})
