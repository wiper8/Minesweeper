source("src/clicker/probabilistic_clicker.R")
source("src/game_engine/init_solved_around.R")

test_that("compute_grid_probabilities finds good probabilities", {
  grid <- matrix(
    c(
      -1, -2, -1, -1, -2,
      -1, 2, 1, 3, -2,
      -2, 1, 0, 2, -2,
      -1, 2, 2, 3, -1,
      -1, -1, -2, -2, -1
    ),
    ncol = 5,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  expect_equal(
    compute_grid_probabilities(grid, mines_left = 7, solved_around),
    matrix(
      c(
        9, 1, 9, 1, 9,
        9, NA, NA, NA, 9,
        9, NA, NA, NA, 9,
        1, NA, NA, NA, 9,
        1, 9, 9, 9, 9
      ) / 10,
      ncol = 5,
      byrow = TRUE
    )
  )
})

# TODO
test_that("compute_grid_probabilities finds good probabilities avec void présent", {
  grid <- matrix(
    c(
      -1, -1, -2, -1, -1,
      -1, -2, -1, -1, -2,
      -1, 2, 1, 3, -2,
      -2, 1, 0, 2, -2,
      -1, 2, 2, 3, -1,
      -1, -1, -2, -2, -1
    ),
    ncol = 5,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  expect_equal(
    compute_grid_probabilities(grid, mines_left = 8, solved_around),
    matrix(
      c(
        ?, ?, ?, ?, ?,
        9, 1, 9, 1, 9,
        9, NA, NA, NA, 9,
        9, NA, NA, NA, 9,
        1, NA, NA, NA, 9,
        1, 9, 9, 9, 9
      ) / 10,
      ncol = 6,
      byrow = TRUE
    )
  )
})
