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
        1, 1, 7, 1, 4,
        3, 2, 1, 3, 7,
        5, 1, 0, 2, 8,
        1, 2, 2, 3, 3,
        1, 4, 7, 7, 2
      ) / 9,
      ncol = 5,
      byrow = TRUE
    )
  )
})

test_that("compute_grid_probabilities finds good probabilities avec cluster complexes indépendants", {
  # TODO faire une grille avec 2 clusters. Le nombre de mines dans l'autre cluster doit grandement influencer
  # les probabilitées dans le cluster actuel
  solved_around <- init_solved_around(grid)
  expect_equal(
    compute_grid_probabilities(grid, mines_left = 7, solved_around),
    TODO
  )
})
