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
  debugonce(compute_grid_probabilities)
  expect_equal(
    compute_grid_probabilities(grid, mines_left = 7, solved_around),
    matrix(
      c(
        2, 1, 7, 1, 4,
        3, NA, NA, NA, 7,
        5, NA, NA, NA, 8,
        1, NA, NA, NA, 3,
        1, 4, 7, 7, 2
      ) / 9,
      ncol = 5,
      byrow = TRUE
    )
  )
})

test_that("compute_grid_probabilities sait pondérer selon les combinaisons dans les cases totalement inconnues", {
  grid <- matrix(
    c(
      # TODO ajuster au besoin, mon but est que les probs soient pondérées par si une mine est dans le 1ere rangée ou non
      -1, -1, -1, -1, -1,
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
  debugonce(compute_grid_probabilities)
  expect_equal(
    compute_grid_probabilities(grid, mines_left = 7, solved_around),
    # TODO vérifier les probs de cette matrice
    matrix(
      (
        c(
          # TODO ajuster au besoin, mon but est que les probs soient pondérées par si une mine est dans le 1ere rangée ou non
          c(1, 1, 1, 1, 1) / 5 * 4 / 5,
          c(
            2, 1, 7, 1, 4,
            3, NA, NA, NA, 7,
            5, NA, NA, NA, 8,
            1, NA, NA, NA, 3,
            1, 4, 7, 7, 2
          ) / 9 * choose(5, 4)
        ) +
          c(
            0, 0, 0, 0, 0,
            0, 0, 1, 0, 0,
            0, NA, NA, NA, 1,
            1, NA, NA, NA, 1,
            0, NA, NA, NA, 0,
            0, 0, 1, 1, 0
          ) * choose(5, 5)
      ) / (choose(5, 4) + choose(5, 5)),
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
