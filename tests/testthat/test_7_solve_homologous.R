source("src/clicker/certain_core.R")
source("src/clicker/helper/clustering.R")
source("src/clicker/probabilistic_clicker.R")
source("src/game_engine/init_solved_around.R")
source("src/clicker/helper/solve_homologous.R")
source("src/clicker/helper/compute_mine_probability.R")

test_that("solve_homologous retourne des probabilités réalistes", {
  grid <- matrix(
    c(
      rep(-11, 9),
      -11, -5, -5, 2, -11, -11, -11, -11, -11,
      -11, -9, 4, 4, -11, -11, -11, -11, -11,
      -11, -8, 3, -5, -11, -5, -5, 4, -11,
      -11, -8, -8, -9, -10, -10, 5, -5, -11,
      -11, -11, -11, -10, 4, -10, -10, -10, -11,
      -11, -11, -10, -10, -10, 1, 2, -10, -11,
      -11, -11, -10, 4, -10, 2, 3, -10, -11,
      -11, -9, -9, -9, -10, -10, -10, -10, -11,
      -11, -8, 3, -8, -11, -11, -11, -11, -11,
      -11, -8, -8, -8, -11, -11, -11, -11, -11,
      rep(-11, 9)
    ),
    ncol = 9,
    byrow = TRUE
  )
  expect_true(
    max(
      solve_homologous(
        grid,
        8,
        init_solved_around(grid),
        in_cluster = grid != -11,
        homologous = list(homologous_i = c(31, 32), max_mines = 4),
        hypothesis = 1
      )$probs,
      na.rm = TRUE
    ) <= 1
  )
})
