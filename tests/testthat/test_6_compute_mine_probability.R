source("src/game_engine/init_solved_around.R")
source("src/clicker/compute_mine_probability.R")

test_that("compute_mine_probability calcule les bonnes probabilitées", {
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
    compute_mine_probability(
      grid,
      mine_i = 6,
      all_combins = list(
        matrix(
          c(
            -8, -9, -8, -8, -9,
            -8, 2, 1, 3, -9,
            -9, 1, 0, 2, -9,
            -8, 2, 2, 3, -8,
            -8, -8, -9, -9, -8
          ),
          ncol = 5,
          byrow = TRUE
        ),
        matrix(
          c(
            -8, -8, -9, -8, -9,
            -8, 2, 1, 3, -9,
            -9, 1, 0, 2, -8,
            -8, 2, 2, 3, -9,
            -8, -8, -9, -9, -8
          ),
          ncol = 5,
          byrow = TRUE
        ),
        matrix(
          c(
            -8, -8, -9, -8, -9,
            -8, 2, 1, 3, -8,
            -9, 1, 0, 2, -9,
            -8, 2, 2, 3, -9,
            -8, -9, -8, -9, -8
          ),
          ncol = 5,
          byrow = TRUE
        ),
        matrix(
          c(
            -8, -8, -9, -8, -9,
            -9, 2, 1, 3, -8,
            -8, 1, 0, 2, -9,
            -8, 2, 2, 3, -9,
            -8, -9, -9, -8, -8
          ),
          ncol = 5,
          byrow = TRUE
        ),
        matrix(
          c(
            -8, -8, -9, -8, -8,
            -8, 2, 1, 3, -9,
            -9, 1, 0, 2, -9,
            -8, 2, 2, 3, -8,
            -8, -9, -8, -9, -9
          ),
          ncol = 5,
          byrow = TRUE
        ),
        matrix(
          c(
            -8, -8, -9, -8, -8,
            -9, 2, 1, 3, -9,
            -8, 1, 0, 2, -9,
            -8, 2, 2, 3, -8,
            -8, -9, -9, -8, -9
          ),
          ncol = 5,
          byrow = TRUE
        ),
        matrix(
          c(
            -8, -8, -9, -8, -8,
            -9, 2, 1, 3, -9,
            -8, 1, 0, 2, -9,
            -8, 2, 2, 3, -8,
            -9, -8, -9, -9, -8
          ),
          ncol = 5,
          byrow = TRUE
        ),
        matrix(
          c(
            -9, -8, -9, -8, -8,
            -8, 2, 1, 3, -9,
            -8, 1, 0, 2, -9,
            -9, 2, 2, 3, -8,
            -8, -8, -9, -9, -8
          ),
          ncol = 5,
          byrow = TRUE
        ),
        matrix(
          c(
            -9, -8, -8, -9, -8,
            -8, 2, 1, 3, -9,
            -9, 1, 0, 2, -9,
            -8, 2, 2, 3, -8,
            -8, -8, -9, -9, -8
          ),
          ncol = 5,
          byrow = TRUE
        )
      )),
    1 / 9
  )
})

test_that("compute_mine_probability calcule les bonnes probabilitées dans des cas complexes de clusters indépendants", {
  # TODO
})
