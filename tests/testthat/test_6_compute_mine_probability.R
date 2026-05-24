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
  debugonce(compute_mine_probability)
  expect_equal(
    compute_mine_probability(grid, mine_pos = c(1, 2), mines_left = 7, solved_around),
    1 / 9 # TODO vérifier
  )
})

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


grid <- matrix(
  c(
    -1, -1, -2, -1, -2,
    -1, 2, 1, 3, -2,
    -2, 1, 0, 2, -1,
    -1, 2, 2, 3, -2,
    -1, -1, -2, -2, -1
  ),
  ncol = 5,
  byrow = TRUE
)

grid <- matrix(
  c(
    -1, -1, -2, -1, -2,
    -1, 2, 1, 3, -1,
    -2, 1, 0, 2, -2,
    -1, 2, 2, 3, -2,
    -1, -2, -1, -2, -1
  ),
  ncol = 5,
  byrow = TRUE
)

grid <- matrix(
  c(
    -1, -1, -2, -1, -2,
    -2, 2, 1, 3, -1,
    -1, 1, 0, 2, -2,
    -1, 2, 2, 3, -2,
    -1, -2, -2, -1, -1
  ),
  ncol = 5,
  byrow = TRUE
)

grid <- matrix(
  c(
    -1, -1, -2, -1, -1,
    -1, 2, 1, 3, -2,
    -2, 1, 0, 2, -2,
    -1, 2, 2, 3, -1,
    -1, -2, -1, -2, -2
  ),
  ncol = 5,
  byrow = TRUE
)

grid <- matrix(
  c(
    -1, -1, -2, -1, -1,
    -2, 2, 1, 3, -2,
    -1, 1, 0, 2, -2,
    -1, 2, 2, 3, -1,
    -1, -2, -2, -1, -2
  ),
  ncol = 5,
  byrow = TRUE
)

grid <- matrix(
  c(
    -1, -1, -2, -1, -1,
    -2, 2, 1, 3, -2,
    -1, 1, 0, 2, -2,
    -1, 2, 2, 3, -1,
    -2, -1, -2, -2, -1
  ),
  ncol = 5,
  byrow = TRUE
)

grid <- matrix(
  c(
    -2, -1, -2, -1, -1,
    -1, 2, 1, 3, -2,
    -1, 1, 0, 2, -2,
    -2, 2, 2, 3, -1,
    -1, -1, -2, -2, -1
  ),
  ncol = 5,
  byrow = TRUE
)

grid <- matrix(
  c(
    -2, -1, -1, -2, -1,
    -1, 2, 1, 3, -2,
    -2, 1, 0, 2, -2,
    -1, 2, 2, 3, -1,
    -1, -1, -2, -2, -1
  ),
  ncol = 5,
  byrow = TRUE
)

test_that("compute_mine_probability calcule les bonnes probabilitées dans des cas complexes de clusters indépendants", {
  # TODO
})
