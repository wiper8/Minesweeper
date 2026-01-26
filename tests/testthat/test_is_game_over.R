source(here("src/game_engine/is_game_over.R"))

test_that("une partie vierge est en cours", {
  set.seed(2026L)
  grid <- matrix(sample(c(-1, -2), 12, replace = TRUE), nrow = 4)
  expect_equal(
    is_game_over(grid),
    0
  )
})

test_that("une partie avec une mine découverte est perdue", {
  set.seed(2026L)
  grid <- matrix(c(sample(c(-1, -2), 11, replace = TRUE), -4), nrow = 4)
  expect_equal(
    is_game_over(grid),
    -1
  )
})

test_that("une partie sans boîte sans mine est terminée", {
  set.seed(2026L)
  grid <- matrix(sample(c(0:9, -2, -5), 200, replace = TRUE), nrow = 10)
  expect_equal(
    is_game_over(grid),
    1
  )
})

test_that("trop ou pas assez de mines génère une erreur", {
  set.seed(2026L)
  grid <- matrix(
    c(
      -2, -2, -2,
      -5, -5, -1,
      -1, -1, -1,
      -1, -1, -1
    ),
    ncol = 3,
    byrow = TRUE
  )
  expect_error(
    is_game_over(grid, 6)
  )
  expect_error(
    is_game_over(grid, 4)
  )
  expect_no_error(
    is_game_over(grid, 5)
  )
  
  grid <- matrix(
    c(
      -8, -8, -5, -10, -10,
      1, 3, 3, 4, -10,
      -9, 3, -5, -10, -10,
      -8, 4, -5, 3, 1,
      2, -5, 2, 1, 0,
      -9, 2, 1, 0, 0,
      -8, 1, 0, 0, 0,
      1, 2, 1, 2, 1,
      -9, 3, -5, 3, -5,
      -9, -8, -8, -10, -10
    ),
    nrow = 10,
    byrow = TRUE
  )
  expect_equal(
    is_game_over(grid, 12),
    0
  )
  grid <- matrix(
    c(
      -8, -8, -5, -9, -8,
      1, 3, 3, 4, -8,
      -9, 3, -5, -8, -9,
      -8, 4, -5, 3, 1,
      2, -5, 2, 1, 0,
      -9, 2, 1, 0, 0,
      -8, 1, 0, 0, 0,
      1, 2, 1, 2, 1,
      -9, 3, -5, 3, -5,
      -9, -8, -8, -9, -10
    ),
    nrow = 10,
    byrow = TRUE
  )
  expect_error(
    is_game_over(grid, 12)
  )
})
