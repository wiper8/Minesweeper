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
})
