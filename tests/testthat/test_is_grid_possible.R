source(here("src/game_engine/is_grid_possible.R"))

test_that("is_grid_possible retourne faux pour des nombres qui supposerait trop de mines", {
  expect_false(
    is_grid_possible(matrix(c(-1, -2, 2, -1), nrow = 2))
  )
})

test_that("is_grid_possible retourne faux si un nombre n'a plus assez de place pour placer toutes ses mines", {
  grid <- matrix(c(-2, -1, -3, 2, 6, 1, 1, 1, 1), 3, 3)
  expect_false(is_grid_possible(grid))
})

test_that("is_grid_possible retourne faux pour des nombres qui supposerait trop peu de mines", {
  expect_false(
    is_grid_possible(matrix(c(-1, -1, -1, -5, 0, -1, -1, -1, -1), nrow = 3))
  )
  expect_false(
    is_grid_possible(matrix(c(-1, -2, -1, -2, 1, -1, -1, -1, -1), nrow = 3))
  )
})

test_that("is_grid_possible retourne les bonnes valeurs selon tous les hyperparamètres possibles", {
  base_grid <- matrix(c(-1:-6), nrow = 3, ncol = 4)
  expect_false(all(sapply(
    c(0:2, 4:8),
    function(number) {
      grid <- base_grid
      grid[1] <- number
      is_grid_possible(grid)
    }
  )))
  grid <- base_grid
  grid[1] <- 3
  expect_true(is_grid_possible(grid))
  
  expect_false(all(sapply(
    c(0:1, 3:8),
    function(number) {
      grid <- base_grid
      grid[2] <- number
      is_grid_possible(grid)
    }
  )))
  grid <- base_grid
  grid[2] <- 2
  expect_true(is_grid_possible(grid))
  
  expect_false(all(sapply(
    c(0:1, 3:8),
    function(number) {
      grid <- base_grid
      grid[3] <- number
      is_grid_possible(grid)
    }
  )))
  grid <- base_grid
  grid[3] <- 2
  expect_true(is_grid_possible(grid))

  # TODO peut-être en rajouter
})
