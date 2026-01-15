source(here("src/game_engine/is_grid_possible.R"))

test_that("is_grid_possible retourne faux si un nombre n'a plus assez de place pour placer toutes ses mines", {
  grid <- matrix(c(-2, -1, -3, 2, 6, 1, 1, 1, 1), 3, 3)
  expect_false(is_grid_possible(grid))
})
