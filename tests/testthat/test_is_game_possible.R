test_that("is_game_possible retourne faux si on flag un pattern incorrect", {
  grid <- matrix(c(-2, 1, 0, -1, 2, 1, -6, -5, 1), 3, 3)
  expect_false(is_game_possible(grid))
})
