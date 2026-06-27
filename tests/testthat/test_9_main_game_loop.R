source("src/simulate_game.R")
source("src/clicker/certain_core.R")

test_that("main_game_loop n'a pas de récursion infinie quand la partie est déjà terminée", {
  expect_no_error(
    main_game_loop(
      matrix(c(-11, -11, -11, -9, -9, 1, -11, -11, -11, -9, 4, -8, -11, -11, -11, -9, -8, -8, -11, -11, -11, -11, -11, -11), nrow = 6),
      0,
      certain_core, 
      matrix(1, nrow = 6, ncol = 4),
      hypothesis = 2
    )
  )
})
