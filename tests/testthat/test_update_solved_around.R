source(here("src/game_engine/apply_action.R"))

test_that("update_solved_around met à jour autour du i ciblé", {
  grid <- matrix(
    c(
      -1, -1, -1, -2, -1, -1, -2, -2, -1,
      -2, -2, -1, -5, 3, 1, 2, -1, -1,
      -2, 3, 3, -5, 2, 0, 1, -1, -1,
      -1, -1, 2, 1, 1, 1, 2, -2, -1,
      -1, -2, 2, 0, 0, 2, -5, -1, -1,
      -1, -5, 4, 2, 1, 3, -5, -1, -1,
      -1, -1, -5, -2, -1, 4, -2, -1, -1,
      -1, -1, -1, -1, -2, -2, -1, -1, -1
    ),
    ncol = 9,
    byrow = TRUE
  )
  solved_around <- matrix(
    c(
      c(-1, -1, -1, rep(0, 9 * 4 - 3)),
      rep(1, 9),
      rep(0, 9 * 3)
    ),
    ncol = 9,
    byrow = TRUE
  )
  # cas où on vient d'ajouter un flag
  expect_equal(
    update_solved_around(grid, solved_around, 26)[17],
    0
  )
  expect_equal(
    update_solved_around(grid, solved_around, 28)[28],
    1
  )
  expect_equal(
    update_solved_around(grid, solved_around, 35)[35],
    1
  )
  # cas où je viens de cliquer quelque part
  expect_equal(
    update_solved_around(grid, solved_around, 43)[43],
    1
  )
  expect_equal(
    update_solved_around(grid, solved_around, 44)[44],
    1
  )
  expect_equal(
    update_solved_around(grid, solved_around, 65)[c(57, 58, 66)],
    c(0, 0, 0)
  )
})
