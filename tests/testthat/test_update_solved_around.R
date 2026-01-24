source(here("src/game_engine/apply_action.R"))

test_that("update_solved_around fonctionne", {
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
      rep(0, 9 * 4),
      rep(1, 9),
      rep(0, 9 * 3)
    ),
    ncol = 9,
    byrow = TRUE
  )
  expect_equal(
    update_solved_around(grid, solved_around),
    matrix(
      c(
        rep(0, 9),
        rep(0, 9),
        c(0, 0, 0, 0, 1, 1, 0, 0, 0),
        c(0, 0, 0, 1, 1, 1, 0, 0, 0),
        rep(1, 9),
        rep(0, 9),
        rep(0, 9),
        rep(0, 9)
      ),
      ncol = 9,
      byrow = TRUE
    )
  )
})
