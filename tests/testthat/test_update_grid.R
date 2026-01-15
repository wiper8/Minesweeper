source(here("hp.R"))
source(here("src/game_engine/update_grid.R"))

test_that("update_grid révelle bien les case autour de celle cliquée", {
  grid <- matrix(
    c(
      -2, -1, -1,
      -1, -3, -2,
      -2, -1, -2
    ),
    nrow = 3, ncol = 3,
    byrow = TRUE
  )
  expect_equal(
    update_grid(grid),
    matrix(
      c(
        -2, -1, -1,
        -1, 4, -2,
        -2, -1, -2
      ),
      nrow = 3, ncol = 3,
      byrow = TRUE
    )
  )
})

test_that("update_grid continue de réveller les cases vides", {
  grid <- matrix(
    c(
      -1, -1, -1,
      -1, -3, -1,
      -1, -1, -1,
      -1, -1, -2,
      -1, -1, -1,
      -1, -1, -1
    ),
    nrow = 6, ncol = 3,
    byrow = TRUE
  )
  expect_equal(
    update_grid(grid),
    matrix(
      c(
        0, 0, 0,
        0, 0, 0,
        0, 1, 1,
        0, 1, -2,
        0, 1, 1,
        0, 0, 0
      ),
      nrow = 6, ncol = 3,
      byrow = TRUE
    )
  )
})
