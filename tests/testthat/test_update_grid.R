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
    update_grid(grid, NA)[[1]],
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
      -1, -1, -2
    ),
    nrow = 6, ncol = 3,
    byrow = TRUE
  )
  expect_equal(
    update_grid(grid, NA)[[1]],
    matrix(
      c(
        0, 0, 0,
        0, 0, 0,
        0, 1, 1,
        0, 1, -2,
        0, 2, -1,
        0, 1, -2
      ),
      nrow = 6, ncol = 3,
      byrow = TRUE
    )
  )
})

test_that("update_grid flag toutes les cases mines si la partie est terminée", {
  grid <- matrix(
    c(
      -1, -1, -1,
      -1, -3, -1,
      -1, -1, -1,
      -1, -1, -2,
      -2, -1, -1,
      -2, -1, -3
    ),
    nrow = 6, ncol = 3,
    byrow = TRUE
  )
  expect_equal(
    update_grid(grid, 3)[[1]],
    matrix(
      c(
        0, 0, 0,
        0, 0, 0,
        0, 1, 1,
        1, 2, -5,
        -5, 3, 1,
        -5, 2, 0
      ),
      nrow = 6, ncol = 3,
      byrow = TRUE
    )
  )
  expect_equal(
    update_grid(grid, 3)[[3]],
    0
  )
})

test_that("update_grid propage bien les update_solved_around quand il y a des 0", {
  grid <- matrix(
    c(
      -2, -1, -1, -3, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -2,
      -1, -1, -1, -1, -1, -1, -2,
      -1, -1, -1, -2, -2, -1, -1
    ), nrow = 4, byrow = TRUE
  )
  expect_equal(
    (update_grid(grid, 4, grid * 0 - 1)[[2]] == 1) |>
      which(),
    which(matrix(
      c(
        NA, 0, 1, 1, 1, 0, NA,
        0, 0, 1, 1, 1, 0, NA,
        1, 1, 0, 0, 0, 0, NA,
        1, 1, 0, NA, NA, NA, NA
      ), nrow = 4, byrow = TRUE
    ) == 1)
  )
})
