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
    update_grid(grid)[[1]],
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
    update_grid(grid)[[1]],
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
    update_grid(grid)[[1]],
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
})

test_that("update_grid n'effectue sa mise à jour que sur les cellules qui ne sont pas considérées résolues
          et change la matrice solved_around", {
  grid <- matrix(
    c(
      -1, -1, -1,
      -1, -1, -1,
      -3, -1, -1,
      -1, -1, -2,
      -2, -1, -1,
      -2, -1, -3
    ),
    nrow = 6, ncol = 3,
    byrow = TRUE
  )
  solved_around <- matrix(
    c(
      1, 1, 1,
      1, 0, 0,
      0, 0, 0,
      0, 0, 0,
      0, 0, 0,
      0, 0, 0
    ),
    nrow = 6, ncol = 3,
    byrow = TRUE
  )
  expect_equal(
    update_grid(grid, solved_around),
    list(
      matrix(
      c(
        -3, -3, -3,
        -3, 0, 0,
        0, 1, 1,
        1, 2, -2,
        -2, 3, 1,
        -2, 2, 0
      ),
      nrow = 6, ncol = 3,
      byrow = TRUE
    ),
    matrix(
      c(
        1, 1, 1,
        1, 1, 1,
        1, 0, 0,
        0, 0, 0,
        0, 0, 0,
        0, 0, 1
      ),
      nrow = 6, ncol = 3,
      byrow = TRUE
    )
    )
  )
})

