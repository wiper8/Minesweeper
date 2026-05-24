source(here("hp.R"))
source(here("src/game_engine/apply_action.R"))

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
  
  grid <- matrix(
    c(
      0, 1, -5, 1, -3, -1,
      0, 1, 1, 1, 1, -1,
      0, 0, 0, 0, 1, -2,
      0, 0, 1, 2, 4, -1,
      0, 0, 2, -5, -5, -2
    ),
    nrow = 5, byrow = TRUE
  )
  solved_around <- matrix(
    c(
      1, 1, 1, 0, 0, 0,
      1, 1, 1, 0, 0, 0,
      1, 1, 1, 1, 0, 0,
      1, 1, 1, 1, 0, 0,
      1, 1, 1, 0, 0, 0
    ), nrow = 5, byrow = TRUE
  )
  expect_equal(
    update_grid(grid, 2, solved_around)[[2]][16:17],
    c(1, 1)
  )
})

test_that("update_grid fonctionne lors de propagation hypothesis", {
  grid4 <- matrix(
    c(
      0, 1, -5, 1, 0, 1, -10, -10, -10,
      0, 1, 1, 1, 1, 2, -10, -10, -10,
      0, 0, 0, 0, 1, -5, 2, 3, -10,
      0, 0, 1, 2, 4, 3, 2, 2, -10,
      0, 0, 2, -5, -5, -5, 2, 1, -10,
      0, 0, 3, -5, -3, -9, -10, -10, -10,
      1, 1, 3, -5, 3, 2, -10, -10, -10,
      2, -5, 2, 1, 1, 1, -10, -10, -10,
      -5, 4, 3, 1, 1, 1, -10, -10, -10,
      -10, -5, -10, -10, 2, -10, -10, -10, -10
    ),
    ncol = 9,
    byrow = TRUE
  )
  solved_around4 <- matrix(
    c(
      rep(1, 5), rep(0, 4),
      rep(1, 5), rep(0, 4),
      rep(1, 5), rep(0, 4),
      rep(1, 7), 0, 0,
      1, 1, 1, rep(0, 6),
      1, 1, 1, rep(0, 6),
      1, 1, 1, rep(0, 6),
      rep(1, 5), 0, 0, -1, -1,
      rep(0, 7), -1, -1,
      rep(0, 7), -1, -1
    ),
    ncol = 9,
    byrow = TRUE
  )

  expect_equal(
    update_grid(grid4, 28, solved_around4, hypothesis = 2)[[2]],
    matrix(
      c(
        rep(1, 5), rep(0, 4),
        rep(1, 5), rep(0, 4),
        rep(1, 5), rep(0, 4),
        rep(1, 7), 0, 0,
        1, 1, 1, 1, 1, rep(0, 4),
        1, 1, 1, 1, 1, rep(0, 4),
        1, 1, 1, 1, 1, rep(0, 4),
        rep(1, 5), 0, 0, -1, -1,
        rep(0, 7), -1, -1,
        rep(0, 7), -1, -1
      ),
      ncol = 9,
      byrow = TRUE
    )
  )

  grid4[6, 5] <- -9
  grid4[6, 6] <- -3
  expect_equal(
    update_grid(grid4, 28, solved_around4, hypothesis = 2)[[2]][45:47],
    c(1, 1, 1)
  )
})
