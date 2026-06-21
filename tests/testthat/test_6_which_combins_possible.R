source(here("src/clicker/certain_core.R"))

test_that("which_combins_possible peut retourner TRUE sur un cas incertain", {
  grid <- matrix(
    c(
      -1, -1, -1,
      -2, 1, -1,
      -1, -1, -1
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_true(
    which_combins_possible(
      grid,
      matrix(1),
      matrix(
        c(
          1, 1,
          2, 1,
          3, 1,
          1, 2,
          3, 2,
          1, 3,
          2, 3,
          3, 3
        ),
        ncol = 2,
        byrow = TRUE
      ),
      solved_around = matrix(0, 3, 3),
      mines_left = NA
    )$possible
  )
  
  grid <- matrix(
    c(
      -1, -1, -2,
      -2, 2, -1,
      -1, -1, -1
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    which_combins_possible(
      grid,
      matrix(c(1, 2, 2, 3, 2, 4, 2, 5, 2, 6, 2, 7, 2, 8), nrow = 2),
      matrix(
        c(
          1, 1,
          2, 1,
          3, 1,
          1, 2,
          3, 2,
          1, 3,
          2, 3,
          3, 3
        ),
        ncol = 2,
        byrow = TRUE
      ),
      solved_around = matrix(0, 3, 3),
      mines_left = NA
    )$possible,
    c(TRUE, rep(NA, 6))
  )
  
  grid <- matrix(
    c(
      -2, -1, -2, -1,
      1, 2, 1, 1
    ),
    nrow = 2,
    byrow = TRUE
  )
  expect_true(
    which_combins_possible(
      grid,
      matrix(1),
      matrix(c(1, 1, 1, 2), nrow = 2),
      solved_around = matrix(0, nrow = 2, ncol = 4),
      mines_left = NA
    )$possible
  )
})

test_that("which_combins_possible doit retourner FALSE sur un cas impossible", {
  grid <- matrix(
    c(
      -1, -1, -1,
      -2, 1, -1,
      -1, 1, -1
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_false(
    which_combins_possible(
      grid,
      matrix(1),
      matrix(c(1:3, 1, 1:3, 1, 1, 1, 2, 3, 3, 3), ncol = 2),
      solved_around = grid * 0,
      mines_left = NA
    )$possible
  )
})

test_that("which_combins_possible pour des cas complexes", {
  grid <- matrix(
    c(
      -1, -1, -5, -2, -1,
      1, 3, 3, 4, -1,
      -2, 3, -5, -1, -2,
      -1, 4, -5, 3, 1,
      2, -5, 2, 1, 0,
      -2, 2, 1, 0, 0,
      -1, 1, 0, 0, 0,
      1, 2, 1, 2, 1,
      -2, 3, -5, 3, -5,
      -1, -1, -2, -1, -1
    ),
    nrow = 10,
    byrow = TRUE
  )
  solved_around <- grid * 0
  solved_around[c(25:28, 35:38, 45:48)] <- 1
  
  expect_true(
    which_combins_possible(
      grid,
      matrix(2:3, nrow = 1),
      matrix(c(10, 10, 10, 3:5), ncol = 2),
      solved_around = solved_around,
      mines_left = NA
    )$possible[1]
  )
  
  expect_equal(
    which_combins_possible(
      grid,
      combins = matrix(c(1, 2, 2, 3, 2, 4), nrow = 2),
      pos_unknown = matrix(c(9, 10, 10, 10, 1, 1, 2, 3), ncol = 2),
      solved_around = solved_around,
      mines_left = 12
    )$possible,
    c(FALSE, FALSE, FALSE)
  )
})

test_that("which_combins_possible pour des cas complexes", {
  grid <- matrix(
    c(
      -8, -8, -5, -10, -10,
      1, 3, 3, 4, -10,
      -9, 3, -5, -10, -10,
      -8, 4, -5, 3, 1,
      2, -5, 2, 1, 0,
      -9, 2, 1, 0, 0,
      -8, 1, 0, 0, 0,
      1, 2, 1, 2, 1,
      -9, 3, -5, 3, -5,
      -10, -10, -10, -10, -10
    ),
    nrow = 10,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  
  expect_false(
    which_combins_possible(
      grid,
      matrix(1, nrow = 1),
      matrix(c(10, 10, 10, 1:3), ncol = 2),
      solved_around = solved_around,
      mines_left = 3
    )$possible[1]
  )
  expect_false(
    which_combins_possible(
      grid,
      matrix(2, nrow = 1),
      matrix(c(10, 10, 10, 1:3), ncol = 2),
      solved_around = solved_around,
      mines_left = 3
    )$possible[1]
  )
  expect_true(
    which_combins_possible(
      grid,
      matrix(3, nrow = 1),
      matrix(c(10, 10, 10, 1:3), ncol = 2),
      solved_around = solved_around,
      mines_left = 3
    )$possible[1]
  )
})
