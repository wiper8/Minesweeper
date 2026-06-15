source(here("src/clicker/helper/find_best_i_to_investigate.R"))

test_that("find_best_i_to_investigate peut retourner un vecteur de longueur 0 dans des edge cases", {
  grid <- matrix(
    c(
      -2, -1, -1, -1,
      -2, -1, -1, -2,
      -1, -1, -1, -2,
      -1, -1, -1, -1,
      -5, -5, -1, -1,
      3, -5, -1, -1
    ),
    nrow = 6,
    byrow = TRUE
  )
  expect_equal(
    length(find_best_i_to_investigate(grid, solved_around = init_solved_around(grid), click_order = NULL)),
    0
  )
})

test_that("find_best_i_to_investigate ne retourne pas de NA", {
  grid <- matrix(
    c(
      -1, -2, -2, -1,
      -2, -2, -2, -2,
      -1, -2, -2, -2,
      -5, -5, -5, -5,
      5, -5, 5, 2,
      -5, -5, 2, 0
    ),
    nrow = 6,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  solved_around[6, 3:4] <- 0
  expect_true(
    all(!is.na(
      find_best_i_to_investigate(
        grid, solved_around,
        click_order = matrix(
          c(
            5, 1,
            6, 3,
            5, 3,
            6, 4
          ),
          ncol = 2,
          byrow = TRUE
        )
      )
    ))
  )
})

test_that("find_best_i_to_investigate works", {
  grid <- matrix(
    c(
      0, 1, -5, 1, 0, 1, -10, -10, -10,
      0, 1, 1, 1, 1, 2, -10, -10, -10,
      0 ,0, 0, 0, 1, -5, 2, 3, -10,
      0, 0, 1, 2, 4, 3, 2, 2, -10,
      0, 0, 2, -5, -5, -5, 2, 1, -10,
      0, 0, 3, -5, -9, -8, -10, -10, -10,
      1, 1, 3, -5, 3, 2, -10, -10, -10,
      2, -5, 2, 1, 1, 1, -10, -10, -10,
      -5, 4, 3, 1, 1, 1, -10, -10, -10,
      -9, -5, -8, -9, 2, -8, -10, -10, -10,
      rep(-10, 7 * 9)
    ),
    nrow = 17,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  expect_true(
    find_best_i_to_investigate(grid, solved_around, click_order = NULL)[1] %in% c(123, 124)
  )
  expect_true(
    find_best_i_to_investigate(grid, solved_around, click_order = matrix(c(7, 6, 5, 6), nrow = 2))[1] %in% c(123, 124)
  )
})
