source(here("src/clicker/certain_core.R"))

test_that("certain_core appelle can_flag_all_around", {
  stub(certain_core, "can_flag_all_around", list(c(1, 1), FALSE))
  
  grid <- matrix(
    c(
      -2, 1, -1,
      1, 1, -1,
      -1, -1, -1
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    certain_core(grid, NA, matrix(0, nrow(grid), ncol(grid))),
    list(c(1, 1), FALSE)
  )
  
  stub(certain_core, "can_flag_all_around", list(c(1, 5), FALSE))
  grid <- matrix(
    c(
      -5, 1, 0, 2, -2,
      1, 1, 1, 4, -2,
      -1, -1, -5, 3, -2
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    certain_core(grid, NA, matrix(0, nrow(grid), ncol(grid))),
    list(c(1, 5), FALSE)
  )
})

test_that("certain_core appelle can_click_all_around", {
  stub(certain_core, "can_click_all_around", list(c(1, 3), TRUE))
  
  grid <- matrix(
    c(
      -5, 1, -1,
      1, 1, -1,
      0, 0, -1
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    certain_core(grid, NA, matrix(0, nrow(grid), ncol(grid))),
    list(c(1, 3), TRUE)
  )
  
  stub(certain_core, "can_click_all_around", list(c(3, 1), TRUE))
  grid <- matrix(
    c(
      -5, 1, 0, 2, -5,
      1, 1, 1, 4, -5,
      -1, -1, -5, 3, -5
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    certain_core(grid, NA, matrix(0, nrow(grid), ncol(grid))),
    list(c(3, 1), TRUE)
  )
})

test_that("certain_core appelle can_deduce_pattern", {
  stub(certain_core, "can_deduce_pattern", list(c(1, 1), FALSE))
  
  grid <- matrix(
    c(
      -2, -1, -2, -1,
      1, 2, 1, 1
    ),
    nrow = 2,
    byrow = TRUE
  )
  expect_equal(
    certain_core(grid, NA, matrix(0, nrow(grid), ncol(grid))),
    list(c(1, 1), FALSE)
  )
})

test_that("certain_core donne du random s'il ne sait pas quoi faire", {
  grid <- matrix(
    c(
      -1, -1, -1,
      -2, 1, -1,
      -1, -1, -1
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    certain_core(grid, 1, matrix(0, nrow(grid), ncol(grid))),
    NULL
  )
})
