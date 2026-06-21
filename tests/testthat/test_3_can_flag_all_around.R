source(here("src/clicker/certain_core.R"))

test_that("can_flag_all_around ajoute les flags qui sont certains autour des cases", {
  grid <- matrix(
    c(
      -2, 1, -1,
      1, 1, -1,
     0, 0, -1
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    can_flag_all_around(grid, 1, grid * 0)$clicks,
    list(list(c(1, 1), FALSE, "certain"))
  )
  
  grid <- matrix(
    c(
      -5, 1, 0, 2, -2,
      1, 2, 1, 4, -2,
      -1, -1, -5, 3, -2
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    can_flag_all_around(grid, 3, grid * 0)$clicks,
    list(list(c(1, 5), FALSE, "certain"), list(c(2, 5), FALSE, "certain"))
  )
  
  grid <- matrix(
    c(
      -8, 1,
      2, -5,
      -10, 2
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    can_flag_all_around(grid, 1, grid * 0)$clicks,
    list(list(c(3, 1), FALSE, "certain"))
  )
})

test_that("can_flag_all_around ne peut ajouter de drapeau s'il ne reste pas de mines disponible", {
  grid <- matrix(
    c(
      -2, 1, -1,
      1, 1, -1,
      0, 0, -1
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    can_flag_all_around(grid, 1, grid * 0)$clicks,
    list(list(c(1, 1), FALSE, "certain"))
  )
  
  grid <- matrix(
    c(
      -5, 1, 0, 2, -5,
      1, 2, 1, 4, -2,
      -1, -1, -5, 3, -5
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    can_flag_all_around(grid, 0, grid * 0)$clicks,
    "impossible"
  )
})
