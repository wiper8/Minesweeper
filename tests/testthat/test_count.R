source(here("src/indicies/count.R"))

grid <- matrix(
  c(
    -2, 1, 2, -2, 5, -2,
    1, 1, 2, -5, -2, -5,
    0, 0, 0, 2, 3, 2,
    1, 1, 1, 0, 1, -1,
    -6, -2, 1, 0, 1, -2
  ),
  nrow = 5,
  byrow = TRUE
)

test_that("count_unknown donne les bons résultats", {
  expect_equal(
    count_unknown(grid, 7),
    1
  )
  expect_equal(
    count_unknown(grid, 21),
    3
  )
  expect_equal(
    count_unknown(grid, 9),
    1
  )
  expect_equal(
    count_unknown(grid, 25),
    2
  )
})

test_that("count_mines_left_around donne les bons résultats", {
  expect_equal(
    count_mines_left_around(grid, 7),
    1
  )
  expect_equal(
    count_mines_left_around(grid, 21),
    3
  )
  expect_equal(
    count_mines_left_around(grid, 9),
    0
  )
  expect_equal(
    count_mines_left_around(grid, 25),
    1
  )
  expect_equal(
    count_mines_left_around(matrix(c(-8, 2, -10, 4, -5, 2), ncol = 2), 2),
    1
  )
})
