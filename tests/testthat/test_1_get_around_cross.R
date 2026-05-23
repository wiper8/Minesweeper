source(here("src/indicies/get_around_square.R"))

test_that("get_around_cross retourne un carré 3x3 lorsque la position est centrale", {
  expect_equal(
    get_around_cross(c(2, 2), matrix(1:12, ncol = 3)),
    matrix(c(NA, 2, NA, 5:7, NA, 10, NA), ncol = 3)
  )
  expect_equal(
    get_around_cross(c(3, 2), matrix(1:12, ncol = 3)),
    matrix(c(NA, 3, NA, 6:8, NA, 11, NA), ncol = 3)
  )
})

test_that("get_around_cross retourne un rectangle 2x3 ou 3x2 lorsque la position est sur un côté", {
  expect_equal(
    get_around_cross(c(2, 1), matrix(1:12, ncol = 3)),
    matrix(c(1:3, NA, 6, NA), ncol = 2)
  )
  expect_equal(
    get_around_cross(c(3, 1), matrix(1:12, ncol = 3)),
    matrix(c(2:4, NA, 7, NA), ncol = 2)
  )
  expect_equal(
    get_around_cross(c(1, 2), matrix(1:12, ncol = 3)),
    matrix(c(1, NA, 5:6, 9, NA), ncol = 3)
  )
  expect_equal(
    get_around_cross(c(4, 2), matrix(1:12, ncol = 3)),
    matrix(c(NA, 4, 7:8, NA, 12), ncol = 3)
  )
  expect_equal(
    get_around_cross(c(2, 3), matrix(1:12, ncol = 3)),
    matrix(c(NA, 6, NA, 9:11), ncol = 2)
  )
})

test_that("get_around_cross retourne un rectangle 2x2 lorsque la position est dans un coin", {
  expect_equal(
    get_around_cross(c(1, 1), matrix(1:12, ncol = 3)),
    matrix(c(1:2, 5, NA), ncol = 2)
  )
  expect_equal(
    get_around_cross(c(1, 3), matrix(1:12, ncol = 3)),
    matrix(c(5, NA, 9:10), ncol = 2)
  )
  expect_equal(
    get_around_cross(c(4, 3), matrix(1:12, ncol = 3)),
    matrix(c(NA, 8, 11:12), ncol = 2)
  )
  expect_equal(
    get_around_cross(c(4, 1), matrix(1:12, ncol = 3)),
    matrix(c(3:4, NA, 8), ncol = 2)
  )
})
