source(here("src/indicies/get_around_square.R"))

test_that("get_around_square retourne un carré 3x3 lorsque la position est centrale", {
  expect_equal(
    get_around_square(c(2, 2), matrix(1:12, ncol = 3)),
    matrix(c(1:3, 5:7, 9:11), ncol = 3)
  )
  expect_equal(
    get_around_square(c(3, 2), matrix(1:12, ncol = 3)),
    matrix(c(2:4, 6:8, 10:12), ncol = 3)
  )
})

test_that("get_around_square retourne un rectangle 2x3 ou 3x2 lorsque la position est sur un côté", {
  expect_equal(
    get_around_square(c(2, 1), matrix(1:12, ncol = 3)),
    matrix(c(1:3, 5:7), ncol = 2)
  )
  expect_equal(
    get_around_square(c(3, 1), matrix(1:12, ncol = 3)),
    matrix(c(2:4, 6:8), ncol = 2)
  )
  expect_equal(
    get_around_square(c(1, 2), matrix(1:12, ncol = 3)),
    matrix(c(1:2, 5:6, 9:10), ncol = 3)
  )
  expect_equal(
    get_around_square(c(4, 2), matrix(1:12, ncol = 3)),
    matrix(c(3:4, 7:8, 11:12), ncol = 3)
  )
  expect_equal(
    get_around_square(c(2, 3), matrix(1:12, ncol = 3)),
    matrix(c(5:7, 9:11), ncol = 2)
  )
})

test_that("get_around_square retourne un rectangle 2x2 lorsque la position est dans un coin", {
  expect_equal(
    get_around_square(c(1, 1), matrix(1:12, ncol = 3)),
    matrix(c(1:2, 5:6), ncol = 2)
  )
  expect_equal(
    get_around_square(c(1, 3), matrix(1:12, ncol = 3)),
    matrix(c(5:6, 9:10), ncol = 2)
  )
  expect_equal(
    get_around_square(c(4, 3), matrix(1:12, ncol = 3)),
    matrix(c(7:8, 11:12), ncol = 2)
  )
  expect_equal(
    get_around_square(c(4, 1), matrix(1:12, ncol = 3)),
    matrix(c(3:4, 7:8), ncol = 2)
  )
})
