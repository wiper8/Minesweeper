source(here("src/indicies/square_pos.R"))

test_that("square_pos retourne un carré 3x3 lorsque la position est centrale", {
  expect_equivalent(
    square_pos(c(2, 2), matrix(1:12, ncol = 3)),
    expand.grid(1:3, 1:3) |> as.matrix()
  )
  expect_equivalent(
    square_pos(c(3, 2), matrix(1:12, ncol = 3)),
    expand.grid(2:4, 1:3) |> as.matrix()
  )
})

test_that("square_pos retourne un rectangle 2x3 ou 3x2 lorsque la position est sur un côté", {
  expect_equivalent(
    square_pos(c(2, 1), matrix(1:12, ncol = 3)),
    expand.grid(1:3, 1:2) |> as.matrix()
  )
  expect_equivalent(
    square_pos(c(3, 1), matrix(1:12, ncol = 3)),
    expand.grid(2:4, 1:2) |> as.matrix()
  )
  expect_equivalent(
    square_pos(c(1, 2), matrix(1:12, ncol = 3)),
    expand.grid(1:2, 1:3) |> as.matrix()
  )
  expect_equivalent(
    square_pos(c(4, 2), matrix(1:12, ncol = 3)),
    expand.grid(3:4, 1:3) |> as.matrix()
  )
  expect_equivalent(
    square_pos(c(2, 3), matrix(1:12, ncol = 3)),
    expand.grid(1:3, 2:3) |> as.matrix()
  )
})

test_that("square_pos retourne un rectangle 2x2 lorsque la position est dans un coin", {
  expect_equivalent(
    square_pos(c(1, 1), matrix(1:12, ncol = 3)),
    expand.grid(1:2, 1:2) |> as.matrix()
  )
  expect_equivalent(
    square_pos(c(1, 3), matrix(1:12, ncol = 3)),
    expand.grid(1:2, 2:3) |> as.matrix()
  )
  expect_equivalent(
    square_pos(c(4, 3), matrix(1:12, ncol = 3)),
    expand.grid(3:4, 2:3) |> as.matrix()
  )
  expect_equivalent(
    square_pos(c(4, 1), matrix(1:12, ncol = 3)),
    expand.grid(3:4, 1:2) |> as.matrix()
  )
})
