source(here("src/indicies/i_and_positions.R"))

test_that("position_to_i converti donne le bon résultat", {
  expect_equal(
    position_to_i(c(1, 1), c(10, 8)),
    1
  )
  expect_equal(
    position_to_i(c(2, 3), c(10, 8)),
    10 * 2 + 2
  )
  expect_equal(
    position_to_i(c(10, 8), c(10, 8)),
    10 * 8
  )
})

test_that("position_to_i_mat converti bien une matrice de longueur 1 et donne le bon résultat", {
  expect_equal(
    position_to_i_mat(c(1, 1) |> matrix(nrow = 1), c(10, 8)),
    1
  )
  expect_equal(
    position_to_i_mat(c(2, 3) |> matrix(nrow = 1), c(10, 8)),
    22
  )
  expect_equal(
    position_to_i_mat(c(10, 8) |> matrix(nrow = 1), c(10, 8)),
    80
  )
})

test_that("position_to_i_mat effectue bien des opérations sur des matrices et donne le bon résultat", {
  expect_equal(
    position_to_i_mat(
      matrix(c(1, 1, 2, 3, 10, 8), ncol = 2, byrow = TRUE), c(10, 8)),
    c(1, 22, 80)
  )
})

dims <- c(4, 7)

test_that("i_to_position ne donne pas de positions à 0", {
  pos <- sapply(1:prod(dims), i_to_position, dims = dims)
  expect_true(
    all(pos >= 1)
  )
})

test_that("i_to_position ne donne pas de positions qui dépasse les rangées et colonnes", {
  pos <- sapply(1:prod(dims), i_to_position, dims = dims)
  expect_true(
    all(pos[1, ] <= dims[1]) & all(pos[2, ] <= dims[2])
  )
})

test_that("conversions entre position_to_i et i_to_position redonnent les mêmes résultats", {
  i <- 1:prod(dims)
  pos <- sapply(i, i_to_position, dims = dims)
  new_pos <- apply(pos, 2, position_to_i, dims = dims)
  expect_true(
    all(i == new_pos)
  )
})

