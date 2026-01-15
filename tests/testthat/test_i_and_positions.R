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
