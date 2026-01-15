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
