source("src/indicies/square_pos_and_get_around_square.R")

test_that("square_pos_and_get_around_square works", {
  expect_equal(
    square_pos_and_get_around_square(c(4, 2), matrix(1:12, ncol = 3)),
    list(
      expand.grid(3:4, 1:3) |> as.matrix() |> unname(),
      matrix(c(3:4, 7:8, 11:12), ncol = 3)
    )
  )
})
