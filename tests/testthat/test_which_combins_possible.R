test_that("which_combins_possible propage bien l'information", {
  grid <- matrix(
    c(
      -2, -1, -2, -1,
      1, 2, 1, 1
    ),
    nrow = 2,
    byrow = TRUE
  )
  my_mock <- mock(FALSE, TRUE, TRUE)
  stub(which_combins_possible, "is_mine_propagation_possible", my_mock)
  expect_equal(
    which_combins_possible(
      grid,
      matrix(c(1, 2, 1, 3, 2, 3), nrow = 2),
      matrix(c(1, 1, 1, 1, 2, 3), ncol = 2),
      solved_around = matrix(0, nrow = 2, ncol = 4)
    ),
    c(FALSE, TRUE, TRUE)
  )
})

