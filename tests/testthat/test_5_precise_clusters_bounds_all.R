source(here("src/clicker/probabilistic_clicker.R"))

test_that("precise_clusters_bounds_all donne de bons résultats", {
  grid <- matrix(
    c(
      0, 1,-5, 1, 0, 0, 0, 1, 1,
      1, 2, 1, 1, 0, 0, 0, 2,-5,
      -5, 1, 0, 0, 1, 2, 2, 3,-5,
      2, 2, 1, 2, 3,-5,-5, 2, 1,
      -5, 1, 1,-5,-5, 3, 2, 2, 1,
      1, 1, 2, 3, 4, 3, 2, 2,-5,
      1, 2, 2,-5, 2,-5,-5, 4, 2,
      -5, 2,-5, 2, 3, 5,-5,-2,-1,
      1, 3, 3, 3, 2,-5,-5,-1,-1,
      0, 2,-5,-5, 3, 4,-2,-1,-1,
      1, 3,-5, 4,-2,-1,-1,-1,-1,
      -5, 2, 1, 2,-1,-1,-2,-1,-1,
      2, 3, 2, 2, 2,-1,-2,-1,-1,
      -1,-2,-5,-1,-2,-2,-1,-1,-2,
      -1,-1, 3,-2,-1,-2,-1,-2,-1,
      -1,-1, 2, 1,-1,-2,-1,-2, 3,
      -1,-2,-1,-1,-1,-2,-1,-1,-2
    ),
    nrow = 17,
    ncol = 9,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  clusters <- independant_clusters(grid, solved_around, 17)
  tmp <- precise_clusters_bounds_all(grid, solved_around, mines_left = 17, clusters)
  expect_equal(
    tmp$clusters[[2]]$bornes_mines,
    c(1, 1)
  )
  expect_equal(
    tmp$clusters[[3]]$bornes_mines,
    c(3, 3)
  )
  expect_equal(
    tmp$clusters[[1]]$bornes_mines,
    c(7, 8) # n'a pas été humainement vérifié
  )
})
