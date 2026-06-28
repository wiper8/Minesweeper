source("src/clicker/helper/is_cluster_island.R")

test_that("is_cluster_island est fonctionnel", {
  grid <- matrix(
    c(
      1, 2, -5,
      -5, 4, 2,
      -5, -2, -1,
      -5, 4, 1,
      -5, 5, 2,
      -5, -5, -5,
      3, 5, -5,
      -5, 4, 2,
      -5, -2, -1,
      -1, -1, -1,
      -2, -1, -1
    ),
    ncol = 3,
    byrow = TRUE
  )

  expect_equal(
    is_cluster_island(
      grid,
      independant_clusters(grid, init_solved_around(grid), sum(grid == -2), hypothesis = 0)
    ),
    c(TRUE, FALSE)
  )
})
