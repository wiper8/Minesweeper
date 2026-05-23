source(here("src/clicker/is_mine_propagation_possible.R"))

test_that("la propagation de create_cluster_from_i se fait comme prévue sur tout le contour", {
  grid <- matrix(
    c(
      -10, -10, -10, -10, -10,
      -10, -10, -10, -5, 3,
      -10, 1, -10, -10, -10,
      -10, -10, -10, -10, -10
    ),
    ncol = 5,
    byrow = TRUE
  )
  cluster <- grid * 0
  cluster[2] <- 1
  expect_equal(
    create_cluster_from_i(grid, 7, cluster),
    matrix(
      c(
        0, 0, 0, 0, 0,
        1, 1, 1, 0, 0,
        1, 1, 1, 0, 0,
        1, 1, 1, 0, 0
      ),
      ncol = 5,
      byrow = TRUE
    )
  )
})

test_that("des clusters très rapprochés restent indépendants par cause de flag", {
  grid <- matrix(
    c(
      -10, -10, -10, -10, -10,
      -10, -10, -10, -5, 3,
      -10, 1, -10, -10, -10,
      -10, -10, -10, -10, -10
    ),
    ncol = 5,
    byrow = TRUE
  )
  expect_equal(
    create_cluster_from_i(grid, 2),
    matrix(
      c(
        0, 0, 0, 0, 0,
        1, 1, 1, 0, 0,
        1, 1, 1, 0, 0,
        1, 1, 1, 0, 0
      ),
      ncol = 5,
      byrow = TRUE
    )
  )
  
  expect_equal(
    create_cluster_from_i(grid, 18),
    matrix(
      c(
        0, 0, 0, 1, 1,
        0, 0, 0, 1, 1,
        0, 0, 0, 1, 1,
        0, 0, 0, 0, 0
      ),
      ncol = 5,
      byrow = TRUE
    )
  )
})
