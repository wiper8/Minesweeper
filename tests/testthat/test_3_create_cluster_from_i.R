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
  expect_equal(
    create_cluster_from_i(grid, 1, init_solved_around(grid)),
    matrix(
      0,
      nrow = 4,
      ncol = 5,
      byrow = TRUE
    )
  )

  cluster <- grid * 0
  cluster[7] <- 1
  expect_equal(
    create_cluster_from_i(grid, 7, init_solved_around(grid), cluster),
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
      -10, -10, -10, -10, -10,
      -10, -5, -10, -10, -10,
      -10, -10, -10, -10, -10
    ),
    ncol = 5,
    byrow = TRUE
  )
  expect_equal(
    create_cluster_from_i(grid, 7, init_solved_around(grid)),
    matrix(
      c(
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 1, 0, 0, 0,
        0, 0, 0, 0, 0
      ),
      ncol = 5,
      byrow = TRUE
    )
  )
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
    create_cluster_from_i(grid, 7, init_solved_around(grid)),
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
    create_cluster_from_i(grid, 18, init_solved_around(grid)),
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
  
  grid <- matrix(
    c(
      -1, -1, -1, -2, 2,
      -5, -2, -1, -1, -2,
      -1, -1, -2, -1, -5,
      -1, -2, 2, -1, -2,
      2, 2, 1, 3, -5,
      -2, 1, 0, 2, -2,
      -1, 1, 0, 1, -1
    ),
    ncol = 5,
    byrow = TRUE
  )
  expect_equal(
    create_cluster_from_i(grid, 4, init_solved_around(grid)),
    matrix(
      c(
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 1, 1, 1, 0,
        1, 1, 1, 1, 1,
        1, 1, 1, 1, 1,
        1, 1, 0, 1, 1,
        1, 1, 0, 1, 1
      ),
      ncol = 5,
      byrow = TRUE
    )
  )

  grid <- matrix(
    c(
      -5, 4, -5, 3, -2, 1,
      -5, -1, -5, -1, -1, 2,
      -5, -1, -1, 2, 2, -5,
      -5, 3, 2, -2, 2, 1,
      2, -1, -2, -1, 3, 1,
      1, -2, 3, -5, -5, 1
    ),
    nrow = 6,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  debugonce(create_cluster_from_i)
  expect_true(
    all(
      sapply(seq_along(grid), function(i) {
        all.equal(
          create_cluster_from_i(grid, i, solved_around),
          matrix(
            1,
            nrow = 6,
            ncol = 6,
            byrow = TRUE
          )
        )
      })
    )
  )
})
