source(here("src/clicker/helper/is_mine_propagation_possible.R"))

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
    create_cluster_from_i(grid, 5, init_solved_around(grid)),
    matrix(
      c(
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 1, 1, 1, 0,
        1, 1, 1, 1, 1,
        1, 1, 1, 1, 1,
        1, 1, 1, 1, 1,
        1, 1, 1, 1, 1
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
  expect_equal(
    create_cluster_from_i(grid, 7, solved_around),
    matrix(
      c(
        1, 1, 1, 0, 0, 0,
        1, 1, 1, 0, 0, 0,
        0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0
      ),
      nrow = 6,
      ncol = 6,
      byrow = TRUE
    )
  )
  expect_true(
    all(
      sapply(setdiff(which(grid %in% known_but_no_flag), c(7, 34:36)), function(i) {
        all.equal(
          create_cluster_from_i(grid, i, solved_around),
          matrix(
            c(
              0, 0, 1, 1, 1, 1,
              0, 0, 1, 1, 1, 1,
              1, 1, 1, 1, 1, 1,
              1, 1, 1, 1, 1, 1,
              1, 1, 1, 1, 1, 1,
              1, 1, 1, 1, 1, 1
            ),
            nrow = 6,
            ncol = 6,
            byrow = TRUE
          )
        )
      })
    )
  )
})

test_that("create_cluster_from_i n'inclue pas les zones pleinement résolues", {
  grid <- matrix(
    c(
      0, 1, -5, 1, 0, 1, -10, -10, -10,
      0, 1, 1, 1, 1, 2, -10, -10, -10,
      0, 0, 0, 0, 1, -5, 2, 3, -10,
      0, 0, 1, 2, 4, 3, 2, 2, -10,
      0, 0, 2, -5, -5, -5, 2, 1, -10,
      0, 0, 3, -5, -10, -10, -10, -10, -10,
      1, 1, 3, -5, 3, 2, -10, -10, -10,
      2, -5, 2, 1, 1, 1, -10, -10, -10,
      -5, 4, 3, 1, 1, 1, -10, -10, -10,
      -10, -5, -10, -10, 2, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -5, 3,
      -9, -10, -10, -10, -10, 1, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      1, -10, 1, -10, -10, -10, -10, -10, -5
    ),
    nrow = 17,
    byrow = TRUE
  )
  expect_equal(
    create_cluster_from_i(grid, 26, init_solved_around(grid)),
    matrix(
      c(
        0, 0, 0, 0, 1, 1, 1, 0, 0,
        0, 0, 0, 0, 1, 1, 1, 1, 1,
        0, 0, 0, 0, 1, 1, 1, 1, 1,
        0, 0, 0, 0, 0, 1, 1, 1, 1,
        0, 0, 0, 0, 0, 1, 1, 1, 1,
        0, 0, 0, 1, 1, 1, 1, 1, 1,
        0, 0, 0, 1, 1, 1, 1, 0, 0,
        1, 1, 1, 1, 1, 1, 1, 0, 0,
        1, 1, 1, 1, 1, 1, 1, 0, 0,
        1, 1, 1, 1, 1, 1, 1, 0, 0,
        0, 0, 0, 1, 1, 1, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0
      ),
      nrow = 17,
      byrow = TRUE
    )
  )
})

test_that("create_cluster_from_i fonctionne avec le mode hypothesis", {
  grid <- matrix(
    c(
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -8, -8, -8, -8,
      1, 1, 2, 3, -8, -8, 2, 2, -5,
      1, 2, 2, -5, 2, -5, -5, 4, 2,
      -9, -9, -8, 2, 3, 5, -9, -10, -10,
      -10, -10, -8, -8, -9, -8, -9, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10
    ),
    nrow = 17,
    byrow = TRUE
  )
  expect_equal(
    create_cluster_from_i(grid, 126, init_solved_around(grid)),
    matrix(
      c(
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 1, 1, 1,
        0, 0, 0, 0, 0, 0, 1, 1, 1,
        0, 0, 0, 0, 0, 0, 1, 1, 1,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0
      ),
      nrow = 17,
      byrow = TRUE
    )
  )
})
