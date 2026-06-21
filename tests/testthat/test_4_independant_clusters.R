source(here("src/clicker/helper/is_mine_propagation_possible.R"))

test_that("independant_clusters fonctionne pour des hypothesis != 2", {
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
  solution_grid1 <- matrix(
    c(
      -11, -11, -5, 3, -2, 1,
      -11, -11, -5, -1, -1, 2,
      -5, -1, -1, 2, 2, -5,
      -5, 3, 2, -2, 2, 1,
      2, -1, -2, -1, 3, 1,
      1, -2, 3, -5, -5, 1
    ),
    nrow = 6,
    byrow = TRUE
  )
  solution_grid2 <- matrix(
    c(
      -5, 4, -5, -11, -11, -11,
      -5, -1, -5, -11, -11, -11,
      -11, -11, -11, -11, -11, -11,
      -11, -11, -11, -11, -11, -11,
      -11, -11, -11, -11, -11, -11,
      -11, -11, -11, -11, -11, -11
    ),
    nrow = 6,
    byrow = TRUE
  )
  expect_equal(
    independant_clusters(grid, solved_around, mines_left = 5),
    list(
      void = list(
        grid = grid * 0 - 11,
        solved_around = grid * 0 - 1,
        in_cluster = matrix(FALSE, nrow = nrow(grid), ncol = ncol(grid)),
        bornes_mines = c(0, 0),
        possible = rep("NA", 1)
      ),
      known_but_does_nothing = list(
        in_cluster = matrix(FALSE, nrow = nrow(grid), ncol = ncol(grid))
      ),
      clusters = list(
        list(
          grid = solution_grid1,
          solved_around = init_solved_around(solution_grid1),
          in_cluster = solution_grid1 != -11,
          bornes_mines = c(0, 5),
          possible = rep("NA", 6)
        ),
        list(
          grid = solution_grid2,
          solved_around = init_solved_around(solution_grid2),
          in_cluster = solution_grid2 != -11,
          bornes_mines = c(0, 1),
          possible = rep("NA", 2)
        )
      )
    )
  )
})

test_that("independant_clusters ne met pas des cases hypothéthiques mais connues dans le void", {
  grid <- matrix(
    c(
      2, -5, -10,
      -9, -8, -10,
      -8, -10, -10,
      -10, -10, -10
    ),
    ncol = 3,
    byrow = TRUE
  )
  expect_false(
    independant_clusters(grid, init_solved_around(grid), 3)$void$in_cluster[3]
  )
})

test_that("independant_clusters crée un void adéquat", {
  grid <- matrix(
    c(
      -1, -5, 1, 0,
      -1, 3, 2, 0,
      -2, -5, 1, 0,
      -1, 3, 2, 0,
      -1, -5, 2, 0,
      -2, -5, 2, 0
    ),
    nrow = 6,
    byrow = TRUE
  )
  expect_equal(
    independant_clusters(grid, init_solved_around(grid), 2)$void$in_cluster,
    matrix(
      c(
        F, F, F, F,
        F, F, F, F,
        F, F, F, F,
        F, F, F, F,
        F, F, F, F,
        T, F, F, F
      ),
      nrow = 6,
      byrow = TRUE
    )
  )
})

test_that("independant_clusters fonctionne avec le mode hypothesis", {
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
  expect_no_error(
    independant_clusters(grid, init_solved_around(grid), 31)
  )
  grid <- matrix(
    c(
      -10, -10, -5, -10, -10,
      1, 3, 3, 4, -10,
      -10, 3, -5, -10, -10,
      -10, 4, -5, 3, 1,
      2, -5, 2, 1, 0,
      -10, 2, 1, 0, 0,
      -10, 1, 0, 0, 0,
      1, 2, 1, 2, 1,
      -9, 3, -5, 3, -5,
      -9, -8, -8, -10, -10
    ),
    ncol = 5,
    byrow = TRUE
  )
  expect_true(all(
    independant_clusters(grid, init_solved_around(grid), 5)$known_but_does_nothing$in_cluster[c(10, 20)]
  ))
})

test_that("independant_clusters works properly", {
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
  solved_around <- init_solved_around(grid)
  tmp <- independant_clusters(grid, solved_around, mines_left = 32)
  expect_equal(
    tmp$void,
    list(
      grid = grid * 0 - 11,
      solved_around = grid * 0 - 1,
      in_cluster = matrix(
        c(
          F, F, F, F, F, F, F, T, T,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, T, T,
          F, F, F, F, F, F, F, T, T,
          F, F, F, F, F, F, F, T, T,
          F, F, F, F, F, F, F, T, T,
          T, T, T, F, F, F, T, F, F,
          T, T, T, T, F, F, F, F, F,
          F, T, T, T, F, F, F, F, F,
          T, T, T, T, F, F, F, T, T,
          T, T, T, T, T, T, T, T, T,
          F, F, F, F, T, T, T, T, T,
          F, F, F, F, T, T, T, T, F
        ),
        nrow = 17,
        byrow = TRUE
      ),
      bornes_mines = c(0, 32),
      possible = rep("NA", 33)
    )
  )
  expect_equal(
    tmp$known_but_does_nothing,
    list(
      in_cluster = matrix(
        c(
          T, T, T, T, F, F, F, F, F,
          T, T, T, T, F, F, F, F, F,
          T, T, T, T, F, F, F, F, F,
          T, T, T, T, T, F, F, F, F,
          T, T, T, T, T, F, F, F, F,
          T, T, T, F, F, F, F, F, F,
          T, T, T, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          T, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, T
        ),
        nrow = 17,
        byrow = TRUE
      )
    )
  )
  expect_equal(
    tmp$clusters[[1]][c(1, 3:5)],
    list(
      grid = matrix(
        c(
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -10, -10, -10, -10, -11, -11, -11, -11, -11,
          1, -10, 1, -10, -11, -11, -11, -11, -11
        ),
        nrow = 17,
        byrow = TRUE
      ),
      in_cluster = matrix(
        c(
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          T, T, T, T, F, F, F, F, F,
          T, T, T, T, F, F, F, F, F
        ),
        nrow = 17,
        byrow = TRUE
      ),
      bornes_mines = c(0, 6),
      possible = rep("NA", 7)
    )
  )
  expect_equal(
    tmp$clusters[[2]][c(1, 3:5)],
    list(
      grid = matrix(
        c(
          -11, -11, -11, -11, 0, 1, -10, -11, -11,
          -11, -11, -11, -11, 1, 2, -10, -10, -10,
          -11, -11, -11, -11, 1, -5, 2, 3, -10,
          -11, -11, -11, -11, -11, 3, 2, 2, -10,
          -11, -11, -11, -11, -11, -5, 2, 1, -10,
          -11, -11, -11, -5, -10, -10, -10, -10, -10,
          -11, -11, -11, -5, 3, 2, -10, -11, -11,
          2, -5, 2, 1, 1, 1, -10, -11, -11,
          -5, 4, 3, 1, 1, 1, -10, -11, -11,
          -10, -5, -10, -10, 2, -10, -10, -11, -11,
          -11, -11, -11, -10, -10, -10, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11
        ),
        nrow = 17,
        byrow = TRUE
      ),
      in_cluster = matrix(
        c(
          F, F, F, F, T, T, T, F, F,
          F, F, F, F, T, T, T, T, T,
          F, F, F, F, T, T, T, T, T,
          F, F, F, F, F, T, T, T, T,
          F, F, F, F, F, T, T, T, T,
          F, F, F, T, T, T, T, T, T,
          F, F, F, T, T, T, T, F, F,
          T, T, T, T, T, T, T, F, F,
          T, T, T, T, T, T, T, F, F,
          T, T, T, T, T, T, T, F, F,
          F, F, F, T, T, T, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F
        ),
        nrow = 17,
        byrow = TRUE
      ),
      bornes_mines = c(0, 23),
      possible = rep("NA", 24)
    )
  )
  expect_equal(
    tmp$clusters[[3]][c(1, 3:5)],
    list(
      grid = matrix(
        c(
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -10, -10, -10, -11, -11,
          -11, -11, -11, -11, -10, 1, -10, -11, -11,
          -11, -11, -11, -11, -10, -10, -10, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11
        ),
        nrow = 17,
        byrow = TRUE
      ),
      in_cluster = matrix(
        c(
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, T, T, T, F, F,
          F, F, F, F, T, T, T, F, F,
          F, F, F, F, T, T, T, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F
        ),
        nrow = 17,
        byrow = TRUE
      ),
      bornes_mines = c(0, 8),
      possible = rep("NA", 9)
    )
  )
  expect_equal(
    tmp$clusters[[4]][c(1, 3:5)],
    list(
      grid = matrix(
        c(
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -10, -10,
          -11, -11, -11, -11, -11, -11, -11, -5, 3,
          -11, -11, -11, -11, -11, -11, -11, -10, -10,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11,
          -11, -11, -11, -11, -11, -11, -11, -11, -11
        ),
        nrow = 17,
        byrow = TRUE
      ),
      in_cluster = matrix(
        c(
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, T, T,
          F, F, F, F, F, F, F, T, T,
          F, F, F, F, F, F, F, T, T,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F
        ),
        nrow = 17,
        byrow = TRUE
      ),
      bornes_mines = c(0, 4),
      possible = rep("NA", 5)
    )
  )
})

test_that("independant_clusters fonctionne lors de génération de combinaisons", {
  grid <- matrix(
    c(
      -11, -11, -11, -11, -11, -11, -11, -11, -11,
      -11, -11, -11, -11, -11, -11, -11, -11, -11,
      -11, -11, -11, -11, -11, -11, -11, -11, -11,
      -11, -11, -11, -11, -11, -11, -11, -11, -11,
      -11, -11, -11, -11, -11, -11, -11, -11, -11,
      -11, -11, -11, -11, -11, -11, -11, -11, -11,
      -11, -11, -11, -11, -11, -11, -11, -11, -11,
      -11, -11, -11, -11, -11, -11, -11, -11, -11,
      -11, -11, -11,   3,   2,  -5,  -5, -11, -11,
      -11, -11,  -5,  -5,   3,   4, -10, -10, -10,
      -11, -11,  -5,   4, -10, -10, -10,   2, -10,
      -5,   2,   1,   2, -10, -10, -10, -10, -10,
      2,   3,   2,   2,   2, -10, -11, -11, -11,
      -8,  -9,  -5,  -8, -10, -10, -11, -11, -11,
      -11, -10,   3, -10, -11, -11, -11, -11, -11,
      -11, -10,   2, -10, -11, -11, -11, -11, -11,
      -11, -10, -10, -10, -11, -11, -11, -11, -11
    ),
    nrow = 17,
    ncol = 9,
    byrow = TRUE
  )
  expect_equal(
    independant_clusters(grid, init_solved_around(grid), 7)$clusters |>
      length(),
    2
  )
})
