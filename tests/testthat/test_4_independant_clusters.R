source(here("src/clicker/is_mine_propagation_possible.R"))

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
  expect_equal(
    independant_clusters(grid, solved_around, mines_left = 5),
    list(
      void = list(
        grid = grid * 0 - 10,
        solved_around = grid * 0 - 1,
        in_cluster = matrix(FALSE, nrow = nrow(grid), ncol = ncol(grid)),
        bornes_mines = c(0, 5),
        possible = rep("NA", 6),
        last_success_mines = NA
      ),
      known_but_does_nothing = list(
        in_cluster = matrix(FALSE, nrow = nrow(grid), ncol = ncol(grid))
      ),
      clusters = list(
        list(
          grid = grid,
          solved_around = solved_around,
          in_cluster = matrix(TRUE, nrow = nrow(grid), ncol = ncol(grid)),
          bornes_mines = c(0, 5),
          possible = rep("NA", 6),
          last_success_mines = NA
        )
      )
    )
  )
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
  expect_equal(
    independant_clusters(grid, solved_around, mines_left = 32),
    list(
      void = list(
        grid = grid * 0 - 10,
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
        possible = rep("NA", 33),
        last_success_mines = NA
      ),
      known_but_does_nothing = list(
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
            T, F, F, F, F, F, F, F, F,
            F, F, F, F, F, F, F, F, F,
            F, F, F, F, F, F, F, F, F,
            F, F, F, F, F, F, F, F, F,
            F, F, F, F, F, F, F, F, T
          ),
          nrow = 17,
          byrow = TRUE
        )
      ),
      clusters = list(
        list(
          grid = matrix(
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
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10
            ),
            nrow = 17,
            byrow = TRUE
          ),
          solved_around = matrix(
            c(
              1, 1, 1, 1, 1, 0, 0, -1, -1,
              1, 1, 1, 1, 1, 0, 0, 0, 0,
              1, 1, 1, 1, 1, 0, 0, 0, 0,
              1, 1, 1, 1, 1, 1, 1, 0, 0,
              1, 1, 1, 0, 0, 0, 0, 0, 0,
              1, 1, 1, 0, 0, 0, 0, 0, 0,
              1, 1, 1, 0, 0, 0, 0, -1, -1,
              1, 1, 1, 1, 1, 0, 0, -1, -1,
              0, 0, 0, 0, 0, 0, 0, -1, -1,
              0, 0, 0, 0, 0, 0, 0, -1, -1,
              -1, -1, -1, 0, 0, 0, -1, -1, -1,
              rep(c(-1, -1, -1, -1, -1, -1, -1, -1, -1), 6)
            ),
            nrow = 17,
            byrow = TRUE
          ),
          in_cluster = matrix(
            c(
              T, T, T, T, T, T, T, F, F,
              T, T, T, T, T, T, T, T, T,
              T, T, T, T, T, T, T, T, T,
              T, T, T, T, T, T, T, T, T,
              T, T, T, T, T, T, T, T, T,
              T, T, T, T, T, T, T, T, T,
              T, T, T, T, T, T, T, F, F,
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
          possible = rep("NA", 24),
          last_success_mines = NA
        ),
        list(
          grid = matrix(
            c(
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              1, -10, 1, -10, -10, -10, -10, -10, -10
            ),
            nrow = 17,
            byrow = TRUE
          ),
          solved_around = matrix(
            c(
              rep(-1, 9 * 15),
              c(0, 0, 0, 0, rep(-1, 5)),
              c(0, 0, 0, 0, rep(-1, 5))
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
          possible = rep("NA", 7),
          last_success_mines = NA
        ),
        list(
          grid = matrix(
            c(
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, 1, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10
            ),
            nrow = 17,
            byrow = TRUE
          ),
          solved_around = matrix(
            c(
              rep(-1, 9 * 11),
              c(-1, -1, -1, -1, 0, 0, 0, -1, -1),
              c(-1, -1, -1, -1, 0, 0, 0, -1, -1),
              c(-1, -1, -1, -1, 0, 0, 0, -1, -1),
              rep(-1, 9 * 3)
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
          possible = rep("NA", 9),
          last_success_mines = NA
        ),
        list(
          grid = matrix(
            c(
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -5, 3,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10,
              -10, -10, -10, -10, -10, -10, -10, -10, -10
            ),
            nrow = 17,
            byrow = TRUE
          ),
          solved_around = matrix(
            c(
              rep(-1, 9 * 10),
              c(-1, -1, -1, -1, -1, -1, -1, 0, 0),
              c(-1, -1, -1, -1, -1, -1, -1, 0, 0),
              c(-1, -1, -1, -1, -1, -1, -1, 0, 0),
              rep(-1, 9 * 4)
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
          possible = rep("NA", 5),
          last_success_mines = NA
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
