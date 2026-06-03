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
  clusters <- list(
    void = list(
      grid = matrix(-10, nrow = 17, ncol = 9),
      solved_around = matrix(-1, nrow = 17, ncol = 9),
      in_cluster = matrix(
        c(
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,
          TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE,
          TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE,
          TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE
        ),
        nrow = 17,
        ncol = 9,
        byrow = TRUE
      ),
      bornes_mines = c(0, 17),
      possible = rep("NA", 18),
      last_success_mines = NA
    ),
    known_but_does_nothing = list(
      in_cluster = matrix(FALSE, nrow = 17, ncol = 9)
    ),
    clusters = list(
      list(
        grid = matrix(
          c(
            0,  1, -5,  1,  0,  0,  0,  1,  1,
            1,  2,  1,  1,  0,  0,  0,  2, -5,
            -5,  1,  0,  0,  1,  2,  2,  3, -5,
            2,  2,  1,  2,  3, -5, -5,  2,  1,
            -5,  1,  1, -5, -5,  3,  2,  2,  1,
            1,  1,  2,  3,  4,  3,  2,  2, -5,
            1,  2,  2, -5,  2, -5, -5,  4,  2,
            -5,  2, -5,  2,  3,  5, -5, -2, -1,
            1,  3,  3,  3,  2, -5, -5, -10, -10,
            0,  2, -5, -5,  3,  4, -2, -10, -10,
            1,  3, -5,  4, -2, -1, -1, -10, -10,
            -5,  2,  1,  2, -1, -1, -10, -10, -10,
            2,  3,  2,  2,  2, -1, -10, -10, -10,
            -1, -2, -5, -1, -2, -2, -10, -10, -10,
            -10, -1,  3, -2, -1, -10, -10, -10, -10,
            -10, -1,  2,  1, -1, -10, -10, -10, -10,
            -10, -2, -1, -1, -1, -10, -10, -10, -10
          ),
          nrow = 17,
          ncol = 9,
          byrow = TRUE
        ),
        solved_around = matrix(
          c(
            1,  1,  1,  1,  1,  1,  1,  1,  1,
            1,  1,  1,  1,  1,  1,  1,  1,  1,
            1,  1,  1,  1,  1,  1,  1,  1,  1,
            1,  1,  1,  1,  1,  1,  1,  1,  1,
            1,  1,  1,  1,  1,  1,  1,  1,  1,
            1,  1,  1,  1,  1,  1,  1,  1,  1,
            1,  1,  1,  1,  1,  1,  0,  0,  0,
            1,  1,  1,  1,  1,  1,  0,  0,  0,
            1,  1,  1,  1,  1,  0,  0, -1, -1,
            1,  1,  1,  0,  0,  0,  0, -1, -1,
            1,  1,  1,  0,  0,  0,  0, -1, -1,
            1,  1,  1,  0,  0,  0, -1, -1, -1,
            0,  0,  0,  0,  0,  0, -1, -1, -1,
            0,  0,  0,  0,  0,  0, -1, -1, -1,
            -1,  0,  0,  0,  0, -1, -1, -1, -1,
            -1,  0,  0,  0,  0, -1, -1, -1, -1,
            -1,  0,  0,  0,  0, -1, -1, -1, -1
          ),
          nrow = 17,
          ncol = 9,
          byrow = TRUE
        ),
        in_cluster = matrix(
          c(
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
            TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
            FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
            FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
            FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE
          ),
          nrow = 17,
          ncol = 9,
          byrow = TRUE
        ),
        bornes_mines = c(0, 17),
        possible = rep("NA", 18),
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
            -10, -10, -10, -10, -10, -10, -10,  -2,  -1,
            -10, -10, -10, -10, -10, -10, -10,  -2,   3,
            -10, -10, -10, -10, -10, -10, -10,  -1,  -2
          ),
          nrow = 17,
          ncol = 9,
          byrow = TRUE
        ),
        solved_around = matrix(
          c(
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1, -1, -1,
            -1, -1, -1, -1, -1, -1, -1,  0,  0,
            -1, -1, -1, -1, -1, -1, -1,  0,  0,
            -1, -1, -1, -1, -1, -1, -1,  0,  0
          ),
          nrow = 17,
          ncol = 9,
          byrow = TRUE
        ),
        in_cluster = matrix(
          c(
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE
          ),
          nrow = 17,
          ncol = 9,
          byrow = TRUE
        ),
        bornes_mines = c(0, 5),
        possible = rep("NA", 6),
        last_success_mines = NA
      )
    )
  )
  
  tmp <- precise_clusters_bounds_all(grid, solved_around, mines_left = 17, clusters)
  expect_equal(
    tmp$clusters[[2]],
    c(3, 3)
  )
  browser() # TODO ajouter des tests sur clusters[[1]]
})
