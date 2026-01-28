source(here("src/clicker/certain_core.R"))

test_that("is_mine_propagation_possible fonctionne généralement", {
  grid <- matrix(
    c(
      1, 1, 1, -1,
      -1, -2, -5, -1
    ),
    nrow = 2,
    byrow = TRUE
  )
  expect_false(
    is_mine_propagation_possible(grid, mines_left = 1, matrix(0, nrow(grid), ncol(grid)))
  )
  grid[2, 2] <- -5
  grid[2, 3] <- -1
  expect_true(
    is_mine_propagation_possible(grid, mines_left = 0, matrix(0, nrow(grid), ncol(grid)))
  )
})

test_that("is_mine_propagation_possible fonctionne pour un pattern possible de mines, mais pas le bon nombre de mines
          total dans la grille",{
            grid <- matrix(
              c(
                -2, -5,
                -5, -5
              ),
              nrow = 2,
              byrow = TRUE
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = NA, matrix(0, nrow(grid), ncol(grid)))
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = 1, matrix(0, nrow(grid), ncol(grid)))
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = 0, matrix(0, nrow(grid), ncol(grid)))
            )
            
            # situation complexe où il faut connaître le nombre de mines pour pouvoir avancer
            grid <- matrix(
              c(
                -8, -8, -5, -10, -10,
                1, 3, 3, 4, -10,
                -9, 3, -5, -10, -10,
                -10, 4, -5, 3, 1,
                2, -5, 2, 1, 0,
                -10, 2, 1, 0, 0,
                -10, 1, 0, 0, 0,
                1, 2, 1, 2, 1,
                -10, 3, -5, 3, -5,
                -10, -10, -10, -10, -10
              ),
              nrow = 10,
              byrow = TRUE
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = NA, grid * 0)
            )
            
            grid <- matrix(
              c(
                -1, -1, -5, -2, -1,
                1, 3, 3, 4, -1,
                -2, 3, -5, -1, -2,
                -1, 4, -5, 3, 1,
                2, -5, 2, 1, 0,
                -2, 2, 1, 0, 0,
                -1, 1, 0, 0, 0,
                1, 2, 1, 2, 1,
                -2, 3, -5, 3, -5,
                -1, -1, -2, -1, -1
              ),
              nrow = 10,
              byrow = TRUE
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = 6, matrix(0, nrow(grid), ncol(grid)))
            )
            # oui c'est une solution sans le total de mines, c'est jusqu'on peut pu jouer, mais la partie est valide
            expect_true(
              is_mine_propagation_possible(grid, mines_left = NA, matrix(0, nrow(grid), ncol(grid)))
            )
            
            # patterns complexe avec combinaisons et nombre de mines total important
            grid <- matrix(
              c(
                -5, 4, -5, 3, -2, 1,
                -5, -2, -1, -2, -1, 2,
                -5, -1, -2, 2, 2, -5,
                -5, 3, 2, -1, 2, 1,
                2, -1, -1, -2, 3, 1,
                1, -2, 3, -5, -5, 1
              ),
              nrow = 6,
              byrow = TRUE
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = NA, matrix(0, nrow(grid), ncol(grid)))
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = 6, matrix(0, nrow(grid), ncol(grid)))
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = 5, matrix(0, nrow(grid), ncol(grid)))
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
            expect_true(
              is_mine_propagation_possible(grid, mines_left = NA, matrix(0, nrow(grid), ncol(grid)))
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = 5, matrix(0, nrow(grid), ncol(grid)))
            )
            expect_false(
              is_mine_propagation_possible(grid, mines_left = 6, matrix(0, nrow(grid), ncol(grid)))
            )
          }
)

test_that("is_mine_propagation_possible avec des boîtes inconnues", {
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
      -10, 3, -5, 3, -5,
      -10, -10, -8, -9, -8
    ),
    ncol = 5,
    byrow = TRUE
  )
  expect_true(
    is_mine_propagation_possible(grid, mines_left = NA, grid * 0)
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
  expect_false(
    is_mine_propagation_possible(grid, mines_left = 6, grid * 0)
  )
})

test_that("is_mine_propagation est faux avec une grille possible mais pas le bon nombre de mines", {
  grid <- matrix(
    c(
      -5, 4, -5, 3, -10, 1,
      -5, -8, -9, -10, -9, 2,
      -5, -8, -8, 2, 2, -5,
      -5, 3, 2, -8, 2, 1,
      2, -9, -8, -9, 3, 1,
      1, -8, 3, -5, -5, 1
    ),
    nrow = 6,
    byrow = TRUE
  )
  tmp <- is_mine_propagation_possible(grid, mines_left = 1, solved_around = grid * 0)
  expect_false(
    tmp
  )
  expect_no_error(
    tmp
  )
})
