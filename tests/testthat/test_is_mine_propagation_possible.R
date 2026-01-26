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
    is_mine_propagation_possible(grid, total_mines = 1, matrix(0, nrow(grid), ncol(grid)))
  )
  grid[2, 2] <- -5
  grid[2, 3] <- -1
  expect_true(
    is_mine_propagation_possible(grid, total_mines = 1, matrix(0, nrow(grid), ncol(grid)))
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
              is_mine_propagation_possible(grid, total_mines = NA, matrix(0, nrow(grid), ncol(grid)))
            )
            grid[1] <- -5
            expect_false(
              is_mine_propagation_possible(grid, total_mines = 3, matrix(0, nrow(grid), ncol(grid)))
            )
            expect_true(
              is_mine_propagation_possible(grid, total_mines = 4, matrix(0, nrow(grid), ncol(grid)))
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
              is_mine_propagation_possible(grid, total_mines = NA, grid * 0)
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
              is_mine_propagation_possible(grid, total_mines = 12, matrix(0, nrow(grid), ncol(grid)))
            )
            # oui c'est une solution sans le total de mines, c'est jusqu'on peut pu jouer, mais la partie est valide
            expect_true(
              is_mine_propagation_possible(grid, total_mines = NA, matrix(0, nrow(grid), ncol(grid)))
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
              is_mine_propagation_possible(grid, total_mines = NA, matrix(0, nrow(grid), ncol(grid)))
            )
            expect_true(
              is_mine_propagation_possible(grid, total_mines = 14, matrix(0, nrow(grid), ncol(grid)))
            )
            expect_true(
              is_mine_propagation_possible(grid, total_mines = 13, matrix(0, nrow(grid), ncol(grid)))
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
              is_mine_propagation_possible(grid, total_mines = NA, matrix(0, nrow(grid), ncol(grid)))
            )
            expect_true(
              is_mine_propagation_possible(grid, total_mines = 13, matrix(0, nrow(grid), ncol(grid)))
            )
            expect_false(
              is_mine_propagation_possible(grid, total_mines = 14, matrix(0, nrow(grid), ncol(grid)))
            )
          }
)

test_that("exemples supplémentaires de cas pour is_mine_propagation_possible", {
  grid <- matrix(
    c(
      -2, -1, -2, -1,
      1, 2, 1, 1
    ),
    nrow = 2,
    byrow = TRUE
  )
  pos_unknown <- matrix(c(1, 1, 1, 1, 2, 3), ncol = 2)
  solved_around <- matrix(0, nrow = 2, ncol = 4)
  expect_true(
    which_combins_possible(
      grid,
      matrix(c(1, 3), nrow = 2),
      pos_unknown,
      solved_around = solved_around,
      total_mines = NA
    )
  )
  expect_false(
    which_combins_possible(
      grid,
      matrix(c(1, 2), nrow = 2),
      pos_unknown,
      solved_around = solved_around,
      total_mines = NA
    )
  )
  expect_false(
    which_combins_possible(
      grid,
      matrix(c(2, 3), nrow = 2),
      pos_unknown,
      solved_around = solved_around,
      total_mines = NA
    )
  )
})

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
    is_mine_propagation_possible(grid, total_mines = NA, grid * 0)
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
    is_mine_propagation_possible(grid, total_mines = 12, grid * 0)
  )
})


