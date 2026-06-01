source(here("src/clicker/certain_core.R"))
source(here("src/game_engine/init_solved_around.R"))

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
    is_mine_propagation_possible(grid, mines_left = 1, init_solved_around(grid))
  )
  grid[2, 2] <- -5
  grid[2, 3] <- -1
  expect_true(
    is_mine_propagation_possible(grid, mines_left = 0, init_solved_around(grid))
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
              is_mine_propagation_possible(grid, mines_left = NA, init_solved_around(grid))
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = 1, init_solved_around(grid))
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = 0, init_solved_around(grid))
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
              is_mine_propagation_possible(grid, mines_left = NA, init_solved_around(grid))
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
              is_mine_propagation_possible(grid, mines_left = 6, init_solved_around(grid))
            )
            # oui c'est une solution sans le total de mines, c'est jusqu'on peut pu jouer, mais la partie est valide
            expect_true(
              is_mine_propagation_possible(grid, mines_left = NA, init_solved_around(grid))
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
              is_mine_propagation_possible(grid, mines_left = NA, init_solved_around(grid))
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = 6, init_solved_around(grid))
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = 5, init_solved_around(grid))
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
              is_mine_propagation_possible(grid, mines_left = NA, init_solved_around(grid))
            )
            expect_true(
              is_mine_propagation_possible(grid, mines_left = 4, init_solved_around(grid))
            )
            expect_false(
              is_mine_propagation_possible(grid, mines_left = 5, init_solved_around(grid))
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
  solved <- init_solved_around(grid)
  expect_true(
    is_mine_propagation_possible(grid, mines_left = 5, solved)
  )
  expect_false(
    is_mine_propagation_possible(grid, mines_left = 6, solved)
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

test_that("is_mine_propagation dans des parties avancées", {
  grid <- matrix(
    c(
      0, 1, -5, 1, 0, 0, 0, 1, -8,
      1, 2, 1, 1, 0, 0, 0, 2, -9,
      -5, 1, 0, 0, 1, 2, 2, 3, -9,
      2, 2, 1, 2, 3, -5, -5, 2, -8,
      -5, 1, 1, -5, -5, 3, 2, 2, -8,
      1, 1, 2, 3, 4, 3, 2, 2, -9,
      1, 2, 2, -5, 2, -5, -5, 4, -8,
      -5, 2, -5, 2, 3, 5, -5, -9, -8,
      1, 3, 3, 3, 2, -5, -5, -10, -10,
      0, 2, -5, -5, 3, 4, -9, -10, -10,
      1, 3, -5, 4, -9, -8, -8, -10, -10,
      -5, 2, 1, 2, -8, -10, -10, -10, -10,
      2, 3, 2, 2, -8, -10, -10, -10, -10,
      -8, -9, -9, -8, -9, rep(-10, 31)
    ),
    nrow = 17,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  expect_true(
    is_mine_propagation_possible(grid, mines_left = 12, solved_around)
  )
})

test_that("is_mine_propagation ne se trompe pas dans des parties avancées", {
  grid <- matrix(
    c(
      0, 1, -5, 1, 0, 1, -10, -8, -8,
      0, 1, 1, 1, 1, 2, -10, -10, -10,
      0 ,0, 0, 0, 1, -5, 2, 3, -10,
      0, 0, 1, 2, 4, 3, 2, 2, -10,
      0, 0, 2, -5, -5, -5, 2, 1, -10,
      0, 0, 3, -5, -8, -9, -10, -10, -10,
      1, 1, 3, -5, 3, 2, -10, -8, -8,
      2, -5, 2, 1, 1, 1, -10, -8, -8,
      -5, 4, 3, 1, 1, 1, -10, -8, -8,
      -10, -5, -10, -10, 2, -10, -8, -8, -8,
      rep(-8, 7 * 9)
    ),
    nrow = 17,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  expect_false(
    is_mine_propagation_possible(grid, mines_left = 10, solved_around, click_order = matrix(c(7, 6, 5, 6), nrow = 2))
  )
})

test_that("is_mine_propagation dans des parties avancées", {
  grid <- matrix(
    c(
      0, 1, -5, 1, 0, 1, -10, -10, -10,
      0, 1, 1, 1, 1, 2, -10, -10, -10,
      0 ,0, 0, 0, 1, -5, 2, 3, -10,
      0, 0, 1, 2, 4, 3, 2, 2, -10,
      0, 0, 2, -5, -5, -5, 2, 1, -10,
      0, 0, 3, -5, -9, -8, -10, -10, -10,
      1, 1, 3, -5, 3, 2, -10, -10, -10,
      2, -5, 2, 1, 1, 1, -10, -10, -10,
      -5, 4, 3, 1, 1, 1, -10, -10, -10,
      -9, -5, -10, -10, 2, -10, -10, -10, -10,
      rep(-10, 7 * 9)
    ),
    nrow = 17,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  expect_false(
    is_mine_propagation_possible(grid, mines_left = 30, solved_around, click_order = matrix(c(7, 6, 5, 6), nrow = 2))
  )
  grid[10] <- -8
  solved_around <- init_solved_around(grid)
  expect_false(
    is_mine_propagation_possible(grid, mines_left = 31, solved_around, click_order = matrix(c(7, 6, 5, 6), nrow = 2))
  )
  grid[10] <- -10
  solved_around <- init_solved_around(grid)
  expect_false(
    is_mine_propagation_possible(grid, mines_left = 31, solved_around, click_order = matrix(c(7, 6, 5, 6), nrow = 2))
  )
})

test_that("is_mine_propagation pour des cas clusters indépendants", {
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
      -10, -10, -10, -10, -10, -10, -10, -10, 3,
      -10, -10, -10, -10, -10, 1, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      -10, -10, -10, -10, -10, -10, -10, -10, -10,
      1, -10, 1, -10, -10, -10, -10, -10, -10
    ),
    nrow = 17,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)

  expect_true(
    is_mine_propagation_possible(grid, mines_left = 31, solved_around, click_order = matrix(c(7, 5), ncol = 2))
  )
})

test_that("is_mine_propagation_possible fonctionne sans perdre la partie", {
  grid <- matrix(
    c(
      -1, -2, -2, -1,
      -2, -2, -2, -2,
      -1, -2, -2, -2,
      -5, -5, -5, -5,
      5, -5, 5, 2,
      -5, -5, 2, 0
    ),
    ncol = 4,
    byrow = TRUE
  )
  expect_no_error(
    is_mine_propagation_possible(
      convert_grid_solution_to_human_grid(grid, init_solved_around(grid)),
      0,
      init_solved_around(grid),
      to_clusterise = FALSE
    )
  )
})

test_that("is_mine_propagation_possible ne retourne pas de faux vrais", {
  grid <- matrix(
    c(
      0, 1, -5, 1, 0, 0, 0, 1, 1,
      1, 2, 1, 1, 0, 0, 0, 2, -5,
      -5, 1, 0, 0, 1, 2, 2, 3, -5,
      2, 2, 1, 2, 3, -5, -5, 2, 1,
      -5, 1, 1, -5, -5, 3, 2, 2, 1,
      1, 1, 2, 3, 4, 3, 2, 2, -5,
      1, 2, 2, -5, 2, -5, -5, 4, 2,
      -5, 2, -5, 2, 3, 5, -5, -10, -10,
      1, 3, 3, 3, 2, -5, -5, -10, -10,
      0, 2, -5, -5, 3, 4, -10, -10, -10,
      1, 3, -5, 4, -9, -8, -10, -10, -10,
      -5, 2, 1, 2, -8, -10, -10, -10, -10,
      2, 3, 2, 2, 2, -10, -10, -10, -10,
      -9, -8, -5, -9, -8, -10, -10, -10, -10,
      rep(-10, 3 * 9)
    ),
    nrow = 17,
    byrow = TRUE
  )
  expect_false(
    is_mine_propagation_possible(grid, mines_left = 2, init_solved_around(grid), to_clusterise = FALSE)
  )
  
  # TODO test à gérer : c'est normal que is_mine_propagation_possible retourne vrai car il peut
  # mettre les mines en trop dans le void. Mais mon but est de forcer que le void n'est pas de mines mises
  # dedans vu que 13 est le nombre pour le cluster en cours
  grid <- matrix(
    c(
      0, 1, -5, 1, 0, 0, 0, 1, 1,
      1, 2, 1, 1, 0, 0, 0, 2, -5,
      -5, 1, 0, 0, 1, 2, 2, 3, -5,
      2, 2, 1, 2, 3, -5, -5, 2, 1,
      -5, 1, 1, -5, -5, 3, 2, 2, 1,
      1, 1, 2, 3, 4, 3, 2, 2, -5,
      1, 2, 2, -5, 2, -5, -5, 4, 2,
      -5, 2, -5, 2, 3, 5, -5, -10, -10,
      1, 3, 3, 3, 2, -5, -5, -10, -10,
      0, 2, -5, -5, 3, 4, -10, -10, -10,
      1, 3, -5, 4, -10, -10, -10, -10, -10,
      -5, 2, 1, 2, -10, -10, -10, -10, -10,
      2, 3, 2, 2, 2, -10, -10, -10, -10,
      -10, -10, -5, -10, -10, -10, -10, -10, -10,
      rep(-10, 3 * 9)
    ),
    nrow = 17,
    byrow = TRUE
  )
  clust <- matrix(
    c(
      rep(T, 9),
      rep(T, 9),
      rep(T, 9),
      rep(T, 9),
      rep(T, 9),
      rep(T, 9),
      rep(T, 9),
      rep(T, 9),
      c(T, T, T, T, T, T, T, F, F),
      c(T, T, T, T, T, T, T, F, F),
      c(T, T, T, T, T, T, T, F, F),
      c(T, T, T, T, T, T, F, F, F),
      c(T, T, T, T, T, T, F, F, F),
      c(T, T, T, T, T, T, F, F, F),
      rep(F, 9),
      rep(F, 9),
      rep(F, 9)
    ),
    nrow = 17,
    byrow = TRUE
  )
  expect_false(
    is_mine_propagation_possible(
      grid,
      12,
      init_solved_around(grid),
      to_clusterise = FALSE,
      cluster = clust
    )
  )
  expect_true(
    is_mine_propagation_possible(
      grid,
      6,
      init_solved_around(grid),
      to_clusterise = FALSE,
      cluster = clust
    )
  )
})
