source(here("src/game_engine/apply_action.R"))

base_grid <- matrix(
  c(
    1, 2, 2, 1, 0, 0,
    1, -5, -2, -6, 0, 0,
    1, 2, 2, 1, 0, 0
  ),
  nrow = 3,
  byrow = TRUE
)

test_that("la partie est perdue si on clique sur une mine", {
  expect_equal(
    apply_action(base_grid, c(2, 3), TRUE, NA)[[2]],
    -1
  )
})

test_that("la partie est gagnée lorsque je clique sur la dernière boîte restante sans mine", {
  grid <- base_grid
  grid[2, 4] <- -1
  expect_equal(
    apply_action(grid, c(2, 4), TRUE, 1)[[2]],
    1
  )
})

test_that("Ajouter un drapeau fonctionne", {
  tmp <- apply_action(base_grid, c(2, 3), FALSE, 1)
  
  expect_equal(
    tmp[[1]][2, 3],
    -5
  )
  expect_equal(
    tmp[[2]],
    0
  )
})

test_that("Retirer un drapeau fonctionne", {
  grid <- base_grid
  mines_left <- 10
  tmp <- apply_action(grid, c(2, 2), FALSE, mines_left)
  grid <- tmp[[1]]
  mines_left <- tmp[[3]]
  tmp <- apply_action(grid, c(2, 4), FALSE, mines_left)
  mines_left <- tmp[[3]]
  
  expect_equal(
    mines_left,
    12
  )
  expect_equal(
    tmp[[1]][2, 2],
    -2
  )
  expect_equal(
    tmp[[1]][2, 4],
    -1
  )
  expect_equal(
    tmp[[2]],
    0
  )
})

test_that("Erreur dans apply_action si aucun drapeau n'est disponible et qu'on tente d'en ajouter un", {
  grid <- base_grid
  expect_error(
    apply_action(grid, c(2, 3), FALSE, 0)
  )
})

test_that("apply_action ne s'étend pas trop de 0", {
  grid2 <- matrix(
    c(
      -2, -1, -1, -1, -1, -1, -1, -2, -1,
      -2, -2, -1, -2, -1, -1, -1, -1, -1,
      -1, -2, -1, -1, -2, -1, -2, -1, -1,
      -1, -1, -2, -1, -1, -1, -1, -2, -1,
      -1, -2, -1, -1, -2, -3, -1, -2, -2,
      -1, -2, -2, -1, -1, -1, -1, -2, -2,
      -1, -2, -1, -1, -2, -1, -1, -2, -1,
      -1, -1, -1, -1, -1, -1, -2, -2, -1,
      -2, -1, -1, -1, -1, -2, -1, -1, -1
    ),
    ncol = 9,
    byrow = TRUE
  )
  solved_around <- apply_action(grid2, c(5, 6), TRUE, 25, grid2 * 0 - 1)[[4]]
  expect_equal(
    solved_around,
    matrix(
      c(
        rep(-1, 9 * 3),
        -1, -1, -1, -1, 0, 0, 0, -1, -1,
        -1, -1, -1, -1, 0, 0, 0, -1, -1,
        -1, -1, -1, -1, 0, 0, 0, -1, -1,
        rep(-1, 9 * 3)
      ),
      ncol = 9,
      byrow = TRUE
    )
  )
})
