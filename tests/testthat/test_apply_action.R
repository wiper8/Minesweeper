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
    apply_action(base_grid, c(2, 3), TRUE)[[2]],
    -1
  )
})

test_that("la partie est gagnée lorsque je clique sur la dernière boîte restante sans mine", {
  grid <- base_grid
  grid[2, 4] <- -1
  expect_equal(
    apply_action(grid, c(2, 4), TRUE)[[2]],
    1
  )
})

test_that("Ajouter un drapeau fonctionne", {
  tmp <- apply_action(base_grid, c(2, 3), FALSE)
  
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
  tmp <- apply_action(grid, c(2, 2), FALSE)
  grid <- tmp[[1]]
  tmp <- apply_action(grid, c(2, 4), FALSE)
  
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
