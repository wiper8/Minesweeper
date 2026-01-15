source(here("hp.R"))
source(here("src/game_engine/update_grid.R"))

test_that("Selon une grille sans mines, toutes les cases sont révélées", {
  grid <- matrix(covered_no_mine, nrow = 4, ncol = 3)
  
  expect_true(
    sapply(
      1:prod(dim(grid)),
      function(i) {
        grid[i] <- uncovered_no_mine
        all.equal(
          update_grid(grid),
          matrix(0, nrow = nrow(grid), ncol = ncol(grid))
        )
      }
    )
  )
})

test_that("Selon une grille avec 1 mine et des cases à 0, les cases autour des 0 sont révélées", {
  grid <- matrix(covered_no_mine, nrow = 4, ncol = 3)
  grid[3, 3] <- covered_mine
  grid[1, 1] <- uncovered_no_mine # clic
  expect_true(
    all.equal(
      update_grid(grid),
      matrix(
        c(0, 0, 0, 0, 0, 1, 1, 1, 0, 1, uncovered_mine, uncovered_no_mine),
        nrow = 4
      )
    )
  )
})
