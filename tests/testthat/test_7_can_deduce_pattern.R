source(here("src/clicker/certain_core.R"))

# see https://minesweeper.online/help/patterns
patterns <- list(
  matrix(
    c(
      0, 0, 1, -1, 
      0, 0, 2, -2, 
      0, 0, 3, -2,
      1, 1, 2, -2,
      -2, -1, -1, -1
    ),
    nrow = 5,
    byrow = TRUE
  ),
  matrix(
    c(
      0, 0, 2, -2,
      0, 0, 3, -2,
      1, 1, 2, -2,
      -2, -1, -1, -1
    ),
    nrow = 4,
    byrow = TRUE
  ),
  matrix(
    c(
      -1, -2, -1, -1, -2,
      1, 1, 1, 1, 1,
      0, 0, 0, 0, 0
    ),
    nrow = 3,
    byrow = TRUE
  ),
  matrix(
    c(
      -2, -1, -2, -1, -2,
      1, 2, 1, 2, 1,
      0, 0, 0, 0, 0
    ),
    nrow = 3,
    byrow = TRUE
  ),
  matrix(
    c(
      -2, -1, -2,
      1, 4, -2,
      0, 2, -2,
      0, 1, -1
    ),
    nrow = 4,
    byrow = TRUE
  ),
  matrix(
    c(
      -2, -1, -2, -1, -2, -2,
      -1, 3, 1, 2, 3, -1,
      -2, 2, 0, 0, 2, -2,
      -2, 2, 0, 0, 2, -2
    ),
    nrow = 4,
    byrow = TRUE
  ),
  matrix(
    c(
      -1, -1, -2, -2, -1, -1,
      0, 1, 2, 2, 1, 0
    ),
    nrow = 2,
    byrow = TRUE
  ),
  matrix(
    c(
      0, 1, -2, -2,
      1, 2, -1, -1,
      -5, 2, -1, -1,
      3, 4, -2, -1,
      -5, -5, 2, -1
    ),
    nrow = 5,
    byrow = TRUE
  ),
  matrix(
    c(
      -2, -1, -1, -1, -1,
      -1, -2, 1, -1, -2,
      1, 1, 1, 1, 1
    ),
    nrow = 3,
    byrow = TRUE
  ),
  matrix(
    c(
      -1, -1, -1, -1, -2,
      -1, -2, 1, -1, -2,
      -1, -1, 2, 1, 1,
      -2, -2, 1, 0, 0,
      -2, -1, 1, 0, 0
    ),
    nrow = 5,
    byrow = TRUE
  ),
  matrix(
    c(
      -1, -1, -2, -1, -2,
      -1, 1, 1, 3, -2,
      -1, 1, 0, 1, -1,
      -2, 1, 0, 1, -1,
      -1, -1, -1, -1, -2
    ),
    nrow = 5,
    byrow = TRUE
  )
)

test_that("can_deduce_pattern peut déduire un pattern simple", {
  grid <- matrix(
    c(
      -1, -1, -1,
      -2, 1, -1,
      -1, 1, -1
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    can_deduce_pattern(grid, NA, grid * 0, FALSE),
    list(c(1, 1), TRUE)
  )
})

test_that("can_deduce_pattern ne donne pas d'action s'il ne sait pas quoi faire", {
  grid <- matrix(
    c(
      -1, -1, -1,
      -2, 1, -1,
      -1, -1, -1
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    can_deduce_pattern(grid, NA, grid * 0, FALSE),
    NULL
  )
})

test_that("can_deduce_pattern ajoute les flags qui sont certains autour des cases", {
  grid <- matrix(
    c(
      -2, -1, -2, -1,
      1, 2, 1, 1
    ),
    nrow = 2,
    byrow = TRUE
  )
  expect_equal(
    can_deduce_pattern(grid, NA, grid * 0, FALSE),
    list(c(1, 1), FALSE)
  )
})

test_that("can_deduce_pattern works for well known minesweeper patterns", {
  res <- mapply(can_deduce_pattern, patterns, NA, sapply(patterns, init_solved_around), FALSE, SIMPLIFY = FALSE)
  expect_true(
    all(!sapply(res, is.null))
  )
})

test_that(
  "can_deduce_pattern sait quand une partie est impossible dans des situations complexes où le nombre de mines importe",
  {
    grid <- matrix(
      c(
        -8, -8, -5, -10, -10,
        1, 3, 3, 4, -10,
        -9, 3, -5, -10, -10,
        -8, 4, -5, 3, 1,
        2, -5, 2, 1, 0,
        -9, 2, 1, 0, 0,
        -8, 1, 0, 0, 0,
        1, 2, 1, 2, 1,
        -9, 3, -5, 3, -5,
        -9, -8, -8, -10, -10
      ),
      ncol = 5,
      byrow = TRUE
    )
    expect_equal(
      can_deduce_pattern(grid, mines_left = 2, grid * 0, hypothesis = TRUE),
      "impossible"
    )
  }
)

test_that("can_deduce_pattern peut déduire un pattern si le nombre total de mines le permet", {
  # situation complexe où il faut connaître le nombre de mines pour pouvoir avancer
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
  solved_around <- grid * 0
  solved_around[c(26:28, 35:38, 45:48)] <- 1
  
  tmp <- can_deduce_pattern(grid, mines_left = 6, solved_around, FALSE)
  
  expect_true(
    grid[tmp[[1]][1], tmp[[1]][2]] == ifelse(tmp[[2]], -1, -2)
  )
  expect_true(
    !(all(tmp[[1]] == c(1, 1)) & tmp[[2]] == FALSE) # ne pas flagguer le premier carré
  )
  expect_false(
    is.list(can_deduce_pattern(grid, mines_left = NA, grid * 0, FALSE))
  )
})

test_that("can_deduce_pattern peut déduire un pattern si le nombre total de mines le permet V3", {
  grid <- matrix(
    c(
      -5, 4, -5, 3, -2, 1,
      -5, -1, -2, -1, -1, 2,
      -5, -1, -2, 2, 2, -5,
      -5, 3, 2, -1, 2, 1,
      2, -1, -1, -2, 3, 1,
      1, -2, 3, -5, -5, 1
    ),
    nrow = 6,
    byrow = TRUE
  )
  tmp <- can_deduce_pattern(grid, 5, grid * 0, FALSE)
  expect_true(
    grid[tmp[[1]][1], tmp[[1]][2]] == ifelse(tmp[[2]], -1, -2)
  )
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
  tmp <- can_deduce_pattern(grid, 6, grid * 0, FALSE)
  expect_true(
    grid[tmp[[1]][1], tmp[[1]][2]] == ifelse(tmp[[2]], -1, -2)
  )
  expect_equal(
    can_deduce_pattern(grid, NA, grid * 0, FALSE),
    NULL
  )
})

test_that("can_deduce_pattern n'a pas de bug de récursion quasi-infinie", {
  grid <- matrix(
    c(
      0, 1, -5, 1, 0, 1, -1, -1, -1,
      0, 1, 1, 1, 1, 2, -2, -1, -2,
      0, 0, 0, 0, 1, -5, 2, 3, -2,
      0, 0, 1, 2, 4, 3, 2, 2, -1,
      0, 0, 2, -5, -5, -5, 2, 1, -2,
      0, 0, 3, -5, -1, -2, -1, -1, -1,
      1, 1, 3, -5, 3, 2, -1, -1, -1,
      2, -5, 2, 1, 1, 1, -2, -1, -2,
      -5, 4, 3, 1, 1, 1, -1, -2, -1,
      -2, -5, -1, -2, 2, -1, -1, -1, -1,
      -1, -2, -1, -1, -2, -1, -2, -1, -1,
      -1, -1, -2, -1, -1, -1, -1, -2, -1,
      -1, -2, -1, -1, -2, -1, -1, -2, -2,
      -1, -2, -2, -1, -1, -1, -1, -2, -2,
      -1, -2, -1, -1, -2, -1, -1, -2, -1,
      -1, -1, -1, -1, -1, -1, -2, -2, -1,
      -2, -1, -1, -1, -1, -2, -1, -1, -1
    ),
    nrow = 17,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  tmp <- can_deduce_pattern(grid, mines_left = 32, solved_around, FALSE, click_order = matrix(c(7, 5), nrow = 1), ori = TRUE)
  expect_true(
    is.list(tmp) && length(tmp) == 2 && is.logical(tmp[[2]])
  )
})

test_that("can_deduce_pattern n'a pas de bug de récursion quasi-infinie avec clusters indépendants", {
  grid <- matrix(
    c(
      0, 1, -5, 1, 0, 1, -1, -1, -1,
      0, 1, 1, 1, 1, 2, -2, -1, -2,
      0, 0, 0, 0, 1, -5, 2, 3, -2,
      0, 0, 1, 2, 4, 3, 2, 2, -1,
      0, 0, 2, -5, -5, -5, 2, 1, -2,
      0, 0, 3, -5, -1, -2, -1, -1, -1,
      1, 1, 3, -5, 3, 2, -1, -1, -1,
      2, -5, 2, 1, 1, 1, -2, -1, -2,
      -5, 4, 3, 1, 1, 1, -1, -2, -1,
      -2, -5, -1, -2, 2, -1, -1, -1, -1,
      -1, -2, -1, -1, -2, -1, -2, -1, -1,
      -1, -1, -2, -1, -1, -1, -1, -2, 3,
      -1, -2, -1, -1, -2, 1, -1, -2, -2,
      -1, -2, -2, -1, -1, -1, -1, -2, -2,
      -1, -2, -1, -1, -2, -1, -1, -2, -1,
      -1, -1, -1, -1, -1, -1, -2, -2, -1,
      -2, -1, -1, -1, -1, -2, -1, -1, -1
    ),
    nrow = 17,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  set.seed(1L)
  tmp <- can_deduce_pattern(grid, mines_left = sum(grid == covered_mine), solved_around, hypothesis = FALSE)
  expect_true(
    is.list(tmp) && length(tmp) == 2 && is.logical(tmp[[2]])
  )
})

test_that("can_deduce_pattern est rapide dans des clusters simples indépendants", {
  grid <- matrix(
    c(
      -1,-1,-2,-1, 2,-2,-1,-1,-2,
      -2,-2,-1,-2,-1,-1,-1,-2,-1,
      -1,-1,-1,-2,-1,-1,-1,-1,-1,
      -1,-2,-1, 2,-1,-2,-1,-2,-1,
      -1,-2,-2,-1,-1,-2,-2,-1,-1,
      -1,-1,-1,-1,-2,-1,-1,-1,-1,
      -1,-1,-1,-2,-2,-1,-2,-2,-2,
      -1,-1,-1,-2,-1,-1,-1,-1,-1,
      -1,-1,-1,-2,-1,-2,-1,-1,-2,
      -1,-1,-1,-1,-2,-2,-1,-1,-1,
      -1,-1,-2,-1,-1,-1,-1,-1,-1,
      -1,-2,-1,-1,-1,-2,-1,-1,-1,
      2,-1,-1,-1,-1,-1,-2,-1,-2,
      -2,-1,-1,-1,-1,-2,-1,-1,-1,
      -2, 3,-1,-1,-1,-1,-1,-2,-1,
      -1,-1,-2,-1,-2,-1,-1,-1,-1,
      -1,-2,-1,-1,-1,-1,-1,-1,-2
    ),
    nrow = 17,
    byrow = TRUE
  )
  a <- Sys.time()
  can_deduce_pattern(grid, 40, init_solved_around(grid), hypothesis = FALSE,
                     click_order = matrix(c(1, 5, 15, 2, 4, 4), ncol = 2, byrow = TRUE))
  b <- Sys.time()
  expect_true(as.numeric(difftime(b, a, units = "secs")) < 6) # secondes
})

test_that("can_deduce_pattern est rapide avec des grilles avancées en résolution", {
  grid <- matrix(
    c(
      0,  1, -2, -1, -1, -2, -5,  1,  0,
      1,  2, -1, -2, -2, -2,  4,  2,  0,
      1, -5,  3,  3, -1, -1, -5,  1,  0,
      1,  1,  3, -5, -1, -2,  4,  3,  1,
      0,  0,  2, -5,  4, -5, -5,  3, -5,
      0,  0,  2,  2,  4,  4, -5,  3,  1,
      0,  0,  1, -5,  2, -5,  3,  2,  0,
      0,  0,  1,  2,  3,  4, -5,  3,  1,
      2,  2,  1,  1, -5,  4, -5, -1, -2,
      -5, -5,  1,  1,  1,  4, -5, -1, -1,
      -2,  3,  1,  1,  1,  3, -5,  3, -2,
      -1,  3,  2,  2, -5,  2,  1,  2, -1,
      -1, -5, -2, -1,  3,  2,  0,  1, -1,
      -2, -1, -2, -2, -5,  3,  2,  3, -2,
      -1, -2,  4, -1, -2, -1, -2, -2, -1,
      1,  1,  2, -2, -1, -1, -1, -1, -1,
      0,  0,  1, -1, -1, -1, -1, -1, -1
    ),
    ncol = 9,
    byrow = TRUE
  )
  click_order <- matrix(
    c(
      15,13,8,10,8,9,6,10,11,11,11,12,12,12,5,6,7,8,10,11,
      9,10,11,12,12,13,13,11,3,6,8,7,6,7,8,6,7,5,4,4,4,3,3,2,3,3,2,1,
      
      3,5,1,3,4,4,4,4,2,3,4,2,3,4,5,5,5,5,5,5,
      6,6,6,6,7,6,7,8,3,6,6,7,8,8,8,9,9,8,7,8,9,8,9,7,4,1,1,1
    ),
    ncol = 2
  )
  solved_around <- init_solved_around(grid)
  solved_around[1:2, 9] <- 0
  a <- Sys.time()
  can_deduce_pattern(grid, 19, solved_around, hypothesis = FALSE, click_order = click_order)
  b <- Sys.time()
  expect_true(as.numeric(difftime(b, a, units = "secs")) < 4) # secondes
})

