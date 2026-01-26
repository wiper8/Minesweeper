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
    can_deduce_pattern(grid, NA, grid * 0),
    list(c(1, 1), FALSE)
  )
})

test_that("can_deduce_pattern works for well known minesweeper patterns", {
  res <- mapply(can_deduce_pattern, patterns, NA, sapply(patterns, function(mat) mat * 0), SIMPLIFY = FALSE)
  expect_true(
    all(!sapply(res, is.null))
  )
})

test_that("can_deduce_pattern peut encore déduire des patterns", {
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
    can_deduce_pattern(grid, NA, matrix(0, nrow(grid), ncol(grid))),
    list(c(1, 1), TRUE)
  )
})

test_that("can_deduce_pattern donne du random s'il ne sait pas quoi faire", {
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
    can_deduce_pattern(grid, NA, matrix(0, nrow(grid), ncol(grid))),
    NULL
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
      can_deduce_pattern(grid, total_mines = 12, grid * 0, hypothesis = TRUE),
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
  solved_around[c(26:28, 35:37, 45:47)] <- 1
  
  tmp <- can_deduce_pattern(grid, total_mines = 12, solved_around)
  expect_true(
    is.list(tmp)
  )
  expect_true(
    !(all(tmp[[1]] == c(1, 1)) & tmp[[2]] == FALSE) # ne pas flagguer le premier carré
  )
  expect_false(
    is.list(can_deduce_pattern(grid, total_mines = NA, grid * 0))
  )
})

test_that("can_deduce_pattern peut déduire un pattern si le nombre total de mines le permet V2", {
  # patterns complexe avec combinaisons et nombre de mines total important
  grid <- matrix(
    c(
      -5, 4, -5, 3, -10, 1,
      -5, -10, -10, -10, -10, 2,
      -5, -10, -10, 2, 2, -5,
      -5, 3, 2, -10, 2, 1,
      2, -10, -10, -10, 3, 1,
      1, -10, 3, -5, -5, 1
    ),
    nrow = 6,
    byrow = TRUE
  )
  expect_true(
    is.list(can_deduce_pattern(grid, 14, grid * 0))
  )
  expect_true(
    is.list(can_deduce_pattern(grid, 13, grid * 0))
  )
  expect_equal(
    can_deduce_pattern(grid, NA, grid * 0),
    NULL
  )
})


test_that("can_deduce_pattern peut déduire un pattern si le nombre total de mines le permet V3", {
  grid <- matrix(
    c(
      -5, 4, -5, 3, -5, 1,
      -5, -1, -5, -5, -1, 2,
      -5, -2, -1, 2, 2, -5,
      -5, 3, 2, -1, 2, 1,
      2, -1, -1, -2, 3, 1,
      1, -2, 3, -5, -5, 1
    ),
    nrow = 6,
    byrow = TRUE
  )
  expect_true(
    is.list(can_deduce_pattern(grid, 13, grid * 0))
  )
  expect_true(
    is.list(can_deduce_pattern(grid, 14, grid * 0))
  )
})

test_that("can_deduce_pattern peut déduire un pattern si le nombre total de mines le permet V3", {
  grid <- matrix(
    c(
      -5, 4, -5, 3, -2, 1,
      -5, -1, -2, -2, -1, 2,
      -5, -1, -1, 2, 2, -5,
      -5, 3, 2, -1, 2, 1,
      2, -2, -1, -2, 3, 1,
      1, -1, 3, -5, -5, 1
    ),
    nrow = 6,
    byrow = TRUE
  )
  expect_true(
    is.list(can_deduce_pattern(grid, 13, grid * 0))
  )
  expect_true(
    is.list(can_deduce_pattern(grid, 14, grid * 0))
  )
  expect_equal(
    can_deduce_pattern(grid, NA, grid * 0),
    NULL
  )
})

