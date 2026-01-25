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
      1, 2, 1, 1, 1,
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
      -1, -1, -1, -1, -1
    ),
    nrow = 5,
    byrow = TRUE
  ),
  # situation complexe où il faut connaître le nombre de mines pour pouvoir avancer
  matrix(
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
  ),
  # patterns complexe avec combinaisons et nombre de mines total important
  matrix(
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
    can_deduce_pattern(grid, matrix(0, nrow(grid), ncol(grid))),
    list(c(1, 1), FALSE)
  )
})

test_that("can_deduce_pattern works for well known minesweeper patterns", {
  res <- mapply(can_deduce_pattern, patterns, sapply(patterns, function(mat) mat * 0), SIMPLIFY = FALSE)
  expect_true(
    all(!sapply(res, is.null))
  )
})
