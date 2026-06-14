source(here("src/clicker/certain_core.R"))

test_that("can_click_all_around clique sur les cases autour qui sont certaines", {
  grid <- matrix(
    c(
      -5, 1, -1,
      1, 1, -1,
     0, 0, -1
    ),
    nrow = 3,
    byrow = TRUE
  )
  res <- can_click_all_around(grid, grid * 0)$clicks
  expect_true(
    isTRUE(all.equal(res, list(list(c(1, 3), TRUE), list(c(2, 3), TRUE), list(c(3, 3), TRUE)))) || 
    isTRUE(all.equal(res, list(list(c(1, 3), TRUE), list(c(2, 3), TRUE))))
  )
  
  grid <- matrix(
    c(
      -5, 1, 0, 2, -2,
      1, 2, 1, 4, -2,
      -1, -1, -5, 3, -2
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    can_click_all_around(grid, grid * 0)$clicks,
    list(list(c(3, 1), TRUE), list(c(3, 2), TRUE))
  )
  
  grid <- matrix(
    c(
      -8, 4,
      2, -5,
      -10, 2
    ),
    nrow = 3,
    byrow = TRUE
  )
  expect_equal(
    can_click_all_around(grid, grid * 0)$clicks,
    NULL
  )
})
