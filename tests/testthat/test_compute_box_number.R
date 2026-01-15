source("src/game_engine/compute_box_number.R")

grid <- matrix(c(-2, -5, -6, -1, -1, -1, -1, -1, -1), ncol = 3)
test_that("compute_box_number retourne le bon nombre de mines selon l'ordinateur", {
  expect_equal(
    compute_box_number(grid),
    2
  )
})

test_that("compute_box_number retourne le bon nombre de mines selon l'humain", {
  expect_equal(
    compute_box_number(grid, human = TRUE),
    3
  )
})
