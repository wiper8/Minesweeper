source("src/clicker/helper/test_trial.R")

test_that(
  "test_trial_shortcut fonctionne et donne les même résultats que la version non-shortcut pour precise_bounds_one_cluster",
  {
    lst <- list(
      grid = matrix(c(
        -11,-11,-11,-11,-11,-11,-11,-11,-11,
        -11,-11,-11,-11,-11,-11,-11,-11,-11,
        -11,-11,-11,-11,-11,-11,-11,-11,-11,
        -11,-11,-11,-11,-11,-11,-11,-11,-11,
        -11,-11,-11,-11,-11,-11,-11,-11,-11,
        -11,-11,-11,-11,-11,-11,-11,-11,-11,
        -11,-11,-11,-11,-11,-11,-11,-11,-11,
        -11,-11,-11,-11,-11,-11,-11,-11,-11,
        -11,-11,-11,  3,  2, -5, -5,-11,-11,
        -11,-11, -5, -5,  3,  4, -2,-11,-11,
        -11,-11, -5,  4, -2, -1, -1,-11,-11,
        -5,  2,  1,  2, -1, -1,-11,-11,-11,
        2,  3,  2,  2,  2, -1,-11,-11,-11,
        -1, -2, -5, -1, -2, -2,-11,-11,-11,
        -11, -1,  3, -2, -1,-11,-11,-11,-11,
        -11, -1,  2,  1, -1,-11,-11,-11,-11,
        -11, -2, -1, -1, -1,-11,-11,-11,-11
      ), nrow = 17, byrow = TRUE),

      solved_around = matrix(c(
        1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,0,0,-1,-1,
        1,1,1,0,0,0,0,-1,-1,
        1,1,1,0,0,0,0,-1,-1,
        1,1,1,0,0,0,-1,-1,-1,
        0,0,0,0,0,0,-1,-1,-1,
        0,0,0,0,0,0,-1,-1,-1,
        -1,0,0,0,0,-1,-1,1,1,
        -1,0,0,0,0,-1,-1,1,1,
        -1,0,0,0,0,-1,-1,1,1
      ), nrow = 17, byrow = TRUE),

      in_cluster = matrix(c(
        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
        FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
        FALSE,FALSE,FALSE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,
        FALSE,FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,
        FALSE,FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,
        FALSE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,FALSE,
        FALSE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,FALSE,
        FALSE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,FALSE
      ), nrow = 17, byrow = TRUE),
      bornes_mines = c(0, 17),
      possible = c(
        rep("NA", 14),
        "NA", "FALSE", "FALSE", "FALSE"
      )
    )

    grid <- matrix(c(
      0,  1, -5,  1,  0,  0,  0,  1,  1,
      1,  2,  1,  1,  0,  0,  0,  2, -5,
      -5,  1,  0,  0,  1,  2,  2,  3, -5,
      2,  2,  1,  2,  3, -5, -5,  2,  1,
      -5,  1,  1, -5, -5,  3,  2,  2,  1,
      1,  1,  2,  3,  4,  3,  2,  2, -5,
      1,  2,  2, -5,  2, -5, -5,  4,  2,
      -5,  2, -5,  2,  3,  5, -5, -2, -1,
      1,  3,  3,  3,  2, -5, -5, -1, -1,
      0,  2, -5, -5,  3,  4, -2, -1, -1,
      1,  3, -5,  4, -2, -1, -1, -1, -1,
      -5,  2,  1,  2, -1, -1, -2, -1, -1,
      2,  3,  2,  2,  2, -1, -2, -1, -1,
      -1, -2, -5, -1, -2, -2, -1, -1, -2,
      -1, -1,  3, -2, -1, -2, -1, -2, -1,
      -1, -1,  2,  1, -1, -2, -1, -2,  3,
      -1, -2, -1, -1, -1, -2, -1, -1, -2
    ), nrow = 17, byrow = TRUE)

    mines_left <- 17

    # mocking pour test_trial_shortcut qu'il retourne NULL
    original_test_trial <- test_trial
    stub(test_trial, "test_trial_shortcut", NULL)

    a1 <- Sys.time()
    res1 <- precise_bounds_one_cluster(lst, grid, mines_left)
    b1 <- Sys.time()

    # reset le stub
    test_trial <- original_test_trial

    a2 <- Sys.time()
    res2 <- precise_bounds_one_cluster(lst, grid, mines_left)
    b2 <- Sys.time()

    expect_identical(
      res1,
      res2
    )

    # le shortcut sert à accélérer (marge de 10% tolélé si plus lent)
    expect_true(
      as.numeric(difftime(b2, a2, units = "secs")) < as.numeric(difftime(b1, a1, units = "secs")) * 1.1
    )
  }
)

test_that("test_trial fonctionne avec le shortcut", {
  grid <- matrix(c(
    0,  1, -5,  1,  0,  0,  0,  1,  1,
    1,  2,  1,  1,  0,  0,  0,  2, -5,
    -5,  1,  0,  0,  1,  2,  2,  3, -5,
    2,  2,  1,  2,  3, -5, -5,  2,  1,
    -5,  1,  1, -5, -5,  3,  2,  2,  1,
    1,  1,  2,  3,  4,  3,  2,  2, -5,
    1,  2,  2, -5,  2, -5, -5,  4,  2,
    -5,  2, -5,  2,  3,  5, -5, -2, -1,
    1,  3,  3,  3,  2, -5, -5, -1, -1,
    0,  2, -5, -5,  3,  4, -2, -1, -1,
    1,  3, -5,  4, -2, -1, -1, -1, -1,
    -5,  2,  1,  2, -1, -1, -2, -1, -1,
    2,  3,  2,  2,  2, -1, -2, -1, -1,
    -1, -2, -5, -1, -2, -2, -1, -1, -2,
    -1, -1,  3, -2, -1, -2, -1, -2, -1,
    -1, -1,  2,  1, -1, -2, -1, -2,  3,
    -1, -2, -1, -1, -1, -2, -1, -1, -2
  ), nrow = 17, byrow = TRUE)
  in_cluster <- matrix(c(
    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
    FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
    FALSE,FALSE,FALSE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,
    FALSE,FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,
    FALSE,FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,
    FALSE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,FALSE,
    FALSE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,FALSE,
    FALSE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,FALSE
  ), nrow = 17, byrow = TRUE)
  
  expect_false(test_trial(grid, 17, in_cluster, 5))
  expect_false(test_trial(grid, 17, in_cluster, 6))
  expect_true(test_trial(grid, 17, in_cluster, 7))
  expect_true(test_trial(grid, 17, in_cluster, 8))
  expect_false(test_trial(grid, 17, in_cluster, 9))
  expect_false(test_trial(grid, 17, in_cluster, 10))
})
