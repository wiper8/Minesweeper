source("src/clicker/probabilistic_clicker.R")
source("src/game_engine/init_solved_around.R")

test_that("compute_grid_probabilities finds good probabilities sans void", {
  grid <- matrix(
    c(
      -1, -2, -1, -1, -2,
      -1, 2, 1, 3, -2,
      -2, 1, 0, 2, -2,
      -1, 2, 2, 3, -1,
      -1, -1, -2, -2, -1
    ),
    ncol = 5,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  blind_grid <- convert_grid_solution_to_human_grid(grid, solved_around)
  mines_left <- 7
  which_every_combins <- which(!grid %in% known)
  every_combins <- combn(length(which_every_combins), mines_left)
  
  # TODO accélérer le test
  possible_grid <- apply(
    every_combins,
    2,
    function(i_to_flag) {
      i_to_flag <- which_every_combins[i_to_flag]
      blind_grid[i_to_flag] <- flag_on_mine
      if (is_mine_propagation_possible(blind_grid, 0, solved_around, to_clusterise = FALSE)) {
        blind_grid
      } else {
        NULL
      }
    },
    simplify = FALSE
  )
  possible_grid <- possible_grid[!sapply(possible_grid, is.null)]
  possible_grid <- lapply(possible_grid, function(grid) grid %in% known |> matrix(nrow = nrow(grid)))
  true_probs <- Reduce(`+`, possible_grid) / length(possible_grid)
  true_probs[grid %in% known] <- NA
  
  expect_equal(
    sum(true_probs, na.rm = TRUE),
    mines_left
  )
  expect_equal(
    compute_grid_probabilities(grid, mines_left = mines_left, solved_around),
    true_probs
  )
})

test_that("compute_grid_probabilities finds good probabilities avec void présent", {
  grid <- matrix(
    c(
      -1, -1, -2, -1, -1,
      -1, -2, -1, -1, -2,
      -1, 2, 1, 3, -2,
      -2, 1, 0, 2, -2,
      -1, 2, 2, 3, -1,
      -1, -1, -2, -2, -1
    ),
    ncol = 5,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  blind_grid <- convert_grid_solution_to_human_grid(grid, solved_around)
  mines_left <- 8
  which_every_combins <- which(!grid %in% known)
  every_combins <- combn(length(which_every_combins), mines_left)
  # filtrage pour accélérer le test
  every_combins <- every_combins[, apply(
    every_combins,
    2,
    function(x) sum(3:5 %in% x) == 1 &
      sum(c(8, 11, 14) %in% x) == 1 &
      sum(18:20 %in% x) == 2 &
      sum(c(9, 12, 15) %in% x) == 2)
    ,
    drop = FALSE
  ]

  possible_grid <- apply(
    every_combins,
    2,
    function(i_to_flag) {
      i_to_flag <- which_every_combins[i_to_flag]
      blind_grid[i_to_flag] <- flag_on_mine
      if (is_mine_propagation_possible(blind_grid, 0, solved_around, to_clusterise = FALSE)) {
        blind_grid
      } else {
        NULL
      }
    },
    simplify = FALSE
  )
  possible_grid <- possible_grid[!sapply(possible_grid, is.null)]
  possible_grid <- lapply(possible_grid, function(grid) grid %in% known |> matrix(nrow = nrow(grid)))
  true_probs <- Reduce(`+`, possible_grid) / length(possible_grid)
  true_probs[grid %in% known] <- NA

  expect_equal(
    sum(true_probs, na.rm = TRUE),
    mines_left
  )
  expect_equal(
    compute_grid_probabilities(grid, mines_left = mines_left, solved_around),
    true_probs
  )
})

# TODO refaire le même genre de test avec 2 clusters
test_that("compute_grid_probabilities finds good probabilities avec void présent", {
  grid <- matrix(
    c(
      -2, -1, -1, -2, 2,
      -5, -2, -1, -1, -2,
      -1, -1, -2, -1, -5,
      -1, -2, 2, -1, -2,
      2, 2, 1, 3, -5,
      -2, 1, 0, 2, -2,
      -1, 1, 0, 1, -1
    ),
    ncol = 5,
    byrow = TRUE
  )
  certain_core(grid, sum(grid == -2), init_solved_around(grid), hypothesis = 0)

  solved_around <- init_solved_around(grid)
  blind_grid <- convert_grid_solution_to_human_grid(grid, solved_around)
  mines_left <- 8
  which_every_combins <- which(!grid %in% known)
  every_combins <- combn(length(which_every_combins), mines_left)
  # filtrage pour accélérer le test
  # TODO accélérer le test
  every_combins <- every_combins[, apply(
    every_combins,
    2,
    function(x) sum(4:5 %in% x) == 1 &
      sum(c(9, 16) %in% x) == 1 &
      sum(19:20 %in% x) == 1 &
      sum(c(16, 18, 19) %in% x) == 2)
    ,
    drop = FALSE
  ]
  
  possible_grid <- apply(
    every_combins,
    2,
    function(i_to_flag) {
      i_to_flag <- which_every_combins[i_to_flag]
      blind_grid[i_to_flag] <- flag_on_mine
      if (is_mine_propagation_possible(blind_grid, 0, solved_around, to_clusterise = FALSE)) {
        blind_grid
      } else {
        NULL
      }
    },
    simplify = FALSE
  )
  possible_grid <- possible_grid[!sapply(possible_grid, is.null)]
  possible_grid2 <- lapply(possible_grid, function(grid) grid %in% known |> matrix(nrow = nrow(grid)))
  true_probs <- Reduce(`+`, possible_grid2) / length(possible_grid2)
  true_probs[grid %in% known] <- NA

  expect_equal(
    sum(true_probs, na.rm = TRUE),
    mines_left
  )
  debugonce(compute_grid_probabilities)
  expect_equal(
    compute_grid_probabilities(grid, mines_left = mines_left, solved_around),
    true_probs
  )
})
