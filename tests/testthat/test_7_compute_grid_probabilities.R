source("src/clicker/probabilistic_clicker.R")
source("src/game_engine/init_solved_around.R")

compute_true_probs <- function(grid, blind_grid, mines_left, solved_around, exceptions_fun) {
  which_every_combins <- which(!grid %in% known)
  every_combins <- combn(length(which_every_combins), mines_left)
  # filtrage pour accélérer le test
  every_combins <- every_combins[, apply(every_combins, 2, exceptions_fun), drop = FALSE]
  
  possible_grid <- apply(
    every_combins,
    2,
    function(i_to_flag) {
      i_to_flag <- which_every_combins[i_to_flag]
      blind_grid[i_to_flag] <- flag_on_mine
      if (is_mine_propagation_possible(blind_grid, 0, solved_around, to_clusterise = FALSE)$possible) {
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
  true_probs
}

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
  true_probs <- compute_true_probs(grid, blind_grid, mines_left, solved_around, function(x) sum(2:4 %in% x) == 1 &
                                     sum(c(6, 8, 10) %in% x) == 1 &
                                     sum(13:15 %in% x) == 2 &
                                     sum(c(7, 9, 11) %in% x) == 2)

  expect_equal(
    sum(true_probs, na.rm = TRUE),
    mines_left
  )
  expect_equal(
    compute_grid_probabilities(grid, mines_left = mines_left, solved_around)$probs,
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
  true_probs <- compute_true_probs(grid, blind_grid, mines_left, solved_around, function(x) sum(3:5 %in% x) == 1 &
                                     sum(c(8, 11, 14) %in% x) == 1 &
                                     sum(18:20 %in% x) == 2 &
                                     sum(c(9, 12, 15) %in% x) == 2 &
                                     sum(c(11, 14, 17:19) %in% x) == 3)

  expect_equal(
    sum(true_probs, na.rm = TRUE),
    mines_left
  )
  expect_equal(
    compute_grid_probabilities(grid, mines_left = mines_left, solved_around)$probs,
    true_probs
  )
})

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
  # doit fonctionner sans erreur
  certain_core(grid, sum(grid == -2), init_solved_around(grid), hypothesis = 0)
  
  solved_around <- init_solved_around(grid)
  blind_grid <- convert_grid_solution_to_human_grid(grid, solved_around)
  mines_left <- 8
  true_probs <- compute_true_probs(grid, blind_grid, mines_left, solved_around, function(x) sum(4:5 %in% x) == 1 &
      sum(c(9, 16) %in% x) == 1 &
      sum(19:20 %in% x) == 1 &
      sum(c(16, 18, 19) %in% x) == 2)

  expect_equal(
    sum(true_probs, na.rm = TRUE),
    mines_left
  )
  expect_equal(
    compute_grid_probabilities(grid, mines_left = mines_left, solved_around)$probs,
    true_probs
  )
})

test_that("compute_grid_probabilities fait des clusters indépendants pendant la propagation des mines pour accélérer", {
  
  # grille avec 3 clusters, dont 1 qui se sépare facilement en 3 clusters aussi
  grid <- matrix(
    c(
      1, 1, 0, 1, -2, -1, -5, -5, -5, -5,
      -5, 1, 0, 1, -1, -1, 5, -2, -5, -5,
      2, 3, 2, 2, 2, -2, -1, -5, -1, -2,
      -1, -2, -5, -1, -2, -1, -5, -5, -5, -2,
      -1, -1, 5, -2, -5, -5, -5, -5, -5, 4,
      -5, -5, -5, -1, -1, 4, -2, -5, -1, -5
    ),
    ncol = 10,
    byrow = TRUE
  )
  solved_around <- init_solved_around(grid)
  mines_left <- sum(grid == -2)
  clusters <- independant_clusters(grid, solved_around, mines_left)
  clusters <- precise_clusters_bounds_all(grid, solved_around, mines_left, clusters)
  
  # tester d'abord un cas avec un seul cluster initialement
  # il ne faut pas pogner de browser ici
  expect_no_error(
    generate_all_probs(
      clusters$clusters[[1]]$grid,
      6,
      clusters$clusters[[1]]$solved_around,
      in_cluster = clusters$clusters[[1]]$in_cluster
    )
  )


  # puis tester des cas qui se divisent en sous-clusters
  blind_grid <- convert_grid_solution_to_human_grid(grid, solved_around)
  true_probs <- compute_true_probs(grid, blind_grid, mines_left, solved_around, function(x) sum(c(1, 3) %in% x) == 1 &
      sum(3:7 %in% x) == 2 &
      sum(c(3, 5) %in% x) == 1 &
      sum(c(5, 9, 10) %in% x) == 1 &
      sum(c(5, 9, 10, 13:15) %in% x) == 2 &
      sum(c(11, 17) %in% x) == 1 &
      sum(c(20, 22) %in% x) == 1 &
      sum(c(12:14, 16, 18) %in% x) == 2
  )

  expect_equal(
    sum(true_probs, na.rm = TRUE),
    mines_left
  )
  expect_equal(
    compute_grid_probabilities(grid, mines_left = mines_left, solved_around, hypothesis = 0)$probs,
    true_probs
  )
})

test_that("compute_grid_probabilities est rapide pour les clusters avec plusieurs combinaisons", {
  grid <- matrix(
    c(
      -1, -1, -1, -1,
      -2, 3, -1, -2,
      -2, -1, -2, 3,
      -1, -1, -2, -1,
      -2, 4, -2, -1,
      1, -1, -2, -2
    ),
    nrow = 6,
    byrow = TRUE
  )
  
  solved_around <- init_solved_around(grid)
  blind_grid <- convert_grid_solution_to_human_grid(grid, solved_around)
  mines_left <- 9
  true_probs <- compute_true_probs(grid, blind_grid, mines_left, solved_around, function(x) sum(c(5, 9) %in% x) == 1 &
                                     sum(c(1:3, 6:7, 10:12) %in% x) == 3 &
                                     sum(c(4:5, 8:9, 13:15) %in% x) == 4 &
                                     sum(c(11:13, 17:18) %in% x) == 3)
  
  expect_equal(
    sum(true_probs, na.rm = TRUE),
    mines_left
  )
  expect_equal(
    compute_grid_probabilities(grid, mines_left = mines_left, solved_around)$probs,
    true_probs
  )
  debugonce(generate_all_probs)# TODO déboguer le cas avec 8 mines_left dans le cluster
  a <- Sys.time()
  compute_grid_probabilities(
    grid,
    mines_left = mines_left,
    solved_around = solved_around,
    hypothesis = 0,
    click_order = NULL,
    global_cache = NULL,
    clusters_cache = NULL
  )
  b <- Sys.time()
  expect_true(as.numeric(difftime(a, b, unites = "secs")) < 10)
})
