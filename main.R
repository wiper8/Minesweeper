library(profvis)
source("src/simulate_a_game.R")
source("src/compute_probs_success.R")
source("src/plots.R")
source("src/clicker.R")

simulate_a_game(40, c(17, 9), human_clicker, verbose = TRUE)

# profvis(
#   replicate(50, simulate_a_game(40, c(17, 9), certain_else_random_clicker))
# )
# profvis(
#   replicate(50, simulate_a_game(40, c(17, 9), human_clicker))
# )

compute_probs_success(n = 1000, mines = 56, c(17, 9), human_clicker)

# graphique
compare_clickers(n = 200, c(6, 4))
compare_clickers(n = 200, c(17, 9))


hypothesis_test(20, mines = 4, dims = c(10, 8), random_clicker, certain_else_random_clicker)
hypothesis_test(50, mines = 4, dims = c(10, 8), certain_else_random_clicker, human_clicker)

compute_probs_success(n = 100, mines = 7, c(10, 8), human_clicker) # begginner
compute_probs_success(n = 100, mines = 15, c(14, 9), human_clicker) # easy
compute_probs_success(n = 100, mines = 40, c(20, 15), human_clicker) # intermediate
compute_probs_success(n = 100, mines = 99, c(26, 19), human_clicker) # expert
