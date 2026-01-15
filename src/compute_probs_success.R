source("src/simulate_a_game.R")
source("src/plots.R")

compute_probs_success <- function(n, mines, dims = c(17, 9), clicker) {
  x <- replicate(n, simulate_a_game(mines, dims, clicker))
  list(
    mean = mean(x),
    interval = prob_interval(sum(x), n)
  )
}
