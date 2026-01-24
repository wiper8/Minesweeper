source("src/simulate_game.R")
source("src/plots.R")

compute_probs_success <- function(n, mines, dims = c(17, 9), clicker) {
  all_simuls <- replicate(n, simulate_game(mines, dims, clicker), simplify = FALSE)
  wins <- sapply(all_simuls, function(lst) lst[[2]] == "win")
  list(
    mean = mean(wins),
    interval = prob_interval(sum(wins), n)
  )
}
