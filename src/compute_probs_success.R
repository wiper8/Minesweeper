source("src/simulate_game.R")
source("src/plots.R")

compute_probs_success <- function(n, total_mines, dims = c(17, 9), clicker) {
  all_simuls <- replicate(n, simulate_game(total_mines, dims, clicker), simplify = FALSE)
  wins <- sapply(all_simuls, function(lst) lst[[2]] == "win")
  pct_done <- sapply(all_simuls, function(lst) mean(lst[[1]] %in% c(0:9, flag_on_mine)))
  list(
    mean = mean(wins),
    interval = prob_interval(sum(wins), n),
    pct_done = mean(pct_done)
  )
}
