library(progress)
source("src/simulate_game.R")
source("src/plots.R")

compute_probs_success <- function(n, total_mines, dims = c(17, 9), clicker, show_progress_bar = TRUE) {
  set.seed(2026L)
  if (show_progress_bar) pb <- progress_bar$new(total = n, format = "[:bar] :percent eta::eta")
  all_simuls <- lapply(seq_len(n), function(useless) {
    if (show_progress_bar) pb$tick()
    simulate_game(total_mines, dims, clicker)
  })
  wins <- sapply(all_simuls, function(lst) lst[[2]] == "win")
  pct_done <- sapply(all_simuls, function(lst) mean(lst[[1]] %in% c(0:9, flag_on_mine)))
  list(
    mean = mean(wins),
    interval = prob_interval(sum(wins), n),
    avg_pct_done = mean(pct_done)
  )
}
