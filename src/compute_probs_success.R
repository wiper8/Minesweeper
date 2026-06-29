library(progress)
source("src/simulate_game.R")
source("src/plots.R")

compute_probs_success <- function(n, total_mines, dims = c(17, 9), clicker,
                                  show_progress_bar = TRUE, overwrite = TRUE, save = TRUE) {
  set.seed(2026L)
  if (show_progress_bar) pb <- progress_bar$new(total = n, format = "[:bar] :percent eta::eta")
  all_simuls <- lapply(seq_len(n), function(useless) {
    if (show_progress_bar) pb$tick()
    simulate_game(total_mines, dims, clicker)
  })
  wins <- sapply(all_simuls, function(lst) lst[[2]] == "win")
  pct_done <- sapply(all_simuls, function(lst) mean(lst[[1]] %in% c(0:9, flag_on_mine)))
  res <- list(
    mean = mean(wins),
    interval = prob_interval(sum(wins), n),
    avg_pct_done = mean(pct_done)
  )

  if (!save) return(res)

  filepath <- "data/compute_probs_success.RDS"
  new_res <- res
  new_res$inputs <- list(
    n = n,
    total_mines = total_mines,
    dims = dims
  )
  if (!file.exists(filepath)) {
    saveRDS(list(new_res), filepath)
    return(res)
  }

  res_old <- readRDS(filepath)
  iden <- sapply(
    res_old,
    function(lst) lst$inputs$total_mines == total_mines && all(lst$inputs$dims == dims)
  )
  if (any(iden)) {
    if (!overwrite) return(res)

    tmp <- res_old[[which(iden)]]
    old_wins <- round(tmp$inputs$n * tmp$mean)
    old_wins <- c(rep(TRUE, old_wins), rep(FALSE, tmp$inputs$n - old_wins))
    wins <- c(old_wins, wins)
    pct_done <- (tmp$avg_pct_done * tmp$inputs$n + res$avg_pct_done * n) / (tmp$inputs$n + n)
    n <- tmp$inputs$n + n
    new_res <- list(
      mean = mean(wins),
      interval = prob_interval(sum(wins), n),
      avg_pct_done = mean(pct_done),
      inputs = list(
        n = n,
        total_mines = total_mines,
        dims = dims
      )
    )
    res_old[[which(iden)]] <- new_res
    saveRDS(res_old, filepath)
    return(res)
  }

  new_list <- append(
    res_old,
    list(new_res)
  )
  saveRDS(new_list, filepath)

  res
}
