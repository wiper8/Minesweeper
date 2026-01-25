library(ggplot2)

show_first_click_probs <- function(mines, dims) {
  # TODO
}

show_box_probs <- function(mines, grid) {
  # montre la grille avec toutes les boites non flaguées leur prob d'avoir une mine'
  # TODO
}

compare_clickers <- function(n, dims, ...) {
  print("random")
  df_random <- cbind(compute_mines_probs_df(n, dims, clicker = random_clicker, ...), clicker = "random")
  print("certain")
  df_certain <- cbind(compute_mines_probs_df(n, dims, clicker = certain_else_random_clicker, ...), clicker = "certain")
  # print("human")
  # df_human <- cbind(compute_mines_probs_df(n, dims, clicker = human_clicker, ...), clicker = "human")
  # df_smart <- cbind(compute_mines_probs_df(n, dims, clicker = smart_clicker), clicker = "smart")
  # df <- rbind(df_random, df_certain, df_human, df_smart)
  rbind(df_random, df_certain) # , df_human)
}

show_mines_difficulty <- function(df) {
  ggplot(df) +
    geom_line(aes(x = mines, y = probs, col = clicker)) +
    geom_line(aes(x = mines, y = probs, col = clicker)) +
    geom_line(aes(x = mines, y = pct_done, col = clicker), linetype = "dashed") +
    geom_ribbon(aes(x = mines, ymin = probs_low, ymax = probs_high, fill = clicker), alpha = 0.2)
  # TODO ajouter des seuils visuels de facile, moyen, difficile, expert en me basant sur les probs de réussite des vraies applications
}

compute_mines_probs_df <- function(n, dims, ...) {
  mines <- seq_len(prod(dims) - 1)
  probs <- rep(NA, length(mines))
  probs_low <- rep(NA, length(mines))
  probs_high <- rep(NA, length(mines))
  pct_done <- rep(NA, length(mines))

  stop_threshold <- 1 / 100
  for (i in seq_along(probs)) {
    print(paste0(i, " mines"))
    tmp <- compute_probs_success(n, mines[i], dims, ...)
    probs[i] <- tmp[[1]]
    probs_low[i] <- tmp[[2]][1]
    probs_high[i] <- tmp[[2]][2]
    pct_done[i] <- tmp[[3]]
    if (probs_high[i] <= stop_threshold) {
      print(paste0("stopped at ", i, " / ", length(probs)))
      break
    }
  }
  for (i in setdiff(rev(seq_along(probs)), which(!is.na(probs)))) {
    tmp <- compute_probs_success(n, mines[i], dims, ...)
    probs[i] <- tmp[[1]]
    probs_low[i] <- tmp[[2]][1]
    probs_high[i] <- tmp[[2]][2]
    pct_done[i] <- tmp[[3]]
    if (probs_high[i] <= stop_threshold) {
      print(paste0("stopped at ", i, " / ", length(probs)))
      break
    }
  }
  
  data.frame(mines = mines, probs = probs, probs_low = probs_low, probs_high = probs_high, pct_done = pct_done)
}

hypothesis_test <- function(n, mines, dims = c(17, 9), clicker1, clicker2) {
  x <- n * compute_probs_success(n, mines, dims, clicker1)[[1]]
  y <- n * compute_probs_success(n, mines, dims, clicker2)[[1]]
  prop.test(c(x, y), c(n, n), alternative = "less")
}

prob_interval <- function(success, tries, alpha = 0.05) {
  qbeta(c(alpha / 2, 1 - alpha / 2), success + 1, tries - success + 1)
}
