library(ggplot2)
source("src/indicies/i_and_positions.R")

show_first_click_probs <- function(n, total_mines, dims) {
  # positions de clicks initiaux à essayer, sans symétries
  first_click_to_try <- expand.grid(seq_len(ceiling(dims[1] / 2)), seq_len(ceiling(dims[2] / 2))) |> unname() |> as.matrix()
  upper_left <- sapply(
    seq_len(nrow(first_click_to_try)),
    function(k) {
      first_click <- first_click_to_try[k, ]
      print(paste0(k, " / ", nrow(first_click_to_try)))
      # TODO remplacer par human_clicker
      all_simuls <- replicate(n, simulate_game(total_mines, dims, certain_else_random_clicker, first_click = first_click), simplify = FALSE)
      wins <- mean(sapply(all_simuls, function(lst) lst[[2]] == "win"))
    }
  ) |>
    matrix(nrow = ceiling(dims[1] / 2))

  # Limiter les symétries en ne faisant pas le expand.grid au complet et en propagant les résultats
  # en miroitant les symétries
  left <- upper_left
  right <- upper_left[, seq_len(dims[2] - ncol(upper_left)), drop = FALSE]
  right <- right[, rev(seq_len(ncol(right))), drop = FALSE]
  full_row_block <- cbind(left, right)
  # Mirror vertically (top-bottom)
  top <- full_row_block
  bottom <- full_row_block[seq_len(dims[1] - nrow(upper_left)), , drop = FALSE]
  bottom <- bottom[rev(seq_len(nrow(bottom))), , drop = FALSE]
  res <- rbind(top, bottom)

  # plot
  df <- data.frame(
    row = rep(seq_len(dims[1]), times = dims[2]),
    col = rep(seq_len(dims[2]), each = dims[1]),
    value = as.vector(res)
  )
  print(
    ggplot(df, aes(x = col, y = row, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "red", high = "green") +
      scale_y_reverse() +
      coord_fixed() +
      theme_minimal() +
      labs(x = "Column", y = "Row", fill = "Value")
  )

  res
}

show_box_probs <- function(grid, mines_left) {
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
    geom_line(aes(x = total_mines, y = probs, col = clicker)) +
    geom_line(aes(x = total_mines, y = probs, col = clicker)) +
    geom_line(aes(x = total_mines, y = avg_pct_done, col = clicker), linetype = "dashed") +
    geom_ribbon(aes(x = total_mines, ymin = probs_low, ymax = probs_high, fill = clicker), alpha = 0.2)
  # TODO ajouter des seuils visuels de facile, moyen, difficile, expert en me basant sur les probs de réussite des vraies applications
}

compute_mines_probs_df <- function(n, dims, ...) {
  mines <- seq_len(prod(dims) - 1)
  probs <- rep(NA, length(mines))
  probs_low <- rep(NA, length(mines))
  probs_high <- rep(NA, length(mines))
  avg_pct_done <- rep(NA, length(mines))

  stop_threshold <- 1 / 100
  for (i in seq_along(probs)) {
    print(paste0(i, " mines"))
    tmp <- compute_probs_success(n, mines[i], dims, ...)
    probs[i] <- tmp[[1]]
    probs_low[i] <- tmp[[2]][1]
    probs_high[i] <- tmp[[2]][2]
    avg_pct_done[i] <- tmp[[3]]
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
    avg_pct_done[i] <- tmp[[3]]
    if (probs_high[i] <= stop_threshold) {
      print(paste0("stopped at ", i, " / ", length(probs)))
      break
    }
  }
  
  data.frame(total_mines = mines, probs = probs, probs_low = probs_low, probs_high = probs_high, avg_pct_done = avg_pct_done)
}

hypothesis_test <- function(n, total_mines, dims = c(17, 9), clicker1, clicker2) {
  x <- n * compute_probs_success(n, total_mines, dims, clicker1)[[1]]
  y <- n * compute_probs_success(n, total_mines, dims, clicker2)[[1]]
  prop.test(c(x, y), c(n, n), alternative = "less")
}

prob_interval <- function(success, tries, alpha = 0.05) {
  qbeta(c(alpha / 2, 1 - alpha / 2), success + 1, tries - success + 1)
}
