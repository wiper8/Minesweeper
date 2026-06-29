library(ggplot2)
library(progress)
source("src/indicies/i_and_positions.R")
source("src/clicker/probabilistic_clicker.R")

show_first_click_probs <- function(n, total_mines, dims, overwrite = TRUE, save = TRUE) {
  filepath_rds <- "data/show_first_click_probs.RDS"
  if (!file.exists(filepath_rds)) set.seed(2026L)

  # positions de clicks initiaux à essayer, sans symétries
  first_click_to_try <- expand.grid(seq_len(ceiling(dims[1] / 2)), seq_len(ceiling(dims[2] / 2))) |> unname() |> as.matrix()
  pb <- progress_bar$new(total = nrow(first_click_to_try), format = "[:bar] :percent eta::eta")
  upper_left <- sapply(
    seq_len(nrow(first_click_to_try)),
    function(k) {
      pb$tick()
      first_click <- first_click_to_try[k, ]
      all_simuls <- replicate(n, simulate_game(total_mines, dims, smart_clicker, first_click = first_click), simplify = FALSE)
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
    value = as.vector(res),
    n = n,
    dims1 = dims[1],
    dims2 = dims[2],
    total_mines = total_mines
  )

  p1 <- ggplot(df, aes(x = col, y = row, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "red", high = "green") +
      scale_y_reverse() +
      coord_fixed() +
      theme_minimal() +
      labs(x = "Column", y = "Row", fill = "Value")

  print(p1)

  if (!save) return(res)
  filepath <- paste0("data/show_first_click_probs_n", n, "_", dims[1], "X", dims[2], "_mines", total_mines, ".png")
  ggsave(filepath, p1)

  new_res <- list(mat = res)
  new_res$inputs <- list(
    n = n,
    total_mines = total_mines,
    dims = dims
  )
  if (!file.exists(filepath_rds)) {
    saveRDS(list(new_res), filepath_rds)
    return(res)
  }
  
  res_old <- readRDS(filepath_rds)
  iden <- sapply(
    res_old,
    function(lst) lst$inputs$total_mines == total_mines && all(lst$inputs$dims == dims)
  )
  if (any(iden)) {
    if (!overwrite) return(res)

    tmp <- res_old[[which(iden)]]
    old_wins <- round(tmp$inputs$n * tmp$mat)

    new_mat <- (old_wins + res * n) / (tmp$inputs$n + n)
    new_res <- list(
      mat = new_mat,
      inputs = list(
        n = tmp$inputs$n + n,
        total_mines = total_mines,
        dims = dims
      )
    )
    res_old[[which(iden)]] <- new_res
    saveRDS(res_old, filepath_rds)
    return(res)
  }
  
  new_list <- append(
    res_old,
    list(new_res)
  )
  saveRDS(new_list, filepath_rds)
  
  res
}

show_box_probs <- function(grid, mines_left) {
  # montre la grille avec toutes les boites non flaguées leur prob d'avoir une mine
  probs_grid_lst <- compute_grid_probabilities(grid, mines_left, init_solved_around(grid), hypothesis = 0)

  dims <- dim(grid)
  probs <- probs_grid_lst$probs

  df <- data.frame(
    row = rep(seq_len(dims[1]), times = dims[2]),
    col = rep(seq_len(dims[2]), each = dims[1]),
    value = as.vector(probs)
  )

  ggplot(df, aes(x = col, y = row, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "green",
      mid = "yellow",
      high = "red",
      midpoint = max(probs, na.rm = TRUE) / 2,
      limits = c(0, max(probs, na.rm = TRUE))
    ) +
    scale_y_reverse() + # ensures probs[1, 1] is upper-left
    coord_fixed() +
    theme_minimal() +
    labs(fill = "Probability")
}

compare_clickers <- function(n, dims, ...) {
  print("random")
  df_random <- cbind(compute_mines_probs_df(n, dims, clicker = random_clicker, ...), clicker = "random",
                     n = n, dims1 = dims[1], dims2 = dims[2])
  print("certain")
  df_certain <- cbind(compute_mines_probs_df(n, dims, clicker = certain_else_random_clicker, ...), clicker = "certain",
                      n = n, dims1 = dims[1], dims2 = dims[2])
  print("smart")
  a <- Sys.time()
  df_smart <- cbind(compute_mines_probs_df(n, dims, clicker = smart_clicker), clicker = "smart",
                    n = n, dims1 = dims[1], dims2 = dims[2])
  b <- Sys.time()
  print(b - a)
  rbind(df_random, df_certain, df_smart)
}

show_mines_difficulty <- function(df, save = TRUE) {
  # TODO difficulé expert
  p1 <- ggplot(df) +
    geom_line(aes(x = total_mines, y = probs, col = clicker)) +
    geom_line(aes(x = total_mines, y = probs, col = clicker)) +
    # geom_line(aes(x = total_mines, y = avg_pct_done, col = clicker), linetype = "dashed") +
    geom_ribbon(aes(x = total_mines, ymin = probs_low, ymax = probs_high, fill = clicker), alpha = 0.2) +
    geom_hline(aes(yintercept = 0.936, col = "beginner"), linetype = "dashed") + # begginner
    geom_hline(aes(yintercept = 0.857, col = "easy"), linetype = "dashed") + # easy
    geom_hline(aes(yintercept = 0.793, col = "intermediate"), linetype = "dashed") + # intermediate
    geom_hline(aes(yintercept = NA, col = "expert"), linetype = "dashed") + # expert
    scale_color_manual(
      name = "Difficulty",
      values = c(
        beginner = "blue",
        easy = "green",
        intermediate = "orange",
        expert = "red"
      )
    )
    # TODO ajouter des seuils visuels d'expert

  print(p1)
  # TODO enregistrer df intelligemment
  if (!save) return(NULL)
  args <- list(n = df$n[1], dims = c(df$dims1[1], df$dims2[1]))
  filepath <- paste0("data/compare_clickers_n", args$n, "_", args$dims[1], "X", args$dims[2], ".png")
  ggsave(filepath, p1)
  NULL
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
    tmp <- compute_probs_success(n, mines[i], dims, ..., show_progress_bar = FALSE, save = FALSE)
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
    tmp <- compute_probs_success(n, mines[i], dims, ..., show_progress_bar = FALSE, save = FALSE)
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

hypothesis_test <- function(n, total_mines, dims = c(17, 9), clicker1, clicker2, alternative = "less") {
  print("evaluating clicker1")
  a <- Sys.time()
  x <- n * compute_probs_success(n, total_mines, dims, clicker1, save = FALSE)[[1]]
  b <- Sys.time()
  print(b - a)
  print("50% done")
  print("evaluating clicker2")
  a <- Sys.time()
  y <- n * compute_probs_success(n, total_mines, dims, clicker2, save = FALSE)[[1]]
  b <- Sys.time()
  print(b - a)
  prop.test(c(x, y), c(n, n), alternative = alternative)
}

prob_interval <- function(success, tries, alpha = 0.05) {
  qbeta(c(alpha / 2, 1 - alpha / 2), success + 1, tries - success + 1)
}
