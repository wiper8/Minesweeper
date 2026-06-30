library(cli)
library(ggplot2)
source("src/simulate_game.R")
source("src/indicies/i_and_positions.R")
source("src/clicker/smart_clicker.R")
source("src/clicker/random_clicker.R")
source("src/clicker/certain_else_random_clicker.R")
source("src/clicker/probabilistic_clicker.R")

first_click_probs <- function(n, total_mines, dims, overwrite = TRUE, save = TRUE) {
  filepath_rds <- "data/first_click_probs_RDS.RDS"
  if (!file.exists(filepath_rds) && save) set.seed(2026L)
  
  # positions de clicks initiaux à essayer, sans symétries
  first_click_to_try <- expand.grid(seq_len(ceiling(dims[1] / 2)), seq_len(ceiling(dims[2] / 2))) |> unname() |> as.matrix()
  upper_left <- sapply(
    seq_len(nrow(first_click_to_try)),
    function(k) {
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
  
  if (!save) return(list(mat = res, n = n))

  new_res <- list(mat = res)
  new_res$inputs <- list(
    n = n,
    total_mines = total_mines,
    dims = dims
  )
  if (!file.exists(filepath_rds)) {
    saveRDS(list(new_res), filepath_rds)
    return(list(mat = res, n = n))
  }
  
  res_old <- readRDS(filepath_rds)
  iden <- sapply(
    res_old,
    function(lst) lst$inputs$total_mines == total_mines && all(lst$inputs$dims == dims)
  )
  if (any(iden)) {
    if (!overwrite) return(list(mat = res, n = n))

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
    return(list(mat = new_res$mat, n = new_res$inputs$n))
  }
  
  new_list <- append(
    res_old,
    list(new_res)
  )
  saveRDS(new_list, filepath_rds)

  return(list(mat = res, n = n))
}

show_first_click_probs <- function(res) {
  dims <- dim(res$mat)
  df <- data.frame(
    row = rep(seq_len(dims[1]), times = dims[2]),
    col = rep(seq_len(dims[2]), each = dims[1]),
    value = as.vector(res$mat)
  )
  n <- res$n

  success <- df$value * n
  intervals <- mapply(prob_interval, success, n)

  print(ggplot(df, aes(x = col, y = row, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "red", mid = "yellow", high = "green",
        midpoint = mean(range(intervals)),
        limits = range(intervals)
      ) +
      scale_y_reverse() +
      coord_fixed() +
      theme_minimal() +
      labs(x = "Column", y = "Row", fill = "Value"))
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
  browser()
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

compare_clickers <- function(n, dims, overwrite = TRUE, save = TRUE, verbose = TRUE, ...) {
  filepath <- "data/compare_clickers_RDS.RDS"
  if (!file.exists(filepath) && save) set.seed(2026L)
  
  if (verbose) message("random")
  df_random <- cbind(compute_mines_probs_df(n, dims, clicker = random_clicker, verbose = verbose, ...), clicker = "random")
  if (verbose) message("certain")
  df_certain <- cbind(compute_mines_probs_df(n, dims, clicker = certain_else_random_clicker, verbose = verbose, ...), clicker = "certain")
  if (verbose) message("smart")
  df_smart <- cbind(compute_mines_probs_df(n, dims, clicker = smart_clicker, verbose = verbose, ...), clicker = "smart")

  res <- rbind(df_random, df_certain, df_smart)

  if (!save) return(res)

  new_res <- list(df = res)
  new_res$inputs <- list(
    n = n,
    dims = dims
  )
  if (!file.exists(filepath)) {
    saveRDS(list(new_res), filepath)
    return(res)
  }

  res_old <- readRDS(filepath)
  iden <- sapply(
    res_old,
    function(lst) all(lst$inputs$dims == dims)
  )

  if (any(iden)) {
    if (!overwrite) return(res)

    tmp <- res_old[[which(iden)]]
    
    old_wins <- round(tmp$inputs$n * tmp$df$probs)

    new_df <- res

    new_n <- tmp$inputs$n + n
    new_df$avg_pct_done <- (tmp$df$avg_pct_done * tmp$inputs$n + res$avg_pct_done * n) / new_n
    new_df$probs <- (old_wins + res$probs * n) / new_n
    intervals <- mapply(prob_interval, old_wins + res$probs * n, new_n, SIMPLIFY = FALSE)
    new_df$probs_low <- sapply(intervals, `[`, 1)
    new_df$probs_high <- sapply(intervals, `[`, 2)

    new_df$probs[is.na(new_df$probs)] <- res$probs
    new_df$probs_low[is.na(new_df$probs_low)] <- res$probs_low
    new_df$probs_high[is.na(new_df$probs_high)] <- res$probs_high
    new_df$avg_pct_done[is.na(new_df$avg_pct_done)] <- res$avg_pct_done

    new_res <- list(
      df = new_df,
      inputs = list(
        n = new_n,
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

show_mines_difficulty <- function(df) {
  print(ggplot(df) +
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
    ))
    # TODO ajouter des seuils visuels d'expert
}

compute_mines_probs_df <- function(n, dims, verbose = TRUE, ...) {
  mines <- seq_len(prod(dims) - 1)
  probs <- rep(NA, length(mines))
  probs_low <- rep(NA, length(mines))
  probs_high <- rep(NA, length(mines))
  avg_pct_done <- rep(NA, length(mines))

  stop_threshold <- 1 / 100
  for (i in seq_along(probs)) {
    if (verbose) message(paste0(i, " mines"))
    tmp <- compute_probs_success(n, mines[i], dims, ..., show_progress_bar = FALSE, save = FALSE)
    probs[i] <- tmp[[1]]
    probs_low[i] <- tmp[[2]][1]
    probs_high[i] <- tmp[[2]][2]
    avg_pct_done[i] <- tmp[[3]]
    if (probs_high[i] <= stop_threshold) {
      if (verbose) message(paste0("stopped at ", i, " / ", length(probs)))
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
      if (verbose) message(paste0("stopped at ", i, " / ", length(probs)))
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

compute_probs_success <- function(n, total_mines, dims = c(17, 9), clicker,
                                  show_progress_bar = TRUE, overwrite = TRUE, save = TRUE) {
  filepath <- "data/compute_probs_success_RDS.RDS"
  if (!file.exists(filepath) && save) set.seed(2026L)
  itr <- seq_len(n)
  all_simuls <- lapply(
    if (show_progress_bar) cli_progress_along(itr) else itr,
    function(useless) {
      simulate_game(total_mines, dims, clicker)
    }
  )
  wins <- sapply(all_simuls, function(lst) lst[[2]] == "win")
  pct_done <- sapply(all_simuls, function(lst) mean(lst[[1]] %in% c(0:9, flag_on_mine)))
  res <- list(
    mean = mean(wins),
    interval = prob_interval(sum(wins), n),
    avg_pct_done = mean(pct_done)
  )
  
  if (!save) return(res)
  
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

