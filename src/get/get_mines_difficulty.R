library(cli)
source("src/plots.R")

# en lien avec compare_clickers
get_mines_difficulty <- function(n, dims, pb = NULL) {
  filepath <- "data/compare_clickers_RDS.RDS"

  if (!file.exists(filepath)) {
    compare_clickers(n = 1, dims = dims, verbose = FALSE)
    return(get_mines_difficulty(n, dims))
  }
  old <- readRDS(filepath)

  iden <- sapply(
    old,
    function(lst) all(lst$inputs$dims == dims)
  )

  if (all(!iden)) {
    compare_clickers(n = 1, dims = dims, verbose = FALSE)
    return(get_mines_difficulty(n, dims))
  }

  tmp <- old[[which(iden)]]

  # maximum `n` atteint
  if (all(tmp$inputs$n >= n)) {
    tmp$inputs <- NULL
    return(tmp$df)
  }

  missing_n <- ceiling(n - tmp$inputs$n)

  if (missing_n <= 0) {
    cli_progress_done(id = pb)
    tmp$inputs <- NULL
    return(tmp$df)
  }

  if (is.null(pb)) pb <- cli_progress_bar(total = missing_n)

  mines <- sort(unique(tmp$df$total_mines))
  which_to_train_again <- lapply(mines, function(mine) {
    subset_df <- tmp$df[tmp$df$total_mines == mine, , drop = FALSE]
    pairs <- combn(seq_len(nrow(subset_df)), 2)
    overlaps <- apply(pairs, 2, function(id) !(subset_df$probs_high[id[1]] <= subset_df$probs_low[id[2]] || subset_df$probs_low[id[1]] >= subset_df$probs_high[id[2]]))
    overlap_pairs <- pairs[, which(overlaps)]
    unique(as.vector(overlap_pairs))
  })
  focus = list(
    random = mines[sapply(which_to_train_again, function(x) any(x == 1))],
    certain = mines[sapply(which_to_train_again, function(x) any(x == 2))],
    smart = mines[sapply(which_to_train_again, function(x) any(x == 3))]
  )
  compare_clickers(n = 1, dims = dims, verbose = FALSE, focus = focus)
  
  
  safe_update(pb)
  get_mines_difficulty(n, dims, pb = pb)
}

safe_update <- function(pb) {
  tryCatch(
    {
      cli_progress_update(id = pb)
    },
    error = function(e) {
      NULL # do nothing?
    }
  )
}
