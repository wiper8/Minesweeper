library(cli)
source("src/plots.R")

# en lien avec compare_clickers
get_mines_difficulty <- function(n, dims, pb = NULL, max_ic_width = 0.2) {
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

  mines <- sort(unique(tmp$df$total_mines))
  which_to_train_again <- lapply(mines, function(mine) {
    subset_df <- tmp$df[tmp$df$total_mines == mine, , drop = FALSE]
    # trier
    order_map <- c("random" = 1, "certain" = 2, "smart" = 3)
    subset_df <- subset_df[order(order_map[subset_df$clicker]), ]
    pairs <- combn(seq_len(nrow(subset_df)), 2)
    overlaps <- apply(pairs, 2, function(id) !(subset_df$probs_high[id[1]] <= subset_df$probs_low[id[2]] || subset_df$probs_low[id[1]] >= subset_df$probs_high[id[2]]))
    overlap_pairs <- unique(as.vector(pairs[, which(overlaps)]))

    large <- which(subset_df$probs_high - subset_df$probs_low > max_ic_width)
    exclude <- which(subset_df$n >= n)

    # overlap ou interval de confiance large, mais on exclut lorsque n est atteint
    setdiff(c(overlap_pairs, large), exclude)
  })

  # maximum `n` atteint
  if (all(mapply(
    function(m, c) {
      !switch(
        c,
        "random" = 1,
        "certain" = 2,
        "smart" = 3
      ) %in% which_to_train_again[[which(mines == m)]]
    },
    tmp$df$total_mines,
    tmp$df$clicker
  ))) {
    tmp$inputs <- NULL
    return(tmp$df)
  }

  if (is.null(pb)) pb <- cli_progress_bar(total = max(n - tmp$df$n))

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
