library(cli)
source("src/plots.R")

# en lien avec compare_clickers
get_mines_difficulty <- function(n, dims, pb = NULL, max_ic_width = 0.2, extra_pb = 0, render = FALSE) {
  filepath <- "data/compare_clickers_RDS.RDS"

  if (!file.exists(filepath)) {
    compare_clickers(n = 1, dims = dims, verbose = FALSE)
    return(get_mines_difficulty(n, dims, extra_pb = extra_pb, render = render))
  }
  old <- readRDS(filepath)

  iden <- sapply(
    old,
    function(lst) all(lst$inputs$dims == dims)
  )

  if (all(!iden)) {
    compare_clickers(n = 1, dims = dims, verbose = FALSE)
    return(get_mines_difficulty(n, dims, extra_pb = extra_pb, render = render))
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

  distances <- lapply(mines, function(mine) {
    subset_df <- tmp$df[tmp$df$total_mines == mine, , drop = FALSE]
    # trier
    order_map <- c("random" = 1, "certain" = 2, "smart" = 3)
    subset_df <- subset_df[order(order_map[subset_df$clicker]), ]
    pairs <- combn(seq_len(nrow(subset_df)), 2)
    keep <- apply(pairs, 2, function(id) any(subset_df$n[id] < n))
    if (all(!keep)) return(list(dist = Inf, which = c()))
    distance <- apply(pairs[, keep, drop = FALSE], 2, function(id) {
      c(subset_df$probs_low[id[1]] - subset_df$probs_high[id[2]], subset_df$probs_low[id[2]] - subset_df$probs_high[id[1]])[which.max(subset_df$probs[id])]
    })
    pairs <- pairs[, keep, drop = FALSE]
    keep_pair <- pairs[, which.min(distance)]

    list(dist = min(distance), which = keep_pair[subset_df$n[keep_pair] < n])
  })

  if (extra_pb == 0 && all(sapply(which_to_train_again, length) == 0)) {
    extra_pb <- 1
  }

  closest <- sapply(distances, function(dist_lst) dist_lst$dist)
  if (min(closest) < Inf) {
    # ajouter la paire la plus proche en termes d'intervalles de confiance
    keep <- which.min(closest)
    which_to_train_again[[keep]] <- union(
      which_to_train_again[[keep]],
      distances[[keep]]$which
    )
  }

  # maximum `n` atteint
  if (length(unlist(which_to_train_again)) == 0 || render) {
    tmp$inputs <- NULL
    return(tmp$df)
  }

  if (extra_pb == 1) {
    extra_pb <- 2
    safe_done(pb)
    print("Now in extra iterations. May stop earlier at anytime if desired")
    pb <- cli_progress_bar("Extra iters", total = sum(n - tmp$df$n))
  }

  if (is.null(pb)) pb <- cli_progress_bar(total = max(n - tmp$df$n))

  focus = list(
    random = mines[sapply(which_to_train_again, function(x) any(x == 1))],
    certain = mines[sapply(which_to_train_again, function(x) any(x == 2))],
    smart = mines[sapply(which_to_train_again, function(x) any(x == 3))]
  )
  compare_clickers(n = 1, dims = dims, verbose = FALSE, focus = focus)

  safe_update(pb)
  get_mines_difficulty(n, dims, pb = pb, extra_pb = extra_pb, render = render)
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

safe_done <- function(pb) {
  tryCatch(
    {
      cli_progress_done(id = pb)
    },
    error = function(e) {
      NULL # do nothing?
    }
  )
}
