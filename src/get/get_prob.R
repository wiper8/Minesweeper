library(cli)
source("src/plots.R")

# en lien avec compute_probs_success
get_prob <- function(n, total_mines, dims = c(17, 9)) {
  filepath <- "data/compute_probs_success_RDS.RDS"

  if (!file.exists(filepath)) {
    cli_progress_bar(total = n)
    for (i in seq_len(n)) {
      compute_probs_success(n = 1, total_mines = total_mines, dims = dims, clicker = smart_clicker)
      cli_progress_update()
    }
    cli_progress_done()
    return(get_prob(n, total_mines, dims))
  }
  old <- readRDS(filepath)

  iden <- sapply(
    old,
    function(lst) lst$inputs$total_mines == total_mines && all(lst$inputs$dims == dims)
  )
  if (all(!iden)) {
    cli_progress_bar(total = n)
    for (i in seq_len(n)) {
      compute_probs_success(n = 1, total_mines = total_mines, dims = dims, clicker = smart_clicker)
      cli_progress_update()
    }
    cli_progress_done()
    return(get_prob(n, total_mines, dims))
  }
  tmp <- old[[which(iden)]]
  if (tmp$inputs$n >= n) {
    tmp$inputs <- NULL
    return(tmp)
  }
  missing_n <- n - tmp$inputs$n
  cli_progress_bar(total = missing_n)
  for (i in seq_len(missing_n)) {
    compute_probs_success(n = 1, total_mines = total_mines, dims = dims, clicker = smart_clicker)
    cli_progress_update()
  }
  cli_progress_done()
  return(get_prob(n, total_mines, dims))
  get_prob(n, total_mines, dims)
}
