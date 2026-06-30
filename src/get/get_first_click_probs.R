library(cli)
source("src/plots.R")

# en lien avec show_first_click_probs
get_first_click_probs <- function(n, total_mines, dims) {
  filepath <- "data/first_click_probs_RDS.RDS"

  if (!file.exists(filepath)) {
    cli_progress_bar(total = n)
    for (i in seq_len(n)) {
      first_click_probs(n = 1, total_mines = total_mines, dims = dims)
      cli_progress_update()
    }
    cli_progress_done()
    return(get_first_click_probs(n, total_mines, dims))
  }
  old <- readRDS(filepath)

  iden <- sapply(
    old,
    function(lst) lst$inputs$total_mines == total_mines && all(lst$inputs$dims == dims)
  )
  if (all(!iden)) {
    cli_progress_bar(total = n)
    for (i in seq_len(n)) {
      first_click_probs(n = 1, total_mines = total_mines, dims = dims)
      cli_progress_update()
    }
    cli_progress_done()
    return(get_first_click_probs(n, total_mines, dims))
  }
  tmp <- old[[which(iden)]]
  if (tmp$inputs$n >= n) {
    tmp$inputs <- NULL
    return(tmp$mat)
  }

  missing_n <- n - tmp$inputs$n
  cli_progress_bar(total = missing_n)
  for (i in seq_len(missing_n)) {
    first_click_probs(n = 1, total_mines = total_mines, dims = dims)
    cli_progress_update()
  }
  cli_progress_done()
  get_first_click_probs(n, total_mines, dims)
}
