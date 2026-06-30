library(cli)
source("src/plots.R")

# en lien avec compare_clickers
get_mines_difficulty <- function(n, dims) {
  filepath <- "data/compare_clickers_RDS.RDS"

  if (!file.exists(filepath)) {
    cli_progress_bar(total = n)
    for (i in seq_len(n)) {
      compare_clickers(n = 1, dims = dims, verbose = FALSE)
      cli_progress_update()
    }
    cli_progress_done()
    return(get_mines_difficulty(n, dims))
  }
  old <- readRDS(filepath)

  iden <- sapply(
    old,
    function(lst) all(lst$inputs$dims == dims)
  )
  if (all(!iden)) {
    cli_progress_bar(total = n)
    for (i in seq_len(n)) {
      compare_clickers(n = 1, dims = dims, verbose = FALSE)
      cli_progress_update()
    }
    cli_progress_done()
    return(get_mines_difficulty(n, dims))
  }
  tmp <- old[[which(iden)]]
  if (tmp$inputs$n >= n) {
    tmp$inputs <- NULL
    return(tmp$df)
  }
  missing_n <- n - tmp$inputs$n
  
  cli_progress_bar(total = missing_n)
  for (i in seq_len(missing_n)) {
    compare_clickers(n = 1, dims = dims, verbose = FALSE)
    cli_progress_update()
  }
  cli_progress_done()
  get_mines_difficulty(n, dims)
}
