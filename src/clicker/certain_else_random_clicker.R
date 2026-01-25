source("src/clicker/certain_core.R")
source("src/clicker/random_clicker.R")

certain_else_random_clicker <- function(grid, total_mines, ...) {
  tmp <- certain_core(grid, total_mines, ...)
  if (!is.null(tmp)) {
    return(tmp)
  }
  # si aucune stratégie
  random_clicker(grid, total_mines, ...)
}
