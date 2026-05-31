source("src/clicker/certain_core.R")
source("src/clicker/random_clicker.R")

certain_else_random_clicker <- function(grid, mines_left, ...) {
  tmp <- certain_core(grid, mines_left, ...)
  if (!is.null(tmp)) {
    return(tmp)
  }
  if (any(is.na(tmp[[1]]))) browser()
  # si aucune stratégie
  random_clicker(grid, ...)
}
