source("src/clicker/certain_core.R")
source("src/clicker/random_clicker.R")

smart_clicker <- function(grid, mines_left, ...) {
  tmp <- certain_core(grid, mines_left, ...)
  if (!is.null(tmp)) {
    return(tmp)
  }
  browser()
  tmp <- probabilistic_clicker(grid, mines_left = mines_left, ...)
  if (!is.null(tmp)) {
    return(tmp)
  }
  # si aucune stratégie
  browser() # pas sensé ce rendre ici
}
