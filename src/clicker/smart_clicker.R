source("src/clicker/certain_core.R")
source("src/clicker/random_clicker.R")
source("src/clicker/probabilistic_clicker.R")

smart_clicker <- function(grid, mines_left, ...) {
  tmp <- certain_core(grid, mines_left, ...)
  if (!is.null(tmp$clicks)) {
    return(tmp)
  }
  tmp <- probabilistic_clicker(grid, mines_left = mines_left, ...)
  if (!is.null(tmp$clicks)) {
    return(tmp)
  }
  # si aucune stratégie
  browser() # pas sensé ce rendre ici
}

smart_clicker2 <- function(grid, mines_left, ...) {
  tmp <- certain_core(grid, mines_left, ...)
  if (!is.null(tmp$clicks)) {
    return(tmp)
  }
  tmp <- probabilistic_clicker(grid, mines_left = mines_left, try_risky = TRUE, ...)
  if (!is.null(tmp$clicks)) {
    return(tmp)
  }
  # si aucune stratégie
  browser() # pas sensé ce rendre ici
}
