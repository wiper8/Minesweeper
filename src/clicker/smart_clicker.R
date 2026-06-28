source("src/clicker/certain_core.R")
source("src/clicker/random_clicker.R")
source("src/clicker/probabilistic_clicker.R")

smart_clicker <- function(grid, mines_left, clusters_cache = NULL, ...) {
  tmp <- certain_core(grid, mines_left, ...)
  if (!is.null(tmp$clicks)) {
    return(tmp)
  }
  clusters_cache <- tmp$clusters_cache
  tmp <- probabilistic_clicker(grid, mines_left = mines_left, clusters_cache = clusters_cache, ...)
  if (!is.null(tmp$clicks)) {
    return(tmp)
  }
  # si aucune stratégie
  browser() # pas sensé ce rendre ici
}
