update_global_cache <- function(global_cache, grid, new_grid) {
  if (is.null(global_cache) || length(global_cache) == 0) return(global_cache)

  # TODO quand même conserver les cas dans les clusters qui n'ont pas bougés
  keep <- !sapply(global_cache, `[[`, 2)
  global_cache <- global_cache[keep]
}

update_clusters_cache <- function(clusters_cache, grid, new_grid, ...) {
  if (is.null(clusters_cache)) return(clusters_cache)

  # identifier les clusters touchés par les changements
  i_changed <- which(grid != new_grid & new_grid != unknown_box & new_grid != void_box)
  if (length(i_changed) == 0) return(clusters_cache)
  browser() # TODO pas encore implémenté. attention, doit être très rapide
}
