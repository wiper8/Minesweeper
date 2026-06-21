update_global_cache <- function(global_cache, grid, new_grid) {
  if (is.null(global_cache) || length(global_cache) == 0) return(global_cache)
  
  # TODO quand même conserver les cas dans les clusters qui n'ont pas bougés
  keep <- !sapply(global_cache, `[[`, 2)
  global_cache <- global_cache[keep]
}
