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
  browser()
  new_clusters <- independant_clusters(new_grid, ...)
  # changement dans le void
  if (any(clusters_cache$void$in_cluster[i_changed])) {
    # TODO peut-être mieux gérer pour éviter les mapply. bien réfléchirs aux cas possible si clic/flag dans le void
    browser() # TODO est-ce vraiment possible de tomber ici? si j'ai cliqué, ça crée un cluster, si j'ai flaggué, ça
    # va dans known_but_useless
    if (length(new_clusters$clusters) == 0) return(new_clusters)
    if (length(clusters_cache$clusters) == length(new_clusters$clusters) && all(mapply(
      function(clust, new_clust) all(clust$in_cluster == clust$in_cluster),
      clusters_cache$clusters,
      new_clusters$clusters
    ))) {
      # possible <- NA pour le dernier possible (plus élevé)
      new_clusters$clusters <- lapply(
        clusters_cache$clusters,
        function(clust) {
          i_possible <- which(clust$possible == "TRUE")
          if (length(i_possible) > 0) clust$possible[i_possible] <- "NA"
          clust
        }
      )
      return(new_clusters)
    }

    new_clusters$clusters <- lapply(new_clusters$clusters, fun, clusters_cache = clusters_cache)

    return(new_clusters)
  }
  
  which_clusters_affected <- sapply(clusters_cache$clusters, function(clust) any(clust$in_cluster[i_changed]))
  if (sum(which_clusters_affected) == 0) browser() # car on a déjà testé le void précédemment

  is_identical_as_before <- lapply(new_clusters$clusters, function(clust_new) {
    which(sapply(clusters_cache$clusters, function(clust_old) {
      all(clust_old$in_cluster == clust_new$in_cluster) && all(clust_old$grid == clust_new$grid)
    }))
  })

  new_clusters$clusters <- mapply(
    function(id, new_clust) {
      if (length(id) == 0) return(new_clust)
      old_clust <- clusters_cache$clusters[[id]]
      browser()
      new_clust <- old_clust
      # quand même reset ça pour éviter des bogues. ça va quand même accélérer
      # de savoir que les possibilités restantes sont bornées entre les bornes min et max
      new_clust$possible <- rep("NA", length(new_clust$possible))
      new_clust
    },
    is_identical_as_before,
    new_clusters$clusters,
    SIMPLIFY = FALSE
  )

  new_clusters
}

fun <- function(clust, clusters_cache) {
  is_there <- which(sapply(clusters_cache$clusters, function(clust_old) all(clust_old$in_cluster == clust$in_cluster)))
  if (length(is_there) > 1) browser()
  if (length(is_there) == 0) return(clust) # nouveau cluster dans le void
  
  # possible de réutiliser un vieux cluster légèrement adapté, car on a pt mis une mine dans le void
  i_possible <- which(clusters_cache$clusters[[is_there]]$possible == "TRUE")
  if (length(i_possible) > 0) clusters_cache$clusters[[is_there]]$possible[tail(i_possible, 1)] <- "NA"
  clust
}
