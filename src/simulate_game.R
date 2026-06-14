source("hp.R")
source("src/game_engine/update_grid.R")
source("src/game_engine/apply_action.R")
source("src/game_engine/is_game_over.R")
source("src/indicies/i_and_positions.R")
source("src/clicker/random_first_click.R")

#' Simuler une partie de Minsweeper
#'
#' @param total_mines nombre entier : nombre de total_mines total dans la grille
#' @param dims vecteur de deux entiers : nombre de cases horizontalement et verticalement de la grille
#' @param clicker fonction de type clicker permettant d'effectuer des choix quant aux prochaines actions à prendre
#'
#' @returns liste de la grille et de "win" ou "lost" selon la situation
#' @export
#'
#' @examples
#' simulate_game(10, c(10, 8), random_clicker)
simulate_game <- function(total_mines, dims = c(17, 9), clicker, first_click = NULL) {
  grid <- matrix(NA, nrow = dims[1], ncol = dims[2])
  # TODO changer pour une meilleure fonction, il est possible que commencer au centre ou aux coins est avantageux
  # faiblement
  if (is.null(first_click)) first_click <- random_first_click(dims)
  grid <- init_grid_after_first_click(grid, first_click, total_mines)
  solved_around <- matrix(-1, nrow = nrow(grid), ncol = ncol(grid))
  mines_left <- total_mines
  tmp <- apply_action(grid, first_click, TRUE, mines_left, solved_around = solved_around)
  grid <- tmp[[1]]
  mines_left <- tmp[[3]]
  solved_around <- tmp[[4]]
  if (tmp[[2]] == 1) return(list(grid, "win", solved_around))
  main_game_loop(grid, total_mines, clicker, solved_around = solved_around)
}

init_grid_after_first_click <- function(grid, pos, total_mines) {
  dims <- dim(grid)
  grid[pos[1], pos[2]] <- uncovered_no_mine
  # put mines in the game
  grid[sample(setdiff(seq_len(prod(dims)), position_to_i(pos, dims)), total_mines)] <- covered_mine
  grid[is.na(grid)] <- covered_no_mine
  grid
}

main_game_loop <- function(grid, mines_left, clicker, solved_around, hypothesis = 0, click_order = NULL,
                           global_cache = list(), ...) {
  seuil_verbose_duration_click <- 5
  repeat {
    # if (hypothesis == 0) print(mean(grid %in% known))
    a <- Sys.time()
    # choisir la prochaine action
    tmp <- clicker(grid, mines_left = mines_left, solved_around = solved_around, hypothesis = hypothesis,
                   click_order = click_order, global_cache = global_cache, ...)
    b <- Sys.time()
    duration_for_click <- as.numeric(difftime(b, a, units = "secs"))
    if (hypothesis == 0 && duration_for_click > seuil_verbose_duration_click) {
      print(paste0("slow selection after ", nrow(click_order), " clicked. ", round(duration_for_click), " secs"))
      if (duration_for_click > 10) browser()
    }
    # "partie impossible"
    # ne devrait pas être possible car
    # quand on calcule les probabilitées, c'est que tous les clics étaient possibles
    if (hypothesis == 1 && isTRUE(all.equal(tmp$clicks, "impossible"))) browser()
    if (hypothesis == 2 && isTRUE(all.equal(tmp$clicks, "impossible"))) return(list(grid, "partie impossible", solved_around, mines_left))
    if (isTRUE(all.equal(tmp$clicks, "impossible"))) browser()
    if (hypothesis != 0 && is.null(tmp$clicks)) return(list(grid, "le clicker ne sait pu quoi faire", solved_around, mines_left))

    # mettre à jour la cache
    if (!is.null(tmp$global_cache)) {
      global_cache <- tmp$global_cache
      keep <- !sapply(global_cache, `[[`, 2)
      global_cache <- global_cache[keep]
    }
    if (is.null(tmp$clicks)) browser()
    
    for (new_action in tmp$clicks) {
      if (new_action[[2]]) click_order <- rbind(click_order, new_action[[1]])
      if (any(is.na(new_action[[1]]))) browser()
      tmp2 <- apply_action(grid, new_action[[1]], new_action[[2]], mines_left, solved_around = solved_around, hypothesis = hypothesis, ...)
      grid <- tmp2[[1]]
      mines_left <- tmp2[[3]]
      solved_around <- tmp2[[4]]
      if (tmp2[[2]] == 1) {
        if (hypothesis == 2 && !is_grid_possible(grid)) return(list(grid, "partie impossible", solved_around, mines_left))
        return(list(grid, "win", solved_around, mines_left))
      }
      if (tmp2[[2]] == -1) return(list(grid, "lost", solved_around, mines_left))
    }
  }
}
