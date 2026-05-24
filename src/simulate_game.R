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

main_game_loop <- function(grid, mines_left, clicker, solved_around, hypothesis = FALSE, click_order = NULL, ...) {
  seuil_verbose_duration_click <- 5
  repeat {
    a <- Sys.time()
    # choisir la prochaine action
    tmp <- clicker(grid, mines_left = mines_left, solved_around = solved_around, hypothesis = hypothesis,
                   click_order = click_order, ...)
    b <- Sys.time()
    duration_for_click <- as.numeric(difftime(b, a, units = "secs"))
    if (duration_for_click > seuil_verbose_duration_click) {
      print(paste0("slow selection after ", nrow(click_order), " clicked. ", round(duration_for_click), " secs"))
      if (duration_for_click > 60) browser()
    }
    if (hypothesis && isTRUE(all.equal(tmp, "impossible"))) return(list(grid, "partie impossible", solved_around))
    if (isTRUE(all.equal(tmp, "impossible"))) browser()
    if (hypothesis && is.null(tmp)) return(list(grid, "le clicker ne sait pu quoi faire", solved_around))
    if (is.null(tmp)) browser()
    if (tmp[[2]]) click_order <- rbind(click_order, tmp[[1]])
    tmp2 <- apply_action(grid, tmp[[1]], tmp[[2]], mines_left, solved_around = solved_around, hypothesis = hypothesis, ...)
    grid <- tmp2[[1]]
    mines_left <- tmp2[[3]]
    solved_around <- tmp2[[4]]
    if (tmp2[[2]] == 1) {
      if (hypothesis && !is_grid_possible(grid)) return(list(grid, "partie impossible", solved_around))
      return(list(grid, "win", solved_around))
    }
    if (tmp2[[2]] == -1) return(list(grid, "lost", solved_around))
  }
}
