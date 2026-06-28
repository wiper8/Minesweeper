source("src/indicies/i_and_positions.R")
source("src/clicker/helper/homologous_next_i.R")

filter_homologous_combins <- function(grid, mines_left, solved_around, hypothesis, combins, pos_unknown, dims) {
  if (hypothesis == 2 && !any(grid %in% hp_to_hypo_no_mine) && any(grid %in% c(void_box, unknown_box, hypothetical_mine, hypothetical_no_mine))) {
    around_i <- position_to_i_mat(pos_unknown, dims)
    homologous <- homologous_next_i(grid, around_i[1], in_cluster = grid != -11, dims)
    combins[around_i[combins] %in% homologous$homologous_i] <- NA
    combins_without_homologous <- combins
    combins_without_homologous[around_i[combins] %in% homologous$homologous_i] <- NA
    combins_without_homologous <- apply(combins_without_homologous, 2, sort, na.last = FALSE, simplify = FALSE)
    
    # filtrer avec homologous pour conserver une seule combin parmi les homologues
    combins_without_homologous <- combins_without_homologous[!duplicated(combins_without_homologous)]
    
    # réimputer des valeurs homologes
    i_homologous <- which(around_i %in% homologous$homologous_i)
    combins <- do.call(
      cbind,
      lapply(combins_without_homologous, function(x) {
        x[is.na(x)] <- head(i_homologous, sum(is.na(x)))
        x
      })
    )
  }
  combins
}
