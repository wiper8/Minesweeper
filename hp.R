# 0:9 sont quand les cases sont révélées
# -1 est une case non révélée sans mine
covered_no_mine <- -1L
# -2 est une case non révélée avec mine
covered_mine <- -2L
# -3 est une case révélée sans mine, pas encore propagée
uncovered_no_mine <- -3L
# -4 est une case révélée sans mine
uncovered_mine <- -4L
# -5 est un flag de posé sur une mine
flag_on_mine <- -5L
# -6 est un flag de posé sur une non-mine, soit une erreur
flag_on_no_mine <- -6L
uncovered_unknown <- -7L
hypothetical_mine <- -9L
hypothetical_no_mine <- -8L
unknown_box <- -10L

# pour accélérer les calculs, pas besoin de réinstancier à chaque fois
known <- c(0:9, flag_on_mine, flag_on_no_mine, hypothetical_mine, hypothetical_no_mine)
