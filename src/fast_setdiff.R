fast_setdiff <- function(x, y) {
  x <- fast_unique(x)
  y <- fast_unique(y)
  x[match(x, y, 0L) == 0L]
}

# à utiliser seulement quand on sait que x et y n'ont pas de doublons
fast_setdiff_no_unique <- function(x, y) {
  x[match(x, y, 0L) == 0L]
}

fast_unique <- function (x) {
  .Internal(unique(x, FALSE, FALSE, NA))
}
