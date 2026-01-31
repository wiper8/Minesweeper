fast_pmax <- function(..., na.rm = FALSE) {
  .Internal(pmax(na.rm, ...))
}
