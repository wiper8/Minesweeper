fast_apply <- function(X, MARGIN, FUN, ..., simplify = TRUE) {
  d <- dim(X)
  dl <- length(d)
  ds <- seq_len(dl)
  d.call <- d[-MARGIN]
  d.ans <- d[MARGIN]
  s.call <- ds[-MARGIN]
  s.ans <- ds[MARGIN]
  d2 <- prod(d.ans)
  if (d2 == 0L) {
    newX <- array(vector(typeof(X), 1L), dim = c(prod(d.call), 
                                                 1L))
    ans <- forceAndCall(1, FUN, if (length(d.call) < 2L) newX[, 
                                                              1] else array(newX[, 1L], d.call, NULL), ...)
    return(if (is.null(ans)) ans else if (length(d.ans) < 
                                          2L) ans[1L][-1L] else array(ans, d.ans, NULL))
  }
  
  newX <- aperm(X, c(s.call, s.ans))
  dim(newX) <- c(prod(d.call), d2)
  ans <- vector("list", d2)
  for (i in 1L:d2) {
    tmp <- forceAndCall(1, FUN, newX[, i], ...)
    if (!is.null(tmp)) 
      ans[[i]] <- tmp
  }
  unlist(ans, recursive = FALSE)
}

