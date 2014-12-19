identify_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)

  y <- rep(FALSE, length(x))
  y[x < qnt[1]] <- TRUE
  y[x > qnt[2]] <- TRUE
  y
}