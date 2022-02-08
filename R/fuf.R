# Collection of Frequently Used Functions (FUF)


# Bind with empty dataframe
cbind.all <- function (...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x) rbind(x, matrix(, n - nrow(x),
                                                        ncol(x)))))
}
