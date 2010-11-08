muRownames <- function(x, ...) {
  UseMethod("muRownamesSummary")
}

muRownames.default <- function(x, ...) {
  lab <- label(x)
  class(lab) <- "muRownamesNumeric"
  lab
}

muRownames.factor <- function(x, ...) {
  ret <- c(label(x), levels(x))
  class(ret) <- "muRownamesFactor"
  ret
}
