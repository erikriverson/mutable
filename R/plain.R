muStratPlain <- function(x, name, data, ... ) {
  UseMethod("muStratPlain")
}

muStratPlain.default <- function(x, name, data, round.digits = 2, ... ) {
  val <- paste(round(x, round.digits), collapse = "/")
  names(val) <- name
  val
}

muStratPlain.character <- function(x, name, data, round.digits = 2, ... ) {
  val <- x
  names(val) <- name
  val
}


muStratPlain.table <- function(x, name, data, round.digits = 0, ...) {
  dft <- as.data.frame(x)
  pct <- paste(round(x / sum(x) * 100, round.digits), "%", sep = "")
  val <- paste(pct, paste(dft[["Freq"]], "/", sum(x), sep = ""))
  names(val) <- paste(name, names(x), sep = "")
  val
}


muResponsePlain <- function(x, name, data, ... ) {
  UseMethod("muResponsePlain")
}

muResponsePlain.default <- muPrintIdentity

muResponsePlain.muResponseSummaryFactor <- function(x, name, data, round.digits = 0, ...) {
  val <- sapply(x, muStratPlain.default, name, data)
  names(val) <- paste(name, names(x), sep = "")
  val
}
