muExportPlain <- function(x, name, data, ... ) {
  UseMethod("muExportPlain")
}

muExportPlain.muStratTestNumeric <- muFormatPvalue
muExportPlain.muStratTestFactor <- muFormatPvalue
muExportPlain.muResponseTestNumeric <- muFormatPvalue
muExportPlain.muResponseTestFactor <- muFormatPvalue

muExportPlain.muRownamesFactor <- function(x, name, data, ...) {
  ret <- c(x[1], paste("", tail(x, length(x) - 1)))
  names(ret) <- c(name, paste(name,  levels(data[[name]]), sep = ""))
  ret
}

muExportPlain.muRownamesNumeric <- muPrintIdentity

muExportPlain.muStratSummaryNumeric <- function(x, name, data, round.digits = 2, ... ) {
  val <- paste(round(x, round.digits), collapse = "/")
  names(val) <- name
  val
}

muExportPlain.muStratSummaryFactor <- function(x, name, data, round.digits = 0, ...) {
  dft <- as.data.frame(x)
  pct <- paste(round(x / sum(x) * 100, round.digits), "%", sep = "")
  val <- paste(pct, paste(dft[["Freq"]], "/", sum(x), sep = ""))
  names(val) <- paste(name, names(x), sep = "")
  val
}

muExportPlain.muResponseSummaryNumeric <- function(x, name, data, round.digits = 2, ...) {
  x <- round(x, round.digits)
  val <- ps(x[1], " (", x[2], " - ", x[3], ")")
  names(val) <- name
  val
}

muExportPlain.muResponseSummaryFactor <- function(x, name, data, round.digits = 0, ...) {
  val <- sapply(x, muStratPlain.default, name, data)
  names(val) <- paste(name, names(x), sep = "")
  val
}
