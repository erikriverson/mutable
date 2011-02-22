muExportPlain <- function(x, name, data, ... ) {
  UseMethod("muExportPlain")
}

muExportPlain.muStratTestNumeric <- muFormatPvalue
muExportPlain.muStratTestFactor <- muFormatPvalue
muExportPlain.muResponseTestNumeric <- muFormatPvalue
muExportPlain.muResponseTestFactor <- muFormatPvalue

muExportPlain.muRownamesFactor <- function(x, name, data, ...) {
  ret <- c(x[1], paste("", tail(x, length(x) - 1)))
  names(ret) <- c(name, ps(name,  levels(data[[name]])))
  ret
}

muExportPlain.muRownamesNumeric <- muPrintIdentity

muExportPlain.muStratSummaryNumeric <- function(x, name, data, round.digits = 2, ... ) {
  val <- paste(round(x, round.digits), collapse = "/")
  names(val) <- name
  val
}

muExportPlain.muStratSummaryFactor <- function(x, name, data, round.digits = 0, ...) {
  dft <- as.data.frame(as.table(x))
  pct <- ps(round(x / sum(x) * 100, round.digits), "%")
  val <- ps(pct, paste(dft[["Freq"]], "/", sum(x)))
  names(val) <- ps(name, names(x))
  val
}

muExportPlain.muResponseSummaryNumeric <- function(x, name, data, round.digits = 2, ...) {
  x <- round(x, round.digits)
  val <- ps(x[1], " (", x[2], " - ", x[3], ")")
  names(val) <- name
  val
}

muExportPlain.muResponseSummaryFactor <- function(x, name, data, round.digits = 0, ...) {
  val <- sapply(x, muExportPlain.muStratSummaryNumeric, name, data)
  names(val) <- ps(name, names(x))
  val
}

muExportPlain.default <- muPrintIdentity
