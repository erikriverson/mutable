muStratSummary <- function(x, strat, data, ...) {
  UseMethod("muStratSummary")
}


muStratSummary.default <- function(x, strat, data, ...) {
  quant <- quantile(x, probs = c(.25, .5, .75), na.rm = TRUE, ...)
  class(quant) <- "muStratSummaryNumeric"
  quant
}


muStratSummary.factor <- function(x, strat, data, ...) {
  tbl <- table(x, ...)
  class(tbl) <- "muStratSummaryFactor"
  tbl
}

muStratTest <- function(x, strat, data, ...) {
  UseMethod("muStratTest")
}

muStratTest.default <- function(x, strat, data, round.digits = 2, ...) {
  ret <- list(pvalue = round(t.test(x ~ strat, ...)$p.value, round.digits),
              test = "t-test")
  class(ret) <- "muStratTestNumeric"
  ret
}

muStratTest.factor <- function(x, strat, data, round.digits = 2, ...) {
  ret <- list(pvalue = round(fisher.test(x, strat, ...)$p.value, round.digits),
              test = "Fisher test")
  class(ret) <- "muStratTestFactor"
  ret
}

