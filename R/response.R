muResponseSummary <- function(x, strat, data, ...) {
  UseMethod("muResponseSummary")
}
 
muResponseSummary.default <- function(x, strat, data, ...) {
  ct <- cor.test(x, strat)
  ret <- c(ct$estimate, ct$conf.int)
  class(ret) <- "muResponseSummaryNumeric"
  ret
}

muResponseSummary.factor <- function(x, strat, data, ...) {
  ta <- tapply(strat, x, quantile, probs = c(.25, .5, .75))
  class(ta) <-  "muResponseSummaryFactor"
  ta
}

muResponseTest <- function(x, strat, data, ...) {
  UseMethod("muResponseTest")
}

muResponseTest.default <- function(x, strat, data, round.digits = 2, ...) {
  ret <- list(pvalue = round(cor.test(x, strat, ...)$p.value, round.digits),
              test = "Correlation test")
  class(ret) <- "muResponseTestNumeric"
  ret
}

muResponseTest.factor <- function(x, strat, data, round.digits = 2, ...) {
  ret <- list(pvalue = round(coef(summary(lm(strat ~ x, ...)))[2, 4], round.digits),
              test = "ANOVA F-test")
  class(ret) <- "muResponseTestFactor"
  ret
}

