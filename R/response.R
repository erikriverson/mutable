muResponseSummary <- function(x, columnVariable, data, ...) {
  UseMethod("muResponseSummary")
}
 
muResponseSummary.default <- function(x, columnVariable, data, ...) {
  ct <- cor.test(x, columnVariable, ...)
  ret <- c(ct$estimate, ct$conf.int)
  class(ret) <- "muResponseSummaryNumeric"
  ret
}

muResponseSummary.factor <- function(x, columnVariable, data, ...) {
  ta <- tapply(columnVariable, x, quantile, probs = c(.25, .5, .75))
  class(ta) <-  "muResponseSummaryFactor"
  ta
}

muResponseTest <- function(x, columnVariable, data, ...) {
  UseMethod("muResponseTest")
}

muResponseTest.default <- function(x, columnVariable, data, round.digits = 2, ...) {
  ret <- list(pvalue = round(cor.test(x, columnVariable, ...)$p.value, round.digits),
              test = "Correlation test")
  class(ret) <- "muResponseTestNumeric"
  ret
}

muResponseTest.factor <- function(x, columnVariable, data, round.digits = 2, ...) {
  ret <- list(pvalue = round(coef(summary(lm(columnVariable ~ x, ...)))[2, 4], round.digits),
              test = "ANOVA F-test")
  class(ret) <- "muResponseTestFactor"
  ret
}

