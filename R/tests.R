muResponseTest <- function(x, strat, data, ...) {
  UseMethod("muResponseTest")
}

muResponseTest.default <- function(x, strat, data, round.digits = 2, ...) {
  list(pvalue = round(cor.test(x, strat, ...)$p.value, round.digits),
       test = "Correlation test")
}

muResponseTest.factor <- function(x, strat, data, round.digits = 2, ...) {
  list(pvalue = round(coef(summary(lm(strat ~ x, ...)))[2, 4], round.digits),
       test = "ANOVA F-test")
}

muStratTest <- function(x, strat, data, ...) {
  UseMethod("muStratTest")
}

muStratTest.default <- function(x, strat, data, round.digits = 2, ...) {
  list(pvalue = round(t.test(x ~ strat, ...)$p.value, round.digits),
       test = "t-test")

}

muStratTest.factor <- function(x, strat, data, round.digits = 2, ...) {
  list(pvalue = round(fisher.test(x, strat, ...)$p.value, round.digits),
       test = "Fisher test")
}

mulmCoef <- function(formula, data, round.digits = 2, ...) {
  ret <- round(coef(lm(formula, data, ...))[-1], round.digits)
  list(plain = ret, latex = ret, html = ret)
}

pvalSummaryHook <- function(ret) {
  nms <- unique(sapply(ret, "[[", "test"))
  unt <- 1:length(nms)
  names(unt) <- nms

  ret <- lapply(ret, function(x) c(x, testsuper = as.vector(unt[x$test])))
  attr(ret, "testnames") <- nms
  ret 
}

pvalMarkupHook <- function(return.list, ret, ...) {
  attr(return.list, "testnames") <- attr(ret, "testnames")
  return.list
}
