
mutableLMCoef <- function(formula, data, colname, round.digits = 2, ...) {
  fm <- lm(formula, data, ...)
  ret <- round(coef(fm)[-1], round.digits)
  nms <- names(ret)
  ret <- paste(ret, "$(", round(confint(fm)[-1,1], round.digits),
               "$ -- $", round(confint(fm)[-1,2], round.digits)
               , ")$")
  names(ret) <- nms
  
  ret <- as.matrix(ret, ncol = 1)
  colnames(ret) <- colname
  ret.list <-   list(plain = ret, latex = ret, html = ret)
  class(ret.list) <- "mutable"
  ret.list
}

mutableGLMCoef <- function(formula, data, colname, round.digits = 2, ...) {
  
  fm <- glm(formula, data, family = "binomial", ...)
  ret <- round(exp(coef(fm))[-1], round.digits)

  nms <- names(ret)
  ret <- paste(ret, "(", round(exp(confint(fm)[-1,1]), round.digits),
               "-", round(exp(confint(fm)[-1,2]), round.digits)
               , ")")
  names(ret) <- nms
  
  ret <- as.matrix(ret, ncol = 1)
  colnames(ret) <- colname

  ret.list <- list()
  ret.list$markup <- list(plain = ret, latex = ret, html = ret)

  class(ret.list) <- "mutable"
  ret.list
}

pvalSummaryHook <- function(ret) {
  cls <- class(ret[[1]])
  nms <- unique(sapply(ret, "[[", "test"))
  unt <- 1:length(nms)
  names(unt) <- nms

  ret <- lapply(ret, function(x) c(x, testsuper = as.vector(unt[x$test])))
  attr(ret, "testnames") <- nms
  lapply(ret, "class<-", cls)
  ret 
}

pvalMarkupHook <- function(return.list, ret, ...) {
  attr(return.list, "testnames") <- attr(ret, "testnames")
  return.list
}

