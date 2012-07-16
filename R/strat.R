
#' @export
muStratSummary <- function(x, stratVariable, data, ...) {
  UseMethod("muStratSummary")
}

#' @S3method muStratSummary default
muStratSummary.default <- function(x, stratVariable, data, ...) {
  quant <- quantile(x, probs = c(.25, .5, .75), na.rm = TRUE, ...)
  class(quant) <- "muStratSummaryNumeric"
  quant
}

#' @S3method muStratSummary default
muStratSummary.default <- function(x, stratVariable, data, ...) {
  mn <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  ret <- c(mn, sd)
  class(ret) <- "muStratSummaryMean"
  ret
}

#' @S3method muStratSummary factor
muStratSummary.factor <- function(x, stratVariable, data, ...) {
  tbl <- table(x, ...)
  class(tbl) <- "muStratSummaryFactor"
  tbl
}

#' @export
muStratTest <- function(x, stratVariable, data, ...) {
  UseMethod("muStratTest")
}

#' @S3method muStratTest default
muStratTest.default <- function(x, stratVariable, data, round.digits = 2, ...) {
  ret <- list(pvalue = round(t.test(x ~ stratVariable, ...)$p.value, round.digits),
              test = "t-test")
  class(ret) <- "muStratTestNumeric"
  ret
}

#' @S3method muStratTest factor
muStratTest.factor <- function(x, stratVariable, data, round.digits = 2, ...) {
  ret <- list(pvalue = round(fisher.test(x, stratVariable, ...)$p.value, round.digits),
              test = "Fisher test")
  class(ret) <- "muStratTestFactor"
  ret
}

