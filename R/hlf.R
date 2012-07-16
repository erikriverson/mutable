
mutableN <- function(x, columnVariable, data, ...) {
  sum(!is.na(x))
}

#' @export
mutableStrat <- function(formula, data, firstcol = "Variable", overall = TRUE,
                         drop = FALSE, markup.functions = NULL, ...) {
  if(drop) {
    ind <- sapply(data, is.factor)
    data[ind] <- lapply(data[ind], "[", drop = TRUE)
  }

  if(is.null(markup.functions))
    markup.functions <- list(plain = muExportPlain,
                             latex = muExportLatex,
                             html  = muExportHTML)

  first <- mutable(formula, data = data, colname = firstcol,
                   summary.function = muRownames,
                   markup.functions =  markup.functions,
                   useVarName = TRUE) + 
                     mutable(summary.function = mutableN,
                             colname = "N", ...)

  middle <-
    Reduce("+",
           lapply(split(data,
                        data[[as.character(as.list(formula)[[2]])]]),
                  function(x) mutable(formula, data = x,
                                      summary.function = muStratSummary,
                                      markup.functions = markup.functions,
                                      colname = x[[as.character(formula)[[2]]]][1],
                                      ...)))

  table <- first + middle

  if(length(unique(data[[as.character(as.list(formula)[[2]])]])) > 1 & overall) {
    last <- mutable(formula, data,
                    summary.function = muStratSummary,
                    markup.functions = markup.functions, 
                    colname = "Overall", ...)

    table <- table + last
  }

  table
}

#' @export
mutableResponse <- function(formula, data,
                            firstColumn = "Variable",
                            summaryColumn = "Summary Statistics",
                            drop = FALSE, ...) {
  if(drop) {
    ind <- sapply(data, is.factor)
    data[ind] <- lapply(data[ind], "[", drop = TRUE)
  }

  mutable(formula, data = data,
          summary.function = muRownames,
          markup.functions = list(plain = muExportPlain,
            latex = muExportLatex,
            html = muExportHTML),
          colname = firstColumn, ...) +
  mutable(summary.function = mutableN,
          colname = "N", ...) +
  mutable(summary.function = muResponseSummary,
          colname = summaryColumn, ...)
}















