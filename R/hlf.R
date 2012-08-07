
mutableN <- function(x, columnVariable, data, ...) {
  sum(!is.na(x))
}



#' @export
mutable.formula <- function(formula, data, firstcol = "Variable", overall = TRUE,
                         drop = FALSE, markup.functions = NULL, ...) {
  if(drop) {
    ind <- sapply(data, is.factor)
    data[ind] <- lapply(data[ind], "[", drop = TRUE)
  }

  if(is.null(markup.functions))
    markup.functions <- list(plain = muExportPlain,
                             latex = muExportLatex,
                             html  = muExportHTML)

  first <- muColumn(formula, data = data, colname = firstcol,
                   summary.function = muRownames,
                   markup.functions =  markup.functions,
                   useVarName = TRUE) + 
                     muColumn(summary.function = mutableN,
                                   colname = "N", ...)

  middle <-
    Reduce("+",
           lapply(split(data,
                        data[[as.character(as.list(formula)[[2]])]]),
                  function(x) muColumn(formula, data = x,
                                      summary.function = muStratSummary,
                                      markup.functions = markup.functions,
                                      colname = x[[as.character(formula)[[2]]]][1],
                                      ...)))

  table <- first + middle

  if(length(unique(data[[as.character(as.list(formula)[[2]])]])) > 1 & overall) {
    last <- muColumn(formula, data,
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

  muColumn(formula, data = data,
          summary.function = muRownames,
          markup.functions = list(plain = muExportPlain,
            latex = muExportLatex,
            html = muExportHTML),
          colname = firstColumn, ...) +
  muColumn(summary.function = mutableN,
          colname = "N", ...) +
  muColumn(summary.function = muResponseSummary,
          colname = summaryColumn, ...)
}















