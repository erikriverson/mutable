## try to replicate an Hmisc type table with a function
mutableN <- function(x, strat, data, ...) {
  sum(!is.na(x))
}

mutableStrat <- function(formula, data, firstcol = "Variable") {
  
  first <- mutable(formula, data = data, colname = firstcol,
                   summary.function = muRownames,
                   markup.list = list(plain = muExportPlain,
                     latex = muExportLatex,
                     html = muExportHTML)) + 
           mutable(summary.function = mutableN,
                   colname = "N")


  middle <-
    Reduce("+",
           lapply(split(data,
                        data[[as.character(as.list(formula)[[2]])]]),
                  function(x) mutable(formula, data = x,
                                      summary.function = muStratSummary,
                                      markup.list = list(plain = muExportPlain,
                                        latex = muExportLatex,
                                        html = muExportHTML),
                                      colname = x[[as.character(formula)[[2]]]][1])))

  table <- first + middle

  if(length(unique(data[[as.character(as.list(formula)[[2]])]])) > 1) {
    last <- mutable(formula, data,
                    summary.function = muStratSummary,
                    markup.list = list(plain = muExportPlain,
                      latex = muExportLatex,
                      html = muExportHTML),
                    colname = "Combined")

    table <- table + last
  }

  table
}

mutableResponse <- function(formula, data, firstcol = "Variable") {
  mutable(formula, data = data,
          summary.function = muRownames,
          markup.list = list(plain = muExportPlain,
            latex = muExportLatex,
            html = muExportHTML),
          colname = firstcol) +
  mutable(summary.function = mutableN,
          colname = "N") +
  mutable(summary.function = muResponseSummary,
          colname = "Summary Statistics")
}















