## try to replicate an Hmisc type table with a function
eN <- function(x, strat, data, ...) {
  length(x)
}

eStratTable <- function(formula, data) {
  
  first <- etable(formula, data = data, colname = "Variable",
                  summary.function = erownames) +
           etable(summary.function = eN, colname = "N") +
           etable(colname = "Combined")

  middle <- Reduce("+",
                   lapply(split(data,
                                data[[as.character(as.list(formula)[[2]])]]),
                          function(x) etable(formula, data = x,
                                             colname = x[[as.character(formula)[[2]]]][1])))
                            
  last <- etable(summary.function = etest, colname = "P-value")

  first + middle + last
}




