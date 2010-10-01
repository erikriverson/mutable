## try to replicate an Hmisc type table with a function
mutableN <- function(x, strat, data, ...) {
  length(x)
}

mutableStrat <- function(formula, data) {
  
  first <- mutable(formula, data = data, colname = "Variable",
                   summary.function = muRownames) +
           mutable(summary.function = mutableN, colname = "N") +
           mutable(colname = "Combined")

  middle <- Reduce("+",
                   lapply(split(data,
                                data[[as.character(as.list(formula)[[2]])]]),
                          function(x) mutable(formula, data = x,
                                              colname = x[[as.character(formula)[[2]]]][1])))
                            
  last <- mutable(summary.function = muTest, colname = "P-value")

  first + middle + last
}











