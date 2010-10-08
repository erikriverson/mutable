## try to replicate an Hmisc type table with a function
mutableN <- function(x, strat, data, ...) {
  sum(!is.na(x))
}

mutableStrat <- function(formula, data, firstcol) {
  
  first <- mutable(formula, data = data, colname = firstcol,
                   summary.function = muRownamesSummary) +
           mutable(summary.function = mutableN,
                   latex.function = muPrintIdentity,
                   colname = "N") 


  middle <- Reduce("+",
                   lapply(split(data,
                                data[[as.character(as.list(formula)[[2]])]]),
                          function(x) mutable(formula, data = x,
                                              colname = x[[as.character(formula)[[2]]]][1])))


  last <- mutable(formula, data, colname = "Combined")
                            
#  last <- mutable(summary.function = muStratTest, colname = "P-value")
#  first + middle + last
  first + middle + last
}











