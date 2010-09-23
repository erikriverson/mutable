## try to replicate an Hmisc type table with a function
eN <- function(x, strat, data, ...) {
  length(x)
}

tab1 <- etable(form, data = pead.bl, colname = "Variable",
              summary.function = erownames) +
  etable(summary.function = eN, colname = "N") +
  etable(colname = "Combined") +
  etable(subset = hiv == "Positive", colname = "Positive") +
  etable(subset = hiv == "Negative", colname = "Negative") +
  etable(elm, update(form, diffs ~ .), data = pead.bl) +
  etable(summary.function = etest, colname = "P-value")

tab1


eStratTable <- function(formula, data) {
  
  first <- etable(formula, data = data, colname = "Variable",
                  summary.function = erownames) +
           etable(summary.function = eN, colname = "N") +
           etable(colname = "Combined")

  ## this needs the bound argument to be data, but that creates
  ## a problem for combining calls from completely separate
  ## datasets, need to deduce what pfargs is doing, and if
  ## I can use environments or some such thing to get at the
  ## proper data.frame
  
  middle <- Reduce("+", lapply(split(data, data[[as.character(as.list(form)[[2]])]]),
                               function(data) etable(formula, data = data)))
                            
  last <- etable(summary.function = etest, colname = "P-value")
  first + middle + last
}

eStratTable(form, pead.bl)
