muRownames <- function(x, ...) {
  UseMethod("muRownames")
}

muRownames.default <- function(x, stratVariable, data, 
                               useVarName = TRUE, ...) {
  lab <- label(x)

  if((lab == "") && useVarName)
    lab <- "unlabeled variable"
    
  class(lab) <- "muRownamesNumeric"
  lab
}

muRownames.factor <- function(x, stratVariable, data, 
                              useVarName = TRUE, ...) {
  lab <- label(x)

  if((lab == "") && useVarName)
    lab <- "unlabeled variable"
  
  ret <- c(lab, levels(x))
  class(ret) <- "muRownamesFactor"
  ret
  
}
