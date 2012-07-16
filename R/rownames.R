#' @export
muRownames <- function(x, ...) {
  UseMethod("muRownames")
}

#' @S3method muRownames default
muRownames.default <- function(x, stratVariable, data, 
                               useVarName = TRUE, ...) {
  lab <- label(x)

  if((lab == "") && useVarName)
    lab <- "Unlabeled variable"
    
  class(lab) <- "muRownamesNumeric"
  lab
}

#' @S3method muRownames factor
muRownames.factor <- function(x, stratVariable, data, 
                              useVarName = TRUE, ...) {
  lab <- label(x)

  if((lab == "") && useVarName)
    lab <- "Unlabeled variable"
  
  ret <- c(lab, levels(x))
  class(ret) <- "muRownamesFactor"
  ret
  
}
