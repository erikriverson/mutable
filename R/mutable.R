
ps <- function(...) paste(..., sep = "")

#' @export
generateRownames <- function(var, ...) {
  UseMethod("generateRownames")
}

#' @S3method generateRownames default
generateRownames.default <- function(var, varName , ...) {
  varName
}

#' @S3method generateRownames factor
generateRownames.factor <- function(var, varName, ...) {
  c(varName, ps(varName, names(table(var))))
}

#' @export
muInsertRow <- function(mat, row, after) {
  if(length(row) == 1)
    row <- c(row, rep("", times = ncol(mat) - 1))
  
  if(after >= 1) 
  rbind(mat[1:after, ],
        row,
        mat[(after+1):nrow(mat),])
  else
    rbind(row, mat)
}

#' @export
mutable <- function(x, ...) {
  UseMethod("mutable")
}

#' @S3method mutable function
mutable.function <- function(x, ...) {
  if(!missing(x)) {
    do.call(x, list(...))
  } else ## a named function argument (e.g., summary.function)
    mutable.default(...)  
}

#' @export
`+.mutable` <- function(x, y) {

  frame <- y$frame
  y$frame <- NULL

  if(!is.null(attr(y, "resolve")) && attr(y, "resolve")) {
    
    if(is.null(y$data))
      y$data <- x$data

    if(is.symbol(y$data))
      y$data <- eval(y$data)

    if(is.null(y$formula))
      y$formula <- x$formula

    if(is.null(y$summary.function)) {
      y$summary.function <- x$summary.function
    }
    
    if(is.null(y$markup.functions))
      y$markup.functions <- x$markup.functions

    y <- do.call(mutable.formula, y, envir = frame)
  }
  
  addelement <- function(x, y) {
    if(!is.matrix(y))
      y <- as.matrix(y)

    rx <- rownames(x, do.NULL = FALSE)
    ry <- rownames(y, do.NULL = FALSE)
    
    un <- union(rx, ry)

    xx <- as.data.frame(unclass(as.matrix(x)))
    yy <- as.data.frame(unclass(as.matrix(y)))

    xx[[".rns"]] <- rx
    yy[[".rns"]] <- ry

    ## base::merge because of my own merge with message()
    mydf <- base::merge(xx, yy, by = ".rns", all = TRUE) 
    mydf <- mydf[match(un, mydf[[".rns"]]), ]
    rns <- mydf[[".rns"]]
    ret <- as.matrix(mydf[!names(mydf) %in% ".rns"])
    rownames(ret) <- rns
    ret
  }
  mapvars.x <- names(x$markup)
  mapvars.y <- names(y$markup)

  if((length(mapvars.x) != length(mapvars.y)) ||
     (any(sort(mapvars.x) != sort(mapvars.y))))
    stop(paste("There are different markup elements in the tables. The left-hand table contains
markup for (", ps(mapvars.x, collapse = " "), ").",
               "The right-hand table contains markup for (",
               paste(mapvars.y, collapse = " "), ").
To combine two tables, they must contain the same markup elements.",
               collapse = ""))

  ret <- list()

  ret$markup <- mapply(addelement, x$markup[mapvars.x], y$markup[mapvars.x],
                       SIMPLIFY = FALSE)
  ret$formula <- x$formula
  ret$data <- x$data

  ret$markup.functions <- x$markup.functions
  ret$summary.function <- x$summary.function
  ret$summaries <- c(x[["summaries"]], y[["summaries"]])
  
  attr(ret, "resolve") <- FALSE
  class(ret) <- c("mutable")
  ret
}

#' @S3method mutable default
mutable.default <- function(x, ...) {
  m <- as.list(match.call())[-1]
  if(!"data" %in% names(m))
    lst <- c(list(formula = NULL, data = NULL), m)
  else
    lst <- c(list(formula = NULL), m)

  lst <- c(lst, frame = parent.frame())
  attr(lst, "resolve") <- TRUE
  class(lst) <- "mutable"
  lst
}

#' @S3method mutable formula
mutable.formula <- function(formula, data,
                            summary.function,
                            markup.functions, 
                            post.summary.hook,
                            post.markup.hook, 
                            colname,
                            na.action,
                            drop = FALSE,
                            ...) {

  ## if there was no data argument, we have to go find it first return
  ## from function knowing this, and resolve the data argument later,
  ## since we know `+.mutable.` must have been called. 
  
  if(missing(data) || missing(markup.functions)) {
    lst <- c(as.list(match.call()[-1]),
             frame = parent.frame())
    attr(lst, "resolve") <- TRUE
    class(lst) <- "mutable"
    return(lst)
  }


  mf.call <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "na.action"), names(mf.call), 0L)
  mf.call <- mf.call[c(1L, m)]

  if(missing(na.action))
    mf.call$na.action <- na.pass
  
  mf.call[[1L]] <- as.name("model.frame")
  mf <- eval(mf.call, parent.frame())

  if(drop) {
    ind <- sapply(mf, is.factor)
    mf[ind] <- lapply(mf[ind], "[", drop = TRUE)
    ind <- sapply(data, is.factor)
    data[ind] <- lapply(data[ind], "[", drop = TRUE)
  }

  Terms <- terms(formula)
  columnVariable <- attr(Terms, "response")

  if(columnVariable != 0)
    df.vars <- mf[-columnVariable]                 #drop arg?
  else {
    df.vars <- mf
    mf$fakeColumnVariable <- gl(1, nrow(mf), labels = "")
    columnVariable <- match("fakeColumnVariable", names(mf)) 
  }

  tableData <- list(rowVariables = df.vars,
                    columnVariable = mf[[columnVariable]])

  internalRownames <- unlist(mapply(generateRownames, tableData$rowVariables,
                                    names(tableData$rowVariables),
                                    SIMPLIFY = FALSE))

  ret <- lapply(tableData$rowVariables, summary.function,
                tableData$columnVariable, data, ...)

  if(!missing(post.summary.hook))
    ret <- post.summary.hook(ret)
  
  markupFunction <- function(mf) {
    markup.ret <- NULL
    for(i in 1:length(ret)) {
      val <- mf(ret[[i]], names(ret)[i], data, colname = colname)
      markup.ret <- c(markup.ret, val)
    }
    markup.ret <- as.matrix(markup.ret[internalRownames], ncol = 1)
    rownames(markup.ret) <- internalRownames
    colnames(markup.ret) <- colname
    markup.ret
  }

  markup <- lapply(markup.functions, markupFunction)

  return.list <- c(markup = list(markup),
                   formula = formula,
                   data = list(data),
                   markup.functions = list(markup.functions),
                   summary.function = summary.function,
                   summaries = list(list(ret)))

  if(!missing(post.markup.hook))
    return.list <- post.markup.hook(return.list, ret, ...)
  
  attr(return.list, "resolve") <- FALSE
  class(return.list) <- c("mutable")
  return.list
}

#' @S3method print mutable
print.mutable <- function(x, quote = FALSE, na.print = "--", print.rownames = FALSE, ...) {
  x <- x$markup[["plain"]]

  if(is.null(x)) {
    stop("No plain text table present in this object\n")
  }
  
  if(!print.rownames)
    rownames(x) <- NULL
  print.default(unclass(x), quote = quote, na.print = na.print, ...)
}

#' @export
muPrintIdentity <- function(x, name, data, ...) {
  ret <- x
  names(ret) <- name
  ret
}

#' @export
muFormatPvalue <- function(x, name, data, threshold = 0.0001, ...) {
  val <- ifelse(x$pvalue < threshold, paste("<", threshold), x$pvalue)
  names(val) <- name
  val
}

#' @S3method summary mutable
summary.mutable <- function(x) {
  if(!is.null(x$formula)) {
    cat("\nCall:\n", deparse(x$formula), "\n\n")
  }

  cat(paste(length(x$summaries), "Columns:"), "\n")
  cat(paste(" ", colnames(x$markup[["plain"]]), collapse = "\n"),
      "\n\n")

  cat(paste(length(x$markup), "markup objects:", "\n"))
  cat(paste(" ", names(x$markup), collapse = "\n"), "\n")
}

is.mutable <- function(x) {
  if("mutable" %in% class(x))
    TRUE
  else FALSE
}
