muStratLatex <- function(x, ...) {
  UseMethod("muStratLatex")
}

muStratLatex.default <- function(x, name, data, round.digits = 1, ...) {
  ret <- paste(round(x[2], round.digits), "(", 
               round(x[1], round.digits), "-",
               round(x[3], round.digits), ")", 
               sep = " ")
  names(ret) <- name
  ret
}

muStratLatex.table <- function(x, name, data, round.digits = 0, ...) {
  dft <- as.data.frame(x)

  pct <- paste(round(x / sum(x) * 100, round.digits), "\\%", sep = "")

  val <- paste(pct, paste("$\\frac{",
                          dft[["Freq"]],
                          "}{",
                          sum(x),
                          "}$",
                          sep = ""))

  names(val) <- paste(name, names(x), sep = "")
  val
}


muResponseLatex <- function(x, ...) {
  UseMethod("muResponseLatex")
}

muResponseLatex.default <- muPrintIdentity

muResponseLatex.muResponseSummaryFactor <- function(x, name, data, round.digits = 0, ...) {
  val <- sapply(x, muStratLatex.default, name, data)
  names(val) <- paste(name, names(x), sep = "")
  val
}

muRownamesLatex <- function(x, ...) {
  UseMethod("muRownamesLatex")
}

muRownamesLatex.rowFactor <- function(x, name, data, ...) {
  ret <- c(x[1], paste("~~~", tail(x, length(x) - 1)))
  names(ret) <- c(name, paste(name,  levels(data[[name]]), sep = ""))
  ret
}

latex.mutable <- function(object, na.print = "", file = "", headerFunction = muLatexHeaderTabular,
                         footerFunction = muLatexFooterTabular, caption = "", ...) {
  x <- object$latex
  x[is.na(x)] <- na.print
  cat(paste(headerFunction(x, caption, ...), collapse = "\n"), "\n", file = file)
  cat(paste(apply(x, 1, paste, collapse = "&"), collapse = "\\\\\n"), "\\\\\n", file = file,
      append = TRUE)
  cat(paste(footerFunction(x, caption), collapse = "\n"), "\n", file = file, append = TRUE)
  
}

muLatexHeaderTabular <- function(x, caption, collabel.just) {
  if(missing(collabel.just))
    collabel.just <- paste(c("l", rep("c", ncol(x) - 1)), collapse = "")
  
  c("\\begin{table}",
    ps("\\begin{tabular}{", collabel.just, "}"),
    c(paste("\\multicolumn{1}{c}{", colnames(x), "}", collapse = "&"), "\\\\"),
    "\\hline\\\\")
}

muLatexFooterTabular <- function(x, caption) {
    c("\\end{tabular}",
      ps("\\caption{", caption , "}"),
      "\\end{table}")
  }

muLatexHeaderLongtable <- function(x, caption, collabel.just) {
  if(missing(collabel.just))
    collabel.just <- paste(c("l", rep("c", ncol(x) - 1)), collapse = "")
  c(paste("\\begin{longtable}{", collabel.just, "}"),
    paste("\\caption{", caption, "}\\\\"),
    "\\hline\\hline",
    c(paste("\\multicolumn{1}{c}{", colnames(x), "}", collapse = "&"), "\\\\"),
    "\\hline",
    "\\endhead",
    "\\hline")
}

muLatexFooterLongtable <- function(x, caption) {
  c("\\hline",
    "\\end{longtable}")
}


