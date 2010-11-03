muStratTestLatex <- function(x, name, data, colname, ...) {
  val <- ifelse(x$pvalue < .0001, ps("$< .0001^{", x$testsuper, "}$"),
                ps("$", x$pvalue, "^{", x$testsuper, "}$"))
  names(val) <- name
  val
}

muStratLatex <- function(x, ...) {
  UseMethod("muStratLatex")
}

muStratLatex.default <- function(x, name, data, round.digits = 1, ...) {
  ret <- paste(sprintf(ps("%.", round.digits, "f"), x[2]),
               "$(", 
               sprintf(ps("%.", round.digits, "f"), x[1]),
               "$ -- $",
               sprintf(ps("%.", round.digits, "f"), x[3]),
               ")$", 
               sep = " ")
  names(ret) <- name
  ret
}

muStratLatex.table <- function(x, name, data, round.digits = 0, ...) {
  dft <- as.data.frame(x)

  pct <- paste(sprintf(ps("%.", round.digits, "f"), x / sum(x) * 100), "\\%", sep = "")

  val <- paste(pct, paste("{\\scriptsize~$\\frac{",
                          dft[["Freq"]],
                          "}{",
                          sum(x),
                          "}$}",
                          sep = ""))

  names(val) <- paste(name, names(x), sep = "")
  val
}


muResponseLatex <- function(x, ...) {
  UseMethod("muResponseLatex")
}

muResponseLatex.default <- function(x, name, data, round.digits = 2, ...) {
  x <- sprintf(ps("%.", round.digits, "f"), x)
  val <- ps(x[1], " $(", x[2], "$ -- $", x[3], ")$")
  names(val) <- name
  val
}

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
                         footerFunction = muLatexFooterTabular, caption = "",
                          no.table.markup.regex = c("multicol"),
                          ...) {
  x <- object$latex
  x[is.na(x)] <- na.print
  
  cat(paste(headerFunction(x, caption, ...),
            collapse = "\n"), "\n", file = file)

  body <- apply(x, 1, paste, collapse = "&")

  fix.rows <- grep(no.table.markup.regex, body)
  body[fix.rows] <- x[fix.rows, 1]

  body.vec <- paste(body, collapse = "\\\\\n")
  cat(body.vec, "\\\\\n", file = file, append = TRUE)
  
  cat(paste(footerFunction(x, caption, ...), collapse = "\n"),
      "\n", file = file, append = TRUE)
  
}

muLatexHeaderTabular <- function(x, caption, collabel.just, colhead2, size = "\\small", ...) {
  if(missing(collabel.just))
    collabel.just <- paste(c("l", rep("c", ncol(x) - 1)), collapse = "")
  
  c("{\\footnotesize",
    "\\begin{table}", size,
    ps("\\caption{", caption , "}"),
    ps("\\begin{tabular}{", collabel.just, "}"),
    "\\hline\\hline",
    c(paste(paste("\\multicolumn{1}{c}{", colnames(x), "}", collapse = "&"), "\\\\")),
    if(!missing(colhead2))
    c(paste(paste("\\multicolumn{1}{c}{", colhead2, "}", collapse = "&"), "\\\\")),
    "\\hline")
}

muLatexFooterTabular <- function(x, caption, footnote, ...) {
    c("\\hline",
      if(!missing(footnote))
      ps("\\multicolumn{",
         ncol(x),
         "}{p{\\linewidth}}{", footnote, "}\\\\"),
      "\\end{tabular}",
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
