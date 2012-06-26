muExportLatex <- function(x, ...) {
  UseMethod("muExportLatex")
}

muFormatPvalueLatex <- function(x, name, data, colname, ...) {
  pval <- muFormatPvalue(x, name, data)
  val <- ps("$", pval, "^{", x$testsuper, "}$")
  names(val) <- name
  val
}

muExportLatex.muStratTestNumeric <- muFormatPvalueLatex
muExportLatex.muStratTestFactor <- muFormatPvalueLatex
muExportLatex.muResponseTestNumeric <- muFormatPvalueLatex
muExportLatex.muResponseTestFactor <- muFormatPvalueLatex

muExportLatex.muStratSummaryNumeric <- function(x, name, data, round.digits = 1, ...) {
  ret <- paste(sprintf(ps("%.", round.digits, "f"), x[2]),
               "$(", 
               sprintf(ps("%.", round.digits, "f"), x[1]),
               "$ -- $",
               sprintf(ps("%.", round.digits, "f"), x[3]),
               ")$")

  names(ret) <- name
  ret
}

muExportLatex.muStratSummaryFactor <- function(x, name, data, round.digits = 0, ...) {
  dft <- as.data.frame(as.table(x))

  pct <- ps(sprintf(ps("%.", round.digits, "f"), x / sum(x) * 100), "\\%")

  val <- ps(pct, paste("{\\scriptsize~$\\frac{",
                       dft[["Freq"]],
                       "}{",
                       sum(x),
                       "}$}"))

  names(val) <- ps(name, names(x))
  val
}

muExportLatex.muResponseSummaryNumeric <- function(x, name, data, round.digits = 2, ...) {
  x <- sprintf(ps("%.", round.digits, "f"), x)
  val <- ps(x[1], " $(", x[2], "$ -- $", x[3], ")$")
  names(val) <- name
  val
}

muExportLatex.muResponseSummaryFactor <- function(x, name, data, round.digits = 0, ...) {
  val <- sapply(x, muExportLatex.muStratSummaryNumeric, name, data)
  names(val) <- ps(name, names(x))
  val
}

muExportLatex.muRownamesFactor <- function(x, name, data, ...) {
  ret <- c(x[1], paste("~~~", tail(x, length(x) - 1)))
  names(ret) <- c(name, ps(name, tail(x, length(x) - 1)))
  ret
}

muExportLatex.muRownamesNumeric <- muPrintIdentity

muLatexHeaderTabular <- function(x, caption, collabel.just, colhead2,
                                 location = "h", size = "\\small", ...) {
  if(missing(collabel.just))
    collabel.just <- paste(c("l", rep("c", ncol(x) - 1)), collapse = "")
  
  c("{\\footnotesize",
    "\\begin{table}[", location, "]", size,
    ps("\\caption{", caption , "}"),
    "\\begin{center}", 
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
      "\\end{center}", 
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


latex.mutable <- function(object, na.print = "", file = "",
                          headerFunction = muLatexHeaderTabular,
                          footerFunction = muLatexFooterTabular, caption = "",
                          no.table.markup.regex = c("multicol|hline"),
                          ...) {
  x <- object$markup$latex
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

muExportLatex.default <- muPrintIdentity
