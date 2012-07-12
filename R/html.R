html.mutable <- function(object, na.print = "<td></td>", file = "",
                         headerFunction = muHTMLHeader,
                         footerFunction = muHTMLFooter,
                         documentHeaderFunction = muHTMLDocHeader,
                         documentFooterFunction = muHTMLDocFooter,
                         caption = "", 
                         completeDocument = FALSE,
                         cssFile = "",
                         ...) {

  x <- object$markup[["html"]]

  if(is.null(x)) {
    cat("No HTML table present in this object\n")
    return()
  }
  
  x[is.na(x)] <- na.print
  newline <- "\n"

  if(completeDocument)
    cat(paste(documentHeaderFunction(cssFile), collapse = newline),
        newline, file = file, append = FALSE)
      
  cat(paste(headerFunction(x, caption, ...),
            collapse = newline), newline,
      file = file, append = completeDocument)
  
  cat(paste("", apply(x, 1, paste, collapse = " "),
            collapse = ps("</tr>", newline)),
            ps("</tr>", newline), file = file,
            append = TRUE)
  
  cat(paste(footerFunction(x, caption), collapse = newline), newline,
      file = file, append = TRUE)

  if(completeDocument)
    cat(paste(documentFooterFunction(), collapse = newline), newline,
        file = file, append = TRUE)
}


muPrintIdentityHTML <- function(x, name, data, ...) {
  ret <- ps("<td>", muPrintIdentity(x, name, data...), "</td>")
  names(ret) <- name
  ret
}

muHTMLDocHeader <- function(cssFile) {
  c("<html>",
    "<head>",
    ps('<link rel = "stylesheet" type = "text/css" href = "', cssFile, '" </>'),
    "</head>",
    "<body>")
}

muHTMLDocFooter <- function() {
  c("</body>",
    "</html>")
}

muHTMLHeader <- function(x, caption, ...) {
  c("<table>", 
    paste("<caption>", caption, "</caption>"),
    "<colgroup>",
    ps("<col id = \"", gsub(" +", "", colnames(x)), "\" />"),
    "</colgroup>",
    "<tr>",
    paste("<th scope = \"col\">", colnames(x), "</th>"),
    "</tr>")
}

muHTMLFooter <- function(x, caption) {
  c("</table>")
}

muExportHTML <- function(x, ...) {
  UseMethod("muExportHTML")
}

muExportHTML.muStratSummaryNumeric <- function(x, name, data, colname,
                                               round.digits = 1, ...) {
  colname <- gsub(" +", "", colname)

  ret <- paste(ps('<td class = "continuous-cell" id = "', colname, '-', name, '"',
                     ">  \\scriptsize{"),
               round(x[1], round.digits), "}\\;\\normalsize{", 
               round(x[2], round.digits), "}\\;\\scriptsize{",
               round(x[3], round.digits), "}  </td>")

  names(ret) <- name
  ret
}

muExportHTML.muStratSummaryMean <- function(x, name, data, colname,
                                            round.digits = 1, ...) {
  colname <- gsub(" +", "", colname)

  ret <- paste(ps('<td class = "continuous-cell" id = "', colname, '-', name, '"',
                     ">  "),
               round(x[1], round.digits), "  ( ",
               round(x[2], round.digits), "  )</td>")

  names(ret) <- name
  ret
}

muExportHTML.muStratSummaryFactor <- function(x, name, data, colname,
                                              round.digits = 0, ...) {
  colname <- gsub(" +", "", colname)

  dft <- as.data.frame(as.table(x))
  pct <- ps(round(x / sum(x) * 100, round.digits), "\\%")

  val <- c(ps("<td class = \"factor-heading-cell\" id = \"", colname, "-" , name, "\"",
              ");\"></td>"),
           ps(ps("<td class = \"factor-level-cell\" id = \"", colname, "-", name, names(x)),
              "\">  ",
              muExportPlain(x, name, data, round.digits, ...),
              "</td>"))

  names(val) <- c(name, ps(name, names(x)))
  val
}

muExportHTML.muRownamesFactor <- function(x, name, data, ...) {
  ret <- c(x[1], paste("&nbsp;", tail(x, length(x) - 1)))

  ret <- c(paste("<tr class= \"factor-heading factor\"><th scope = \"row\">",
                 ret[1], "</th>"),
           paste("<tr class= \"factor-level factor\"><th scope = \"row\">",
                 tail(ret, length(ret) - 1), "</th>"))
  
  names(ret) <- c(name, ps(name,  levels(data[[name]])))
  ret
}

muExportHTML.muRownamesNumeric <- function(x, name, data, ...) {
  ret <- paste("<tr class = \"continuous\"><th scope = \"row\">", x, "</th>")
  names(ret) <- name
  ret
}

muFormatPvalueHTML <- function(x, name, data, colname, ...) {
  pval <- muFormatPvalue(x, name, data)
  ret <- ps("<td id = \"pval-", name, "\"", "> ", pval, " </td>")
  names(ret) <- name
  ret
}

muExportHTML.muStratTestNumeric <- muFormatPvalueHTML
muExportHTML.muStratTestFactor <- muFormatPvalueHTML
muExportHTML.muResponseTestNumeric <- muFormatPvalueHTML
muExportHTML.muResponseTestFactor <- muFormatPvalueHTML

muExportHTML.default <- muPrintIdentityHTML
muExportHTML.muResponseSummaryNumeric <- muPrintIdentityHTML
muExportHTML.muResponseSummaryFactor <- muPrintIdentityHTML
