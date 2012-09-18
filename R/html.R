#' @S3method html mutable
html.mutable <- function(object, na.print = "<td></td>", file = "",
                         headerFunction = muHTMLTableHeader,
                         markupFunction = muHTMLMarkupGenerator, 
                         footerFunction = muHTMLTableFooter,
                         documentHeaderFunction = muHTMLDocHeader,
                         documentFooterFunction = muHTMLDocFooter,
                         caption = "",
                         footnote = "", 
                         completeDocument = FALSE,
                         cssFile = NULL,
                         markupElement = "html", 
                         ...) {

  x <- object$markup[[markupElement]]

  if(is.null(x)) {
    stop("No HTML table present in this object\n")
  }
  
  x[is.na(x)] <- na.print
  newline <- "\n"

  if(completeDocument)
    cat(paste(documentHeaderFunction(cssFile), collapse = newline),
        newline, file = file, append = FALSE)
      
  cat(paste(headerFunction(x, caption, footnote, ...),
            collapse = newline), newline,
      file = file, append = completeDocument)

  markupFunction(x, file)
  
  cat(paste(footerFunction(x), collapse = newline), newline,
      file = file, append = TRUE)

  if(completeDocument)
    cat(paste(documentFooterFunction(), collapse = newline), newline,
        file = file, append = TRUE)

  ret <- list(file = file, object = object)
  class(ret) <- "mutableHTML"
  invisible(ret)
}

muHTMLMarkupGenerator <- function(x, file) {
  cat(paste("", apply(x, 1, paste, collapse = " "),
            collapse = ps("</tr>", newline)),
            ps("</tr>", newline), file = file,
            append = TRUE)

}

#' @export
print.mutableHTML <- function(x, completeDocument = TRUE, browser) {

  if(x$file == "") {
    filename <- tempfile(pattern = "mutable", fileext = ".html")
    ## but need to write out the HTML to the file, and how do we get
    ## it?
    html(x$object, file = filename, completeDocument = completeDocument)
  } else
    filename <- normalizePath(x$file)

  if(missing(browser))
    browser <- getOption("browser")

  browseURL(ps("file://", filename), browser)
}


#' @export
muPrintIdentityHTML <- function(x, name, data, ...) {
  ret <- ps("<td>", muPrintIdentity(x, name, data...), "</td>")
  names(ret) <- name
  ret
}

muHTMLDocHeader <- function(cssFile) {
  c("<!DOCTYPE HTML>",
    "<html>",
    "<head>",
    if(!is.null(cssFile))
    ps('<link rel = "stylesheet" type = "text/css" href = "', cssFile, '" </>'),
    "</head>",
    "<body>")
}

muHTMLDocFooter <- function() {
  c("</body>",
    "</html>")
}

muHTMLTableHeader <- function(x, caption, footnote, ...) {
  c("<table class = \"data table\">", 
    paste("<caption>", caption, "</caption>"),
    "<colgroup>",
    ps("<col id = \"", gsub(" +", "", colnames(x)), "\" />"),
    "</colgroup>",
    "<thead>",
    "<tr>",
    paste("<th scope = \"col\">", colnames(x), "</th>"),
    "</tr>",
    "</thead>",
    "<tfoot>",
    "<tr>",
    "<td></td>",
    "<td colspan=0>", 
    footnote,
    "</td>",
    "</tr>",
    "</tfoot>",
    "<tbody>")
}

muHTMLTableFooter <- function(x) {
  c("</tbody>",
    "</table>")
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
              "></td>"),
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
