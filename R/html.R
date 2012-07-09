html.mutable <- function(object, na.print = "<td></td>", file = "", headerFunction = muHTMLHeader,
                         footerFunction = muHTMLFooter, caption = "", summary = "",
                         completeDocument = FALSE,
                         documentHeaderFunction = muHTMLDocHeader,
                         documentFooterFunction = muHTMLDocFooter,
                         cssFile = "", 
                         ...) {
  x <- object$markup$html
  x[is.na(x)] <- na.print

  if(completeDocument)
    cat(paste(documentHeaderFunction(cssFile), collapse = "\n"), "\n", file = file, append = FALSE)
      
  cat(paste(headerFunction(x, caption, summary, ...), collapse = "\n"), "\n",
      file = file, append = completeDocument)
  cat(paste("",apply(x, 1, paste, collapse = " "), collapse = "</tr>\n"), "</tr>\n", file = file,
      append = TRUE)
  cat(paste(footerFunction(x, caption), collapse = "\n"), "\n", file = file, append = TRUE)

  if(completeDocument)
    cat(paste(documentFooterFunction(), collapse = "\n"), "\n", file = file, append = TRUE)
}

muHTMLDocHeader <- function(cssFile) {
  c("<html>",
    "<head>",
    paste("<link rel = \"stylesheet\" type = \"text/css\" href = \"", cssFile, "\" </>"),
    "<script type=\"text/javascript\" src=\"../../../../../../../mathjax/MathJax.js\">",
    "<!--/*--><![CDATA[/*><!--*/",
    "MathJax.Hub.Config({",
    "// Only one of the two following lines, depending on user settings",
    "// First allows browser-native MathML display, second forces HTML/CSS",
    "//  config: [\"MMLorHTML.js\"], jax: [\"input/TeX\"],",
    "    jax: [\"input/TeX\", \"output/HTML-CSS\"],",
    "    extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",",
    "                 \"TeX/noUndefined.js\"],",
    "    tex2jax: {",
    "        inlineMath: [ [\"\\(\",\"\\)\"] ],",
    "        displayMath: [ ['$$','$$'], [\"\\[\",\"\\]\"] ],",
    "        skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],",
    "        ignoreClass: \"tex2jax_ignore\",",
    "        processEscapes: false,",
    "        processEnvironments: true,",
    "        preview: \"TeX\"",
    "    },",
    "    showProcessingMessages: true,",
    "    displayAlign: \"center\",",
    "    displayIndent: \"2em\",",
    "    \"HTML-CSS\": {",
    "         scale: 100,",
    "         availableFonts: [\"STIX\",\"TeX\"],",
    "         preferredFont: \"TeX\",",
    "         webFont: \"TeX\",",
    "         imageFont: \"TeX\",",
    "         showMathMenu: true,",
    "    },",
    "    MMLorHTML: {",
    "         prefer: {",
    "             MSIE:    \"MML\",",
    "             Firefox: \"MML\",",
    "             Opera:   \"HTML\",",
    "             other:   \"HTML\"",
    "         }",
    "    }",
    "});",
    "/*]]>*///-->",
    "</script>",
    "</head>",
    "<body>")
}

muHTMLDocFooter <- function() {
  c("</body>",
    "</html>")
}

muHTMLHeader <- function(x, caption, summary, ...) {
  c(paste("<h2> mutable Output </h2>", 
      "<table summary = \"", summary, "\">"),
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

muExportHTML.muStratSummaryNumeric <- function(x, name, data, colname, round.digits = 1, ...) {
  colname <- gsub(" +", "", colname)

  ret <- paste(ps("<td class = \"continuous-cell\" id = \"", colname, "-", name,
                     "\"> $$\\scriptsize{"),
               round(x[1], round.digits), "}\\;\\normalsize{", 
               round(x[2], round.digits), "}\\;\\scriptsize{",
               round(x[3], round.digits), "}$$</td>")

  names(ret) <- name
  ret
}

muExportHTML.muStratSummaryFactor <- function(x, name, data, colname, round.digits = 0, ...) {
  colname <- gsub(" +", "", colname)

  dft <- as.data.frame(as.table(x))
  pct <- ps(round(x / sum(x) * 100, round.digits), "\\%")

  val <- c(ps("<td class = \"factor-heading-cell\" id = \"", colname, "-" , name, "\"",
              "></td>"),
           ps(ps("<td class = \"factor-level-cell\" id = \"", colname, "-", name, names(x)),
              "\"> $$", pct,
              "\\;\\;\\frac{", ps(dft[["Freq"]], "}{", sum(x), "}$$ </td>")))

  names(val) <- c(name, ps(name, names(x)))
  val
}

muExportHTML.muResponseSummaryNumeric <- muPrintIdentity
muExportHTML.muResponseSummaryFactor <- muPrintIdentity

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
  ret <- ps("<td id = \"pval-", name, "\"", ">$$", pval, "$$</td>")
  names(ret) <- name
  ret
}

muExportHTML.muStratTestNumeric <- muFormatPvalueHTML
muExportHTML.muStratTestFactor <- muFormatPvalueHTML
muExportHTML.muResponseTestNumeric <- muFormatPvalueHTML
muExportHTML.muResponseTestFactor <- muFormatPvalueHTML

muExportHTML.default <- muPrintIdentity
