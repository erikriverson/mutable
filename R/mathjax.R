muPrintIdentityHTMLMathJax <- function(x, name, data, ...) {
  ret <- ps("<td> \\(", muPrintIdentity(x, name, data...), "\\) </td>")
  names(ret) <- name
  ret
}

muHTMLMathJaxDocHeader <- function(cssFile) {
  c("<html>",
    "<head>",
    paste("<link rel = \"stylesheet\" type = \"text/css\" href = \"", cssFile, "\" </>"),
    "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js\">",
    "<!--/*--><![CDATA[/*><!--*/",
    "MathJax.Hub.Config({",
    "// Only one of the two following lines, depending on user settings",
    "// First allows browser-native MathML display, second forces HTML/CSS",
    "//  config: [\"MMLorHTML.js\"], jax: [\"input/TeX\"],",
    "    jax: [\"input/TeX\", \"output/HTML-CSS\"],",
    "    extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",",
    "                 \"TeX/noUndefined.js\"],",
    "    tex2jax: {",
    "        inlineMath: [ ['\\\\(','\\\\)'] ]", 
    "        displayMath: [ ['$$','$$'], ['\\[','\\]'] ],",
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

muExportHTMLMathJax <- function(x, ...) {
  UseMethod("muExportHTMLMathJax")
}

muExportHTMLMathJax.muStratSummaryNumeric <- function(x, name, data, colname,
                                               round.digits = 1, ...) {
  colname <- gsub(" +", "", colname)

  ret <- paste(ps('<td class = "continuous-cell" id = "', colname, '-', name, '"',
                     "> \\( \\scriptsize{"),
               round(x[1], round.digits), "}\\;\\normalsize{", 
               round(x[2], round.digits), "}\\;\\scriptsize{",
               round(x[3], round.digits), "} \\) </td>")

  names(ret) <- name
  ret
}

muExportHTMLMathJax.muStratSummaryMean <- function(x, name, data, colname,
                                            round.digits = 1, ...) {
  colname <- gsub(" +", "", colname)

  ret <- paste(ps('<td class = "continuous-cell" id = "', colname, '-', name, '"',
                     "> \\( "),
               round(x[1], round.digits), " \\) ( \\(",
               round(x[2], round.digits), " \\) )</td>")

  names(ret) <- name
  ret
}

muExportHTMLMathJax.muStratSummaryFactor <- function(x, name, data, colname,
                                              round.digits = 0, ...) {
  colname <- gsub(" +", "", colname)

  dft <- as.data.frame(as.table(x))
  pct <- ps(round(x / sum(x) * 100, round.digits), "\\%")

  val <- c(ps("<td class = \"factor-heading-cell\" id = \"", colname, "-" , name, "\"",
              ");\"></td>"),
           ps(ps("<td class = \"factor-level-cell\" id = \"", colname, "-", name, names(x)),
              "\"> \\( ", pct,
              "\\;\\;\\frac{", ps(dft[["Freq"]], "}{", sum(x), "} \\) </td>")))

  names(val) <- c(name, ps(name, names(x)))
  val
}


muExportHTMLMathJax.muRownamesFactor <- function(x, name, data, ...) {
  ret <- c(x[1], paste("&nbsp;", tail(x, length(x) - 1)))

  ret <- c(paste("<tr class= \"factor-heading factor\"><th scope = \"row\">",
                 ret[1], "</th>"),
           paste("<tr class= \"factor-level factor\"><th scope = \"row\">",
                 tail(ret, length(ret) - 1), "</th>"))
  
  names(ret) <- c(name, ps(name,  levels(data[[name]])))
  ret
}

muExportHTMLMathJax.muRownamesNumeric <- function(x, name, data, ...) {
  ret <- paste("<tr class = \"continuous\"><th scope = \"row\">", x, "</th>")
  names(ret) <- name
  ret
}

muFormatPvalueHTMLMathJax <- function(x, name, data, colname, ...) {
  pval <- muFormatPvalue(x, name, data)
  ret <- ps("<td id = \"pval-", name, "\"", "> \\(", pval, "\\) </td>")
  names(ret) <- name
  ret
}

muExportHTMLMathJax.muResponseSummaryNumeric <- muPrintIdentityHTMLMathJax
muExportHTMLMathJax.muResponseSummaryFactor <- muPrintIdentityHTMLMathJax
muExportHTMLMathJax.muStratTestNumeric <- muFormatPvalueHTMLMathJax
muExportHTMLMathJax.muStratTestFactor <- muFormatPvalueHTMLMathJax
muExportHTMLMathJax.muResponseTestNumeric <- muFormatPvalueHTMLMathJax
muExportHTMLMathJax.muResponseTestFactor <- muFormatPvalueHTMLMathJax
muExportHTMLMathJax.default <- muPrintIdentityHTMLMathJax
muHTMLMathJaxDocFooter <- muHTMLDocFooter
