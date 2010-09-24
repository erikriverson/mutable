html.etable <- function(x, na.print = "<td></td>", file = "", headerFunction = eHTMLHeader,
                        footerFunction = eHTMLFooter, caption = "", summary = "",
                        completeDocument = FALSE,
                        documentHeaderFunction = eHTMLDocHeader,
                        documentFooterFunction = eHTMLDocFooter,
                        cssFile = "", 
                        ...) {
  x <- x$html
  x[is.na(x)] <- na.print

  if(completeDocument)
    cat(paste(documentHeaderFunction(cssFile), collapse = "\n"), "\n", file = file, append = FALSE)
      
  cat(paste(headerFunction(x, caption, collabel.just, summary, ...), collapse = "\n"), "\n",
      file = file, append = completeDocument)
  cat(paste("",apply(x, 1, paste, collapse = " "), collapse = "</tr>\n"), "</tr>\n", file = file,
      append = TRUE)
  cat(paste(footerFunction(x, caption), collapse = "\n"), "\n", file = file, append = TRUE)

  if(completeDocument)
    cat(paste(documentFooterFunction(), collapse = "\n"), "\n", file = file, append = TRUE)
}

eHTMLDocHeader <- function(cssFile) {
  c("<html>",
    "<head>",
    paste("<link rel = \"stylesheet\" type = \"text/css\" href = \"", cssFile, "\" </>"),
    "<script type=\"text/javascript\" src=\"MathJax/MathJax.js\">",
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

eHTMLDocFooter <- function() {
  c("</body>",
    "</html>")
}

eHTMLHeader <- function(x, caption, collabel.just, summary, ...) {
  c(paste("<table summary = \"", summary, "\">"),
    paste("<caption>", caption, "</caption>"),
    "<tr>",
    paste("<th scope = \"col\">", colnames(x), "</th>"),
    "</tr>")
}

eHTMLFooter <- function(x, caption) {
  c("</table>")
}

ehtml <- function(x, ...) {
  UseMethod("ehtml")
}

ehtml.default <- function(x, name, data, round.digits = 1, ...) {
  ret <- paste("<td>$$\\scriptsize{", round(x[1], round.digits), "}\\;\\normalsize{", 
               round(x[2], round.digits), "}\\;\\scriptsize{",
               round(x[3], round.digits), "}$$</td>",
               sep = " ")
  names(ret) <- name
  ret
}

ehtml.table <- function(x, name, data, round.digits, ...) {
  dft <- as.data.frame(x)
  pct <- paste(round(x / sum(x) * 100, round.digits), "\\%", sep = "")
  val <- paste("<td> $$", pct,
               "\\;\\;\\frac{", paste(dft[["Freq"]],
                                      "}{", sum(x), "}$$ </td>",
                                      sep = ""))
  names(val) <- paste(name, names(x), sep = "")
  val
}

ehtmlrownames <- function(x, ...) {
  UseMethod("ehtmlrownames")
}

ehtmlrownames.special <- function(x, name, data, ...) {
  ret <- c(x[1], paste("&nbsp;", tail(x, length(x) - 1)))

  ret <- c(paste("<tr class=\"factor-heading\"><th scope = \"row\">",
                 ret[1], "</th>"),
           paste("<tr class=\"factor-level\"><th scope = \"row\">",
                 tail(ret, length(ret) - 1), "</th>"))
  
  names(ret) <- c(name, paste(name,  levels(data[[name]]), sep = ""))
  ret
}

ehtmlrownames.character <- function(x, name, data, ...) {
  ret <- paste("<tr><th scope = \"row\">", x, "</th>")
  names(ret) <- name
  ret
}

ehtmltest <- function(x, name, data, ...) {
  ret <- paste("<td>$$", x, "$$</td>")
  names(ret) <- name
  ret
}

attr(erownames, "html.function") <-  ehtmlrownames
attr(etest, "html.function") <- ehtmltest





