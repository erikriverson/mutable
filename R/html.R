html.mutable <- function(object, na.print = "<td></td>", file = "", headerFunction = muHTMLHeader,
                         footerFunction = muHTMLFooter, caption = "", summary = "",
                         completeDocument = FALSE,
                         documentHeaderFunction = eHTMLDocHeader,
                         documentFooterFunction = eHTMLDocFooter,
                         cssFile = "", 
                         ...) {
  x <- object$html
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

muHTMLDocFooter <- function() {
  c("</body>",
    "</html>")
}

muHTMLHeader <- function(x, caption, summary, ...) {
  c(paste("<table summary = \"", summary, "\">"),
    paste("<caption>", caption, "</caption>"),
    "<colgroup>",
    paste("<col id = \"", gsub(" +", "", colnames(x)), "\" />", sep = ""),
    "</colgroup>",
    "<tr>",
    paste("<th scope = \"col\">", colnames(x), "</th>"),
    "</tr>")
}

muHTMLFooter <- function(x, caption) {
  c("</table>")
}

muStratHTML <- function(x, ...) {
  UseMethod("muStratHTML")
}

muStratHTML.default <- function(x, name, data, colname, round.digits = 1, ...) {
  colname <- gsub(" +", "", colname)

  ## png(paste("html/", colname, "-", name, ".png", sep = ""),
  ##     width = 200, height = 50, bg = "transparent")
  ## p1 <- qplot(data[[name]], geom = "density", colour = I("grey90"), fill = I("grey90")) +
  ##   opts(axis.text.x = theme_blank(),
  ##        axis.text.y = theme_blank(),
  ##        axis.ticks = theme_blank(),
  ##        plot.background = theme_rect("transparent", size = 0),
  ##        panel.background = theme_rect("transparent", size = 0),
  ##        plot.margin = unit(c(0, 0, -2, -2), "lines"),
  ##        axis.ticks.margin = unit(0, "lines")) +
  ##          labs(x = "", y = "")
  ## print(p1)
  ## dev.off()

  ret <- paste(paste("<td class = \"continuous-cell\" id = \"", colname, "-", name,
                     "\" style = \"background-image : url(\'", colname, "-", name, ".png\'",
                     ");\"",
                     ">$$\\scriptsize{", sep = ""),
               round(x[1], round.digits), "}\\;\\normalsize{", 
               round(x[2], round.digits), "}\\;\\scriptsize{",
               round(x[3], round.digits), "}$$</td>",
               sep = " ")
  names(ret) <- name
  ret
}

muStratHTML.table <- function(x, name, data, colname, round.digits = 0, ...) {
  colname <- gsub(" +", "", colname)

  ## png(paste("html/", colname, "-", name, ".png", sep = ""),
  ##     width = 200, height = 50, bg = "transparent")
  
  ## p1 <-qplot(data[[name]]) +
  ##   opts(axis.text.x = theme_blank(),
  ##        axis.text.y = theme_blank(),
  ##        axis.ticks = theme_blank(),
  ##        plot.background = theme_rect("transparent", size = 0),
  ##        panel.background = theme_rect("transparent", size = 0),
  ##        plot.margin = unit(c(0, 0, -2, -2), "lines")) +
  ##          labs(x = "", y = "")
  ## print(p1)
  ## dev.off()
  
  dft <- as.data.frame(x)
  pct <- paste(round(x / sum(x) * 100, round.digits), "\\%", sep = "")

  val <- c(paste("<td class = \"factor-heading-cell\" id = \"", colname, "-" , name, "\"",
                 "style = \"background-image : url(\'", colname, "-", name, ".png\'",
                 ");\"></td>", sep = ""),
           paste(paste("<td class = \"factor-level-cell\" id = \"", colname, "-", name, names(x), sep = ""),
                 "\"> $$", pct,
                 "\\;\\;\\frac{", paste(dft[["Freq"]],
                                        "}{", sum(x), "}$$ </td>",
                                        sep = ""), sep = ""))

  names(val) <- c(name, paste(name, names(x), sep = ""))
  val
}

muRownamesHTML <- function(x, ...) {
  UseMethod("muRownamesHTML")
}

muRownamesHTML.rowFactor <- function(x, name, data, ...) {
  ret <- c(x[1], paste("&nbsp;", tail(x, length(x) - 1)))

  ret <- c(paste("<tr class= \"factor-heading factor\"><th scope = \"row\">",
                 ret[1], "</th>"),
           paste("<tr class= \"factor-level factor\"><th scope = \"row\">",
                 tail(ret, length(ret) - 1), "</th>"))
  
  names(ret) <- c(name, paste(name,  levels(data[[name]]), sep = ""))
  ret
}

muRownamesHTML.character <- function(x, name, data, ...) {
  ret <- paste("<tr class = \"continuous\"><th scope = \"row\">", x, "</th>")
  names(ret) <- name
  ret
}

muStratTestHTML <- function(x, name, data, colname, ...) {
  ret <- paste("<td id = \"pval-", name, "\"", ">$$", x, "$$</td>", sep = "")
  names(ret) <- name
  ret
}






  
