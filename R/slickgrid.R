library(Hmisc)
library(RJSONIO)

df1 <- data.frame(a = 1:10, b = 2:11, c = gl(2,10))
summary(df1)

##
################################################################################

################################################################################
## markup generation

html(mutable(df1))
m1 <- mutable(c ~ a + b, data = df1)

m2 <- mutable(mtcars)

html(m2,
     documentHeaderFunction = muSlickGridDocHeader,
     headerFunction = muSlickGridHeader,
     markupFunction = muSlickGridMarkupGenerator,
     footerFunction = muSlickGridFooter,
     documentFooterFunction = muSlickGridDocFooter,
     completeDocument = TRUE,
     markupElement = "plain",
     file = "/work/eiverson/projects/src/SlickGrid/examples/mut1.html")

##
################################################################################

muSlickGridMarkupGenerator <- function(x, file) {
  cat(c('var data = ',
        toJSON(x) , ";\n"), file = file, append = TRUE)
}

muSlickGridHeader <- function(x, caption, footnote) {
  c('<script>',
    'var grid;',
    'var columns = [',
    paste('{id: "',
          colnames(x),
          '", name: "',
          colnames(x),
          '", field: "',
          colnames(x),
          '"},', sep = ""),
    '               ];',
    '',
    'var options = {',
    '  enableCellNavigation: true,',
    '  enableColumnReorder: false',
    '};',
    '')
}

muSlickGridFooter <- function(x) {
  c('$(function () {',
    'grid = new Slick.Grid("#myGrid", data, columns, options);',
    '})',
    '</script>')
}

muSlickGridDocFooter <- muHTMLDocFooter

muSlickGridDocHeader <- function(cssFile) {
  c('<!DOCTYPE HTML>',
    '<html>',
    '<head>',
    '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">',
    '<title>SlickGrid example 1: Basic grid 2</title>',
    '<link rel="stylesheet" href="../slick.grid.css" type="text/css"/>',
    '<link rel="stylesheet" href="../css/smoothness/jquery-ui-1.8.16.custom.css" type="text/css"/>',
    '<link rel="stylesheet" href="examples.css" type="text/css"/>',
    '</head>',
    '<body>',
    '<table width="100%">',
    '<tr>',
    '<td valign="top" width="50%">',
    '<div id="myGrid" style="width:700px;height:500px;"></div>',
    '</td>',
    '<td valign="top">',
    '<h2>Demonstrates:</h2>',
    '<ul>',
    '<li>basic grid with minimal configuration</li>',
    '</ul>',
    '</td>',
    '</tr>',
    '</table>',
    '<script src="../lib/jquery-1.7.min.js"></script>',
    '<script src="../lib/jquery.event.drag-2.0.min.js"></script>',
    '<script src="../slick.core.js"></script>',
    '<script src="../slick.grid.js"></script>')
}

