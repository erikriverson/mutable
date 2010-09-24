elatex <- function(x, ...) {
  UseMethod("elatex")
}

elatex.default <- function(x, name, data, round.digits = 1, ...) {
  ret <- paste(round(x[1], round.digits), "/", 
               round(x[2], round.digits), "/",
               round(x[3], round.digits),
               sep = " ")
  names(ret) <- name
  ret
}

elatex.table <- function(x, name, data, round.digits, ...) {
  dft <- as.data.frame(x)
  pct <- paste(round(x / sum(x) * 100, round.digits), "\\%", sep = "")
  val <- paste(pct, paste(dft[["Freq"]], "/", sum(x), sep = ""))
  names(val) <- paste(name, names(x), sep = "")
  val
}

elatexrownames <- function(x, ...) {
  UseMethod("elatexrownames")
}

elatexrownames.special <- function(x, name, data, ...) {
  ret <- c(x[1], paste("~~~", tail(x, length(x) - 1)))
  names(ret) <- c(name, paste(name,  levels(data[[name]]), sep = ""))
  ret
}

latex.etable <- function(x, na.print = "", file = "", headerFunction = eLaTeXHeader.tabular,
                         footerFunction = eLaTeXFooter.tabular, caption = "", ...) {
  x <- x$latex
  x[is.na(x)] <- na.print
  cat(paste(headerFunction(x, caption, ...), collapse = "\n"), "\n", file = file)
  cat(paste(apply(x, 1, paste, collapse = "&"), collapse = "\\\\\n"), "\\\\\n", file = file,
      append = TRUE)
  cat(paste(footerFunction(x, caption), collapse = "\n"), "\n", file = file, append = TRUE)
  
}

eLaTeXHeader.tabular <- function(x, caption, collabel.just) {
  if(missing(collabel.just))
    collabel.just <- paste(c("l", rep("c", ncol(x) - 1)), collapse = "")
  
  c("\\begin{table}",
    ps("\\begin{tabular}{", collabel.just, "}"),
    c(paste("\\multicolumn{1}{c}{", colnames(x), "}", collapse = "&"), "\\\\"),
    "\\hline\\\\")
}

eLaTeXFooter.tabular <- function(x, caption) {
    c("\\end{tabular}",
      ps("\\caption{", caption , "}"),
      "\\end{table}")
  }

eLaTeXHeader.longtable <- function(x, caption, collabel.just) {
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

eLaTeXFooter.longtable <- function(x, caption) {
  c("\\hline",
    "\\end{longtable}")
}

elatexrownames.character <- eprintidentity
attr(erownames, "latex.function") <-  elatexrownames
attr(etest, "latex.function") <- eprintidentity

