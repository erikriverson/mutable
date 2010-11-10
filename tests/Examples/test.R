source.mutable()

test.df <- data.frame(hiv = sample(c("Positive", "Negative"), 100, replace = TRUE),
                      age = rnorm(100, c(50, 40), sd = 10),
                      gender = sample(c("Male", "Female"), 100, replace = TRUE),
                      diffs = rnorm(100, 500, sd = 100),
                      inc4x = sample(c("Yes", "No"), 100, replace = TRUE),
                      bmi = rnorm(100, 30, sd = 2))


is.na(test.df$age) <- c(1, 50, 100)
label(test.df$hiv) <- "HIV Status"
label(test.df$age) <- "Age"
label(test.df$gender) <- "Gender"
label(test.df$diffs) <- "V2 GMT - V1 GMT"
label(test.df$inc4x) <- "$$\\frac{V1 GMT}{V2 GMT} >= 4$$"
label(test.df$bmi) <- "BMI" 
                        
form <- hiv ~ age + gender + diffs + inc4x + bmi

tab1 <- mutable(form, data = test.df, colname = "",
                summary.function = muRownames,
                markup.list = list(plain = muExportPlain,
                  html = muExportHTML)) +
  mutable(summary.function = muStratSummary,
          colname = "Combined Categories") 

mutable(form, data = test.df, subset = hiv == "Positive", colname = "Positive")

  mutable(subset = hiv == "Negative", colname = "Negative")

tab1

tab2 <- mutable(form, test.df,
                summary.function = muStratTest,
                markup.list = list(plain = muExportPlain,
                  latex = muExportLatex, 
                  html = muExportHTML),
                colname = "P-value")

tab1 + tab2

tab1 <- mutable(form, data = test.df, colname = "",
                summary.function = muRownamesSummary,
                markupList = list(plain = muRownamesPlain,
                  html = muRownamesHTML)) +
  mutable(colname = "Combined Categories") +
  mutable(subset = hiv == "Positive", colname = "Positive")

mutableStrat(form, test.df)

str(tab1)

html(tab1,
     caption = "Baseline Table",
     completeDocument = TRUE,
     cssFile = "main.css")

latex(tab1,
     caption = "Baseline Table")

form2 <- age ~ gender + bmi

## tab2 <- mutable(form2, data = test.df,
##                 summary.function = muRownamesSummary,
##                 colname = "Variable") + 
##   mutable(summary.function = muResponseSummary,
##           plain.function = muResponsePlain,
##           latex.function = muResponseLatex,
##           html.function = muResponseHTML, 
##           colname = "Summary of Difference in Age",
##           round.digits = 1) +
##   mutable(summary.function = muResponseTest,
##           colname = "P-value")

 
## tab <- mutable(form, test.df,
##                summary.function = muStratTest,
##                post.summary.hook = pvalSummaryHook,
##                post.markup.hook = pvalMarkupHook, 
##                colname = "P-value")


## str(tab)

## tab
## need to figure out na.action.
## what is necessary?
## why is it not getting set to what it was after parseFormula?
## is it bombing within parseFormula?



## test <- function(blah2, ...) {
##   mutable(blah2, test.df,
##           summary.function = muRownamesSummary,
##           markupList = list(plain = muRownamesPlain,
##             latex = muRownamesLatex,
##             html = muRownamesHTML),
##           colname = "P-value", ...)
## }

## test2 <- function(blah, ...) {
##   test(blah, ...)
## }

## model.formula <- form

## test(model.formula)
## test2(form)
## test(model.formula, subset = age > 20)
## test2(model.formula, subset = age > 20)


## set.seed(123)
## df1 <- data.frame(age = rnorm(100, 50, 10),
##                   bmi = rnorm(100, 30, sd = 2))

## testlm <- function(formula, ...) {
##   lm(formula, data = df1, ...)
## }

## testlm(bmi ~ age, subset = age > 50)

## testlm2 <- function(formula, subset) {
##   lm(formula, data = df1, subset = subset)
## }

## testlm2(bmi ~ age, subset = age > 50)
