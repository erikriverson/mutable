source.mutable()

pead.bl <- data.frame(hiv = sample(c("Positive", "Negative"), 100, replace = TRUE),
                      age = rnorm(100, c(50, 40), sd = 10),
                      gender = sample(c("Male", "Female"), 100, replace = TRUE),
                      diffs = rnorm(100, 500, sd = 100),
                      inc4x = sample(c("Yes", "No"), 100, replace = TRUE),
                      bmi = rnorm(100, 30, sd = 2))


is.na(pead.bl$age) <- c(1, 50, 100)
label(pead.bl$hiv) <- "HIV Status"
label(pead.bl$age) <- "Age"
label(pead.bl$gender) <- "Gender"
label(pead.bl$diffs) <- "V2 GMT - V1 GMT"
label(pead.bl$inc4x) <- "$$\\frac{V1 GMT}{V2 GMT} >= 4$$"
label(pead.bl$bmi) <- "BMI" 
                        
form <- hiv ~ age + gender + diffs + inc4x + bmi

tab1 <- mutable(form, data = pead.bl, colname = "",
                summary.function = muRownamesSummary,
                markupList = list(plain = muRownamesPlain,
                  latex = muRownamesLatex,
                  html = muRownamesHTML)) +
  mutable(colname = "Combined Categories") +
  mutable(subset = hiv == "Positive", colname = "Positive") +
  mutable(subset = hiv == "Negative", colname = "Negative")

  mutable(form, pead.bl,
          summary.function = muStratTest, colname = "P-value")


mutableStrat(form, pead.bl)

str(tab1)

html(tab1,
     caption = "Baseline Table",
     completeDocument = TRUE,
     cssFile = "main.css")

latex(tab1,
     caption = "Baseline Table")

form2 <- age ~ gender + bmi

## tab2 <- mutable(form2, data = pead.bl,
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

 
tab <- mutable(form, pead.bl,
               summary.function = muStratTest,
               post.summary.hook = pvalSummaryHook,
               post.markup.hook = pvalMarkupHook, 
               colname = "P-value")


str(tab)

tab
## need to figure out na.action.
## what is necessary?
## why is it not getting set to what it was after parseFormula?
## is it bombing within parseFormula?

