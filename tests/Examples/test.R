source.mutable()

pead.bl <- data.frame(hiv = sample(c("Positive", "Negative"), 100, replace = TRUE),
                      age = rnorm(100, c(50, 40), sd = 10),
                      gender = sample(c("Male", "Female"), 100, replace = TRUE),
                      diffs = rnorm(100, 500, sd = 100),
                      inc4x = sample(c("Yes", "No"), 100, replace = TRUE),
                      bmi = rnorm(100, 30, sd = 2))

label(pead.bl$hiv) <- "HIV Status"
label(pead.bl$age) <- "Age"
label(pead.bl$gender) <- "Gender"
label(pead.bl$diffs) <- "V2 GMT - V1 GMT"
label(pead.bl$inc4x) <- "$$\\frac{V1 GMT}{V2 GMT} >= 4$$"
label(pead.bl$bmi) <- "BMI" 
                        
form <- hiv ~ age + gender + diffs + inc4x + bmi

tab1 <- mutable(form, data = pead.bl, colname = "",
              summary.function = muRownamesSummary) +
  mutable(colname = "Combined Categories") +
  mutable(subset = hiv == "Positive", colname = "Positive") +
  mutable(subset = hiv == "Negative", colname = "Negative") +
  mutable(form, pead.bl,
          summary.function = muStratTest, colname = "P-value")

html(tab1,
     caption = "Baseline Table",
     completeDocument = TRUE,
     cssFile = "main.css")

latex(tab1,
     caption = "Baseline Table")

form2 <- age ~ gender + bmi

tab2 <- mutable(form2, data = pead.bl,
                summary.function = muRownamesSummary,
                colname = "Variable") + 
  mutable(summary.function = muResponseSummary,
          plain.function = muResponsePlain,
          latex.function = muResponseLatex,
          html.function = muResponseHTML, 
          colname = "Summary of Difference in Age",
          round.digits = 1) +
  mutable(summary.function = muResponseTest,
          colname = "P-value")

 
tab2


mutable(form, pead.bl,
        summary.function = muStratTest,
        post.summary.hook = print,
        colname = "P-value")


t1 <- list(list(pvalue = 0.01, test = "ttest"),
           list(pvalue = 0.11, test = "fisher"),
           list(pvalue = 0.50, test = "fisher"),
           list(pvalue = 0.50, test = "ttest"))

## how to add just one element to a list, since
## the function below assumes only x$pvalue will exist
## and we might want additional slots available in the
## future

pvalHook <- function(ret) {
  nms <- unique(sapply(ret, "[[", "test"))
  unt <- 1:length(nms)
  names(unt) <- nms
  
  lapply(ret, function(x) c(ret[!names(ret) %in% "test"],
                                test = unt[x$test]))
}

pvalHook(t1)



lst <- list(pvalue = .0222, test = "ttest")

c(lst[!names(lst) %in% "test"], test = "hi")








