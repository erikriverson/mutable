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

tab1 <- etable(form, data = pead.bl, colname = "",
              summary.function = erownames) +
  etable(colname = "Combined Categories") +
  etable(subset = hiv == "Positive", colname = "Positive") +
  etable(subset = hiv == "Negative", colname = "Negative") +
  etable(summary.function = etest, colname = "P-value")

html(tab1,
     caption = "Baseline Table",
     completeDocument = TRUE,
     cssFile = "main.css")

latex(tab1,
     caption = "Baseline Table")



















