
pead.bl <- data.frame(hiv = sample(c("Positive", "Negative"), 100, replace = TRUE),
                      age = rnorm(100, 50, sd = 5),
                      gender = sample(c("Male", "Female"), 100, replace = TRUE),
                      diffs = rnorm(100, 500, sd = 100),
                      inc4x = sample(c("Yes", "No"), 100, replace = TRUE),
                      bmi = rnorm(100, 30, sd = 3))

label(pead.bl$hiv) <- "HIV Status"
label(pead.bl$age) <- "Age"
label(pead.bl$gender) <- "Gender"
                        
form <- hiv ~ age + gender

tab1 <- etable(form, data = pead.bl, colname = "rownames",
              summary.function = erownames) +
  etable(colname = "Combined") +
  etable(subset = hiv == "Positive", colname = "Positive") +
  etable(subset = hiv == "Negative", colname = "Negative") +
  etable(summary.function = etest, colname = "P-value")

html(tab1, file = "../html/html-test.html", caption = "Baseline Table",
     completeDocument = TRUE)

