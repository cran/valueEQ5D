
# # #####################################################################################################################
context("testing column existence")
test_that("testing column existence", {
  x <- c(0, NA, "dd", 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "age")
  expect_equal(checkColumnExist("age", tempdata), 0)
})
context("testing column existence")
test_that("testing column existence", {
  x <- c(0, NA, "dd", 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "num")
  expect_equal(checkColumnExist("age", tempdata), -1)
})
# # #####################################################################################################################
context("testing file existence")
test_that("test for file existence and access", {
  thisfile <- system.file("extdata", "blank.txt", package = "valueEQ5D")
  expect_identical(testFileExistRead(thisfile), 0)
})
# # # #####################################################################################################################
context("testing mode function")
test_that("testing mode function", {
  x <- c(0, 11, 78, 160)
  expect_equal(getModeForVec(x), 0)
})
context("testing mode function")
test_that("testing mode function", {
  x <- c(0, "f", 78, 160)
  expect_error(getModeForVec(x), "Non numeric data", fixed = TRUE)
})
context("testing mode function")
test_that("testing mode function", {
  x <- c(78, NA, 78, 78)
  expect_equal(getModeForVec(x), 78)
})
context("testing mode function")
test_that("testing mode function", {
  x <- c(78, "NA", 78, 78)
  expect_error(getModeForVec(x), "Non numeric data", fixed = TRUE)
})
# # #####################################################################################################################
context("testing getting column number for existing column names")
test_that("testing getting column number for existing column names", {
  set.seed(20)
  sampledata <- data.frame(
    age = abs(rnorm(10, 60, 20)),
    sex = factor(sample(c("M", "F"), 10, replace = T)),
    arm = factor(sample(c("Control", "Intervention"), 10, replace = T)),
    eq5d3L.q1 = (sample(c(1, 2, 3), 10, replace = T)),
    eq5d3L.q2 = (sample(c(1, 2, 3), 10, replace = T)),
    eq5d3L.q3 = (sample(c(1, 2, 3), 10, replace = T)),
    eq5d3L.q4 = (sample(c(1, 2, 3), 10, replace = T)),
    eq5d3L.q5 = (sample(c(1, 2, 3), 10, replace = T))
  )
  expect_equal(getColNumExistingColNames("sex", sampledata), 2)
  expect_equal(getColNumExistingColNames("age", sampledata), 1)
  expect_equal(getColNumExistingColNames(c("sex", "gender", "male", "female", "f", "m"), sampledata), 2)
  expect_error(getColNumExistingColNames(c("gender", "male", "female", "f", "m"), sampledata), "No column exists with specified column names", fixed = TRUE)
  expect_error(getColNumExistingColNames("", sampledata), "No column exists with specified column names", fixed = TRUE)
})
# # # #####################################################################################################################
context("testing subsetGenderAgeToGroup")
test_that("testing subsetGenderAgeToGroup", {
  set.seed(17)
  sampledata <- data.frame(
    age = abs(rnorm(10, 60, 20)),
    sex = factor(sample(c("M", "F"), 10, replace = T)),
    arm = factor(sample(c("Control", "Intervention"), 10, replace = T)),
    eq5d3L.q1 = (sample(c(1, 2, 3), 10, replace = T)),
    eq5d3L.q2 = (sample(c(1, 2, 3), 10, replace = T)),
    eq5d3L.q3 = (sample(c(1, 2, 3), 10, replace = T)),
    eq5d3L.q4 = (sample(c(1, 2, 3), 10, replace = T)),
    eq5d3L.q5 = (sample(c(1, 2, 3), 10, replace = T))
  )
  one <- subset(sampledata, sex == "F")
  two <- subset(one, one$age >= 10 & one$age <= 70)
  expect_equal(subsetGenderAgeToGroup(sampledata, "female", c(10, 70)), two)
  one <- subset(sampledata, sex == "M")
  two <- subset(one, one$age >= 10 & one$age <= 70)
  expect_equal(subsetGenderAgeToGroup(sampledata, "male", c(10, 70)), two)
  one <- subset(sampledata, sex == "F")
  two <- subset(one, one$age >= 0 & one$age <= 10)
  expect_equal(subsetGenderAgeToGroup(sampledata, "female", c(0, 10)), two)
  expect_error(subsetGenderAgeToGroup(sampledata, "bh", c(10, 70)), "Group by should be euther male or female", fixed = TRUE)
  expect_error(subsetGenderAgeToGroup(sampledata, "bh", NULL), "Group by should be euther male or female", fixed = TRUE)
  one <- subset(sampledata, sex == "M")
  expect_identical(subsetGenderAgeToGroup(sampledata, "male", NULL), one)
})
# # # #####################################################################################################################
context("testing descritpvie statistics")
test_that("testing descritpvie statistics", {
  x <- c(0, 11, 78, 160)
  results <- matrix(c(249, 62.25, 73.72189, 44.5, 0, 36.86, 0, 160, 4), nrow = 1, byrow = TRUE)
  colnames(results) <- c("Sum", "Mean", "SD", "Median", "Mode", "SE", "Minimum", "Maximum", "Count")
  rownames(results) <- "age"
  expect_equal(descriptiveStatDataColumn(x, "age", NA), results, tolerance = 0.001)
})
context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c(0, 11, 78, 160)
  results <- matrix(c(249, 83, 74.62573, 78, 11, 43.08519, 11, 160, 3), nrow = 1, byrow = TRUE)
  colnames(results) <- c("Sum", "Mean", "SD", "Median", "Mode", "SE", "Minimum", "Maximum", "Count")
  rownames(results) <- "age"
  expect_equal(descriptiveStatDataColumn(x, "age", 0), results, tolerance = 0.001)
})
context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c(0, NA, 78, 160)
  results <- matrix(c(238, 79.33333, 80.00833, 78, 0, 46.19283, 0, 160, 3), nrow = 1, byrow = TRUE)
  colnames(results) <- c("Sum", "Mean", "SD", "Median", "Mode", "SE", "Minimum", "Maximum", "Count")
  rownames(results) <- "age"
  expect_equal(descriptiveStatDataColumn(x, "age", NA), results, tolerance = 0.001)
})

context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c(0, NA, "dd", 160)
  expect_error(descriptiveStatDataColumn(x, "age", NA), "Some values-other than NR code is not numeric", fixed = TRUE)
})
context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c("", 11, 78, 160)
  expect_error(descriptiveStatDataColumn(x, "age", NA), "Some values-other than NR code is not numeric", fixed = TRUE)
})
# ## # #####################################################################################################################
context("testing numeric column")
test_that("test for numeric values in a specific column but with no range given", {
  x <- c(0, 11, 78, 120)
  expect_identical(testDataNumNorange(x, 0), 0)
  x <- c(-8, 99, 2, 5, -99)
  expect_identical(testDataNumNorange(x, -99), 0)
  x <- c("sheeja", 99, 2, 5, -99)
  expect_error(testDataNumNorange(x, -99), "Some values-other than NR code is not numeric", fixed = TRUE)
})
# # # #####################################################################################################################
context("Check 3L scores")
test_that("Checking 3L scores ", {
  the.result <- checkScores3L(1, 1, 1, 1, 1)
  expect_identical(the.result, c(1, 1, 1, 1, 1))
  the.result <- checkScores3L(c(1, 1, 1, 1, 1))
  expect_identical(the.result, c(1, 1, 1, 1, 1))
  the.result <- checkScores3L(11111)
  expect_identical(the.result, c(1, 1, 1, 1, 1))
  expect_error(checkScores3L(2, 3, 4, 5, 2), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(checkScores3L(23452), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(checkScores3L(23, -1, 5, 2), "The responses are not valid", fixed = TRUE)
  expect_error(checkScores3L("", -1, 5, 2), "The responses are not valid", fixed = TRUE)
  expect_error(checkScores3L("", -1, 5, 2, NA), "The responses are not valid", fixed = TRUE)
  the.result <- checkScores3L(11221, NA, NA)
  expect_identical(the.result, c(1, 1, 2, 2, 1))
})

# # #####################################################################################################################
context("Check 5L scores")
test_that("Checking 5L scores ", {
  the.result <- checkScores5L(1, 1, 1, 1, 1)
  expect_identical(the.result, c(1, 1, 1, 1, 1))
  the.result <- checkScores5L(c(1, 1, 1, 1, 1))
  expect_identical(the.result, c(1, 1, 1, 1, 1))
  the.result <- checkScores5L(11111)
  expect_identical(the.result, c(1, 1, 1, 1, 1))
  the.result <- checkScores5L(2, 3, 4, 5, 2)
  expect_identical(the.result, c(2, 3, 4, 5, 2))
  the.result <- checkScores5L(23452)
  expect_identical(the.result, c(2, 3, 4, 5, 2))
  expect_error(checkScores5L(23458), "Responses not valid for EQ-5D-5L scores", fixed = TRUE)
  expect_error(checkScores5L(23, -1, 5, 2), "The responses are not valid", fixed = TRUE)
  expect_error(checkScores5L("", -1, 5, 2), "The responses are not valid", fixed = TRUE)
  expect_error(checkScores5L("", -1, 5, 2, NA), "The responses are not valid", fixed = TRUE)
  the.result <- checkScores5L(11221, NA, NA)
  expect_identical(the.result, c(1, 1, 2, 2, 1))
})
# # #####################################################################################################################
context("EQ5D5L scoring ")
test_that("EQ5D5L scoring ", {
  the.result <- value5LInd("England", 1, 1, 1, 1, 2)
  expect_equal(the.result, 0.922, tolerance = 1e-3)
  the.result <- value5LInd("England", 11112)
  expect_equal(the.result, 0.922, tolerance = 1e-3)
  the.result <- value5LInd("England", 11121)
  expect_equal(the.result, 0.937, tolerance = 1e-3)
  the.result <- value5LInd("England", 11211)
  expect_equal(the.result, 0.950, tolerance = 1e-3)
  the.result <- value5LInd("England", 12111)
  expect_equal(the.result, 0.950, tolerance = 1e-3)
  the.result <- value5LInd("England", 21111)
  expect_equal(the.result, 0.942, tolerance = 1e-3)
  the.result <- value5LInd("England", 55555)
  expect_equal(the.result, -0.285, tolerance = 1e-3)
  the.result <- value5LInd("England", 111)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value5LInd("England", c(1, 1, 1, 1, 2))
  expect_equal(the.result, 0.922, tolerance = 1e-3)
  the.result <- value5LInd("England", c(1, 1, 1, 2, 1))
  expect_equal(the.result, 0.937, tolerance = 1e-3)
  the.result <- value5LInd("England", c(1, 1, 2, 1, 1))
  expect_equal(the.result, 0.950, tolerance = 1e-3)
  the.result <- value5LInd("England", c(1, 2, 1, 1, 1))
  expect_equal(the.result, 0.950, tolerance = 1e-3)
  the.result <- value5LInd("Ireland", c(2, 1, 1, 1, 1))
  expect_equal(the.result, 0.937, tolerance = 1e-3)
  the.result <- value5LInd("England", c(5, 5, 5, 5, 5))
  expect_equal(the.result, -0.285, tolerance = 1e-3)
  the.result <- value5LInd("England", 11112, NA, NA, NA, NA)
  expect_equal(the.result, 0.922, tolerance = 1e-3)
  the.result <- value5LInd("England", c(1, 1, 1, 1, 2), NA, NA, NA, NA)
  expect_equal(the.result, 0.922, tolerance = 1e-3)
  the.result <- value5LInd("England", c(1, 1, 1, 1, 2), NA, NA, NA)
  expect_equal(the.result, 0.922, tolerance = 1e-3)
  the.result <- value5LInd("England", 11111)
  expect_equal(the.result, 1, tolerance = 1e-3)

  the.result <- value5LInd("England", NA, 1, 1, 1, 2)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value5LInd("England", NA, 1, 1, 2, 1)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value5LInd("England", NA, 1, 1, 1)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value5LInd("England", 1, 1, 1, 1, NA)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value5LInd("Germany", 1, 1, 1, 1, 1)
  expect_equal(the.result, 1, tolerance = 1e-3)

  expect_error(value5LInd("England", c(1, 1, 1), NA, NA, NA, NA), "Invalid EQ-5D-5L responses-check the responses to each question", fixed = TRUE)
  expect_error(value5LInd("England", c(8, 1, 1, 2, 1), NA, NA, NA, NA), "Responses not valid for EQ-5D-5L scores", fixed = TRUE)
  expect_error(value5LInd("England", c(1, 1, 1)), "Invalid EQ-5D-5L responses-check the responses to each question", fixed = TRUE)
  expect_error(value5LInd("England", c(3, 4, 5, 6, 7, 8)), "Invalid EQ-5D-5L responses-check the responses to each question", fixed = TRUE)
  expect_error(value5LInd("England", c(-1, 1, 1, 1, 1)), "Responses not valid for EQ-5D-5L scores", fixed = TRUE)
  expect_error(value5LInd("England", -1, 1, 1, 1, 1), "The responses are not valid", fixed = TRUE)
  expect_error(value5LInd("US", 1, 1, 1, 1, 1), "No tariffs found for the country you specified for EQ-5D-5L. Please try later", fixed = TRUE)
  expect_error(value5LInd("England", 345678), "Responses not valid for EQ-5D-5L scores", fixed = TRUE)
  expect_error(value5LInd("NM", -11111), "No tariffs found for the country you specified for EQ-5D-5L. Please try later", fixed = TRUE)
})
# # # #################################################################################################################
context("testing EQ5D3L valuation using individual responses")
test_that("test for value3L", {
  the.result <- value3LInd("UK", "TTO", 1, 1, 1, 1, 2)
  expect_equal(the.result, 0.848, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", 11112)
  expect_equal(the.result, 0.848, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", 11121)
  expect_equal(the.result, 0.796, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", 11211)
  expect_equal(the.result, 0.883, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", 12111)
  expect_equal(the.result, 0.815, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", 21111)
  expect_equal(the.result, 0.85, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", 33333)
  expect_equal(the.result, -0.594, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", 111)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", 11112, NA, NA, NA, NA)
  expect_equal(the.result, 0.848, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", c(1, 1, 1, 1, 2), NA, NA, NA, NA)
  expect_equal(the.result, 0.848, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", c(1, 1, 1, 1, 2), NA, NA, NA)
  expect_equal(the.result, 0.848, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", 11111)
  expect_equal(the.result, 1, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", c(1, 1, 1, 1, 2))
  expect_equal(the.result, 0.848, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", c(1, 1, 1, 2, 1))
  expect_equal(the.result, 0.796, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", c(1, 1, 2, 1, 1))
  expect_equal(the.result, 0.883, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", c(1, 2, 1, 1, 1))
  expect_equal(the.result, 0.815, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", c(2, 1, 1, 1, 1))
  expect_equal(the.result, 0.85, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", NA, 1, 1, 1, 2)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", NA, 1, 1, 2, 1)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", NA, 1, 1, 1)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", 1, 1, 1, 1, NA)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value3LInd("UK", "TTO", 123)
  expect_equal(the.result, NA)
  answers <- EQ5D3L_indexvalues.df
  for (i in 1:nrow(answers)) {
    the.scores3L <- convertNumberToIndividualDigits(answers$state[i])
    the.result <- value3LInd("UK", "TTO", answers$state[i])
    expect_equal(the.result, answers$UKTTO[i])
  }
  expect_error(value3LInd("UK", "TTO", 345678), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value3LInd("NM", "TTO", -11111), "No country tariffs found for the country you specified for EQ-5D-3L. Please try later", fixed = TRUE)
  expect_error(value3LInd("UK", "TTO", 55555), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value3LInd("UK", "TTO", c(1, 1, 1)), "Invalid EQ-5D-3L responses-check the responses to each question", fixed = TRUE)
  expect_error(value3LInd("UK", "TTO", c(8, 1, 1, 2, 1)), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value3LInd("UK", "TTO", c(5, 5, 5, 5, 5)), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value3LInd("UK", "TTO", c(1, 1, 1)), "Invalid EQ-5D-3L responses-check the responses to each question", fixed = TRUE)
  expect_error(value3LInd("UK", "TTO", c(3, 4, 5, 6, 7, 8)), "Invalid EQ-5D-3L responses-check the responses to each question", fixed = TRUE)
  expect_error(value3LInd("UK", "TTO", c(-1, 1, 1, 1, 1)), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value3LInd("UK", "TTO", -1, 1, 1, 1, 1), "The responses are not valid", fixed = TRUE)
  expect_error(value3LInd("DE", "TTO", 1, 1, 1, 1, 1), "No country tariffs found for the country you specified for EQ-5D-3L. Please try later", fixed = TRUE)
  expect_error(value3LInd("UK", "TTO", 4, 5, 6, 5, 8), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value3LInd("UK", "TTO", -1, 2, 3, 2, 2), "The responses are not valid", fixed = TRUE)
  expect_error(value3LInd("JP", "TTO", c(1, 2, 3, 2, 3)), "No country tariffs found for the country you specified for EQ-5D-3L. Please try later", fixed = TRUE)
})
# # # #################################################################################################################
context("testing EQ5D3L valuation using individual responses")
test_that("test for value3L", {
  the.result <- value3LInd("UK", "VAS", 1, 1, 1, 1, 2)
  expect_equal(the.result, 0.782, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", 11112)
  expect_equal(the.result, 0.782, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", 11121)
  expect_equal(the.result, 0.761, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", 11211)
  expect_equal(the.result, 0.814, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", 12111)
  expect_equal(the.result, 0.752, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", 21111)
  expect_equal(the.result, 0.774, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", 33333)
  expect_equal(the.result, -0.073, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", 111)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", 11112, NA, NA, NA, NA)
  expect_equal(the.result, 0.782, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", c(1, 1, 1, 1, 2), NA, NA, NA, NA)
  expect_equal(the.result, 0.782, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", c(1, 1, 1, 1, 2), NA, NA, NA)
  expect_equal(the.result, 0.782, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", 11111)
  expect_equal(the.result, 1, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", c(1, 1, 1, 1, 2))
  expect_equal(the.result, 0.782, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", c(1, 1, 1, 2, 1))
  expect_equal(the.result, 0.761, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", c(1, 1, 2, 1, 1))
  expect_equal(the.result, 0.814, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", c(1, 2, 1, 1, 1))
  expect_equal(the.result, 0.752, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", c(2, 1, 1, 1, 1))
  expect_equal(the.result, 0.774, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", NA, 1, 1, 1, 2)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", NA, 1, 1, 2, 1)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", NA, 1, 1, 1)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", 1, 1, 1, 1, NA)
  expect_equal(the.result, NA, tolerance = 1e-3)
  the.result <- value3LInd("UK", "VAS", 123)
  expect_equal(the.result, NA)
  answers <- EQ5D3L_indexvalues.df
  for (i in 1:nrow(answers)) {
    the.scores3L <- convertNumberToIndividualDigits(answers$state[i])
    the.result <- value3LInd("UK", "VAS", answers$state[i])
    expect_equal(the.result, answers$UKVAS[i])
  }
  expect_error(value3LInd("UK", "VAS", 345678), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value3LInd("NM", "VAS", -11111), "No country tariffs found for the country you specified for EQ-5D-3L. Please try later", fixed = TRUE)
  expect_error(value3LInd("UK", "VAS", 55555), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value3LInd("UK", "VAS", c(1, 1, 1), NA, NA, NA, NA), "Invalid EQ-5D-3L responses-check the responses to each question", fixed = TRUE)
  expect_error(value3LInd("UK", "VAS", c(8, 1, 1, 2, 1), NA, NA, NA, NA), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value3LInd("UK", "VAS", c(5, 5, 5, 5, 5)), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value3LInd("UK", "VAS", c(1, 1, 1)), "Invalid EQ-5D-3L responses-check the responses to each question", fixed = TRUE)
  expect_error(value3LInd("UK", "VAS", c(3, 4, 5, 6, 7, 8)), "Invalid EQ-5D-3L responses-check the responses to each question", fixed = TRUE)
  expect_error(value3LInd("UK", "VAS", c(-1, 1, 1, 1, 1)), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value3LInd("UK", "VAS", -1, 1, 1, 1, 1), "The responses are not valid", fixed = TRUE)
  expect_error(value3LInd("DE", "VAS", 1, 1, 1, 1, 1), "No country tariffs found for the country you specified for EQ-5D-3L. Please try later", fixed = TRUE)
  expect_error(value3LInd("UK", "VAS", 4, 5, 6, 5, 8), "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value3LInd("UK", "VAS", -1, 2, 3, 2, 2), "The responses are not valid", fixed = TRUE)
  expect_error(value3LInd("JP", "VAS", c(1, 2, 3, 2, 3)), "No country tariffs found for the country you specified for EQ-5D-3L. Please try later", fixed = TRUE)
})

# ###############################################################################################################
context("EQ5D5L scoring")
test_that("EQ5D5L scoring", {
  answers <- EQ5D5L_indexvalues.df
  total_entries <- seq(1, nrow(answers))
  total_countries <- c(
    "Canada", "China", "England", "Ethiopia", "France", "Germany", "Hong_Kong", "Indonesia", "Ireland",
    "Japan", "Korea", "Malaysia", "Netherlands", "Poland", "Portugal", "Spain", "Taiwan", "Thailand",
    "Uruguay", "USA", "Vietnam"
  )
  total <- length(total_countries)
  for (j in 1:total) {
    this.country <- total_countries[j]
    country.entry <- replaceSpaceUnderscore(total_countries[j])
    print(this.country)
    for (i in 1:nrow(answers)) {
      the.result <- value5LInd(this.country, answers$state[total_entries[i]])
      this.col <- answers[[country.entry]]
      expect_equal(the.result, this.col[total_entries[i]], tolerance = 1e-3)
    }
  }
})
# # ###############################################################################################################
context("EQ5D3L scoring")
test_that("EQ5D3L scoring", {
  answers <- EQ5D3L_indexvalues.df
  total_entries <- seq(1, nrow(answers))
  VAS_countrylist <- c(
    "Argentina", "Belgium", "Denmark", "Europe", "Finland", "Germany", "Malaysia",
    "New_Zealand", "Slovenia", "Spain", "UK"
  )
  TTO_countrylist <- c(
    "Argentina", "Australia", "Brazil", "Canada", "Chile", "China", "Denmark",
    "France", "Germany", "Iran", "Italy", "Japan", "Korea", "Netherlands", "Poland",
    "Portugal", "Singapore", "Spain", "Sri_Lanka", "Sweden",
    "Taiwan", "Thailand", "Trinidad_and_Tobago", "UK", "USA", "Zimbabwe"
  )
  common_countries <- Reduce(intersect, list(VAS_countrylist, TTO_countrylist))
  all_countries <- sort(unique(c(VAS_countrylist, TTO_countrylist)))
  total <- length(all_countries)
  for (j in 1:total) {
    print(all_countries[j])
    if (all_countries[j] %in% common_countries) {
      country.entry <- replaceSpaceUnderscore(all_countries[j])
      TTOcol <- paste(country.entry, "TTO", sep = "")
      VAScol <- paste(country.entry, "VAS", sep = "")
      for (i in 1:nrow(answers)) {
        the.result.TTO <- value3LInd(all_countries[j], "TTO", answers$state[total_entries[i]])
        the.result.VAS <- value3LInd(all_countries[j], "VAS", answers$state[total_entries[i]])
        this.col.TTO <- answers[[TTOcol]]
        this.col.VAS <- answers[[VAScol]]
        expect_equal(the.result.TTO, this.col.TTO[total_entries[i]], tolerance = 1e-2)
        expect_equal(the.result.VAS, this.col.VAS[total_entries[i]], tolerance = 1e-2)
      }
    } else {
      country.entry <- replaceSpaceUnderscore(all_countries[j])
      col <- country.entry
      if (all_countries[j] %in% VAS_countrylist) {
        method <- "VAS"
      } else {
        if (all_countries[j] %in% TTO_countrylist) {
          method <- "TTO"
        } else {
          print("Associated method not found!!!")
          return(-1)
        }
      }
      for (i in 1:nrow(answers)) {
        the.result <- value3LInd(all_countries[j], method, answers$state[total_entries[i]])
        this.col <- answers[[col]]
        expect_equal(the.result, as.numeric(this.col[total_entries[i]]), tolerance = 1e-2)
      }
    }
  }
})
# # # ###############################################################################################################
context("EQ5D5L crosswalk mapping")
test_that("EQ5D5L crosswalk mapping", {
  answers <- EQ5D5L_crosswalk_indexvalues.df
  total_entries <- seq(1, nrow(answers))
  end1 <- length(total_entries)
  total_countries <- c(
    "Denmark", "France", "Germany", "Japan", "Netherlands", "Spain", "Thailand",
    "UK", "USA", "Zimbabwe"
  )
  total <- length(total_countries)
  for (j in 1:total) {
    this.country <- total_countries[j]
    country.entry <- replaceSpaceUnderscore(total_countries[j])
    print(this.country)
    for (i in 1:nrow(answers)) {
      the.result <- map5Lto3LInd(this.country, "CW", answers$state[total_entries[i]])
      this.col <- answers[[country.entry]]
      expect_equal(the.result, this.col[total_entries[i]], tolerance = 9e-2)
    }
  }
})
# # # ###############################################################################################################
