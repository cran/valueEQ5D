# # #######################################################################
context("testing column existence")
test_that("testing column existence", {
  x <- c(0, NA, "dd", 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "age")
  expect_equal(check_column_exist("age", tempdata), 0)
})
context("testing column existence")
test_that("testing column existence", {
  x <- c(0, NA, "dd", 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "num")
  expect_equal(check_column_exist("age", tempdata), -1)
})
# # ###########################################################################
context("testing file existence")
test_that("test for file existence and access", {
  thisfile <- system.file("extdata", "blank.txt", package = "valueEQ5D")
  expect_identical(test_file_exist_read(thisfile), 0)
  nofile <- system.file("extdata", "no.txt", package = "valueEQ5D")
  expect_error(test_file_exist_read(nofile))
})
# # # #########################################################################
context("testing mode function")
test_that("testing mode function", {
  x <- c(0, 11, 78, 160)
  expect_equal(get_mode_for_vec(x), 0)
})
context("testing mode function")
test_that("testing mode function", {
  x <- c(0, "f", 78, 160)
  expect_error(get_mode_for_vec(x), "Non numeric data", fixed = TRUE)
})
context("testing mode function")
test_that("testing mode function", {
  x <- c(78, NA, 78, 78)
  expect_equal(get_mode_for_vec(x), 78)
})
context("testing mode function")
test_that("testing mode function", {
  x <- c(78, "NA", 78, 78)
  expect_error(get_mode_for_vec(x), "Non numeric data", fixed = TRUE)
})
# # ############################################################################
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
  expect_equal(get_column_no_colnames(sampledata, "sex"), 2)
  expect_error(get_column_no_colnames(sampledata, "dob"))
})
# # ############################################################################
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
  expect_equal(get_colno_existing_colnames("sex", sampledata), 2)
  expect_equal(get_colno_existing_colnames("age", sampledata), 1)
  expect_equal(get_colno_existing_colnames(c("sex", "gender", "male", "female", 
                                             "f", "m"), sampledata), 2)
  expect_error(get_colno_existing_colnames(c("gender", "male", "female", "f", 
                                             "m"), sampledata), 
               "No column exists with specified column names", fixed = TRUE)
  expect_error(get_colno_existing_colnames("", sampledata), 
               "No column exists with specified column names", fixed = TRUE)
})
# # # ###########################################################################
context("testing subset_gender_age_to_group")
test_that("testing subset_gender_age_to_group", {
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
  expect_equal(subset_gender_age_to_group(sampledata, "female", c(10, 70)), two)
  one <- subset(sampledata, sex == "M")
  two <- subset(one, one$age >= 10 & one$age <= 70)
  expect_equal(subset_gender_age_to_group(sampledata, "male", c(10, 70)), two)
  one <- subset(sampledata, sex == "F")
  two <- subset(one, one$age >= 0 & one$age <= 10)
  expect_equal(subset_gender_age_to_group(sampledata, "female", c(0, 10)), two)
  expect_error(subset_gender_age_to_group(sampledata, "bh", c(10, 70)), 
               "Group by should be euther male or female", fixed = TRUE)
  expect_error(subset_gender_age_to_group(sampledata, "bh", NULL), 
               "Group by should be euther male or female", fixed = TRUE)
  one <- subset(sampledata, sex == "M")
  expect_identical(subset_gender_age_to_group(sampledata, "male", NULL), one)
  expect_identical(subset_gender_age_to_group(one, NULL, NULL), one)
})
# # # ##########################################################################
context("testing descritpvie statistics")
test_that("testing descritpvie statistics", {
  x <- c("a", "b", "c")
  expect_error(descriptive_stat_data_column(x, "name"))
  x <- c(0, 11, 78, 160)
  results <- matrix(c(249, 62.25, 73.72189, 44.5, 0, 36.86, 0, 160, 4), 
                    nrow = 1, byrow = TRUE)
  colnames(results) <- c("Sum", "Mean", "SD", "Median", "Mode", "SE", 
                         "Minimum", "Maximum", "Count")
  rownames(results) <- "age"
  expect_equal(descriptive_stat_data_column(x, "age", NA), results,
               tolerance = 0.001)
})
context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c(0, 11, 78, 160)
  results <- matrix(c(249, 83, 74.62573, 78, 11, 43.08519, 11, 160, 3),
                    nrow = 1, byrow = TRUE)
  colnames(results) <- c("Sum", "Mean", "SD", "Median", "Mode", "SE", 
                         "Minimum", "Maximum", "Count")
  rownames(results) <- "age"
  expect_equal(descriptive_stat_data_column(x, "age", 0), results, 
               tolerance = 0.001)
})
context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c(0, NA, 78, 160)
  results <- matrix(c(238, 79.33333, 80.00833, 78, 0, 46.19283, 0, 160, 3), 
                    nrow = 1, byrow = TRUE)
  colnames(results) <- c("Sum", "Mean", "SD", "Median", "Mode", "SE", 
                         "Minimum", "Maximum", "Count")
  rownames(results) <- "age"
  expect_equal(descriptive_stat_data_column(x, "age", NA), results,
               tolerance = 0.001)
})

context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c(0, NA, "dd", 160)
  expect_error(descriptive_stat_data_column(x, "age", NA), 
               "Some values-other than NR code is not numeric", fixed = TRUE)
})
context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c("", 11, 78, 160)
  expect_error(descriptive_stat_data_column(x, "age", NA), 
               "Some values-other than NR code is not numeric", fixed = TRUE)
})
# ## # #########################################################################
context("testing numeric column")
test_that("test for numeric values in a specific column but with no range given", {
  x <- c(0, 11, 78, 120)
  expect_identical(test_data_num_norange(x, 0), 0)
  x <- c(-8, 99, 2, 5, -99)
  expect_identical(test_data_num_norange(x, -99), 0)
  x <- c("sheeja", 99, 2, 5, -99)
  expect_error(test_data_num_norange(x, -99), "Some values-other than NR code is not numeric", fixed = TRUE)
})
# ## # #########################################################################
context("testing getting frequency table")
test_that("test getting frequency table", {
  expect_error(get_frequency_table(NULL))
  res <- (get_frequency_table(c(1, 1, 1, 2, 2, 2)))
  expect_equal(res[1, 1], "1")
  expect_equal(res[1, 2], "3")
})
# ## # #########################################################################
context("testing converting number to digits")
test_that("test converting number to digits", {
  res <- c(2, 3, 4)
  expect_equal(convert_number_to_digits(234), res)
  expect_error(convert_number_to_digits("sh"))
})
# ## # #########################################################################
context("testing replacing space with underscore")
test_that("test replacing space with underscore", {
  expect_identical(replace_space_underscore("Sri Lanka"), "Sri_Lanka")
  expect_identical(replace_space_underscore("SriLanka"), "SriLanka")
  expect_error(replace_space_underscore(""))
})
