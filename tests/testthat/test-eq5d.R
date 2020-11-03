# # # #####################################################################################################################
context("Check 3L scores")
test_that("Checking 3L scores ", {
  the_result <- check_scores_3L(1, 1, 1, 1, 1)
  expect_identical(the_result, c(1, 1, 1, 1, 1))
  the_result <- check_scores_3L(c(1, 1, 1, 1, 1))
  expect_identical(the_result, c(1, 1, 1, 1, 1))
  the_result <- check_scores_3L(11111)
  expect_identical(the_result, c(1, 1, 1, 1, 1))
  expect_error(check_scores_3L(2, 3, 4, 5, 2),
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(check_scores_3L(23452), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(check_scores_3L(23, -1, 5, 2), "The responses are not valid",
               fixed = TRUE)
  expect_error(check_scores_3L("", -1, 5, 2), "The responses are not valid", 
               fixed = TRUE)
  expect_error(check_scores_3L("", -1, 5, 2, NA), "The responses are not valid", 
               fixed = TRUE)
  the_result <- check_scores_3L(11221, NA, NA)
  expect_identical(the_result, c(1, 1, 2, 2, 1))
  expect_error(check_scores_3L(34546), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
})
# # ############################################################################
context("Check 5L scores")
test_that("Checking 5L scores ", {
  the_result <- check_scores_5L(1, 1, 1, 1, 1)
  expect_identical(the_result, c(1, 1, 1, 1, 1))
  the_result <- check_scores_5L(c(1, 1, 1, 1, 1))
  expect_identical(the_result, c(1, 1, 1, 1, 1))
  the_result <- check_scores_5L(11111)
  expect_identical(the_result, c(1, 1, 1, 1, 1))
  the_result <- check_scores_5L(2, 3, 4, 5, 2)
  expect_identical(the_result, c(2, 3, 4, 5, 2))
  the_result <- check_scores_5L(23452)
  expect_identical(the_result, c(2, 3, 4, 5, 2))
  expect_error(check_scores_5L(23458), 
               "Responses not valid for EQ-5D-5L scores", fixed = TRUE)
  expect_error(check_scores_5L(23, -1, 5, 2), 
               "The responses are not valid", fixed = TRUE)
  expect_error(check_scores_5L("", -1, 5, 2), 
               "The responses are not valid", fixed = TRUE)
  expect_error(check_scores_5L("", -1, 5, 2, NA), 
               "The responses are not valid", fixed = TRUE)
  the_result <- check_scores_5L(11221, NA, NA)
  expect_identical(the_result, c(1, 1, 2, 2, 1))
})
# # ############################################################################
context("EQ5D5L scoring for individual responses")
test_that("EQ5D5L scoring for individual responses", {
  the_result <- value_5L_Ind("England", 5, 1, 1, 1, 2)
  expect_equal(the_result, 0.648, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", 1, 1, 1, 1, 2)
  expect_equal(the_result, 0.922, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", 11112)
  expect_equal(the_result, 0.922, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", 11121)
  expect_equal(the_result, 0.937, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", 11211)
  expect_equal(the_result, 0.950, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", 12111)
  expect_equal(the_result, 0.950, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", 21111)
  expect_equal(the_result, 0.942, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", 55555)
  expect_equal(the_result, -0.285, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", 111)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", c(1, 1, 1, 1, 2))
  expect_equal(the_result, 0.922, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", c(1, 1, 1, 2, 1))
  expect_equal(the_result, 0.937, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", c(1, 1, 2, 1, 1))
  expect_equal(the_result, 0.950, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", c(1, 2, 1, 1, 1))
  expect_equal(the_result, 0.950, tolerance = 1e-3)
  the_result <- value_5L_Ind("Ireland", c(2, 1, 1, 1, 1))
  expect_equal(the_result, 0.937, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", c(5, 5, 5, 5, 5))
  expect_equal(the_result, -0.285, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", 11112, NA, NA, NA, NA)
  expect_equal(the_result, 0.922, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", c(1, 1, 1, 1, 2), NA, NA, NA, NA)
  expect_equal(the_result, 0.922, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", c(1, 1, 1, 1, 2), NA, NA, NA)
  expect_equal(the_result, 0.922, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", 11111)
  expect_equal(the_result, 1, tolerance = 1e-3)
  expect_equal(value_5L_Ind("Canada", 12244), 0.378, tol = 1e-3)
  
  the_result <- value_5L_Ind("England", NA, 1, 1, 1, 2)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", NA, 1, 1, 2, 1)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", NA, 1, 1, 1)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_5L_Ind("England", 1, 1, 1, 1, NA)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_5L_Ind("Germany", 1, 1, 1, 1, 1)
  expect_equal(the_result, 1, tolerance = 1e-3)
  the_result <- value_5L_Ind("Japan", 1, 1, 1, 1, 2)
  expect_equal(the_result, 0.86, tolerance = 1e-3)
  the_result <- value_5L_Ind("Korea", 1, 1, 1, 1, 4)
  expect_equal(the_result, 0.724, tolerance = 1e-3)
  expect_error(value_5L_Ind("England", -111))
  expect_equal(the_result, 0.724, tolerance = 1e-3)
  
  expect_error(value_5L_Ind("England", c(1, 1, 1), NA, NA, NA, NA), 
          "Invalid EQ-5D-5L responses-check the responses to each question", 
          fixed = TRUE)
  expect_error(value_5L_Ind("England", c(8, 1, 1, 2, 1), NA, NA, NA, NA),
               "Responses not valid for EQ-5D-5L scores", fixed = TRUE)
  expect_error(value_5L_Ind("England", c(1, 1, 1)), 
          "Invalid EQ-5D-5L responses-check the responses to each question", 
          fixed = TRUE)
  expect_error(value_5L_Ind("England", c(3, 4, 5, 6, 7, 8)), 
          "Invalid EQ-5D-5L responses-check the responses to each question",
          fixed = TRUE)
  expect_error(value_5L_Ind("England", c(-1, 1, 1, 1, 1)), 
               "Responses not valid for EQ-5D-5L scores", fixed = TRUE)
  expect_error(value_5L_Ind("England", -1, 1, 1, 1, 1), 
               "The responses are not valid", fixed = TRUE)
  expect_error(value_5L_Ind("US", 1, 1, 1, 1, 1), 
               "No tariffs found for the country you specified for EQ-5D-5L. Please try later", fixed = TRUE)
  expect_error(value_5L_Ind("England", 345678), 
               "Responses not valid for EQ-5D-5L scores", fixed = TRUE)
  expect_error(value_5L_Ind("NM", -11111), 
               "No tariffs found for the country you specified for EQ-5D-5L. Please try later", fixed = TRUE)
  expect_error(value_5L_Ind("England", "sh"), 
               "The responses are not valid", fixed = TRUE)
})
# # # ###########################################################################
context("testing EQ5D3L valuation for individual responses")
test_that("test for value_3L", {
  the_result <- value_3L_Ind("UK", "TTO", 1, 1, 1, 1, 2)
  expect_equal(the_result, 0.848, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", 11112)
  expect_equal(the_result, 0.848, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", 11121)
  expect_equal(the_result, 0.796, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", 11211)
  expect_equal(the_result, 0.883, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", 12111)
  expect_equal(the_result, 0.815, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", 21111)
  expect_equal(the_result, 0.85, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", 33333)
  expect_equal(the_result, -0.594, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", 111)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", 11112, NA, NA, NA, NA)
  expect_equal(the_result, 0.848, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", c(1, 1, 1, 1, 2), NA, NA, NA, NA)
  expect_equal(the_result, 0.848, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", c(1, 1, 1, 1, 2), NA, NA, NA)
  expect_equal(the_result, 0.848, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", 11111)
  expect_equal(the_result, 1, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", c(1, 1, 1, 1, 2))
  expect_equal(the_result, 0.848, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", c(1, 1, 1, 2, 1))
  expect_equal(the_result, 0.796, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", c(1, 1, 2, 1, 1))
  expect_equal(the_result, 0.883, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", c(1, 2, 1, 1, 1))
  expect_equal(the_result, 0.815, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", c(2, 1, 1, 1, 1))
  expect_equal(the_result, 0.85, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", NA, 1, 1, 1, 2)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", NA, 1, 1, 2, 1)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", NA, 1, 1, 1)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", 1, 1, 1, 1, NA)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "TTO", 123)
  expect_equal(the_result, NA)
  expect_error(value_3L_Ind("UK", "TTO", -111))
  answers <- EQ5D3L_indexvalues.df
  for (i in 1:nrow(answers)) {
    the_result <- value_3L_Ind("UK", "TTO", answers$state[i])
    expect_equal(the_result, answers$UKTTO[i])
  }
  expect_error(value_3L_Ind("UK", "TTO", 345678), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value_3L_Ind("NM", "TTO", -11111), 
        "No country tariffs found for the country you specified for EQ-5D-3L. Please try later", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "TTO", 55555),
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "TTO", c(1, 1, 1)), 
            "Invalid EQ-5D-3L responses-check the responses to each question", 
               fixed = TRUE)
  expect_error(value_3L_Ind("UK", "TTO", c(8, 1, 1, 2, 1)), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "TTO", c(5, 5, 5, 5, 5)), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "TTO", c(1, 1, 1)), 
            "Invalid EQ-5D-3L responses-check the responses to each question", 
               fixed = TRUE)
  expect_error(value_3L_Ind("UK", "TTO", c(3, 4, 5, 6, 7, 8)), 
          "Invalid EQ-5D-3L responses-check the responses to each question", 
               fixed = TRUE)
  expect_error(value_3L_Ind("UK", "TTO", c(-1, 1, 1, 1, 1)), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "TTO", -1, 1, 1, 1, 1), 
               "The responses are not valid", fixed = TRUE)
  expect_error(value_3L_Ind("DE", "TTO", 1, 1, 1, 1, 1), 
               "No country tariffs found for the country you specified for EQ-5D-3L. Please try later", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "TTO", 4, 5, 6, 5, 8), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "TTO", -1, 2, 3, 2, 2), 
               "The responses are not valid", fixed = TRUE)
  expect_error(value_3L_Ind("JP", "TTO", c(1, 2, 3, 2, 3)), 
               "No country tariffs found for the country you specified for EQ-5D-3L. Please try later", fixed = TRUE)
})
# # # #########################################################################
context("testing EQ5D3L valuation for individual responses")
test_that("test for value_3L", {
  the_result <- value_3L_Ind("UK", "VAS", 1, 1, 1, 1, 2)
  expect_equal(the_result, 0.782, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", 11112)
  expect_equal(the_result, 0.782, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", 11121)
  expect_equal(the_result, 0.761, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", 11211)
  expect_equal(the_result, 0.814, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", 12111)
  expect_equal(the_result, 0.752, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", 21111)
  expect_equal(the_result, 0.774, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", 33333)
  expect_equal(the_result, -0.073, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", 111)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", 11112, NA, NA, NA, NA)
  expect_equal(the_result, 0.782, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", c(1, 1, 1, 1, 2), NA, NA, NA, NA)
  expect_equal(the_result, 0.782, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", c(1, 1, 1, 1, 2), NA, NA, NA)
  expect_equal(the_result, 0.782, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", 11111)
  expect_equal(the_result, 1, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", c(1, 1, 1, 1, 2))
  expect_equal(the_result, 0.782, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", c(1, 1, 1, 2, 1))
  expect_equal(the_result, 0.761, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", c(1, 1, 2, 1, 1))
  expect_equal(the_result, 0.814, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", c(1, 2, 1, 1, 1))
  expect_equal(the_result, 0.752, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", c(2, 1, 1, 1, 1))
  expect_equal(the_result, 0.774, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", NA, 1, 1, 1, 2)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", NA, 1, 1, 2, 1)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", NA, 1, 1, 1)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", 1, 1, 1, 1, NA)
  expect_equal(the_result, NA, tolerance = 1e-3)
  the_result <- value_3L_Ind("UK", "VAS", 123)
  expect_equal(the_result, NA)
  the_result <- value_3L_Ind("Australia", "TTO", 33132)
  expect_equal(the_result, -0.045, tol = 1e-3)
  answers <- EQ5D3L_indexvalues.df
  expect_error(value_3L_Ind("India", "VAS", 11323), 
               "No country tariffs", fixed = TRUE)
  expect_error(value_3L_Ind("Korea", "VAS", 11323), 
               "No tariff found", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "VAS", 345678), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value_3L_Ind("NM", "VAS", -11111), 
               "No country tariffs found for the country you specified for EQ-5D-3L. Please try later", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "VAS", 55555), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "VAS", c(1, 1, 1), NA, NA, NA, NA), 
            "Invalid EQ-5D-3L responses-check the responses to each question",
               fixed = TRUE)
  expect_error(value_3L_Ind("UK", "VAS", c(8, 1, 1, 2, 1), NA, NA, NA, NA), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "VAS", c(5, 5, 5, 5, 5)), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "VAS", c(1, 1, 1)), 
            "Invalid EQ-5D-3L responses-check the responses to each question", 
               fixed = TRUE)
  expect_error(value_3L_Ind("UK", "VAS", c(3, 4, 5, 6, 7, 8)), 
            "Invalid EQ-5D-3L responses-check the responses to each question",
               fixed = TRUE)
  expect_error(value_3L_Ind("UK", "VAS", c(-1, 1, 1, 1, 1)), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "VAS", -1, 1, 1, 1, 1), 
               "The responses are not valid", fixed = TRUE)
  expect_error(value_3L_Ind("DE", "VAS", 1, 1, 1, 1, 1), 
               "No country tariffs found for the country you specified for EQ-5D-3L. Please try later", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "VAS", 4, 5, 6, 5, 8), 
               "Responses not valid for EQ-5D-3L scores", fixed = TRUE)
  expect_error(value_3L_Ind("UK", "VAS", -1, 2, 3, 2, 2), 
               "The responses are not valid", fixed = TRUE)
  expect_error(value_3L_Ind("JP", "VAS", c(1, 2, 3, 2, 3)), 
               "No country tariffs found for the country you specified for EQ-5D-3L. Please try later", fixed = TRUE)
})

# # # ###########################################################################
context("EQ5D5L crosswalk mapping for individual responses")
test_that("EQ5D5L crosswalk mapping for individual responses", {
  expect_equal(map_5Lto3L_Ind("UK", "CW", NA, 1, 2, 3, 4), NA)
  expect_equal(map_5Lto3L_Ind("UK", "CW", 1, 2, 3, 4), NA)
  expect_equal(map_5Lto3L_Ind("UK", "CW", 12345), 0.0633, tol = 1e-3)
  expect_equal(map_5Lto3L_Ind("UK", "CW", 1, 1, 1, 2, 2), 0.767, tol = 1e-3)
  expect_error(map_5Lto3L_Ind("UK", "CW", 7, 1, 1, 2, 2))
  expect_error(map_5Lto3L_Ind("UK", "CW", 1, 1, 7, 2, 2))
  expect_error(map_5Lto3L_Ind("India", "CW", 1, 1, 2, 2, 2))
  expect_error(map_5Lto3L_Ind("UK", "map", 1, 1, 2, 2, 2))
  expect_error(map_5Lto3L_Ind("UK", "CW", c(10, 1), 2, 2, 2))
  expect_equal(map_5Lto3L_Ind("UK", "CW", c(1, 2, 3, 4, 5)), 0.0633, 
               tol = 1e-3)
  expect_error(map_5Lto3L_Ind("UK", "CW", c(1, 2, 3, 4, 7)))
  expect_error(map_5Lto3L_Ind("UK", "CW", c("sh", 2, 3, 4, 7)))
  expect_error(map_5Lto3L_Ind("UK", "CW", c("-2", 2, 3, 4, 7)))
  expect_error(map_5Lto3L_Ind("UK", "CW", 1, 0, 0, 0, 0))
  expect_error(map_5Lto3L_Ind("UK", "CW", 1, 1, 1, 1, -1))
  expect_error(map_5Lto3L_Ind("UK", "CW", c(1, 1, 1, 1, -1)))
})
# ###############################################################################################################
context("EQ5D5L scoring for all countries")
test_that("EQ5D5L scoring for all countries", {
  answers <- EQ5D5L_indexvalues.df
  total_entries <- seq(1, nrow(answers))
  total_countries <- c(
    "Canada", "China", "England", "Ethiopia", "France", "Germany", "Hong_Kong", 
    "Indonesia", "Ireland",
    "Japan", "Korea", "Malaysia", "Netherlands", "Poland", "Portugal", "Spain", 
    "Taiwan", "Thailand",
    "Uruguay", "USA", "Vietnam"
  )
  total <- length(total_countries)
  for (j in 1:1) {
    this_country <- total_countries[j]
    country_entry <- replace_space_underscore(total_countries[j])
    print(this_country)
    for (i in 1:nrow(answers)) {
      the_result <- value_5L_Ind(this_country, answers$state[total_entries[i]])
      this_col <- answers[[country_entry]]
      expect_equal(the_result, this_col[total_entries[i]], tolerance = 1e-3)
    }
  }
})
# # ########################################################################
context("EQ5D3L scoring for all countries")
test_that("EQ5D3L scoring for all countries", {
  answers <- EQ5D3L_indexvalues.df
  total_entries <- seq(1, nrow(answers))
  VAS_countrylist <- c(
    "Argentina", "Belgium", "Denmark", "Europe", "Finland", "Germany", 
    "Malaysia",
    "New_Zealand", "Slovenia", "Spain", "UK"
  )
  TTO_countrylist <- c(
    "Argentina", "Australia", "Brazil", "Canada", "Chile", "China", "Denmark",
    "France", "Germany", "Iran", "Italy", "Japan", "Korea", "Netherlands", 
    "Poland", "Portugal", "Singapore", "Spain", "Sri_Lanka", "Sweden",
    "Taiwan", "Thailand", "Trinidad_and_Tobago", "UK", "USA", "Zimbabwe"
  )
  common_countries <- Reduce(intersect, list(VAS_countrylist, 
                                             TTO_countrylist))
  all_countries <- sort(unique(c(VAS_countrylist, TTO_countrylist)))
  total <- length(all_countries)
  for (j in 1:1) {
    print(all_countries[j])
    if (all_countries[j] %in% common_countries) {
      country_entry <- replace_space_underscore(all_countries[j])
      TTOcol <- paste(country_entry, "TTO", sep = "")
      VAScol <- paste(country_entry, "VAS", sep = "")
      for (i in 1:nrow(answers)) {
        the_result_TTO <- value_3L_Ind(all_countries[j], "TTO", 
                                       answers$state[total_entries[i]])
        the_result_VAS <- value_3L_Ind(all_countries[j], "VAS", 
                                       answers$state[total_entries[i]])
        this_col_TTO <- answers[[TTOcol]]
        this_col_VAS <- answers[[VAScol]]
        expect_equal(the_result_TTO, this_col_TTO[total_entries[i]], 
                     tolerance = 1e-2)
        expect_equal(the_result_VAS, this_col_VAS[total_entries[i]], 
                     tolerance = 1e-2)
      }
    } else {
      country_entry <- replace_space_underscore(all_countries[j])
      col <- country_entry
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
        the_result <- value_3L_Ind(all_countries[j], method, 
                                   answers$state[total_entries[i]])
        this_col <- answers[[col]]
        expect_equal(the_result, as.numeric(this_col[total_entries[i]]), 
                     tolerance = 1e-2)
      }
    }
  }
})
# # # ###########################################################################
context("EQ5D5L crosswalk mapping for all countries")
test_that("EQ5D5L crosswalk mapping for all countries", {
  answers <- EQ5D5L_crosswalk_indexvalues.df
  total_entries <- seq(1, nrow(answers))
  end1 <- length(total_entries)
  total_countries <- c(
    "Denmark", "France", "Germany", "Japan", "Netherlands", 
    "Spain",  "Thailand", "UK", "USA", "Zimbabwe")
  total <- length(total_countries)
  for (j in 1:1) {
    this_country <- total_countries[j]
    country_entry <- replace_space_underscore(total_countries[j])
    print(this_country)
    for (i in 1:10) {
      the_result <- map_5Lto3L_Ind(this_country, "CW",
                                 answers$state[total_entries[i]])
      this_col <- answers[[country_entry]]
      expect_equal(the_result, this_col[total_entries[i]], 
                   tolerance = 9e-2)
    }
  }
})
# # # ###########################################################################
context("EQ5D5L valuation testing dataset")
test_that("EQ5D5L valuation testing dataset", {
  data <- data.frame(
    age = c(10, 20), sex = c("M", "F"),
    mo = c(1, 2), sc = c(1, 2), ua = c(3, 4), pd = c(3, 4), ad = c(3, 4))
  
    res <- value_5L(data, "mo", "sc", "ua", "pd", "ad", "England", NULL, c(10, 70))
    expect_equal(res$stats[2], 0.459)
    expect_equal(res$stats[9], 2)
    res <- value_5L(data, "mo", "sc", "ua", "pd", "ad", "England", "Male", 
                    c(0, 20))
    expect_equal(res$stats[2], 0.749)
    res <- value_5L(data, "mo", "sc", "ua", "pd", "ad", "England", "Male", 
                    NULL)
    expect_equal(res$stats[2], 0.749)
    
    expect_error(value_5L(data, "mo", "sc", "ua", "pd", "ad", "India", 
                          NULL, c(10, 70)))
    expect_error(value_5L(data, "mo", "sc", "ua", "pd", "ad", "", 
                          NULL, c(10, 70)))
    expect_error(value_5L(data, "mo", "sc", "ua", "pd", "ad", "England", 
                          NULL, c(0, 5)))
    data <- data.frame(
      age = c(10, 20), sex = c("M", "F"),
      mo = c(1, 2), sc = c(NA, 2), ua = c(NA, 4), pd = c(3, 4), ad = c(3, 4))
    res <- value_5L(data, "mo", "sc", "ua", "pd", "ad", "England", 
                    NULL, c(0, 20))
    expect_equal(res$stats[2], 0.169)
    res <- value_5L(data, "mo", "sc", "ua", "pd", "ad", "England", NULL, NULL)
    expect_equal(res$stats[2], 0.169)
    res <- value_5L(data, "mo", "sc", "ua", "pd", "ad", "England", 
                    "Male", NULL)
    expect_equal(res, "No relevant rows with non NA scores")
    res <- value_5L(data, "mo", "sc", "ua", "pd", "ad", "England", "Male", 
                    c(0, 20))
    expect_equal(res, "No relevant rows with non NA scores")
    
    data <- data.frame(
      age = c(10, 20), sex = c("M", "F"),
      one = c(1, 2), two = c(NA, 2), three = c(NA, 4), four = c(3, 4), 
      five = c(3, 4))
    expect_error(value_5L(data, "mo", "sc", "ua", "pd", "ad", "England", 
                          "Male", c(0, 20)))
})
# # # ###########################################################################
context("EQ5D3L valuation testing dataset")
test_that("EQ5D3L valuation testing", {
  data <- data.frame(
    age = c(10, 20), sex = c("M", "F"),
    mo = c(1, 2), sc = c(1, 2), ua = c(3, 3), pd = c(3, 2), ad = c(1, 1))
  res <- value_3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "TTO", NULL, 
                  c(10, 70))
  expect_equal(res$stats[2], 0.215)
  expect_equal(res$stats[9], 2)
  res <- value_3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "TTO", "Male", 
                  c(10, 70))
  expect_equal(res$stats[2], 0.17)
  res <- value_3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "TTO", "Male", 
                  NULL)
  expect_equal(res$stats[2], 0.17)
  # no data with the given criteria
  expect_error(value_3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "TTO", 
                        "Male", c(0, 5)))
  # no country tariff
  expect_error(value_3L(data, "mo", "sc", "ua", "pd", "ad", "India", "TTO", 
                        "Male", NULL))
  # no country tariff
  expect_error(value_3L(data, "mo", "sc", "ua", "pd", "ad", "", "TTO", 
                        "Male", NULL))
  data <- data.frame(
    age = c(10, 20), sex = c("M", "F"),
    mo = c(1, 2), sc = c(NA, 2), ua = c(NA, 2), pd = c(3, 1), ad = c(3, 2))
  res <- value_3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "TTO", "Male", 
                  c(0, 20))
  expect_equal(res, "No relevant rows with non NA scores")
  res <- value_3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "TTO", NULL, NULL)
  expect_equal(res$stats[2], 0.639)
  res <- value_3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "TTO", "Male", NULL)
  expect_equal(res, "No relevant rows with non NA scores")
  res <- value_3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "TTO", "Male", 
                  c(0, 20))
  expect_equal(res, "No relevant rows with non NA scores")
  
  data <- data.frame(
    age = c(10, 20), sex = c("M", "F"),
    one = c(1, 2), two = c(NA, 2), three = c(NA, 4), four = c(3, 4), 
    five = c(3, 4))
  # columns do not exist
  expect_error(value_3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "TTO", 
                        "Male", c(0, 20)))
})
# # # # ##########################################################################
context("EQ5D5L crosswalk  testing dataset")
test_that("EQ5D5L crosswalk  testing dataset", {
  data <- data.frame(
    age = c(10, 20), sex = c("M", "F"),
    mo = c(1, 2), sc = c(1, 2), ua = c(3, 4), pd = c(3, 4), ad = c(3, 4))
  res <- map_5Lto3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "CW", NULL, 
                    c(10, 70))
  expect_equal(res$stats[2], 0.408, tol = 1e-3)
  expect_equal(res$stats[9], 2)
  res <- map_5Lto3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "CW", "Male", 
                    c(0, 20))
  expect_equal(res$stats[2], 0.689)
  res <- map_5Lto3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "CW", "Male", 
                    NULL)
  expect_equal(res$stats[2], 0.689)
  res <- map_5Lto3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "CW", NULL, 
                    NULL)
  expect_equal(res$stats[2], 0.4079, tol = 1e-4)
  expect_error(map_5Lto3L(data, "mo", "sc", "ua", "pd", "ad", "India", "CW", 
                          NULL, c(10, 70)))
  expect_error(map_5Lto3L(data, "mo", "sc", "ua", "pd", "ad", "", "CW", NULL, 
                          c(10, 70)))
  expect_error(map_5Lto3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "CW", 
                          NULL, c(0, 5)))
  expect_error(map_5Lto3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "mp",
                          NULL, c(0, 70)))
  
  data <- data.frame(
    age = c(10, 20), sex = c("M", "F"),
    mo = c(1, 2), sc = c(NA, 2), ua = c(NA, 4), pd = c(3, 4), ad = c(3, 4))
  res <- map_5Lto3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "CW", NULL, 
                    c(0, 10))
  expect_equal(res, "No relevant rows with non NA scores")
  
  data <- data.frame(
    age = c(10, 20), sex = c("M", "F"),
    one = c(1, 2), two = c(NA, 2), three = c(NA, 4), four = c(3, 4), 
    five = c(3, 4))
  # columns do not exist
  expect_error(map_5Lto3L(data, "mo", "sc", "ua", "pd", "ad", "UK", "CW", 
                          "Male", c(0, 20)))
})
