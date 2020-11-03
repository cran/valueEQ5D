
###############################################################################
#' Function to check the EQ-5D-3L scores
#' @param dimen  a must input,response for EQ-5D-3L mobility  or the 5 digit 
#' response, or the vector of responses, e.g. 11111, c(1, 1, 1, 1, 1) or 1
#' @param dimen2 response for EQ-5D-3L self care, or NA if the responses 
#' are given as dimensions
#' @param dimen3  response for EQ-5D-3L usual activities,or NA if the 
#' responses are given as dimensions
#' @param dimen4  response for EQ-5D-3L pain/discomfort, or NA if the 
#' responses are given as dimensions
#' @param dimen5  response for EQ-5D-3L anxiety/depression, or NA if 
#' the responses are given as dimensions
#' @examples
#' check_scores_3L(c(1, 2, 3, 3, 3))
#' check_scores_3L(1, 2, 3, 3, 3)
#' check_scores_3L(1, 2, 3, 2, 3)
#' check_scores_3L(12323)
#' @export
check_scores_3L <- function(dimen, dimen2 = NA, dimen3 = NA, dimen4 = NA, 
                            dimen5 = NA) {
  responses <- c(dimen, dimen2, dimen3, dimen4, dimen5)
  # first value should be not be a NA, do not contain NA
  if (sum(is.na(dimen)) > 0) { 
    this_score <- NA
    return(NA)
  } else {
    if (length(dimen) != 5 && length(dimen) != 1) {
      stop("Invalid EQ-5D-3L responses-check the responses to each question")
    } else {
      if (length(dimen) == 5) { # first value a vector
        this_score <- paste(dimen, collapse = "")
        responses <- dimen
      } else {
        if (length(dimen) == 1) { 
          this_score <- paste(responses[!is.na(responses)], collapse = "")
          # first value 5 digit number or actual response for mobility
          responses <- convert_number_to_digits(this_score)
        }
      }
    }
  }
  if (!all(responses %in% 1:3)) {
    stop("Responses not valid for EQ-5D-3L scores")
  } else {
    this_score <- as.numeric(this_score)
    if (this_score < 11111) {
      return(NA)
    }else{
         return(responses)
    }
  }
}
################################################################################
#' Function to check the EQ-5D-5L scores
#' @param dimen  a must input,response for EQ-5D-3L mobility  or the 5 digit 
#' response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param dimen2 response for EQ-5D-5L  self care, or NA if the responses are
#'  given as dimensions
#' @param dimen3  response for EQ-5D-5L  usual activities,or NA if the responses
#'  are given as dimensions
#' @param dimen4  response for EQ-5D-5L  pain/discomfort, or NA if the responses
#'  are given as dimensions
#' @param dimen5  response for EQ-5D-5L  anxiety/depression, or NA if the 
#' responses are given as dimensions
#' @examples
#' check_scores_5L(c(1, 2, 3, 5, 3))
#' check_scores_5L(1, 2, 3, 4, 3)
#' check_scores_5L(12323)
#' @export
check_scores_5L <- function(dimen, dimen2 = NA, dimen3 = NA, dimen4 = NA, 
                            dimen5 = NA) {
  responses <- c(dimen, dimen2, dimen3, dimen4, dimen5)
  if (sum(is.na(dimen)) > 0) {
    # first value should be not be a NA, do not contain NA
    this_score <- NA
    return(NA)
  } else {
    if (length(dimen) != 5 && length(dimen) != 1) {
      stop("Invalid EQ-5D-5L responses-check the responses to each question")
    } else {
      if (length(dimen) == 5) { # first value a vector
        this_score <- paste(dimen, collapse = "")
        responses <- dimen
      } else {
        if (length(dimen) == 1) { 
          this_score <- paste(responses[!is.na(responses)], collapse = "")
          # first value 5 digit number or actual response for mobility
          responses <- convert_number_to_digits(this_score)
        }
      }
    }
  }
  if (!all(responses %in% 1:5)) {
    stop("Responses not valid for EQ-5D-5L scores")
  } else {
    this_score <- as.numeric(this_score)
    if (this_score < 11111) {
      return(NA)
    }else{
         return(responses)
    }
  }
}
#################################################################################
#' Function to value EQ-5D-5L scores for various countries
#' @param country a country name from the list Canada,China,England,
#' Germany,HongKong,Indonesia,Ireland,Japan,Korea,Malaysia,Netherlands,
#' Poland,Spain,Taiwan,Thailand,and Uruguay
#' @param dimen  a must input,response for EQ-5D-5L mobility  or the 5 digit 
#' response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param dimen2 response for EQ-5D-5L self care, or NA if the responses are 
#' given as dimen
#' @param dimen3  response for EQ-5D-5L usual activities,or NA if the responses 
#' are given as dimen
#' @param dimen4  response for EQ-5D-5L pain/discomfort, or NA if the responses 
#' are given as dimen
#' @param dimen5  response for EQ-5D-5L anxiety/depression, or NA if the 
#' responses are given as dimen
#' @return index values  if success, negative values if failure
#' @examples
#' value_5L_Ind("England", 23434)
#' value_5L_Ind("China", 2, 3, 4, 3, 4)
#' value_5L_Ind("Poland", c(1, 2, 3, 4, 3))
#' @export
value_5L_Ind <- function(country, dimen, dimen2 = NA, dimen3 = NA, dimen4 = NA,
                         dimen5 = NA) {
  country_list <- c(
    "Canada", "China", "England", "Ethiopia", "France", "Germany", "Hong_Kong", 
    "Indonesia", "Ireland", "Japan", "Korea", "Malaysia", "Netherlands", 
    "Poland", "Portugal", "Spain", "Taiwan", "Thailand", "Uruguay", "USA",
    "Vietnam"
  )
  country <- replace_space_underscore(country)
  if (country %in% country_list) {
    scores <- check_scores_5L(dimen, dimen2, dimen3, dimen4, dimen5)
    if (sum(is.na(scores)) > 0)  return(NA)
    if (sum(scores) > 0)  {
        eq5d_valueset <- EQ5D5L_tariffs.df
        names(scores) <- c("MO", "SC", "UA", "PD", "AD")
        rows <- paste0(names(scores), scores)
        rownum1 <- which(row.names(eq5d_valueset) == rows[1])
        rownum2 <- which(row.names(eq5d_valueset) == rows[2])
        rownum3 <- which(row.names(eq5d_valueset) == rows[3])
        rownum4 <- which(row.names(eq5d_valueset) == rows[4])
        rownum5 <- which(row.names(eq5d_valueset) == rows[5])
        rownumfh <- which(row.names(eq5d_valueset) == "fullHealth")
        rownuminter <- which(row.names(eq5d_valueset) == "intercept")
        rownumn4 <- which(row.names(eq5d_valueset) == "N4")
        rownumn45 <- which(row.names(eq5d_valueset) == "Num45sq")
        inter_value <- NA
        if (any(scores > 1) && !is.na(eq5d_valueset[rownuminter, country])) {
          inter_value <- eq5d_valueset[rownuminter, country]
        }
        n4value <- NA
        if (any(scores >= 4) && !is.na(eq5d_valueset[rownumn4, country])) {
          n4value <- eq5d_valueset[rownumn4, country]
        }
        n45 <- which(scores %in% c(4, 5))
        n45value <- NA
        if (length(n45) >= 1 & !is.na(eq5d_valueset[rownumn45, country])) {
          n45value <- (length(n45) - 1)^2 * eq5d_valueset[rownumn45, country]
        }
        n45sall <- 0
        if (length(n45) >= 1) {
          for (i in seq_len(length(n45))) {
            names45row <- paste0(names(scores)[n45[i]], "45")
            rownumn45r <- which(row.names(eq5d_valueset) == names45row)
            if (!is.na(eq5d_valueset[rownumn45r, country])) {
              n45rvalue <- eq5d_valueset[rownumn45r, country]
              n45sall <- n45sall + n45rvalue
            } else {
              n45rvalue <- 0
              n45sall <- n45sall + n45rvalue
            }
          }
        }
        dim_response <- c(
          eq5d_valueset[rownum1, country], eq5d_valueset[rownum2, country], 
          eq5d_valueset[rownum3, country],
          eq5d_valueset[rownum4, country], eq5d_valueset[rownum5, country]
        )
        sum_response <- sum(dim_response, na.rm = TRUE)
        values <- c(
          eq5d_valueset[rownumfh, country], inter_value, sum_response,
          n4value, n45value, n45sall
        )
        values_state <- sum(values, na.rm = TRUE)
       return(values_state)
      } 
    } else {
      stop("No tariffs found for the country you specified for EQ-5D-5L. Please try later")
    }
}

################################################################################
#' Function to value EQ-5D-5L scores for any country and group by gender and age
#' @param eq5dresponse_data the data containing eq5d responses
#' @param mo  column name for EQ-5D-5L mobility
#' @param sc  column name for response for EQ-5D-5L self care
#' @param ua  column name for response for EQ-5D-5L usual activities
#' @param pd  column name for response for EQ-5D-5L pain/discomfort
#' @param ad  column name for response for EQ-5D-5L anxiety/depression
#' @param country  country of interest, by default is England
#' @param groupby  male or female -grouping by gender, default NULL
#' @param agelimit  vector of ages to show upper and lower limits, default NULL
#' @return index value  if success, negative values for failure
#' @examples
#' data <- data.frame(
#'   age = c(10, 20), sex = c("M", "F"),
#'   mo = c(1, 2), sc = c(1, 2), ua = c(3, 4), pd = c(3, 4), ad = c(3, 4)
#' )
#' value_5L(data, "mo", "sc", "ua", "pd", "ad", "England", NULL, c(10, 70))
#' @export
#' @description Function to value EQ-5D-5L descriptive system to index value.
value_5L <- function(eq5dresponse_data, mo, sc, ua, pd, ad, country = "England", 
                     groupby = NULL, agelimit = NULL) {
  country <- replace_space_underscore(country)
  eq5d_colnames <- c(mo, sc, ua, pd, ad)
  ans_eq5d_colnames <- sapply(eq5d_colnames, check_column_exist, eq5dresponse_data)
  if (all(ans_eq5d_colnames == 0)) { # if the eq5d column names match
    working_data <- subset_gender_age_to_group(eq5dresponse_data, groupby, 
                                               agelimit)
    scores <- c()
    if (nrow(working_data) < 1) {
      stop("no entries with the given criteria - Please check the contents 
           or the criteria")
    } else {
      for (j in 1:nrow(working_data)) {
        res1 <- working_data[j, mo]
        res2 <- working_data[j, sc]
        res3 <- working_data[j, ua]
        res4 <- working_data[j, pd]
        res5 <- working_data[j, ad]
        this_score <- value_5L_Ind(country, c(res1, res2, res3, res4, res5))
          scores <- c(scores, this_score)
      }
      new_data <- cbind(working_data, scores)
      colnames(new_data) <- c(colnames(working_data), "EQ-5D-5L scores")
      scores_noNA <- scores[!is.na(scores)]
      if (length(scores_noNA) >= 1) {
        stats <- descriptive_stat_data_column(scores_noNA, "EQ-5D-5L")
        freq_table <- get_frequency_table(scores_noNA)
        first <- is.null(groupby) || toupper(groupby) == "NA" || is.na(groupby)
        second <- is.null(agelimit) || sum(toupper(agelimit) == "NA") != 0 || 
          sum(is.na(agelimit)) != 0
        if (first & second) {
          title <- paste("Histogram of EQ-5D-5L index values", sep = "")
        } else {
          if (first & !second) {
            title <- paste("Histogram of EQ-5D-5L index values",
                           " with ages between ", agelimit[1], " and ", 
                           agelimit[2],
                           sep = ""
            )
          } else {
            if (!first & second) {
              title <- paste("Histogram of EQ-5D-5L index values for ",
                             groupby,
                             sep = ""
              )
            } else {
              title <- paste("Histogram of EQ-5D-5L index values for ",
                             groupby, " with ages between ", agelimit[1], 
                             " and ", agelimit[2],
                             sep = ""
              )
            }
          }
        }
        oldpar <- graphics::par(no.readonly = TRUE)
        graphics::par(mar = c(4, 4, 2, 2))
        
        hist_plot <- graphics::hist(scores_noNA, main = title)
        results <- list("stats" = stats, "freq_table" = freq_table, 
                        "histogram" = hist_plot, "modified_data" = new_data)
        return(results)
        on.exit(graphics::par(oldpar))
      } else {
        print("No relevant rows with non NA scores")
      }
    }
  } else {
    stop("EQ-5D column names do not match")
  }
}
################################################################################
#' Function to value EQ-5D-3L scores for various countries
#' @param country a country name from the list Belgium,Brazil,Canada,Chile,
#' Denmark,Europe,Finland,France,Germany,Italy,Japan,Korea,Netherlands,
#' NewZealand,Poland,Portugal,Slovenia,Spain,Taiwan,Thailand,UK,USA,and Zimbabwe
#' @param method method name either TTO or VAS
#' @param dimen  a must input,response for EQ-5D-5L mobility  or the 5 digit 
#' response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param dimen2 response for EQ-5D-3L self care, or NA if the responses are 
#' given as dimen
#' @param dimen3  response for EQ-5D-3L usual activities,or NA if the responses 
#' are given as dimen
#' @param dimen4  response for EQ-5D-3L pain/discomfort, or NA if the responses 
#' are given as dimen
#' @param dimen5  response for EQ-5D-3L anxiety/depression, or NA if the
#'  responses are given as dimen
#' @return index value based if success, negative values for failure
#' @examples
#' value_3L_Ind("UK", "TTO", 23131)
#' value_3L_Ind("Spain", "TTO", 2, 3, 1, 3, 1)
#' value_3L_Ind("Denmark", "VAS", c(1, 2, 3, 1, 3))
#' @export
value_3L_Ind <- function(country, method, dimen, dimen2 = NA, dimen3 = NA, 
                         dimen4 = NA, dimen5 = NA) {
  countrylist <- c(
    "Argentina", "Australia", "Belgium", "Brazil", "Canada", "Chile", "China",
    "Denmark", "Europe", "Finland", "France", "Germany", "Iran", "Italy", 
    "Japan",
    "Korea", "Malaysia", "Netherlands", "New_Zealand", "Poland", "Portugal", 
    "Singapore", "Slovenia", "Spain", "Sri_Lanka", "Sweden",
    "Taiwan", "Thailand", "Trinidad_and_Tobago", "UK", "USA", "Zimbabwe"
  )
  
  VAS_countrylist <- c(
    "Argentina", "Belgium", "Denmark", "Europe", "Finland", "Germany", 
    "Malaysia",
    "New_Zealand", "Slovenia", "Spain", "UK"
  )
  TTO_countrylist <- c(
    "Argentina", "Australia", "Brazil", "Canada", "Chile", "China", "Denmark",
    "France", "Germany", "Iran", "Italy", "Japan", "Korea", "Netherlands", 
    "Poland",
    "Portugal", "Singapore", "Spain", "Sri_Lanka", "Sweden",
    "Taiwan", "Thailand", "Trinidad_and_Tobago", "UK", "USA", "Zimbabwe"
  )
  
  australia.impalusibleordering.scores <- c(33132, 12133, 13133, 22133, 23133, 
                                            32133, 33133, 12233, 13233, 22233, 
                                            23233, 32233, 33233, 33232, 33323, 
                                            13332, 13333, 23332, 23333, 32333, 
                                            33332, 33333)
  country <- replace_space_underscore(country)
  if (country %in% countrylist) {
    scores <- check_scores_3L(dimen, dimen2, dimen3, dimen4, dimen5)
    if (sum(is.na(scores)) > 0) return(NA)
    if (sum(scores) > 0)  {
        if (method == "TTO" && country %in% TTO_countrylist) {
          eq5d_valueset <- EQ5D3L_tariffs_TTO.df
        } else {
          if (method == "VAS" && country %in% VAS_countrylist) {
            eq5d_valueset <- EQ5D3L_tariffs_VAS.df
          } else {
            stop("No tariff found")
          }
        }
        score_num <- as.numeric(paste(scores, collapse = ""))
        if (country == "Australia" & sum(score_num %in% australia.impalusibleordering.scores) > 0) {
          values_state <- .correctImplausibleOrdering(scores)
        } else {
          names(scores) <- c("MO", "SC", "UA", "PD", "AD")
          rows <- paste0(names(scores), scores)
          col <- check_column_exist(country, eq5d_valueset)
          if (col == 0) {
            min2or3 <- which(scores %in% c(2, 3))
            if (length(min2or3) == 5) {
              all_equals2or3 <- 1
            } else {
              all_equals2or3 <- c()
            }
            which3 <- which(scores %in% c(3))
            which2 <- which(scores %in% c(2))
            rownums <- c()
            dim_response <- NA
            min3_value <- NA
            all_equals2or3_value <- NA
            min2or3_value <- NA
            c3sq_value <- NA
            d1_value <- NA
            i2_value <- NA
            i2_sq_value <- NA
            i3_value <- NA
            i3_sq_value <- NA
            only1sand2s_value <- NA
            only1sand3s_value <- NA
            atleast2andatleast3_value <- NA
            nos2withatleast3_value <- NA
            nos2Sq_value <- NA
            nos3Sq_value <- NA
            mo3sc3_value <- NA
            mo3ua3_value <- NA
            mo3pd3_value <- NA
            mo3ad3_value <- NA
            sc3ua3_value <- NA
            sc3pd3_value <- NA
            sc3ad3_value <- NA
            ua3pd3_value <- NA
            ua3ad3_value <- NA
            pd3ad3_value <- NA
            mo2ua2_value <- NA
            sc3ua2_value <- NA
            rownumfh <- which(row.names(eq5d_valueset) == "FullHealth")
            rownum_min2or3 <- which(row.names(eq5d_valueset) == "Constant")
            rownumn_min3 <- which(row.names(eq5d_valueset) == "N3")
            rownum_only1sand2s <- which(row.names(eq5d_valueset) == "Only1sand2s")
            rownum_only1sand3s <- which(row.names(eq5d_valueset) == "Only1sand3s")
            rownum_atleast2andatleast3 <- which(row.names(eq5d_valueset) == "Atleast2andatleast3")
            rownum_nos2withatleast3 <- which(row.names(eq5d_valueset) == "Nos2withatleast3")
            rownum_nos2Sq <- which(row.names(eq5d_valueset) == "Nos2Sq")
            rownum_nos3Sq <- which(row.names(eq5d_valueset) == "Nos3Sq")
            if (method == "TTO") {
              rownum_all_equals2or3 <- which(row.names(eq5d_valueset) == "X5")
              rownum_C3sq <- which(row.names(eq5d_valueset) == "C3sq")
              rownumn_D1 <- which(row.names(eq5d_valueset) == "D1")
              rownumn_I2 <- which(row.names(eq5d_valueset) == "I2")
              rownumn_I2_sq <- which(row.names(eq5d_valueset) == "I2_sq")
              rownumn_I3 <- which(row.names(eq5d_valueset) == "I3")
              rownumn_I3_sq <- which(row.names(eq5d_valueset) == "I3_sq")
              rownum_MO3SC3 <- which(row.names(eq5d_valueset) == "MO3SC3")
              rownum_MO3UA3 <- which(row.names(eq5d_valueset) == "MO3UA3")
              rownum_MO3PD3 <- which(row.names(eq5d_valueset) == "MO3PD3")
              rownum_MO3AD3 <- which(row.names(eq5d_valueset) == "MO3AD3")
              rownum_SC3UA3 <- which(row.names(eq5d_valueset) == "SC3UA3")
              rownum_SC3PD3 <- which(row.names(eq5d_valueset) == "SC3PD3")
              rownum_SC3AD3 <- which(row.names(eq5d_valueset) == "SC3AD3")
              rownum_UA3PD3 <- which(row.names(eq5d_valueset) == "UA3PD3")
              rownum_UA3AD3 <- which(row.names(eq5d_valueset) == "UA3AD3")
              rownum_PD3AD3 <- which(row.names(eq5d_valueset) == "PD3AD3")
              rownum_MO2UA2 <- which(row.names(eq5d_valueset) == "MO2UA2")
              rownum_SC3UA2 <- which(row.names(eq5d_valueset) == "SC3UA2")
            } else {
              rownum_all_equals2or3 <- NA
              rownum_C3sq <- NA
              rownumn_D1 <- NA
              rownumn_I2 <- NA
              rownumn_I2_sq <- NA
              rownumn_I3 <- NA
              rownumn_I3_sq <- NA
              rownum_MO3SC3 <- NA
              rownum_MO3UA3 <- NA
              rownum_MO3PD3 <- NA
              rownum_MO3AD3 <- NA
              rownum_SC3UA3 <- NA
              rownum_SC3PD3 <- NA
              rownum_SC3AD3 <- NA
              rownum_UA3PD3 <- NA
              rownum_UA3AD3 <- NA
              rownum_PD3AD3 <- NA
              rownum_MO2UA2 <- NA
              rownum_SC3UA2 <- NA
            }
            if (length(min2or3) > 0) {
              for (i in seq_len(length(min2or3))) {
                rownams <- row.names(eq5d_valueset)
                ro <- which(rownams == rows[min2or3[i]])
                rownums <- cbind(rownums, ro)
              }
              dim_response <- eq5d_valueset[rownums, country]
            }
            if (any(scores >= 3) && 
                !is.na(eq5d_valueset[rownumn_min3, country])) {
              min3_value <- eq5d_valueset[rownumn_min3, country]
            }
            if (length(which3) >= 1 & sum(is.na(rownum_C3sq) == 0)) {
              if (!is.na(eq5d_valueset[rownum_C3sq, country])) {
                c3sq_value <- (length(which3))^2 * eq5d_valueset[rownum_C3sq, 
                                                                 country]
              }
            }
            if (length(all_equals2or3) >= 1 & 
                sum(is.na(rownum_all_equals2or3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_all_equals2or3, country])) {
                all_equals2or3_value <- eq5d_valueset[rownum_all_equals2or3, 
                                                      country]
              }
            }
            if (sum(scores) > 5 & length(min2or3) >= 1 & 
                sum(is.na(rownum_min2or3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_min2or3, country])) {
                min2or3_value <- eq5d_valueset[rownum_min2or3, country]
              }
            }
            if (sum(scores) > 5 & length(min2or3) >= 1 & 
                sum(is.na(rownumn_D1) == 0)) {
              if (!is.na(eq5d_valueset[rownumn_D1, country])) {
                d1_value <- (length(min2or3) - 1) * 
                  eq5d_valueset[rownumn_D1, country]
              }
            }
            if (sum(scores) > 5 & length(which2) >= 1 
                & sum(is.na(rownumn_I2) == 0)) {
              if (!is.na(eq5d_valueset[rownumn_I2, country])) {
                i2_value <- (length(which2) - 1) * 
                  eq5d_valueset[rownumn_I2, country]
              }
            }
            if (sum(scores) > 5 & length(which2) >= 1 & 
                sum(is.na(rownumn_I2_sq) == 0)) {
              if (!is.na(eq5d_valueset[rownumn_I2_sq, country])) {
                i2_sq_value <- (length(which2) - 1)^2 * 
                  eq5d_valueset[rownumn_I2_sq, country]
              }
            }
            if (sum(scores) > 5 & length(which3) >= 1 & 
                sum(is.na(rownumn_I3) == 0)) {
              if (!is.na(eq5d_valueset[rownumn_I3, country])) {
                i3_value <- (length(which3) - 1) * 
                  eq5d_valueset[rownumn_I3, country]
              }
            }
            if (sum(scores) > 5 & length(which3) >= 1 & 
                sum(is.na(rownumn_I3_sq) == 0)) {
              if (!is.na(eq5d_valueset[rownumn_I3_sq, country])) {
                i3_sq_value <- (length(which3) - 1)^2 * 
                  eq5d_valueset[rownumn_I3_sq, country]
              }
            }
            if (all(scores <= 2) & !all(scores == 1) & 
                sum(is.na(rownum_only1sand2s) == 0)) {
              if (!is.na(eq5d_valueset[rownum_only1sand2s, country])) {
                only1sand2s_value <- eq5d_valueset[rownum_only1sand2s, country]
              }
            }
            ## !all(scores==3) & need ??
            if (!any(scores == 2) & !all(scores == 1) & 
                sum(is.na(rownum_only1sand3s) == 0)) {
              if (!is.na(eq5d_valueset[rownum_only1sand3s, country])) {
                only1sand3s_value <- eq5d_valueset[rownum_only1sand3s, country]
              }
            }
            if (any(scores == 2) & any(scores == 3) & 
                sum(is.na(rownum_atleast2andatleast3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_atleast2andatleast3, country])) {
                atleast2andatleast3_value <- eq5d_valueset[rownum_atleast2andatleast3, country]
              }
            }
            if (any(scores == 2) & any(scores == 3) & 
                sum(is.na(rownum_nos2withatleast3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_nos2withatleast3, country])) {
                nos2withatleast3_value <- length(which(scores == 2)) * 
                  eq5d_valueset[rownum_nos2withatleast3, country]
              }
            }
            if (any(scores == 2) & sum(is.na(rownum_nos2Sq) == 0)) {
              if (!is.na(eq5d_valueset[rownum_nos2Sq, country])) {
                nos2Sq_value <- (length(which(scores == 2)))^2 * 
                  eq5d_valueset[rownum_nos2Sq, country]
              }
            }
            if (any(scores == 3) & sum(is.na(rownum_nos3Sq) == 0)) {
              if (!is.na(eq5d_valueset[rownum_nos3Sq, country])) {
                nos3Sq_value <- (length(which(scores == 3)))^2 * 
                  eq5d_valueset[rownum_nos3Sq, country]
              }
            }
            if (scores[["MO"]] == 3 & scores[["SC"]] == 3 & 
                sum(is.na(rownum_MO3SC3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_MO3SC3, country])) {
                mo3sc3_value <- eq5d_valueset[rownum_MO3SC3, country]
              }
            }
            if (scores[["MO"]] == 3 & scores[["UA"]] == 3 & 
                sum(is.na(rownum_MO3UA3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_MO3UA3, country])) {
                mo3ua3_value <- eq5d_valueset[rownum_MO3UA3, country]
              }
            }
            if (scores[["MO"]] == 3 & scores[["PD"]] == 3 & 
                sum(is.na(rownum_MO3PD3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_MO3PD3, country])) {
                mo3pd3_value <- eq5d_valueset[rownum_MO3PD3, country]
              }
            }
            if (scores[["MO"]] == 3 & scores[["AD"]] == 3 & 
                sum(is.na(rownum_MO3AD3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_MO3AD3, country])) {
                mo3ad3_value <- eq5d_valueset[rownum_MO3AD3, country]
              }
            }
            if (scores[["SC"]] == 3 & scores[["UA"]] == 3 & 
                sum(is.na(rownum_SC3UA3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_SC3UA3, country])) {
                sc3ua3_value <- eq5d_valueset[rownum_SC3UA3, country]
              }
            }
            if (scores[["SC"]] == 3 & scores[["PD"]] == 3 & 
                sum(is.na(rownum_SC3PD3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_SC3PD3, country])) {
                sc3pd3_value <- eq5d_valueset[rownum_SC3PD3, country]
              }
            }
            if (scores[["SC"]] == 3 & scores[["AD"]] == 3 & 
                sum(is.na(rownum_SC3AD3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_SC3AD3, country])) {
                sc3ad3_value <- eq5d_valueset[rownum_SC3AD3, country]
              }
            }
            if (scores[["UA"]] == 3 & scores[["PD"]] == 3 & 
                sum(is.na(rownum_UA3PD3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_UA3PD3, country])) {
                ua3pd3_value <- eq5d_valueset[rownum_UA3PD3, country]
              }
            }
            if (scores[["UA"]] == 3 & scores[["AD"]] == 3 & 
                sum(is.na(rownum_UA3AD3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_UA3AD3, country])) {
                ua3ad3_value <- eq5d_valueset[rownum_UA3AD3, country]
              }
            }
            if (scores[["PD"]] == 3 & scores[["AD"]] == 3 & 
                sum(is.na(rownum_PD3AD3) == 0)) {
              if (!is.na(eq5d_valueset[rownum_PD3AD3, country])) {
                pd3ad3_value <- eq5d_valueset[rownum_PD3AD3, country]
              }
            }
            if (scores[["MO"]] == 2 & scores[["UA"]] == 2 & 
                sum(is.na(rownum_MO2UA2) == 0)) {
              if (!is.na(eq5d_valueset[rownum_MO2UA2, country])) {
                mo2ua2_value <- eq5d_valueset[rownum_MO2UA2, country]
              }
            }
            if (scores[["SC"]] == 3 & scores[["UA"]] == 2 &
                sum(is.na(rownum_SC3UA2) == 0)) {
              if (!is.na(eq5d_valueset[rownum_SC3UA2, country])) {
                sc3ua2_value <- eq5d_valueset[rownum_SC3UA2, country]
              }
            }
            if (country == "Germany" && method == "VAS") {
              prod.response <- prod(dim_response, na.rm = TRUE)
              values <- c(
                eq5d_valueset[rownumfh, country], prod.response,
                min2or3_value, min3_value, all_equals2or3_value, 
                c3sq_value, d1_value, i2_value,
                i2_sq_value, i3_value, i3_sq_value, only1sand2s_value, 
                only1sand3s_value, atleast2andatleast3_value, 
                nos2withatleast3_value,
                nos2Sq_value, nos3Sq_value
              )
              values_state <- prod(values, na.rm = TRUE)
            } else {
              sum_response <- sum(dim_response, na.rm = TRUE)
              values <- c(
                eq5d_valueset[rownumfh, country], sum_response, min2or3_value, 
                min3_value, all_equals2or3_value, c3sq_value, d1_value, 
                i2_value,
                i2_sq_value, i3_value, i3_sq_value, only1sand2s_value, 
                only1sand3s_value, atleast2andatleast3_value, 
                nos2withatleast3_value,
                nos2Sq_value, nos3Sq_value, mo3sc3_value, mo3ua3_value, 
                mo3pd3_value, mo3ad3_value, sc3ua3_value, sc3pd3_value, 
                sc3ad3_value,
                ua3pd3_value, ua3ad3_value, pd3ad3_value, mo2ua2_value, 
                sc3ua2_value
              )
              values_state <- sum(values, na.rm = TRUE)
            }
          } else {
            stop("No country tariffs on valueset")
          }
        }
        return(values_state)
    }
  } else {
    stop("No country tariffs found for the country you specified for EQ-5D-3L. Please try later")
  }
}
################################################################################
#' Function to value EQ-5D-3L columns to index values for any country and group
#'  by gender and age
#' @param eq5dresponse_data the data containing eq5d responses
#' @param mo  column name for EQ-5D-3L mobility
#' @param sc column name for response for EQ-5D-3L self care
#' @param ua  column name for response for EQ-5D-3L usual activities
#' @param pd  column name for response for EQ-5D-3L pain/discomfort
#' @param ad  column name for response for EQ-5D-3L anxiety/depression
#' @param country  country of interest, by default is UK, if groupby has to 
#' specify the country should be specified
#' @param method Either "TTO" or "VAS"
#' @param groupby  male or female -grouping by gender, default NULL
#' @param agelimit  vector of ages to show upper and lower limits
#' @return the descriptive statistics of index values, frequency table and 
#' the modified data where the last column will be the index values
#' data<-data.frame(age=c(10,20),sex=c("M","F"),mo=c(1,2),sc=c(1,2),ua=c(3,4),
#' pd=c(3,1),ad=c(3,1))
#' value_3L(data, "mo", "sc","ua", "pd", "ad","UK","TTO",NULL,c(10,70))
#' @export
#' @description Main function to value EQ-5D-5L descriptive system to 5L 
#' index values.
value_3L <- function(eq5dresponse_data, mo, sc, ua, pd, ad, country, method, 
                     groupby, agelimit) {
  country <- replace_space_underscore(country)
  eq5d_colnames <- c(mo, sc, ua, pd, ad)
  ans_eq5d_colnames <- sapply(eq5d_colnames, check_column_exist,
                              eq5dresponse_data)
  if (all(ans_eq5d_colnames == 0)) { # if the eq5d column names match
    working_data <- subset_gender_age_to_group(eq5dresponse_data, 
                                               groupby, agelimit)
    if (nrow(working_data) < 1) {
      stop("no entries with the given criteria - Please check 
           the contents or the criteria")
    } else {
      scores <- c()
      for (j in 1:nrow(working_data)) {
        res1 <- working_data[j, mo]
        res2 <- working_data[j, sc]
        res3 <- working_data[j, ua]
        res4 <- working_data[j, pd]
        res5 <- working_data[j, ad]
        this_score <- value_3L_Ind(country, method, res1, res2,
                                   res3, res4, res5)
        scores <- c(scores, this_score)
      }
      new_data <- cbind(working_data, scores)
      colnames(new_data) <- c(colnames(working_data), "EQ-5D-3L scores")
      scores_noNA <- scores[!is.na(scores)]
      if (length(scores_noNA) >= 1) {
        stats <- descriptive_stat_data_column(scores_noNA, "EQ-5D-3L")
        freq_table <- get_frequency_table(scores_noNA)
        first <- is.null(groupby) || toupper(groupby) == "NA" || 
          is.na(groupby)
        second <- is.null(agelimit) || sum(toupper(agelimit) == "NA") != 0 ||
          sum(is.na(agelimit)) != 0
        if (first & second) {
          title <- paste("Histogram of EQ-5D-3L index values", sep = "")
        } else {
          if (first & !second) {
            title <- paste("Histogram of EQ-5D-3L index values",
              " with ages between ", agelimit[1], " and ", agelimit[2],
              sep = ""
            )
          } else {
            if (!first & second) {
              title <- paste("Histogram of EQ-5D-3L index values for ",
                groupby,
                sep = ""
              )
            } else {
              title <- paste("Histogram of EQ-5D-3L index values for ",
                groupby, " with ages between ", agelimit[1], " and ", 
                agelimit[2],
                sep = ""
              )
            }
          }
        }
        hist_plot <- graphics::hist(scores_noNA, main = title)
        results <- list("stats" = stats, "frequency_table" = freq_table, 
                        "histogram" = hist_plot, "modified_data" = new_data)
        return(results)
      } else {
        print("No relevant rows with non NA scores")
      }
    }
  } else {# if the eq 5d column names do not match
    stop("EQ-5D column names do not match")
  }
}
################################################################################
#' Function to map EQ-5D-5L descriptive system to 3L index value
#' @param country  default is "UK"
#' @param method CW cross walk
#' @param dimen  response for EQ-5D-5L mobility  or the 5 digit response, or 
#' the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param dimen2 response for EQ-5D-5L self care, or NA if the responses are 
#' given as dimen
#' @param dimen3  response for EQ-5D-5L usual activities,or NA if the responses
#'  are given as dimen
#' @param dimen4  response for EQ-5D-5L pain/discomfort, or NA if the responses
#'  are given as dimen
#' @param dimen5  response for EQ-5D-5L anxiety/depression, or NA if the 
#' responses are given as dimen
#' @return index value of EQ-5D-3L, -1 if any error
#' @examples
#' map_5Lto3L_Ind("UK", "CW", 11125)
#' map_5Lto3L_Ind("UK", "CW", c(1, 1, 1, 2, 5))
#' map_5Lto3L_Ind("UK", "CW", 1, 1, 1, 2, 5)
#' @export
#' @description Function to map EQ-5D-5L descriptive system to 3L index value
#'(ref:Van Hout et al 2012 and code inspired from 
#'https://github.com/brechtdv/eq5d-mapping)
map_5Lto3L_Ind <- function(country = "UK", method = "CW", dimen, dimen2 = NA, 
                         dimen3 = NA, dimen4 = NA, dimen5 = NA) {
  country_list <- c("Denmark", "France", "Germany", "Japan", "Netherlands", 
                    "Spain", "Thailand", "UK", "USA", "Zimbabwe")
  country <- replace_space_underscore(country)
  if (country %in% country_list) {
    responses <- c(dimen, dimen2, dimen3, dimen4, dimen5)
    if (sum(is.na(dimen)) > 0) {
      # first value should be not be a NA, do not contain NA
      this_score_5L <- NA
      values_state <- NA
      return(values_state)
    } else {
      # check first value should be a vector containing responses or a 
      #5digit number
      if (length(dimen) != 5 && length(dimen) != 1) {
        stop("Expecting the full response as5 digit number or just 
             the response for mobilty")
      } else {# first value a vector or a 5 figit number
        if (length(dimen) == 5) {# first value a vector
          if (any(dimen < 1) || any(dimen > 5)) {
            stop("Invalid EQ-5D-5L responses-check the responses to 
                 each question")
          }
          this_score_5L <- as.numeric(paste(dimen, collapse = ""))
        } else {# first value 5 digit number or actual response for mobility
          if (length(dimen) == 1) { 
            if (dimen >= 11111 && dimen <= 55555) { # valid 5 digit number
              this_score_5L <- dimen
            } else {
              if (dimen <= 5 && dimen > 0) { # valid response to mobility
                four_res <- c(dimen2, dimen3, dimen4, dimen5)
                if (sum(is.na(four_res)) == 0) {
                  if (all(responses <= 5) && all(responses > 0)) {
                    this_score_5L <- paste(responses, collapse = "") 
                    # all valid and generate the score
                  } else {# error values
                    stop("Invalid EQ-5D-5L responses-check the responses 
                         to each question")
                  }
                } else {
                  # missing values
                  this_score_5L <- NA
                  values_state <- NA
                  return(values_state)
                }
              } else {
                stop("Invalid EQ-5D-5L response to mobility")
              }
            }
          }
        }
      }
    }

    ## create a vector of all possible 3L index values (length == 3^5)
    index_3L <- numeric(243)
      ## create a dataframe of all possible 3L scores
      scores_3L <-
        expand.grid(
          AD = seq(3),
          PD = seq(3),
          UA = seq(3),
          SC = seq(3),
          MO = seq(3)
      )
      ## calculate the index value for each score
      ## using function EQ5D_be based on Cleemput et al, 2010
      for (i in seq(243)) {
        index_3L[i] <-
          value_3L_Ind(
            country, "TTO", scores_3L[i, "MO"],
            scores_3L[i, "SC"],
            scores_3L[i, "UA"],
            scores_3L[i, "PD"],
            scores_3L[i, "AD"]
          )
      }
      ## create a dataframe of all possible 5L scores
      scores_5L <-
        expand.grid(
          AD = seq(5),
          PD = seq(5),
          UA = seq(5),
          SC = seq(5),
          MO = seq(5)
        )
      ## 5L to 3L CROSSWALK
      ## load 'probability matrix' from 'EQ-5D-5L_Crosswalk_Value_Sets'
      ## this is saved as dataframe 'm'
    if (toupper(method) == "CW") {
        prob.matrix <- Probability_matrix_crosswalk.df
        m <- prob.matrix
        rows_m <- nrow(m)
        cols_m <- ncol(m)
        if (rows_m != 3125 || cols_m != 243) {
          stop("Error in number of cols or rows of probability matrix")
        }
        ## multiply each row of 't(m)' with 'index_3L'
        m_prod <- t(t(m) * index_3L)
        ## obtain sum per row
        ## crosswalking index value for each 5L score
        m_sums <- rowSums(m_prod)
        ## reorder columns and convert to matrix
        scores_5L <- with(scores_5L, cbind(MO, SC, UA, PD, AD))
        ## create 5L score labels
        scores_5L_chr <- apply(scores_5L, 1, paste, collapse = "")
        this_score <- which(scores_5L_chr == paste(this_score_5L, 
                                                   collapse = ""))
        if (country == "Zimbabwe" & this_score_5L == "11111") {
          return(0.9)
        } else {
          return(m_sums[this_score])
        }
    } else {
        stop("The specified method is not implemented")
    }
  } else {
    stop("Crosswalk for the country specified is not implemented")
  }
}
###############################################################################
#' Function to map EQ-5D-5L scores to EQ-5D-3L index values as per the 
#' specific country and group by gender and age
#' @param eq5dresponse_data the data containing eq5d5L responses
#' @param mobility  column name for EQ-5D-5L mobility
#' @param self_care column name for response for EQ-5D-5L self care
#' @param usual_activities  column name for response for EQ-5D-5L usual
#'  activities
#' @param pain_discomfort  column name for response for EQ-5D-5L pain/discomfort
#' @param anxiety  column name for response for EQ-5D-5L anxiety/depression
#' @param country  country of interest, by default is UK, if groupby has to 
#' specify the country should be specified
#' @param method CW cross walk
#' @param groupby  male or female -grouping by gender, default NULL
#' @param agelimit  vector of ages to show upper and lower limits
#' @return index value  if success, negative values for failure
#' @examples
#' map_5Lto3L(data.frame(
#'   mo = c(1), sc = c(4), ua = c(4), pd = c(3),
#'   ad = c(3)
#' ), "mo", "sc", "ua", "pd", "ad")
#' @export
#' @description Function to map EQ-5D-5L scores to EQ-5D-3L index values
map_5Lto3L <- function(eq5dresponse_data, mobility, self_care, usual_activities, 
                      pain_discomfort, anxiety, country = "UK", method = "CW",
                      groupby = NULL, agelimit = NULL) {
  country <- replace_space_underscore(country)
  eq5d_colnames <- c(mobility, self_care, usual_activities, pain_discomfort,
                     anxiety)
  ans_eq5d_colnames <- sapply(eq5d_colnames, check_column_exist, 
                              eq5dresponse_data)
  if (all(ans_eq5d_colnames == 0)) { # if the eq5d column names match
    working_data <- subset_gender_age_to_group(eq5dresponse_data, groupby,
                                               agelimit)
    scores <- c()
    if (nrow(working_data) < 1) {
      stop("no entries with the given criteria - Please check the contents 
           or the criteria")
    } else {
      for (j in 1:nrow(working_data)) {
        res1 <- working_data[j, mobility]
        res2 <- working_data[j, self_care]
        res3 <- working_data[j, usual_activities]
        res4 <- working_data[j, pain_discomfort]
        res5 <- working_data[j, anxiety]
        this_score <- map_5Lto3L_Ind(country, method, c(res1, res2, res3, 
                                                      res4, res5))
        scores <- c(scores, this_score)

      }
      new_data <- cbind(working_data, scores)
      colnames(new_data) <- c(colnames(working_data), "Mapped EQ-5D-3L scores")
      scores_noNA <- scores[!is.na(scores)]
      if (length(scores_noNA) >= 1) {
          stats <- descriptive_stat_data_column(scores_noNA, "EQ-5D-3L")
          freq_table <- get_frequency_table(scores_noNA)
          first <- is.null(groupby) || toupper(groupby) == "NA" || 
            is.na(groupby)
          second <- is.null(agelimit) || sum(toupper(agelimit) == "NA") != 0 || 
            sum(is.na(agelimit)) != 0
          if (first & second) {
            title <- paste("Histogram of EQ-5D-3L index values", sep = "")
          } else {
            if (first & !second) {
              title <- paste("Histogram of EQ-5D-3L index values",
                " with ages between ", agelimit[1], " and ", agelimit[2],
                sep = ""
              )
            } else {
              if (!first & second) {
                title <- paste("Histogram of EQ-5D-3L index values for ",
                  groupby,
                  sep = ""
                )
              } else {
                title <- paste("Histogram of EQ-5D-3L index values for ",
                  groupby, " with ages between ", agelimit[1], " and ", 
                  agelimit[2], sep = ""
                )
              }
            }
          }
          hist_plot <- graphics::hist(scores, main = title)
          results <- list("stats" = stats, "freq_table" = freq_table, 
                          "histogram" = hist_plot, "modified_data" = new_data)
          return(results)
      } else {
        print("No relevant rows with non NA scores")
      }
    }
  } else {# if the eq 5d column names do not match
    stop("EQ-5D column names do not match")
  }
}
################################################################################
#' Function to correct implausible ordering in Australian valueset for EQ-5D-3L
#' @param scores , EQ-5D-3L scores as a number
#' @return the value that read from the stored dataframe
#' @examples
#' .correctImplausibleOrdering(11121)
#' @export
#' @description Correcting the implausible ordering
.correctImplausibleOrdering <- function(scores) {
  value <- 0
  score_num <- as.numeric(paste(scores, collapse = ""))
  australia_impalusibleordering_scores <- c(
    33132, 12133, 13133, 22133, 23133, 32133, 33133, 12233, 13233, 
    22233, 23233, 32233, 33233,
    33232, 33323, 13332, 13333, 23332, 23333, 32333, 33332, 33333
  )
  australia_impalusibleordering_values <- c(
    -0.045, 0.154, 0.154, 0.086, 0.086, -0.083, -0.083, 0.101, 0.101, 0.033, 
    0.033, -0.136, -0.136, -0.098, -0.199, 0.020, 0.020, -0.048, -0.048, -0.206,
    -0.217, -0.217
  )
  if (sum(score_num %in% australia_impalusibleordering_scores) > 0) {
    index <- which(score_num == australia_impalusibleordering_scores)
    value <- australia_impalusibleordering_values[index]
  }
  return(value)
}
################################################################################
