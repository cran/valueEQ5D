
###########################################################################################################
#' Function to check the EQ-5D-3L scores
#' @param dimen  a must input,response for EQ-5D-3L mobility  or the 5 digit response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param dimen2 response for EQ-5D-3L self care, or NA if the responses are given as dimen
#' @param dimen3  response for EQ-5D-3L usual activities,or NA if the responses are given as dimen
#' @param dimen4  response for EQ-5D-3L pain/discomfort, or NA if the responses are given as dimen
#' @param dimen5  response for EQ-5D-3L anxiety/depression, or NA if the responses are given as dimen
#' @examples
#' checkScores3L(c(1, 2, 3, 3, 3))
#' checkScores3L(1, 2, 3, 3, 3)
#' checkScores3L(1, 2, 3, 2, 3)
#' checkScores3L(12323)
#' @export
checkScores3L <- function(dimen, dimen2 = NA, dimen3 = NA, dimen4 = NA, dimen5 = NA) {
  responses <- c(dimen, dimen2, dimen3, dimen4, dimen5)
  if (sum(is.na(dimen)) > 0) { # first value should be not be a NA, do not contain NA
    this.score <- NA
    return(NA)
  } else {
    if (length(dimen) != 5 && length(dimen) != 1) {
      stop("Invalid EQ-5D-3L responses-check the responses to each question")
      # return(-1)
    } else {
      if (length(dimen) == 5) { # first value a vector
        this.score <- paste(dimen, collapse = "")
        responses <- dimen
      } else {
        if (length(dimen) == 1) { # first value 5 digit number or actual response for mobility
          this.score <- paste(responses[!is.na(responses)], collapse = "")
          if (sum(convertNumberToIndividualDigits(this.score)) != -1) {
            responses <- convertNumberToIndividualDigits(this.score)
          } else {
            responses <- NA
            # stop("Can not convert to individual digits")
          }
        }
      }
    }
  }
  if (!all(responses %in% 1:3)) {
    stop("Responses not valid for EQ-5D-3L scores")
    # return(-2)
  } else {
    this.score <- as.numeric(this.score)
    if (this.score < 11111 || this.score > 33333) {
      if (this.score < 0 || this.score > 33333) {
        stop("Responses not valid for EQ-5D-3L scores")
        # return(-3)
      } else {
        # stop("Responses not valid for EQ-5D-3L scores or some missing")
        return(NA)
      }
    } else {
      return(responses)
    }
  }
}
###########################################################################################################
#' Function to check the EQ-5D-5L scores
#' @param dimen  a must input,response for EQ-5D-3L mobility  or the 5 digit response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param dimen2 response for EQ-5D-5L  self care, or NA if the responses are given as dimen
#' @param dimen3  response for EQ-5D-5L  usual activities,or NA if the responses are given as dimen
#' @param dimen4  response for EQ-5D-5L  pain/discomfort, or NA if the responses are given as dimen
#' @param dimen5  response for EQ-5D-5L  anxiety/depression, or NA if the responses are given as dimen
#' @examples
#' checkScores5L(c(1, 2, 3, 5, 3))
#' checkScores5L(1, 2, 3, 4, 3)
#' checkScores5L(12323)
#' @export
checkScores5L <- function(dimen, dimen2 = NA, dimen3 = NA, dimen4 = NA, dimen5 = NA) {
  responses <- c(dimen, dimen2, dimen3, dimen4, dimen5)
  if (sum(is.na(dimen)) > 0) { # first value should be not be a NA, do not contain NA
    this.score <- NA
    return(NA)
  } else {
    if (length(dimen) != 5 && length(dimen) != 1) {
      stop("Invalid EQ-5D-5L responses-check the responses to each question")
      # return(-1)
    } else {
      if (length(dimen) == 5) { # first value a vector
        this.score <- paste(dimen, collapse = "")
        responses <- dimen
      } else {
        if (length(dimen) == 1) { # first value 5 digit number or actual response for mobility
          this.score <- paste(responses[!is.na(responses)], collapse = "")
          if (sum(convertNumberToIndividualDigits(this.score)) != -1) {
            responses <- convertNumberToIndividualDigits(this.score)
          } else {
            stop("Can not convert to individual digits")
            # return(-4)
          }
        }
      }
    }
  }
  if (!all(responses %in% 1:5)) {
    if (sum(is.na(responses) > 0)) {
      stop("Responses not valid for EQ-5D-5L scores")
      return(NA)
    } else {
      stop("Responses not valid for EQ-5D-5L scores")
      # return(-2)
    }
  } else {
    this.score <- as.numeric(this.score)
    if (this.score < 11111 || this.score > 55555) {
      if (this.score < 0 || this.score > 55555) {
        stop("Responses not valid for EQ-5D-5L scores")
        # return(-3)
      } else {
        # stop("Responses not valid for EQ-5D-5L scores or some missing")
        return(NA)
      }
    } else {
      return(responses)
    }
  }
}
##########################################################################################################
#' Function to value EQ-5D-5L scores for various countries
#' @param country a country name from the list Canada,China,England,Germany,HongKong,Indonesia,Ireland,Japan,Korea,Malaysia,Netherlands,Poland,Spain,Taiwan,Thailand,and Uruguay
#' @param dimen  a must input,response for EQ-5D-5L mobility  or the 5 digit response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param dimen2 response for EQ-5D-5L self care, or NA if the responses are given as dimen
#' @param dimen3  response for EQ-5D-5L usual activities,or NA if the responses are given as dimen
#' @param dimen4  response for EQ-5D-5L pain/discomfort, or NA if the responses are given as dimen
#' @param dimen5  response for EQ-5D-5L anxiety/depression, or NA if the responses are given as dimen
#' @return index values  if success, negative values if failure
#' @examples
#' value5LInd("England", 23434)
#' value5LInd("China", 2, 3, 4, 3, 4)
#' value5LInd("Poland", c(1, 2, 3, 4, 3))
#' @export
value5LInd <- function(country, dimen, dimen2 = NA, dimen3 = NA, dimen4 = NA, dimen5 = NA) {
  countrylist <- c(
    "Canada", "China", "England", "Ethiopia", "France", "Germany", "Hong_Kong", "Indonesia", "Ireland",
    "Japan", "Korea", "Malaysia", "Netherlands", "Poland", "Portugal", "Spain", "Taiwan", "Thailand",
    "Uruguay", "USA", "Vietnam"
  )
  if (replaceSpaceUnderscore(country) == -1) {
    stop("Country name empty")
    # return("Country name empty")
  } else {
    country <- replaceSpaceUnderscore(country)
  }
  if (country %in% countrylist) {
    scores <- checkScores5L(dimen, dimen2, dimen3, dimen4, dimen5)
    if (sum(is.na(scores)) > 0) {
      return(NA)
    } else {
      if (sum(scores) < 0) {
        stop("EQ-5D-5L scores are not valid")
        # return(-2)
      } else {
        eq5d.valueset <- EQ5D5L_tariffs.df
        names(scores) <- c("MO", "SC", "UA", "PD", "AD")
        rows <- paste0(names(scores), scores)
        col <- checkColumnExist(country, eq5d.valueset)
        rownum1 <- which(row.names(eq5d.valueset) == rows[1])
        rownum2 <- which(row.names(eq5d.valueset) == rows[2])
        rownum3 <- which(row.names(eq5d.valueset) == rows[3])
        rownum4 <- which(row.names(eq5d.valueset) == rows[4])
        rownum5 <- which(row.names(eq5d.valueset) == rows[5])
        rownumfh <- which(row.names(eq5d.valueset) == "fullHealth")
        rownuminter <- which(row.names(eq5d.valueset) == "intercept")
        rownumn4 <- which(row.names(eq5d.valueset) == "N4")
        rownumn45 <- which(row.names(eq5d.valueset) == "Num45sq")
        inter.value <- NA
        if (any(scores > 1) && !is.na(eq5d.valueset[rownuminter, country])) {
          inter.value <- eq5d.valueset[rownuminter, country]
        }
        n4value <- NA
        if (any(scores >= 4) && !is.na(eq5d.valueset[rownumn4, country])) {
          n4value <- eq5d.valueset[rownumn4, country]
        }
        n45 <- which(scores %in% c(4, 5))
        n45value <- NA
        if (length(n45) >= 1 & !is.na(eq5d.valueset[rownumn45, country])) {
          n45value <- (length(n45) - 1)^2 * eq5d.valueset[rownumn45, country]
        }
        n45sall <- 0
        if (length(n45) >= 1) {
          for (i in 1:length(n45)) {
            names45row <- paste0(names(scores)[n45[i]], "45")
            rownumn45r <- which(row.names(eq5d.valueset) == names45row)
            if (!is.na(eq5d.valueset[rownumn45r, country])) {
              n45rvalue <- eq5d.valueset[rownumn45r, country]
              n45sall <- n45sall + n45rvalue
            } else {
              n45rvalue <- 0
              n45sall <- n45sall + n45rvalue
            }
          }
        }
        dim.response <- c(
          eq5d.valueset[rownum1, country], eq5d.valueset[rownum2, country], eq5d.valueset[rownum3, country],
          eq5d.valueset[rownum4, country], eq5d.valueset[rownum5, country]
        )
        sum.response <- sum(dim.response, na.rm = TRUE)
        values <- c(
          eq5d.valueset[rownumfh, country], inter.value, sum.response,
          n4value, n45value, n45sall
        )
        values.state <- sum(values, na.rm = TRUE)
      }
    }
    return(values.state)
  } else {
    stop("No tariffs found for the country you specified for EQ-5D-5L. Please try later")
    # return(-3)
  }
}

###########################################################################################################
#' Function to value EQ-5D-5L scores for any country and group by gender and age
#' @param eq5dresponse.data the data containing eq5d responses
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
#' data <- data.frame(age = c(10, 20), sex = c("M", "F"), 
#' mo = c(1, 2), sc = c(1, 2), ua = c(3, 4), pd = c(3, 4), ad = c(3, 4))
#' value5L(data, "mo", "sc", "ua", "pd", "ad", "England", NULL, c(10, 70))
#' @export
#' @description Function to value EQ-5D-5L descriptive system to index value.
value5L <- function(eq5dresponse.data, mo, sc, ua, pd, ad, country = "England", groupby = NULL, agelimit = NULL) {
  if (replaceSpaceUnderscore(country) == -1) {
    stop("Country name empty")
    # return("Country name empty")
  } else {
    country <- replaceSpaceUnderscore(country)
  }
  eq5d.colnames <- c(mo, sc, ua, pd, ad)
  ans.eq5d.colnames <- sapply(eq5d.colnames, checkColumnExist, eq5dresponse.data)
  if (all(ans.eq5d.colnames == 0)) { # if the eq5d column names match
    working.data <- subsetGenderAgeToGroup(eq5dresponse.data, groupby, agelimit)
    scores <- c()
    if (nrow(working.data) < 1 && working.data < 0) {
      stop("No entries with the given criteria - Please check the contents or the criteria")
      # return(-1)
    } else {
      for (j in 1:nrow(working.data)) {
        res1 <- working.data[j, mo]
        res2 <- working.data[j, sc]
        res3 <- working.data[j, ua]
        res4 <- working.data[j, pd]
        res5 <- working.data[j, ad]
        this.score <- value5LInd(country, c(res1, res2, res3, res4, res5))
        if (is.numeric(this.score)) {
          scores <- c(scores, this.score)
        } else {
          warning("EQ-5D-5L responses not valid - 5L scores can not be valued")
          return(-2)
        }
      }
      names(scores) <- "EQ-5D-5Lscores"
      new.data <- cbind(working.data, scores)
      colnames(new.data) <- c(colnames(working.data), "EQ-5D-5L scores")
      stats <- descriptiveStatDataColumn(scores, "EQ-5D-5L")
      freqtable <- getFrequencyTable(scores)
      first <- is.null(groupby) || toupper(groupby) == "NA" || is.na(groupby)
      second <- is.null(agelimit) || sum(toupper(agelimit) == "NA") != 0 || sum(is.na(agelimit)) != 0
      if (first & second) {
        title <- paste("Histogram of EQ-5D-5L index values", sep = "")
      } else {
        if (first & !second) {
          title <- paste("Histogram of EQ-5D-5L index values",
            " with ages between ", agelimit[1], " and ", agelimit[2],
            sep = ""
          )
        } else {
          if (!second & second) {
            title <- paste("Histogram of EQ-5D-5L index values for ",
              groupby,
              sep = ""
            )
          } else {
            title <- paste("Histogram of EQ-5D-5L index values for ",
              groupby, " with ages between ", agelimit[1], " and ", agelimit[2],
              sep = ""
            )
          }
        }
      }
      hist.plot <- graphics::hist(scores, main = title)
      results <- list("stats" = stats, "frequencyTable" = freqtable, "histogram" = hist.plot, "modifiedData" = new.data)
      return(results)
    }
  } else { # if the eq 5d column names do not match
    stop("EQ-5D column names do not match")
    # return(-3)
  }
}
##########################################################################################################
#' Function to value EQ-5D-3L scores for various countries
#' @param country a country name from the list Belgium,Brazil,Canada,Chile,Denmark,Europe,Finland,France,Germany,Italy,Japan,Korea,Netherlands,NewZealand,Poland,Portugal,Slovenia,Spain,Taiwan,Thailand,UK,USA,and Zimbabwe
#' @param method method name either TTO or VAS
#' @param dimen  a must input,response for EQ-5D-5L mobility  or the 5 digit response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param dimen2 response for EQ-5D-3L self care, or NA if the responses are given as dimen
#' @param dimen3  response for EQ-5D-3L usual activities,or NA if the responses are given as dimen
#' @param dimen4  response for EQ-5D-3L pain/discomfort, or NA if the responses are given as dimen
#' @param dimen5  response for EQ-5D-3L anxiety/depression, or NA if the responses are given as dimen
#' @return index value based if success, negative values for failure
#' @examples
#' value3LInd("UK", "TTO", 23131)
#' value3LInd("Spain", "TTO", 2, 3, 1, 3, 1)
#' value3LInd("Denmark", "VAS", c(1, 2, 3, 1, 3))
#' @export
value3LInd <- function(country, method, dimen, dimen2 = NA, dimen3 = NA, dimen4 = NA, dimen5 = NA) {
  countrylist <- c(
    "Argentina", "Australia", "Belgium", "Brazil", "Canada", "Chile", "China", "Denmark", "Europe",
    "Finland", "France", "Germany", "Iran", "Italy", "Japan", "Korea", "Malaysia", "Netherlands",
    "New_Zealand", "Poland", "Portugal", "Singapore", "Slovenia", "Spain", "Sri_Lanka", "Sweden",
    "Taiwan", "Thailand", "Trinidad_and_Tobago", "UK", "USA", "Zimbabwe"
  )

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

  australia.impalusibleordering.scores <- c(33132, 12133, 13133, 22133, 23133, 32133, 33133, 12233, 13233, 22233, 23233, 32233, 33233, 33232, 33323, 13332, 13333, 23332, 23333, 32333, 33332, 33333)

  if (replaceSpaceUnderscore(country) == -1) {
    stop("Country name empty")
    # return(-1)
  } else {
    country <- replaceSpaceUnderscore(country)
  }
  if (country %in% countrylist) {
    scores <- checkScores3L(dimen, dimen2, dimen3, dimen4, dimen5)
    if (sum(is.na(scores)) > 0) {
      return(NA)
    } else {
      if (sum(scores) < 0) {
        stop("EQ-5D-3L scores are not valid")
        # return(-3)
      } else {
        if (method == "TTO" && country %in% TTO_countrylist) {
          eq5d.valueset <- EQ5D3L_tariffs_TTO.df
        } else {
          if (method == "VAS" && country %in% VAS_countrylist) {
            eq5d.valueset <- EQ5D3L_tariffs_VAS.df
          } else {
            stop("No tariff found")
            # return(-4)
          }
        }
        score.num <- as.numeric(paste(scores, collapse = ""))
        if (country == "Australia" & sum(score.num %in% australia.impalusibleordering.scores) > 0) {
          values.state <- .correctImplausibleOrdering(scores)
        } else {
          names(scores) <- c("MO", "SC", "UA", "PD", "AD")
          rows <- paste0(names(scores), scores)
          col <- checkColumnExist(country, eq5d.valueset)
          if (col == 0) {
            min2or3 <- which(scores %in% c(2, 3))
            if (length(min2or3) == 5) {
              all.equals2or3 <- 1
            } else {
              all.equals2or3 <- c()
            }
            which3 <- which(scores %in% c(3))
            which2 <- which(scores %in% c(2))
            rownums <- c()
            dim.response <- NA
            min3.value <- NA
            all.equals2or3.value <- NA
            min2or3.value <- NA
            c3sq.value <- NA
            d1.value <- NA
            I2.value <- NA
            I2_sq.value <- NA
            I3.value <- NA
            I3_sq.value <- NA
            Only1sand2s.value <- NA
            Only1sand3s.value <- NA
            Atleast2andatleast3.value <- NA
            Nos2withatleast3.value <- NA
            Nos2Sq.value <- NA
            Nos3Sq.value <- NA
            MO3SC3.value <- NA
            MO3UA3.value <- NA
            MO3PD3.value <- NA
            MO3AD3.value <- NA
            SC3UA3.value <- NA
            SC3PD3.value <- NA
            SC3AD3.value <- NA
            UA3PD3.value <- NA
            UA3AD3.value <- NA
            PD3AD3.value <- NA
            MO2UA2.value <- NA
            SC3UA2.value <- NA
            rownumfh <- which(row.names(eq5d.valueset) == "FullHealth")
            rownum_min2or3 <- which(row.names(eq5d.valueset) == "Constant")
            rownumn_min3 <- which(row.names(eq5d.valueset) == "N3")
            rownum_Only1sand2s <- which(row.names(eq5d.valueset) == "Only1sand2s")
            rownum_Only1sand3s <- which(row.names(eq5d.valueset) == "Only1sand3s")
            rownum_Atleast2andatleast3 <- which(row.names(eq5d.valueset) == "Atleast2andatleast3")
            rownum_Nos2withatleast3 <- which(row.names(eq5d.valueset) == "Nos2withatleast3")
            rownum_Nos2Sq <- which(row.names(eq5d.valueset) == "Nos2Sq")
            rownum_Nos3Sq <- which(row.names(eq5d.valueset) == "Nos3Sq")
            if (method == "TTO") {
              rownum_all.equals2or3 <- which(row.names(eq5d.valueset) == "X5")
              rownum_C3sq <- which(row.names(eq5d.valueset) == "C3sq")
              rownumn_D1 <- which(row.names(eq5d.valueset) == "D1")
              rownumn_I2 <- which(row.names(eq5d.valueset) == "I2")
              rownumn_I2_sq <- which(row.names(eq5d.valueset) == "I2_sq")
              rownumn_I3 <- which(row.names(eq5d.valueset) == "I3")
              rownumn_I3_sq <- which(row.names(eq5d.valueset) == "I3_sq")
              rownum_MO3SC3 <- which(row.names(eq5d.valueset) == "MO3SC3")
              rownum_MO3UA3 <- which(row.names(eq5d.valueset) == "MO3UA3")
              rownum_MO3PD3 <- which(row.names(eq5d.valueset) == "MO3PD3")
              rownum_MO3AD3 <- which(row.names(eq5d.valueset) == "MO3AD3")
              rownum_SC3UA3 <- which(row.names(eq5d.valueset) == "SC3UA3")
              rownum_SC3PD3 <- which(row.names(eq5d.valueset) == "SC3PD3")
              rownum_SC3AD3 <- which(row.names(eq5d.valueset) == "SC3AD3")
              rownum_UA3PD3 <- which(row.names(eq5d.valueset) == "UA3PD3")
              rownum_UA3AD3 <- which(row.names(eq5d.valueset) == "UA3AD3")
              rownum_PD3AD3 <- which(row.names(eq5d.valueset) == "PD3AD3")
              rownum_MO2UA2 <- which(row.names(eq5d.valueset) == "MO2UA2")
              rownum_SC3UA2 <- which(row.names(eq5d.valueset) == "SC3UA2")
            } else {
              rownum_all.equals2or3 <- NA
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
              for (i in 1:length(min2or3)) {
                rownams <- row.names(eq5d.valueset)
                ro <- which(rownams == rows[min2or3[i]])
                rownums <- cbind(rownums, ro)
              }
              dim.response <- eq5d.valueset[rownums, country]
            }
            if (any(scores >= 3) && !is.na(eq5d.valueset[rownumn_min3, country])) {
              min3.value <- eq5d.valueset[rownumn_min3, country]
            }
            if (length(which3) >= 1 & sum(is.na(rownum_C3sq) == 0)) {
              if (!is.na(eq5d.valueset[rownum_C3sq, country])) {
                c3sq.value <- (length(which3))^2 * eq5d.valueset[rownum_C3sq, country]
              }
            }
            if (length(all.equals2or3) >= 1 & sum(is.na(rownum_all.equals2or3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_all.equals2or3, country])) {
                all.equals2or3.value <- eq5d.valueset[rownum_all.equals2or3, country]
              }
            }
            if (sum(scores) > 5 & length(min2or3) >= 1 & sum(is.na(rownum_min2or3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_min2or3, country])) {
                min2or3.value <- eq5d.valueset[rownum_min2or3, country]
              }
            }
            if (sum(scores) > 5 & length(min2or3) >= 1 & sum(is.na(rownumn_D1) == 0)) {
              if (!is.na(eq5d.valueset[rownumn_D1, country])) {
                d1.value <- (length(min2or3) - 1) * eq5d.valueset[rownumn_D1, country]
              }
            }
            if (sum(scores) > 5 & length(which2) >= 1 & sum(is.na(rownumn_I2) == 0)) {
              if (!is.na(eq5d.valueset[rownumn_I2, country])) {
                I2.value <- (length(which2) - 1) * eq5d.valueset[rownumn_I2, country]
              }
            }
            if (sum(scores) > 5 & length(which2) >= 1 & sum(is.na(rownumn_I2_sq) == 0)) {
              if (!is.na(eq5d.valueset[rownumn_I2_sq, country])) {
                I2_sq.value <- (length(which2) - 1)^2 * eq5d.valueset[rownumn_I2_sq, country]
              }
            }
            if (sum(scores) > 5 & length(which3) >= 1 & sum(is.na(rownumn_I3) == 0)) {
              if (!is.na(eq5d.valueset[rownumn_I3, country])) {
                I3.value <- (length(which3) - 1) * eq5d.valueset[rownumn_I3, country]
              }
            }
            if (sum(scores) > 5 & length(which3) >= 1 & sum(is.na(rownumn_I3_sq) == 0)) {
              if (!is.na(eq5d.valueset[rownumn_I3_sq, country])) {
                I3_sq.value <- (length(which3) - 1)^2 * eq5d.valueset[rownumn_I3_sq, country]
              }
            }
            if (all(scores <= 2) & !all(scores == 1) & sum(is.na(rownum_Only1sand2s) == 0)) {
              if (!is.na(eq5d.valueset[rownum_Only1sand2s, country])) {
                Only1sand2s.value <- eq5d.valueset[rownum_Only1sand2s, country]
              }
            }
            ## !all(scores==3) & need ??

            if (!any(scores == 2) & !all(scores == 1) & sum(is.na(rownum_Only1sand3s) == 0)) {
              if (!is.na(eq5d.valueset[rownum_Only1sand3s, country])) {
                Only1sand3s.value <- eq5d.valueset[rownum_Only1sand3s, country]
              }
            }
            if (any(scores == 2) & any(scores == 3) & sum(is.na(rownum_Atleast2andatleast3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_Atleast2andatleast3, country])) {
                Atleast2andatleast3.value <- eq5d.valueset[rownum_Atleast2andatleast3, country]
              }
            }
            if (any(scores == 2) & any(scores == 3) & sum(is.na(rownum_Nos2withatleast3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_Nos2withatleast3, country])) {
                Nos2withatleast3.value <- length(which(scores == 2)) * eq5d.valueset[rownum_Nos2withatleast3, country]
              }
            }
            if (any(scores == 2) & sum(is.na(rownum_Nos2Sq) == 0)) {
              if (!is.na(eq5d.valueset[rownum_Nos2Sq, country])) {
                Nos2Sq.value <- (length(which(scores == 2)))^2 * eq5d.valueset[rownum_Nos2Sq, country]
              }
            }
            if (any(scores == 3) & sum(is.na(rownum_Nos3Sq) == 0)) {
              if (!is.na(eq5d.valueset[rownum_Nos3Sq, country])) {
                Nos3Sq.value <- (length(which(scores == 3)))^2 * eq5d.valueset[rownum_Nos3Sq, country]
              }
            }
            if (scores[["MO"]] == 3 & scores[["SC"]] == 3 & sum(is.na(rownum_MO3SC3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_MO3SC3, country])) {
                MO3SC3.value <- eq5d.valueset[rownum_MO3SC3, country]
              }
            }
            if (scores[["MO"]] == 3 & scores[["UA"]] == 3 & sum(is.na(rownum_MO3UA3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_MO3UA3, country])) {
                MO3UA3.value <- eq5d.valueset[rownum_MO3UA3, country]
              }
            }
            if (scores[["MO"]] == 3 & scores[["PD"]] == 3 & sum(is.na(rownum_MO3PD3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_MO3PD3, country])) {
                MO3PD3.value <- eq5d.valueset[rownum_MO3PD3, country]
              }
            }
            if (scores[["MO"]] == 3 & scores[["AD"]] == 3 & sum(is.na(rownum_MO3AD3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_MO3AD3, country])) {
                MO3AD3.value <- eq5d.valueset[rownum_MO3AD3, country]
              }
            }
            if (scores[["SC"]] == 3 & scores[["UA"]] == 3 & sum(is.na(rownum_SC3UA3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_SC3UA3, country])) {
                SC3UA3.value <- eq5d.valueset[rownum_SC3UA3, country]
              }
            }
            if (scores[["SC"]] == 3 & scores[["PD"]] == 3 & sum(is.na(rownum_SC3PD3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_SC3PD3, country])) {
                SC3PD3.value <- eq5d.valueset[rownum_SC3PD3, country]
              }
            }
            if (scores[["SC"]] == 3 & scores[["AD"]] == 3 & sum(is.na(rownum_SC3AD3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_SC3AD3, country])) {
                SC3AD3.value <- eq5d.valueset[rownum_SC3AD3, country]
              }
            }
            if (scores[["UA"]] == 3 & scores[["PD"]] == 3 & sum(is.na(rownum_UA3PD3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_UA3PD3, country])) {
                UA3PD3.value <- eq5d.valueset[rownum_UA3PD3, country]
              }
            }
            if (scores[["UA"]] == 3 & scores[["AD"]] == 3 & sum(is.na(rownum_UA3AD3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_UA3AD3, country])) {
                UA3AD3.value <- eq5d.valueset[rownum_UA3AD3, country]
              }
            }
            if (scores[["PD"]] == 3 & scores[["AD"]] == 3 & sum(is.na(rownum_PD3AD3) == 0)) {
              if (!is.na(eq5d.valueset[rownum_PD3AD3, country])) {
                PD3AD3.value <- eq5d.valueset[rownum_PD3AD3, country]
              }
            }
            if (scores[["MO"]] == 2 & scores[["UA"]] == 2 & sum(is.na(rownum_MO2UA2) == 0)) {
              if (!is.na(eq5d.valueset[rownum_MO2UA2, country])) {
                MO2UA2.value <- eq5d.valueset[rownum_MO2UA2, country]
              }
            }
            if (scores[["SC"]] == 3 & scores[["UA"]] == 2 & sum(is.na(rownum_SC3UA2) == 0)) {
              if (!is.na(eq5d.valueset[rownum_SC3UA2, country])) {
                SC3UA2.value <- eq5d.valueset[rownum_SC3UA2, country]
              }
            }
            if (country == "Germany" && method == "VAS") {
              prod.response <- prod(dim.response, na.rm = TRUE)
              values <- c(
                eq5d.valueset[rownumfh, country], prod.response, min2or3.value, min3.value, all.equals2or3.value, c3sq.value, d1.value, I2.value,
                I2_sq.value, I3.value, I3_sq.value, Only1sand2s.value, Only1sand3s.value, Atleast2andatleast3.value, Nos2withatleast3.value,
                Nos2Sq.value, Nos3Sq.value
              )
              values.state <- prod(values, na.rm = TRUE)
            } else {
              sum.response <- sum(dim.response, na.rm = TRUE)
              values <- c(
                eq5d.valueset[rownumfh, country], sum.response, min2or3.value, min3.value, all.equals2or3.value, c3sq.value, d1.value, I2.value,
                I2_sq.value, I3.value, I3_sq.value, Only1sand2s.value, Only1sand3s.value, Atleast2andatleast3.value, Nos2withatleast3.value,
                Nos2Sq.value, Nos3Sq.value, MO3SC3.value, MO3UA3.value, MO3PD3.value, MO3AD3.value, SC3UA3.value, SC3PD3.value, SC3AD3.value,
                UA3PD3.value, UA3AD3.value, PD3AD3.value, MO2UA2.value, SC3UA2.value
              )
              values.state <- sum(values, na.rm = TRUE)
            }
          } else {
            stop("No country tariffs")
            # return(-5)
          }
        }
      }
    }
    return(values.state)
  } else {
    stop("No country tariffs found for the country you specified for EQ-5D-3L. Please try later")
    # return(-2)
  }
}
###########################################################################################################
#' Function to value EQ-5D-3L columns to index values for any country and group by gender and age
#' @param eq5dresponse.data the data containing eq5d responses
#' @param mo  column name for EQ-5D-3L mobility
#' @param sc column name for response for EQ-5D-3L self care
#' @param ua  column name for response for EQ-5D-3L usual activities
#' @param pd  column name for response for EQ-5D-3L pain/discomfort
#' @param ad  column name for response for EQ-5D-3L anxiety/depression
#' @param country  country of interest, by default is UK, if groupby has to specify the country should be specified
#' @param method Either "TTO" or "VAS"
#' @param groupby  male or female -grouping by gender, default NULL
#' @param agelimit  vector of ages to show upper and lower limits
#' @return the descriptive statistics of index values, frequency table and the modified data where the last column will be the index values
#' data<-data.frame(age=c(10,20),sex=c("M","F"),mo=c(1,2),sc=c(1,2),ua=c(3,4),pd=c(3,1),ad=c(3,1))
#' value3L(data, "mo", "sc","ua", "pd", "ad","UK","TTO",NULL,c(10,70))
#' @export
#' @description Main function to value EQ-5D-5L descriptive system to 5L index values.
value3L <- function(eq5dresponse.data, mo, sc, ua, pd, ad, country, method, groupby, agelimit) {
  if (replaceSpaceUnderscore(country) == -1) {
    stop("Country name empty")
    # return(-1)
  } else {
    country <- replaceSpaceUnderscore(country)
  }
  eq5d.colnames <- c(mo, sc, ua, pd, ad)
  ans.eq5d.colnames <- sapply(eq5d.colnames, checkColumnExist, eq5dresponse.data)
  if (all(ans.eq5d.colnames == 0)) { # if the eq5d column names match
    working.data <- subsetGenderAgeToGroup(eq5dresponse.data, groupby, agelimit)
    if (nrow(working.data) < 1 && working.data < 0) {
      stop("No entries with the given criteria - Please check the contents or the criteria")
      # return(-1)
    } else {
      scores <- c()
      for (j in 1:nrow(working.data)) {
        res1 <- working.data[j, mo]
        res2 <- working.data[j, sc]
        res3 <- working.data[j, ua]
        res4 <- working.data[j, pd]
        res5 <- working.data[j, ad]
        this.score <- value3LInd(country, method, res1, res2, res3, res4, res5)
        if (is.numeric(this.score)) {
          scores <- c(scores, this.score)
        } else {
          stop("Responses not valid -3L scores can not be valued")
          # return(-2)
        }
      }
      names(scores) <- "EQ-5D-3Lscores"
      new.data <- cbind(working.data, scores)
      colnames(new.data) <- c(colnames(working.data), "EQ-5D-3L scores")
      stats <- descriptiveStatDataColumn(scores, "EQ-5D-3L")
      freqtable <- getFrequencyTable(scores)
      first <- is.null(groupby) || toupper(groupby) == "NA" || is.na(groupby)
      second <- is.null(agelimit) || sum(toupper(agelimit) == "NA") != 0 || sum(is.na(agelimit)) != 0
      if (first & second) {
        title <- paste("Histogram of EQ-5D-3L index values", sep = "")
      } else {
        if (first & !second) {
          title <- paste("Histogram of EQ-5D-3L index values",
            " with ages between ", agelimit[1], " and ", agelimit[2],
            sep = ""
          )
        } else {
          if (!second & second) {
            title <- paste("Histogram of EQ-5D-3L index values for ",
              groupby,
              sep = ""
            )
          } else {
            title <- paste("Histogram of EQ-5D-3L index values for ",
              groupby, " with ages between ", agelimit[1], " and ", agelimit[2],
              sep = ""
            )
          }
        }
      }
      hist.plot <- graphics::hist(scores, main = title)
      results <- list("stats" = stats, "frequencyTable" = freqtable, "histogram" = hist.plot, "modifiedData" = new.data)
      return(results)
    }
  } else { # if the eq 5d column names do not match
    stop("EQ-5D column names do not match")
    # return(-3)
  }
}
###########################################################################################################
#' Function to map EQ-5D-5L descriptive system to 3L index value
#' @param country  default is "UK"
#' @param method CW cross walk
#' @param dimen  response for EQ-5D-5L mobility  or the 5 digit response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param dimen2 response for EQ-5D-5L self care, or NA if the responses are given as dimen
#' @param dimen3  response for EQ-5D-5L usual activities,or NA if the responses are given as dimen
#' @param dimen4  response for EQ-5D-5L pain/discomfort, or NA if the responses are given as dimen
#' @param dimen5  response for EQ-5D-5L anxiety/depression, or NA if the responses are given as dimen
#' @return index value of EQ-5D-3L, -1 if any error
#' @examples
#' map5Lto3LInd("UK", "CW", 11125)
#' map5Lto3LInd("UK", "CW", c(1, 1, 1, 2, 5))
#' map5Lto3LInd("UK", "CW", 1, 1, 1, 2, 5)
#' @export
#' @description Function to map EQ-5D-5L descriptive system to 3L index value (ref:Van Hout et al 2012 and code inspired from https://github.com/brechtdv/eq5d-mapping)
map5Lto3LInd <- function(country = "UK", method = "CW", dimen, dimen2 = NA, dimen3 = NA, dimen4 = NA, dimen5 = NA) {
  countrylist <- c("Denmark", "France", "Germany", "Japan", "Netherlands", "Spain", "Thailand", "UK", "USA", "Zimbabwe")
  if (replaceSpaceUnderscore(country) == -1) {
    stop("Country name empty")
    # return(-1)
  } else {
    country <- replaceSpaceUnderscore(country)
  }
  if (country %in% countrylist) {
    responses <- c(dimen, dimen2, dimen3, dimen4, dimen5)
    if (sum(is.na(dimen)) > 0) { # first value should be not be a NA, do not contain NA
      this.score.5L <- NA
      values.state <- NA
      return(values.state)
    } else { # check first value should be a vector containiing responses or a 5digit number
      if (length(dimen) != 5 && length(dimen) != 1) {
        stop("Invalid EQ-5D-5L responses-check the responses to each question")
        # return(-3)
      } else { # first value a vector or a 5 figit number
        if (length(dimen) == 5) { # first value a vector
          this.score.5L <- paste(dimen, collapse = "")
        } else {
          if (length(dimen) == 1) { # first value 5 digit number or actual response for mobility
            if (dimen >= 11111 && dimen <= 55555) { # valid 5 digit number
              this.score.5L <- dimen
            } else { # first value might be valid-  a response to mobility
              if (dimen <= 5 && dimen > 0) { # valid response to mobility
                four.res <- c(dimen2, dimen3, dimen4, dimen5)
                if (sum(is.na(four.res)) == 0) {
                  if (any(responses <= 5)) {
                    this.score.5L <- paste(responses, collapse = "") # all valid and generate the score
                  } else { # error values
                    stop("Invalid EQ-5D-5L responses-check the responses to each question")
                    # return(-5)
                  }
                } else {
                  # missing values
                  this.score.5L <- NA
                  values.state <- NA
                  return(values.state)
                }
              } else {
                stop("Invalid EQ-5D-5L responses-check the responses to each question")
                # return(-4)
              }
            }
          }
        }
      }
    }
    if (this.score.5L < 11111 || this.score.5L > 55555) {
      stop("Invalid EQ-5D-5L responses -less than 11111 or more than 55555")
      # return(-6)
    } else {
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
          value3LInd(
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
      ## file <- system.file('extdata', "Probability_matrix_crosswalk.csv",package = 'EQ5Dmapping')
      ## prob.matrix=read.csv(file,header=FALSE)
      if (toupper(method) == "CW") {
        prob.matrix <- Probability_matrix_crosswalk.df
        m <- prob.matrix
        rows_m <- nrow(m)
        cols_m <- ncol(m)
        if (rows_m != 3125 || cols_m != 243) {
          stop("Error in number of cols or rows of probability matrix")
          # return(-7)
        }
        ## multiply each row of 't(m)' with 'index_3L'
        m_prod <- t(t(m) * index_3L)
        ## obtain sum per row
        ## crosswalked index value for each 5L score
        m_sums <- rowSums(m_prod)
        ## reorder columns and convert to matrix
        scores_5L <- with(scores_5L, cbind(MO, SC, UA, PD, AD))
        ## create 5L score labels
        scores_5L_chr <- apply(scores_5L, 1, paste, collapse = "")
        this_score <- which(scores_5L_chr == paste(this.score.5L, collapse = ""))
        if (country == "Zimbabwe" & this.score.5L == "11111") {
          return(0.9)
        } else {
          return(m_sums[this_score])
        }
      } else {
        stop("The specified method is not implemented")
        # return(-8)
      }
    }
  } else {
    stop("Crosswalk for the country specified is not implemented")
    # return(-2)
  }
}
###########################################################################################################
#' Function to map EQ-5D-5L scores to EQ-5D-3L index values as per the specific country and group by gender and age
#' @param eq5dresponse.data the data containing eq5d5L responses
#' @param mobility  column name for EQ-5D-5L mobility
#' @param self.care column name for response for EQ-5D-5L self care
#' @param usual.activities  column name for response for EQ-5D-5L usual activities
#' @param pain.discomfort  column name for response for EQ-5D-5L pain/discomfort
#' @param anxiety  column name for response for EQ-5D-5L anxiety/depression
#' @param country  country of interest, by default is UK, if groupby has to specify the country should be specified
#' @param method CW cross walk
#' @param groupby  male or female -grouping by gender, default NULL
#' @param agelimit  vector of ages to show upper and lower limits
#' @return index value  if success, negative values for failure
#' @examples
#' map5Lto3L(data.frame(mo = c(1), sc = c(4), ua = c(4), pd = c(3),
#'  ad = c(3)), "mo", "sc", "ua", "pd", "ad")
#' @export
#' @description Function to map EQ-5D-5L scores to EQ-5D-3L index values
map5Lto3L <- function(eq5dresponse.data, mobility, self.care, usual.activities, pain.discomfort, anxiety,
                      country = "UK", method = "CW", groupby = NULL, agelimit = NULL) {
  if (replaceSpaceUnderscore(country) == -1) {
    stop("Country name empty")
    # return(-1)
  } else {
    country <- replaceSpaceUnderscore(country)
  }
  eq5d.colnames <- c(mobility, self.care, usual.activities, pain.discomfort, anxiety)
  ans.eq5d.colnames <- sapply(eq5d.colnames, checkColumnExist, eq5dresponse.data)
  if (all(ans.eq5d.colnames == 0)) { # if the eq5d column names match
    working.data <- subsetGenderAgeToGroup(eq5dresponse.data, groupby, agelimit)
    scores <- c()
    if (nrow(working.data) < 1) {
      stop("No entries with the given criteria - Please check the contents or the criteria")
      # return(-2)
    } else {
      for (j in 1:nrow(working.data)) {
        res1 <- working.data[j, mobility]
        res2 <- working.data[j, self.care]
        res3 <- working.data[j, usual.activities]
        res4 <- working.data[j, pain.discomfort]
        res5 <- working.data[j, anxiety]
        this.score <- map5Lto3LInd(country, method, c(res1, res2, res3, res4, res5))
        if (is.numeric(this.score)) {
          scores <- c(scores, this.score)
        } else {
          stop("EQ-5D-5L responses not valid - 5L scores can not be valued")
          # return(-3)
        }
      }
      # names(scores)<-"Mapped EQ-5D-3Lscores"
      new.data <- cbind(working.data, scores)
      colnames(new.data) <- c(colnames(working.data), "Mapped EQ-5D-3L scores")
      stats <- descriptiveStatDataColumn(scores, "EQ-5D-3L")
      freqtable <- getFrequencyTable(scores)
      first <- is.null(groupby) || toupper(groupby) == "NA" || is.na(groupby)
      second <- is.null(agelimit) || sum(toupper(agelimit) == "NA") != 0 || sum(is.na(agelimit)) != 0
      if (first & second) {
        title <- paste("Histogram of EQ-5D-3L index values", sep = "")
      } else {
        if (first & !second) {
          title <- paste("Histogram of EQ-5D-3L index values",
            " with ages between ", agelimit[1], " and ", agelimit[2],
            sep = ""
          )
        } else {
          if (!second & second) {
            title <- paste("Histogram of EQ-5D-3L index values for ",
              groupby,
              sep = ""
            )
          } else {
            title <- paste("Histogram of EQ-5D-3L index values for ",
              groupby, " with ages between ", agelimit[1], " and ", agelimit[2],
              sep = ""
            )
          }
        }
      }
      hist.plot <- graphics::hist(scores, main = title)
      results <- list("stats" = stats, "frequencyTable" = freqtable, "histogram" = hist.plot, "modifiedData" = new.data)
      return(results)
    }
  } else { # if the eq 5d column names do not match
    stop("EQ-5D column names do not match")
    # return(-1)
  }
}
###########################################################################################################
#' Function to correct the implausible ordering in Australian valueset for EQ-5D-3L
#' @param scores , EQ-5D-3L scores as a number
#' @return the value that read from the stored dataframe
#' @examples
#' .correctImplausibleOrdering(11121)
#' @export
#' @description Correcting the implausible ordering
.correctImplausibleOrdering <- function(scores) {
  score.num <- as.numeric(paste(scores, collapse = ""))
  australia.impalusibleordering.scores <- c(
    33132, 12133, 13133, 22133, 23133, 32133, 33133, 12233, 13233, 22233, 23233, 32233, 33233,
    33232, 33323, 13332, 13333, 23332, 23333, 32333, 33332, 33333
  )
  australia.impalusibleordering.values <- c(
    -0.045, 0.154, 0.154, 0.086, 0.086, -0.083, -0.083, 0.101, 0.101, 0.033, 0.033, -0.136, -0.136,
    -0.098, -0.199, 0.020, 0.020, -0.048, -0.048, -0.206, -0.217, -0.217
  )
  if (sum(score.num %in% australia.impalusibleordering.scores) > 0) {
    index <- which(score.num == australia.impalusibleordering.scores)
    value <- australia.impalusibleordering.values[index]
    return(value)
  } else {
    return(-1)
  }
}
###########################################################################################################
