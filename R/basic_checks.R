#############################################################################
#' Function to throw error on invalid directory or file or if the file is 
#' not readable
#' @param filename  name of a file or directory
#' @return 0 if success, non zero negative values if failure
#' @examples
#' test_file_exist_read(system.file("extdata", "blank.txt", 
#' package = "valueEQ5D"))
#' @export
test_file_exist_read <- function(filename) {
  ## Checking if the file exists
  if (file.exists(filename)) {
    ## Checking if the file is accessible to read
    if (file.access(filename, 0) != 0) {
      stop(" Error reading file ")
    }
    return(0)
  } else {
    stop(" Invalid directory or file ")
  }
}
###############################################################################
#' Function to check the given column exists
#' @param column_name a column name
#' @param data data frame
#' @return 0 if success -1 if failure
#' @examples
#' check_column_exist("age", data.frame(
#'   age = rep(20, 4), sex = rep("male", 4),
#'   stringsAsFactors = FALSE
#' ))
#' @export
check_column_exist <- function(column_name, data) {
  one <- toupper(colnames(data))
  two <- toupper(column_name)
  if (any(one == two)) {
    return(0)
  } else {
    return(-1)
  }
}
###############################################################################
#' Function to return the column number for column name
#' @param data a data frame
#' @param column_name column names of the data frame
#' @return column number, if success -1, if failure
#' @examples
#' get_column_no_colnames(data.frame(age = rep(20, 4), 
#' sex = rep("male", 4)), "sex")
#' @export
get_column_no_colnames <- function(data, column_name) {
  data_column_names <- toupper(colnames(data))
  if (any(data_column_names == toupper(column_name))) {
    column_no <- which(data_column_names == toupper(column_name))
    return(column_no)
  } else {
    stop("Column name does not exist")
  }
}
################################################################################
#' Function to return frequency table
#' @param v a vector
#' @return frequency table
#' @examples
#' get_frequency_table(c(1, 1, 1, 12, 2))
#' @export
get_frequency_table <- function(v) {
  if (!is.null(v)) {
    res <- cbind(Freq = table(v), Cumul = cumsum(table(v)), relative = 
                   prop.table(table(v)))
    scores <- rownames(res)
    res <- cbind(scores, res)
    return(res)
  } else {
    stop("Null vector")
  }
}
################################################################################
#' Function to return mode
#' @param v a vector
#' @return mode if success -1 for failure
#' @examples
#' get_mode_for_vec(c(1, 1, 2, 3))
#' @export
get_mode_for_vec <- function(v) {
  if (is.numeric(v)) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  } else {
    stop("Non numeric data")
  }
}
###############################################################################
#' Function to check format of a numeric column when the values are not bounded
#' @param vec a column vector
#' @param nrcode non response code corresponding to the column
#' @return 0, if success -1, if failure
#' @examples
#' test_data_num_norange(c(1, 2, 3, 4, -99), -99)
#' @export
test_data_num_norange <- function(vec, nrcode = NA) {
  entry <- vec
  if (is.na(nrcode)) {
    no_nrcode_entries <- entry[!is.na(entry)]
  } else {
    no_nrcode_entries <- entry[entry != nrcode & !is.na(entry)]
  }
  if (is.numeric(no_nrcode_entries)) {
    return(0)
  } else {
    stop("Some values-other than NR code is not numeric")
  }
}
################################################################################
#' Function to return descriptive statistics, sum, no of observations, 
#' mean, mode. median, range, standard deviation and standard error
#' @param colum column
#' @param column_name the column name
#' @param nrcode non response code corresponding to the column
#' @return the descriptive statistics for success , -1 for failure
#' @examples
#' descriptive_stat_data_column(c(1, 2, 3, 4, NA), "scores", NA)
#' @import stats
#' @export
descriptive_stat_data_column <- function(colum, column_name, nrcode = NA) {
  vec <- colum
  if (test_data_num_norange(vec, nrcode) == 0) {
   this_column <- colum
    if (is.na(nrcode)) {
      this_column <- this_column[!is.na(colum)]
    } else {
      this_column <- this_column[colum != nrcode & !is.na(colum)]
    }
    this_sum <- sum(this_column)
    this_av <- mean(this_column)
    this_med <- median(this_column)
    this_mode <- get_mode_for_vec(this_column)
    this_range_low <- min(this_column)
    this_range_high <- max(this_column)
    this_sd <- sd(this_column)
    this_se <- this_sd / sqrt(length(this_column))
    results <- matrix(c(this_sum, this_av, this_sd, this_med, this_mode, 
                        this_se, this_range_low, this_range_high, 
                        length(this_column)), byrow = TRUE, nrow = 1)
    colnames(results) <- c("Sum", "Mean", "SD", "Median", "Mode", 
                           "SE", "Minimum", "Maximum", "Count")
    rownames(results) <- column_name
    return(results)
  }
}
################################################################################
#' Function to convert a number to individual digits
#' @param this_number a number
#' @return digits
#' @examples
#' convert_number_to_digits(234)
#' @export
convert_number_to_digits <- function(this_number) {
  string_number <- toString(this_number)
  result <- suppressWarnings(as.numeric(strsplit(string_number, "")[[1]]))
  if (any(is.na(result))) {
    stop("The responses are not valid")
  } else {
    return(result)
  }
}
################################################################################
#' Function to return the column number for a given column name 
#' (from list of possible column names that may
#' have used) in a data frame
#' @param column_names column names in a data frame
#' @param data a data frame
#' @return the column number
#' @examples
#' get_colno_existing_colnames(c("age"), data.frame(age = rep(20, 4), 
#' gender = rep("male", 4)))
#' @export
get_colno_existing_colnames <- function(column_names, data) {
  ans_columns <- unlist(lapply(column_names, check_column_exist, data))
  if (sum(ans_columns == 0) > 0) {
    this_col <- which(ans_columns == 0)
    colnum <- get_column_no_colnames(data, column_names[this_col])
    return(colnum)
  } else {
    stop("No column exists with specified column names")
  }
}
################################################################################
#' Function to check the gender column and age column subset based on 
#' the values in it
#' have used) in a data frame
#' @param data a data frame
#' @param gender groupby gender either male or female expected
#' @param agelimit list of ages e.g. c(10,20)
#' @return the column number
#' @examples
#' subset_gender_age_to_group(data.frame(age = rep(20, 4), gender = 
#' rep("male", 4)), "male", c(10, 70))
#' @export
subset_gender_age_to_group <- function(data, gender, agelimit) {
  if (is.null(gender) || toupper(gender) == "NA" || is.na(gender)) { 
    working_data <- data   # if no groupby option given
  } else {# groupby option is given
    # groupby is male or female
    if (toupper(gender) == "MALE" || toupper(gender) == "FEMALE") { 
     gendercolumn <- c("sex", "gender", "male", "female", "f", "m")
      colnum <- get_colno_existing_colnames(gendercolumn, data)
      data_gender <- unlist(data[colnum])
      if (toupper(gender) == "MALE") {# groupby is male
        malech <- c("M", "m", "male", "MALE", "Male")
        charinccol <- malech[malech %in% data_gender]
        working_data <- data[is.element(data_gender, charinccol), ]
      } else {# groupby is female
        femalech <- c("F", "f", "female", "FEMALE", "Female")
        charinccol <- femalech[femalech %in% data_gender]
        working_data <- data[is.element(data_gender, charinccol), ]
      }
    } else {
      stop("Group by should be euther male or female")
    }
  }
  if (is.null(agelimit) || sum(toupper(agelimit) == "NA") != 0 || 
      sum(is.na(agelimit)) != 0) { # no agelimit option given
    working_data <- working_data
  } else {# agelimit option given
    lowerlimit <- agelimit[1]
    upperlimit <- agelimit[2]
    age_columns <- c("age")
    colnum <- get_colno_existing_colnames(age_columns, working_data)
    working_data <- working_data[working_data[colnum] >= lowerlimit & 
                                     working_data[colnum] <= upperlimit, ]
  }
  return(working_data)
}
###############################################################################
#' Function to add an underscore for texts with spaces in between
#' @param this_string a string
#' @return  string where the spaces replaced by "_"
#' @examples
#' replace_space_underscore("Sri Lanka")
#' @export
replace_space_underscore <- function(this_string) {
  sep_string <- unlist(strsplit(this_string, " "))
  if (length(sep_string) < 1) {
    stop("Error in separating the string")
  } else {
    new_string <- sep_string[1]
    if (length(sep_string) > 1) {
      for (i in 2:length(sep_string)) {
        new_string <- cbind(new_string, sep_string[i])
      }
      new_string <- paste(new_string, collapse = "_")
    } else {
      new_string <- sep_string
    }
    return(new_string)
  }
}
