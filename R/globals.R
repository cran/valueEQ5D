#' Required to avoid the note/warning with RMD check "no visible binding for global variable"
#' @import utils
utils::globalVariables(names = c(
  "Probability_matrix_crosswalk.df",
  "EQ5D3L_indexvalues.df", "EQ5D5L_indexvalues.df",
  "EQ5D5L_crosswalk_indexvalues.df",
  "EQ5D5L_tariffs.df", "EQ5D3L_tariffs_TTO.df", "EQ5D3L_tariffs_VAS.df"
), package = "valueEQ5D", add = TRUE)
