#' @title
#' Replace outliers with NA in a column of a dataset
#'
#' @description
#' This function takes a column as input and replaces statistical outliers with
#' NA when the column is compatible to a numerical variable, using Tukey's 
#' method. If the column is not compatible, the function passes through. The
#' function also excludes missing values (déclared in the data dictionary and
#' stored as attributes in the column) to calculation of the outlier.
#'
#' @param data A column containing values (compatible to numeric) with potential 
#' outliers.
#' 
#' @details The function identifies outliers in each numerical variable using 
#' Tukey's method. For each variable, it calculates the first and third 
#' quartiles (Q1 and Q3). Values outside the range [Q1, Q3] are considered 
#' outliers and are replaced with NA.
#'
#' @return A modified column where outliers are replaced with NA.
#'
#' @examples
#' {
#' 
#' library(tibble)
#' your_tibble <- tribble(
#'   ~numeric_var1, ~numeric_var2,
#'   1, 5,
#'   2, 8,
#'   3, 12,
#'   4, 15,
#'   100, 3
#' )
#'
#' # Apply the function
#' your_tibble <- replace_outliers(your_tibble)
#' }
#'
#' @import dplyr haven stats
#' @export
remove_outliers <- function(col, silent = FALSE) {
  
  missing <- attributes(col)$na_values[[1]]
  vec_missing <- col %in% missing
  val_missing <- col[vec_missing]
  col[vec_missing] <- NA
  
  col_test <- try(
    suppressWarnings(as_valueType(haven::zap_labels(col),'decimal')),
    silent = TRUE)
  
  if(class(col_test)[[1]] == 'try-error'){
    col[vec_missing] <- val_missing
    if(silent == FALSE)
      warning('The values are not compatible with numerical equivalent.')
    return(col)}

  lower_bound <- quantile(col_test, 0.05,na.rm = TRUE)
  upper_bound <- quantile(col_test, 0.95,na.rm = TRUE)
  col[col_test < lower_bound | col_test > upper_bound] <- NA
  col[vec_missing] <- val_missing
  return(col)
}
