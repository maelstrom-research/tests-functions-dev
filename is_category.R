#' @title
#' Test if a column in a dataset is a categorical variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Test if a column in a dataset is a categorical variable. This function mainly 
#' helps validate input within other functions of the package.
#'
#' @param x object to be coerced.
#'
#' @return
#' A logical.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' iris %>% summarise(across(everything(), is_category))
#' is_category(iris[['Species']])
#' 
#'}
#'
#' @import dplyr haven
#' @importFrom rlang .data
#'
#' @noRd
is_category <- function(column, threshold = NULL) {
  
  unique_column <- unique(column)
  if(is.factor(unique_column))   return(TRUE)
  if(is.labelled(unique_column)) return(TRUE)
  if(all(is.na(unique_column)))  return(FALSE)
  if(is.null(threshold))         return(FALSE)
  
  return(length(unique_column) <= threshold)
}
