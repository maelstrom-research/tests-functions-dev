#' @title
#' Convert categorical input to a non-categorical variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function is used to convert a vector categorical object to a 
#' non-categorical object. When it has, the 
#' characteristics of the category is added to the data dictionary.
#'
#' @param x object to be coerced.
#'
#' @return
#' A R object.
#'
#' @examples
#' {
#' 
#' drop_category(iris[['Species']])
#' 
#'}
#'
#' @import dplyr haven
#' @importFrom rlang .data
#'
#' @noRd
drop_category <- function(x){
  
  # check if the col is a vector
  if(is.list(x)) stop("'list' object cannot be coerced to a category")
  
  # check if the col is already a category
  if(!is_category(x)) return(x)
  
  fct_att <- attributes(x)
  vT_x <- valueType_of(x)
  
  x = as_valueType(as.character(x),valueType = vT_x)
  vec_att <- attributes(x)
  
  fct_att <- fct_att[!str_detect(names(fct_att),'Categories::')]
  fct_att['class']      <- NULL
  fct_att['labels']     <- NULL
  fct_att['levels']     <- NULL
  fct_att['na_values']  <- NULL
  
  attributes(x) = c(vec_att, fct_att)
  
  return(x)
  
}
