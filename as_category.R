#' @title
#' Convert input to a categorical variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function is used to convert a vector object to a categorical object. It
#' Is analoguous to [as.factor()] function when the vector (variable) has no
#' data dictionary associated. When it has, the characteristics of the category
#' is added to the data dictionary.
#'
#' @param x object to be coerced.
#'
#' @seealso
#' [as.factor()]
#'
#' @return
#' A R Object of class haven_labelled.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' iris %>% mutate(Species = as_category(Species))
#' as_category(iris[['Species']])
#' 
#'}
#'
#' @import dplyr haven
#' @importFrom rlang .data
#'
#' @noRd
as_category <- function(x){
  
  # check if the col is a vector
  if(is.list(x)) stop("'list' object cannot be coerced to a category")
  
  # check if the col is already a category
  if(is_category(x)) return(x)
  
  att <- attributes(x)
  
  # if column has a specific class, it first will be cast as a character
  if(!is.null(attributes(x)[['class']][1])) {
    att[['class']] <- NULL
    x <- as.character(x)}
  
  x_init <- x
  x <- as.factor(x)
  fct_att <- attributes(x)
  
  fct_att$labels <- fct_att$levels
  names(fct_att$labels) <-  fct_att$labels
  fct_att$levels <- NULL
  
  vT_list = valueType_list
  fct_att$`class` <-
    c("haven_labelled","vctrs_vctr",
      vT_list[[which(valueType_list$`valueType` == valueType_of(x)),"class"]])
  
  attributes(x_init) = c(fct_att['labels'],fct_att['class'],att)
  
  return(x_init)
  
}
