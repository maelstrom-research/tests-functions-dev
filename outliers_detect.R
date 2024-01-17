#' @title
#' xxx xxx xxx
#'
#' @description
#' xxx xxx xxx
#'
#' @param dataset A dataset object.
#' @param data_outlier_elements xxx xxx xxx
#' @param data_dict A list of data frame(s) representing metadata.
#' 
#' @details 
#' A dataset is a data table containing variables. A dataset object is a 
#' data frame and can be associated with a data dictionary. If no 
#' data dictionary is provided with a dataset, a minimum workable 
#' data dictionary will be generated as needed within relevant functions.
#' Identifier variable(s) for indexing can be specified by the user. 
#' The id values must be non-missing and will be used in functions that 
#' require it. If no identifier variable is specified, indexing is 
#' handled automatically by the function.
#' 
#' A data dictionary contains the list of variables in a dataset and metadata 
#' about the variables and can be associated with a dataset. A data dictionary 
#' object is a list of data frame(s) named 'Variables' (required) and 
#' 'Categories' (if any). To be usable in any function, the data frame 
#' 'Variables' must contain at least the `name` column, with all unique and 
#' non-missing entries, and the data frame 'Categories' must contain at least 
#' the `variable` and `name` columns, with unique combination of 
#' `variable` and `name`.
#' 
#' xxx xxx xxx
#'
#' @return 
#' A data frame with, for each statement, the values found in the dataset.
#'
#' @examples
#' {
#' 
#' library(dplyr)
#' library(madshapR)
#' library(fabR)
#' library(tidyr)
#' library(stringr)
#' library(crayon)
#' # Example dataset
#' set.seed(384122)
#' dataset = mtcars
#' dataset$entity_id <- make.names(rownames(mtcars))
#' 
#' dataset <- 
#'   dataset %>%
#'   mutate(
#'     carb = as_category(carb)) %>%
#'   valueType_self_adjust() %>%
#'   as_dataset('entity_id')
#' 
#' # Add date variable with random values
#' dataset$date_end = sample(seq(as.Date("2020-01-01"), Sys.Date(), by="day"), 32)
#' 
#' data_dict <- data_dict_extract(dataset)
#' data_dict$Categories$missing = c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE)
#' 
#' data_outlier_elements <-
#'    tibble(variable = c("mpg"          , "carb"        , "disp"        , "hp"         ,
#'                       "drat"         , "wt"          , "qsec"        , "vs"         ,
#'                       "am"           , "am"          , "gear"        , "date_end"  ),
#'          statement = c(">"           , ">"           , "statistical" ,"="           ,
#'                        '!is.na'      , ">="          ,"="            , "!in"        ,
#'                        "!in"         ,"!="           , "<"           , "!in"       ),
#'          value     = c("20"          , "2"           , NA_character_ , "180"        ,
#'                        NA_character_ , "5.25"        ,"0,1,3:6"      , "0:1"        ,
#'                        "0:1"         , "dataset$vs"  , "2"           , "'2021-10-01':'2023-01-01'"),
#'          check_na  = c(FALSE         , TRUE          , FALSE         , FALSE        ,
#'                        FALSE         , FALSE         , TRUE          , FALSE        ,
#'                        FALSE         , TRUE          , FALSE         , FALSE        ))
#' 
#' outliers_detect(dataset, data_outlier_elements,data_dict)
#' 
#' }
#'
#' @import stringr dplyr fabR madshapR tidyr 
#' @importFrom crayon bold 
#' @export
outliers_detect <- function(dataset, data_outlier_elements, data_dict = NULL){
  
  # Internal function to detect statistical outliers. This will become an 
  # independant function in the future
  is_outliers <- function(col, missing_values = NA, silent = TRUE) {
    
    vec_missing <- col %in% missing_values
    val_missing <- col[vec_missing]
    col[vec_missing] <- NA
    is_outliers <- rep(FALSE,length(col))
    
    col_test <- try(
      suppressWarnings(as_valueType(zap_labels(col),'decimal')),
      silent = TRUE)
    
    if(class(col_test)[[1]] == 'try-error'){
      col[vec_missing] <- val_missing
      if(silent == FALSE)
        warning('The values are not compatible with numerical equivalent.')
      stop(col)}
    
    lower_bound <- quantile(col_test, 0.05,na.rm = TRUE)
    upper_bound <- quantile(col_test, 0.95,na.rm = TRUE)
    outlier_ind <- which(col_test < lower_bound | col_test > upper_bound)
    is_outliers[outlier_ind] <- TRUE
    
    return(is_outliers)
  }

  if(is.null(data_dict)){
    data_dict <- data_dict_extract(dataset,as_data_dict_mlstr = TRUE)    
  }else{
    data_dict <- as_data_dict_mlstr(data_dict)}
  
  dataset <- if(is.null(col_id(dataset))){
    add_index(dataset,"madshapR::index",.force = TRUE) %>%
      mutate("madshapR::index" = paste0('line ', .data$`madshapR::index`)) %>%
      as_dataset("madshapR::index")
  } else dataset
  
  data_outlier_statements <- 
    data_outlier_elements %>% 
    select('variable','statement','value','check_na') %>%
    rename('name' = 'variable') %>%
    left_join(data_dict$Variables %>% select('name','valueType'), by='name') %>%
    mutate(valueType = replace_na(.data$`valueType`,('text')))
  
  data_outlier_statements <- 
    data_outlier_statements %>% 
    add_index(.force = TRUE) %>%
    mutate(across(everything(),str_squish)) %>%
    mutate(
      name_var    = paste0("dataset$`",.data$`name`,"`"),
      check_na    = ifelse(.data$`statement` %in% c("is.na","!is.na"),
                           .data$`statement`,
                           ifelse(.data$`check_na` == TRUE,'is.na',NA_character_)),
      statement   = ifelse(.data$`statement` %in% c("is.na","!is.na"),
                           NA_character_,
                           .data$`statement`),
      negate      = ifelse(str_detect(.data$`statement`, "\\!"),"!",NA_character_),
      statement   = str_remove(.data$`statement`, "!"),
      statistical = .data$`statement` %in% "statistical",
      statement   = ifelse(.data$`statement` == "statistical",NA_character_,.data$`statement`),
      statement = str_replace(.data$`statement`, "^=$","==")) %>%
    separate_longer_delim("value",",") %>%
    mutate(value = str_squish(.data$`value`)) %>%
    separate_wider_delim(cols = "value",delim = ":",names = c('min',"max"),too_few = "align_start") %>%
    mutate(
      min = ifelse(.data$`valueType` == "date", paste0('as_date(',.data$`min`,')'),.data$`min`),
      max = ifelse(.data$`valueType` == "date" & !is.na(.data$`max`), paste0('as_date(',.data$`max`,')'),.data$`max`),
      statement_right = ifelse(!is.na(.data$`max`),paste0("& ",.data$`name_var`, " <="),NA_character_),
      statement       = ifelse(!is.na(.data$`max`), ">=",str_replace(.data$`statement`,"^in$","==")),
      open_parent     = "(",
      close_parent    = ")",
    ) %>%
    select("index","name", "name_var","open_parent",
           "statement","min","statement_right","max","close_parent", 
           "negate", "check_na","statistical") %>%
    mutate(across(c('name_var','open_parent', 'close_parent'), 
                  ~ ifelse(is.na(.data$`statement`),NA_character_,.))) %>%
    unite('statement',c('open_parent','name_var','statement','min','statement_right','max','close_parent'),
          sep = ' ',na.rm = TRUE) %>%
    group_by(pick(c('index','name','negate', 'check_na','statistical'))) %>%
    reframe(across(everything(), ~ paste0(.,collapse = ' | '))) %>%
    unite('statement',c('negate','statement'),sep = '',na.rm = TRUE) %>%
    mutate(statement = na_if(.data$`statement`,"")) %>%
    mutate(check_na = ifelse(!is.na(.data$`check_na`), 
                             paste0(.data$`check_na`,"(","dataset$`",.data$`name`,"`",")"),
                             NA_character_)) %>%
    mutate(index = as.integer(.data$`index`)) %>%
    arrange(pick("index"))

  final_output = tibble()
  for (i in 1:nrow(data_outlier_statements)) {
    
    mess <- bold(' - complete')
    missing_values <- 
      data_dict$Categories %>%
      dplyr::filter(
        variable == data_outlier_statements[[i,'name']] &
          missing == TRUE) %>%
      pull(name) %>%
      as_valueType(
        valueType = data_dict$Variables %>% 
          dplyr::filter(name == data_outlier_statements[[i,'name']]) %>%
          pull(.data$`valueType`))
    if(length(missing_values) == 0) missing_values <- NA
    
    test_statement <- tibble()
    if(!is.na(data_outlier_statements[[i,'statement']])){
      test_statement <- 
        try(
          dataset[parceval(data_outlier_statements[[i,'statement']]),] %>%
            select(col_id(dataset), value = data_outlier_statements[[all_of(i),'name']]) %>%
            mutate(
              index = data_outlier_statements[[i,'index']],
              variable_name = data_outlier_statements[[i,'name']],
              statement = data_outlier_statements[[i,'statement']]) %>%
            select("index", col_id(dataset),"variable_name","statement", "value"), silent = TRUE)
      
      if(class(test_statement)[[1]] == "try-error"){
        test_statement <- tibble(
          index = data_outlier_statements[[i,'index']],
          variable_name = data_outlier_statements[[i,'name']],
          statement     = data_outlier_statements[[i,'statement']],
          value         = '[ERR] - Error in formulating the statement')
        mess <- bold(' - **ERROR**')
      }else{
        
        test_statement <- 
          test_statement %>%
          dplyr::filter(!.data$`value` %in% missing_values)
        
        if(nrow(test_statement) == 0){
          
          test_statement <- tibble(
            index = data_outlier_statements[[i,'index']],
            variable_name = data_outlier_statements[[i,'name']],
            statement     = data_outlier_statements[[i,'statement']],
            value         = 'No value found')
        }
      }
    }
    
    test_na <- tibble()
    if(!is.na(data_outlier_statements[[i,'check_na']])){
      test_na <- 
        try(
          dataset[parceval(data_outlier_statements[[i,'check_na']]),] %>%
            select(col_id(dataset), value = data_outlier_statements[[all_of(i),'name']]) %>%
            mutate(
              index = data_outlier_statements[[i,'index']],
              variable_name = data_outlier_statements[[i,'name']],
              statement = data_outlier_statements[[i,'check_na']]) %>%
            select("index",col_id(dataset),"variable_name","statement"), silent = TRUE)
      
      if(class(test_na)[[1]] == "try-error"){
        test_na <- tibble(
          index = data_outlier_statements[[i,'index']],
          variable_name = data_outlier_statements[[i,'name']],
          statement     = data_outlier_statements[[i,'check_na']],
          value         = '[ERR] - Error in formulating the statement')
        mess <- bold(' - **ERROR**')
      }else{
        
        if(nrow(test_na) == 0){
          
          test_na <- tibble(
            index = data_outlier_statements[[i,'index']],
            variable_name = data_outlier_statements[[i,'name']],
            statement     = 'is.na',
            value         = 'No value found')
        }
      }
    }
    
    test_stats <- tibble()
    if(data_outlier_statements[[i,'statistical']] == TRUE){
      test_stats <- 
        try(
          dataset %>%
            select(col_id(dataset), value = data_outlier_statements[[all_of(i),'name']]) %>%
            dplyr::filter(is_outliers(.data$`value`,missing_values)) %>%
            mutate(
              index = data_outlier_statements[[i,'index']],
              variable_name = data_outlier_statements[[i,'name']],
              statement = 'is_outlier') %>%
            select("index",col_id(dataset),"variable_name","statement", "value"), silent = TRUE)
      
      if(class(test_stats)[[1]] == "try-error"){
        test_stats <- tibble(
          index = data_outlier_statements[[i,'index']],
          variable_name = data_outlier_statements[[i,'name']],
          statement     = 'is_outlier',
          value         = '[ERR] - Error in formulating the statement')
        mess <- bold(' - **ERROR**')
      }else{
        
        if(nrow(test_stats) == 0){
          test_stats <- tibble(
            index = data_outlier_statements[[i,'index']],
            variable_name = data_outlier_statements[[i,'name']],
            statement     = 'is_outlier',
            value         = 'No value found')
        }
      }
    }
    
    message("processing : ",i,"/",nrow(data_outlier_statements),mess)
    
    test_statement <- test_statement %>% mutate(across(everything(),as.character))
    test_na        <- test_na %>% mutate(across(everything(),as.character))
    test_stats     <- test_stats %>% mutate(across(everything(),as.character))
    
    final_output <- bind_rows(final_output,test_statement,test_na,test_stats)
  }
  
  final_output <- 
    final_output %>%
    mutate(index = as.integer(.data$`index`)) %>% arrange(pick("index")) %>% select(-"index") %>%
    mutate(statement = str_remove_all(.data$`statement`,'dataset\\$'))
  return(final_output)
}
