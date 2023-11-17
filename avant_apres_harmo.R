avant_apres_harmo <- function(
  dossier = NULL, 
  harmonized_dossier = NULL, 
  dataset = NULL,
  data_dict = NULL,
  harmonized_dataset = NULL,
  data_proc_elem = attributes(harmonized_dossier)$`Rmonize::Data Processing Elements`,
  dataschema = attributes(harmonized_dossier)$`Rmonize::DataSchema`,
  col_dataset = names(dataset),
  col_harmonized_dataset = names(harmonized_dataset)
){
  
  # internal function to extract names of objects in dpe
  extract_var <- function(x){
    x <- x %>%
      str_replace_all('"',"`") %>%
      str_replace_all("'","`") %>%
      str_remove_all("`") 
    x = x[!is.na(x)]
    
    return(x)}
  
  if(!is.null(dossier) & !is.null(dataset)) stop('no dossier AND dataset')
  if(!is.null(harmonized_dossier) & !is.null(harmonized_dataset)) stop('no harmonized dossier AND harmonized dataset')
  
  # case if user provides dataset and harmo dataset
  if(!is.null(dataset) & !is.null(harmonized_dataset)){
    
    dataset = as_dataset(dataset)
    harmonized_dataset = as_dataset(harmonized_dataset)
    
    if(is.null(data_proc_elem)){
      
      if(length(col_harmonized_dataset) != 1) stop('col_harmonized_dataset must be unique')
      
      dataset_from  <- 
        dataset %>%
        select(all_of(col_dataset))
      
      # extract the data_dict input (not used yet. for future dev)
      # data_dict_from  <- 
      #   dataset_from %>%
      #   data_dict_extract()
      
      # extract the dataset output
      dataset_to <- 
        harmonized_dataset %>%
        select(all_of(col_harmonized_dataset))
      
      # extract the data_dict input (not used yet. for future dev)
      # data_dict_to  <- 
      #   dataset_to %>%
      #   data_dict_extract()
       
      if(sum(col_dataset %in% col_harmonized_dataset) > 1) {
        
        names(dataset_to) = paste0(col_dataset,'(output)')
        names(dataset_from) = paste0(col_harmonized_dataset,'(input)')
        
      }
      
      table_all <- bind_cols(dataset_to,dataset_from)
      
      if(ncol(table_all) == 0) return(tibble())
      
      table_all <- 
        table_all %>%
        distinct() %>%
        arrange(across(everything())) %>%
        mutate(
          `  ` = '||',
        ) %>%
        select(1,`  `,everything())
    
      return(table_all) 
      
    }
    
    name_dataset <- unique(data_proc_elem$input_dataset)
    if(length(name_dataset) > 1) stop('names of datasets not unique in DPE')
    
    # extract the data_dict input (not used yet. for future dev)
    # if(!is.null(data_dict)) dataset <- data_dict_apply(dataset, data_dict)
    
    dossier = dossier_create(dataset)
    names(dossier) = name_dataset
    
    # extract the data_dict input (not used yet. for future dev)
    if(is.null(dataschema)) 
      dataschema = list(Variables = tibble(
        name = extract_var(data_proc_elem$dataschema_variable),
        label = name,
        valueType = 'text'
      )) %>% as_dataschema_mlstr()
    
    harmonized_dataset = data_dict_match_dataset(harmonized_dataset,dataschema, output = 'dataset')
    harmonized_dossier = dossier_create(harmonized_dataset)
    names(harmonized_dossier) = name_dataset
    # harmonized_dossier = as_harmonized_dossier(harmonized_dossier, dataschema,data_proc_elem)
    
    return(avant_apres_harmo(
      dossier = dossier, 
      harmonized_dossier = harmonized_dossier,
      data_proc_elem = data_proc_elem,
      dataschema = dataschema)
    )
  }
  
  # case when user provides dossiers 
  # extract global objects: dataschema and dpe
  # dataschema <- 
  #   attributes(harmonized_dossier)$`Rmonize::DataSchema`
  
  data_proc_elem <-
    data_proc_elem %>%
    group_by(input_dataset) %>% group_split()
  
  names(data_proc_elem) = 
    bind_rows(data_proc_elem) %>% arrange(input_dataset) %>% 
    pull(input_dataset) %>% unique %>% extract_var()
  
  # reduce complexity
  dossier <-
    dossier[intersect(names(dossier), names(data_proc_elem))]
  
  data_proc_elem <-
    data_proc_elem[intersect(names(dossier), names(data_proc_elem))]
  
  # initialize
  table_all = tibble(
    dataset = as.character(),
    output_var_name = as.character(),
    input_var_names = as.character(),
    class_output = as.character(),
  )
  
  # for each dataset
  for(dataset_i in names(data_proc_elem)){
    # stop()}
    
    message(str_sub(paste0("\n",
"--before/after of : ",
crayon::bold(dataset_i)," -----------------------------------------------------"),1,81))
    
    # extract the dataset input
    dataset_from  <- 
      dossier[[dataset_i]] %>%
      mutate(across(everything(),as.character))
    
    # extract the data_dict input
    data_dict_from  <- 
      dossier[[dataset_i]] %>%
      data_dict_extract()
    
    # extract the dataset output
    dataset_to <- 
      harmonized_dossier[[dataset_i]] %>%
      mutate(across(everything(),as.character))
    
    # extract the involved data_proc_elem lines
    data_proc_elem_i <- 
      data_proc_elem[[dataset_i]]
    
    # initialize
    table_i = tibble()
    
    for(proc_j in seq_len(nrow(data_proc_elem_i))){
      # stop()}
       
      # for each line of the dpe :
      #   - read the rule, 
      
      var_to_j  <- 
        data_proc_elem_i[proc_j,] %>% 
        pull(.data$`dataschema_variable`) %>%
        extract_var %>%
        set_names('output_value')
      
      err = try({
        
        rule_category = 
          data_proc_elem_i[proc_j,] %>% 
          pull(.data$`Mlstr_harmo::rule_category`)
        
        #   - extract variables (input and output) 
        vars_from_j <-     
          str_squish(unlist(strsplit(
            data_proc_elem_i[proc_j,] %>% 
              pull(.data$`input_variables`),split = ";"))) %>%
          extract_var %>% 
          set_names(paste0('input_value_',c(seq_len(length(.)))))
      
        #   - generate subdataset (input and output)
        if(toString(vars_from_j) %in% '__BLANK__') {
          dataset_from_j  <- dataset_from %>% select(any_of(vars_from_j))          
        }else{
          dataset_from_j  <- dataset_from %>% select(all_of(vars_from_j))
        }

        if(ncol(dataset_from_j) == 0){
          dataset_from_j <- 
            tibble(
              `Rmonize::col` = 
                rep(extract_var(data_proc_elem_i[proc_j,] %>% 
                                  pull(.data$`Mlstr_harmo::algorithm`)),
                    nrow(dataset_from_j)))}
        
        var_to_j  <- var_to_j

        dataset_to_j <- 
          dataset_to %>% 
          select(all_of(var_to_j))
        
        #   - generate subdata dict (input and output)
        data_dict_from_j <- 
          data_dict_from %>%
          data_dict_filter(paste0('name %in% c("',paste0(c(vars_from_j),collapse = '","'),'")'))
        
        data_dict_to_j <- 
          dataschema %>%
          data_dict_filter(paste0('name %in% c("',paste0(c(var_to_j),collapse = '","'),'")'))
        
        #   - add class of each observation (either valid value, missing, 
        #     NA or categorical). Use preprocess function for that
        preprocess_to_j = 
          madshapR::dataset_preprocess(
            distinct(dataset_to_j %>% setNames(var_to_j)),
            data_dict_to_j) %>%
          select(-index_in_dataset,-index_value) %>%
          distinct
        
        preprocess_from_j = 
          madshapR::dataset_preprocess(
            distinct(dataset_from_j %>% setNames(vars_from_j)),
            data_dict_from_j) %>%
          select(-index_in_dataset,-index_value) %>%
          distinct
        
        #   - combine them (output on the left, then input)      
        table_j <- 
          dataset_to_j %>%
          left_join(preprocess_to_j,by = c('output_value' ='value_var')) %>% 
          select(output_value, class_output = valid_class) %>%
          bind_cols(dataset_from_j) %>%
          set_names(c(names(var_to_j),'class_output',names(vars_from_j))) %>%
          mutate(output_value = replace_na(output_value,'NA')) %>%
          mutate(across(starts_with('input_value_'), ~ replace_na(.,'NA'))) %>%
          mutate(
            dataset = dataset_i,
            output_var_name = var_to_j,
            rule_category = rule_category,
            cut = '||',
            input_var_names = 
              paste0(paste0(names(vars_from_j)," = ", vars_from_j),collapse = ' ; ')
          ) %>%
          select(
            dataset,
            output_var_name,
            class_output,
            input_var_names, 
            rule_category,
            ` ` = cut,
            output_value,
            `  ` = cut,
            starts_with('input_value_')) %>%
          distinct
        
        # summarise information for each case determined by the rule category
        if(rule_category == 'id_creation') {
          table_j = 
            table_j %>% 
            mutate(output_value = paste0(var_to_j,'(s)'),
                   input_value_1 = paste0(vars_from_j,'(s)'))}
        
        if(rule_category == 'paste') {
          table_j = table_j %>% 
            mutate(input_value_1 = '__BLANK__')
        }
        
        if(rule_category == 'impossible') {
          table_j = table_j %>% 
            mutate(input_value_1 = '__BLANK__')
        }
        
        if(rule_category == 'undetermined') {
          table_j = table_j %>% 
            mutate(input_value_1 = '__BLANK__')
        }
        
        if(rule_category == 'recode') {
          
          table_j <- table_j %>%
            arrange(across(c('output_value',starts_with('input_value_')))) %>% 
            distinct() %>%
            select(
              dataset,output_var_name,class_output,
              input_var_names,rule_category,` `,output_value,`  `,input_value_1)
        }
        
        if(rule_category == 'case_when') {
          
          table_j <- table_j %>%
            arrange(across(c('output_value',starts_with('input_value_')))) %>% 
            distinct() %>%
            select(
              dataset,output_var_name,class_output,
              input_var_names,rule_category,` `,output_value,`  `,starts_with('input_value_'))
        }
        
        
        if(rule_category == 'operation') {
          
          table_j <- 
            table_j %>%
            arrange(across(c('output_value',starts_with('input_value_')))) %>% 
            distinct() %>%
            select(
              dataset,output_var_name,class_output,
              input_var_names,rule_category,` `,output_value,`  `,starts_with('input_value_'))
        }
        
        if(rule_category == 'other') {
          
          table_j <- table_j %>%
            arrange(across(c('output_value',starts_with('input_value_')))) %>% 
            distinct() %>%
            select(
              dataset,output_var_name,class_output,
              input_var_names,rule_category,` `,output_value,`  `,starts_with('input_value_'))
        }
        
        
        if(rule_category == 'direct_mapping') {
          
          table_j <- table_j %>%
            left_join(preprocess_from_j,by = c('input_value_1' ='value_var')) %>%
            select(dataset:input_value_1,cat_input = `Categorical variable`, class_input = valid_class) %>%
            arrange(across(c('output_value',starts_with('input_value_')))) %>% 
            distinct() %>%
            group_by(class_output,class_input) %>%
            reframe(
              dataset = dataset,
              output_var_name = output_var_name,
              input_var_names = input_var_names,
              rule_category = rule_category,
              ` ` = ` `,
              output_value = paste0(output_value,collapse = ', '),
              `  ` = `  `,
              input_value_1 = paste0(input_value_1,collapse = ', ')) %>%
            distinct %>%
            mutate(input_value_1 = 'identical') %>%
            select(
              dataset,output_var_name,class_output,
              input_var_names,rule_category,` `,output_value,`  `,input_value_1)
          
        }
        
        # clean elements. if the input variable is unique, replace name of the 
        # column input_value by the actual name of the variable
        table_j = 
          table_j %>%
          mutate(
            input_var_names = ifelse(!'input_value_2' %in% names(table_j),vars_from_j,input_var_names),
            class_output = str_sub(class_output, 3,-1)) %>% 
          distinct
        
        table_i = bind_rows(table_i,table_j)
      
      }, silent = TRUE)
      
      err <- ifelse((class(err)[1] == 'try-error'),'**failed**','')
      message(paste0(str_sub(paste0(
        str_trunc(paste0(
          "    ",proc_j,"/",nrow(data_proc_elem_i)," : ",
          var_to_j),width = 49,ellipsis = '[...]'),
        "                                       "),1,50)),bold(err))
    }
    
    # bind elements
    table_all = bind_rows(table_all,distinct(table_i))
  }
  
  # clean elements.
  #  - if one dataset, the column is deleted
  if(length(unique(table_all$dataset)) == 1 ) 
    table_all['dataset'] <- NULL 
  
  #  - if one output variable, the column is renamed/deleted
  if(length(unique(table_all$output_var_name)) == 1) 
    table_all <- table_all %>%
    mutate(output_value = na_if(output_value,'NA')) %>%
    mutate(across(starts_with('input_value_'), ~na_if(.,'NA'))) %>%
    rename_with(.cols = starts_with('output_value'),.fn = ~ c(var_to_j)) %>%
    select(-output_var_name)
    
  #  - if one input variable combo, the column is renamed/deleted
  if(length(unique(table_all$input_var_names)) == 1)
    table_all <- table_all %>%
    rename_with(.cols = starts_with('input_value_'),.fn = ~ c(vars_from_j)) %>%
    select(-input_var_names) 
  
  #  - if one class of variable, the column is deleted
  if(length(unique(table_all$class_output)) == 1)
    table_all <- table_all %>%
    select(-class_output)
  
  # table_all <- distinct(table_all) %>% arrange(pick(1))
  
  return(table_all)
}


# run tests on DEMO files
library(tidyverse)
library(fabR)
library(madshapR)
library(Rmonize)
library(crayon)

dataset_MELBOURNE <- madshapR_DEMO$dataset_MELBOURNE
dataset_PARIS     <- madshapR_DEMO$dataset_PARIS
dataset_TOKYO     <- madshapR_DEMO$dataset_TOKYO

# create the inputs
dossier <- dossier_create(list(dataset_MELBOURNE,dataset_PARIS,dataset_TOKYO))
data_proc_elem <- Rmonize_DEMO$`data_processing_elements - final`
dataschema <- Rmonize_DEMO$`dataschema - final`

# process harmonization
harmonized_dossier <- harmo_process(dossier,dataschema,data_proc_elem)

# avant apres harmo pour tous les dpe
avant_apres_harmo(dossier,harmonized_dossier)

# avant apres harmo pour tous des PE choisis parmi le dpe
avant_apres_harmo(dossier,harmonized_dossier,
                  data_proc_elem = data_proc_elem %>% slice(2:5))

# avant apres harmo en comparant une partie de l'harmo 
avant_apres_harmo(
  dataset = dataset_MELBOURNE,
  harmonized_dataset = harmonized_dossier$dataset_MELBOURNE,
  data_proc_elem = data_proc_elem %>% filter(input_dataset == 'dataset_MELBOURNE'))

# avant apres harmo en comparant une partie de l'harmo : une variable
avant_apres_harmo(
  dataset = dataset_MELBOURNE['Gender'],
  harmonized_dataset = harmonized_dossier$dataset_MELBOURNE['sdc_sex'])

# avant apres harmo en comparant une partie de l'harmo : plusieurs variables
avant_apres_harmo(
  dataset = dataset_MELBOURNE[c('Gender','prg_curr')],
  harmonized_dataset = harmonized_dossier$dataset_MELBOURNE['sdc_sex'])





