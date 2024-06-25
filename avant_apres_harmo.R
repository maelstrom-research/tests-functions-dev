library(tidyverse)
library(Rmonize)
library(crayon)
library(madshapR)
library(fabR)

avant_apres_harmo <- function(
    dossier = NULL, 
    harmonized_dossier = NULL, 
    split_by = NULL,
    summarize_output = TRUE,
    data_proc_elem = attributes(harmonized_dossier)$`Rmonize::Data Processing Elements`,
    dataschema = attributes(harmonized_dossier)$`Rmonize::DataSchema`,
    dataset = NULL,
    data_dict = NULL,
    harmonized_dataset = NULL,
    harmonized_data_dict = NULL,
    cols_dataset = names(dataset),
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
  
  
  if(!is.null(dossier) & !is.null(dataset)) 
    stop('no dossier nor dataset')
  if(!is.null(harmonized_dossier) & !is.null(harmonized_dataset)) 
    stop('no harmonized dossier nor harmonized dataset')
  
  if(!toString(split_by) %in%
     c("","dataschema_variable","input_dataset","Mlstr_harmo::rule_category"))
    stop(call. = FALSE, "
Possible values for `split_by`:\n 
'dataschema_variable','input_dataset' or 'Mlstr_harmo::rule_category' or NULL")
  
  
  
  
  # case if user provides dataset and harmo dataset
  if(!is.null(dataset) & !is.null(harmonized_dataset)){
    
    dataset <- 
      as_dataset(dataset,col_id = col_id(dataset)) %>%
      select(all_of(cols_dataset))
    
    if(is.null(data_dict)) {
      data_dict <- data_dict_extract(dataset)
      data_dict <- data_dict_match_dataset(dataset,data_dict)$data_dict
      dataset <- data_dict_apply(dataset,data_dict)
    }
    
    
    harmonized_dataset <- 
      as_dataset(harmonized_dataset,col_id = col_id(harmonized_dataset)) %>%
      select(all_of(col_harmonized_dataset))
    
    if(is.null(harmonized_data_dict)) {
      
      if(is.null(dataschema)){
        harmonized_data_dict <- data_dict_extract(harmonized_dataset)
      }else{
        harmonized_data_dict <- dataschema
      }
      
      harmonized_data_dict <- data_dict_match_dataset(harmonized_dataset,harmonized_data_dict)$data_dict
      harmonized_dataset <- data_dict_apply(harmonized_dataset,harmonized_data_dict)
    }
    
    dossier <- dossier_create(dataset)
    harmonized_dossier <- dossier_create(harmonized_dataset)
    
    if(is.null(data_proc_elem)){
      
      if(length(col_harmonized_dataset) != 1) 
        stop('col_harmonized_dataset must be unique')
      
      dataset_from  <- 
        dataset %>%
        select(all_of(col_dataset))
      
      # extract the dataset output
      dataset_to <- 
        harmonized_dataset %>%
        select(all_of(col_harmonized_dataset))
      
      if(sum(col_dataset %in% col_harmonized_dataset) > 1) {
        
        names(dataset_to) = paste0(col_dataset,'(output)')
        names(dataset_from) = paste0(col_harmonized_dataset,'(input)')
        
      }
      
      beforafter_all <- bind_cols(dataset_to,dataset_from)
      
      if(ncol(beforafter_all) == 0) return(tibble())
      
      beforafter_all <- 
        beforafter_all %>%
        distinct() %>%
        arrange(across(everything())) %>%
        mutate(
          `  ` = '||',
        ) %>%
        select(1,`  `,everything())
      
      return(beforafter_all) 
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
    
    avant_apres <- 
      avant_apres_harmo(
        dossier = dossier, 
        harmonized_dossier = harmonized_dossier,
        split_by = split_by,
        data_proc_elem = data_proc_elem,
        dataschema = dataschema,
        summarize_output = summarize_output)
    
    return(avant_apres)
  }
  
  
  # case when user provides dossiers 
  # extract global objects: dataschema and dpe
  # dataschema <- 
  #   attributes(harmonized_dossier)$`Rmonize::DataSchema`
  
  # make sure the names of the datasets are properly written in the dpe
  data_proc_elem <- 
    data_proc_elem %>% 
    mutate(input_dataset = extract_var(input_dataset))

  # reduce complexity
  intersected_datasets <- 
    intersect(intersect(names(dossier), names(harmonized_dossier)), data_proc_elem$input_dataset)
  
  dossier <- dossier[intersected_datasets]
  harmonized_dossier <- harmonized_dossier[intersected_datasets]
  data_proc_elem <- data_proc_elem %>% filter(input_dataset %in% intersected_datasets)
  
  data_dicts <- dossier %>% lapply(data_dict_extract)
  harmonized_data_dicts <- harmonized_dossier %>% lapply(data_dict_extract)
  
  
  # # # split by study
  # dpe_lines <-
  #   data_proc_elem %>%
  #   group_split(pick(all_of(c("dataschema_variable","input_dataset")))) %>%
  #   set_names(paste0("Line : ",1:nrow(data_proc_elem)))
  # 
  # test <- 
  #   dpe_lines %>% lapply(nrow) %>% 
  #   lapply(function(x) x != 1) %>%
  #   unlist() %>% sum
  # 
  # if(test > 0) stop('dataschema_variable and input_dataset combination is not unique')
  # 
  # initialize
  beforafter_all = tibble(
    input_dataset = as.character(),
    dataschema_variable = as.character(),
    input_variables = as.character(),
    class_output = as.character(),
  )
  
  # for each dataset
  for(i in seq_len(nrow(data_proc_elem))){
    # stop()}
    
    # # initialize
    beforafter_i = tibble()
    # message(str_sub(paste0("\n",
    #                        "--before/after of : ",
    #                        crayon::bold(dataset_i)," -----------------------------------------------------"),1,81))
  
    # extract the involved dpe_dataset lines
    dpe_i <- data_proc_elem[i,] 
    
    # extract the dataset input
    dataset_i_from <- dossier[[dpe_i$input_dataset]]
    
    # extract the data_dict input
    data_dict_i_from <- data_dicts[[dpe_i$input_dataset]]
    
    # extract the dataset output
    dataset_i_to <- harmonized_dossier[[dpe_i$input_dataset]]
    
    # extract the data_dict output
    data_dict_i_to <- harmonized_data_dicts[[dpe_i$input_dataset]]
    
    # extract variables (input and output) 
    vars_from_i <-
      str_squish(unlist(strsplit(
        dpe_i$`input_variables`,split = ";"))) %>%
      extract_var %>% 
      set_names(paste0('input_value_',c(seq_len(length(.)))))
    
    var_to_i  <- 
      extract_var(dpe_i$`dataschema_variable`) %>%
      set_names('output_value')
      
    `Mlstr_harmo::rule_category` <- dpe_i$`Mlstr_harmo::rule_category`
    `Mlstr_harmo::algorithm` <- dpe_i$`Mlstr_harmo::algorithm`
    
    err = try({
        
      #   - generate subdataset (input and output)
      if(toString(vars_from_i) %in% '__BLANK__') {
        dataset_i_from  <- dataset_i_from %>% select(any_of(vars_from_i))
      }else{
        dataset_i_from  <- dataset_i_from %>% select(all_of(vars_from_i))
      }
      
      if(ncol(dataset_i_from) == 0){
        dataset_i_from <- 
          tibble(`input_value_1` = 
                   rep(extract_var(`Mlstr_harmo::algorithm`),
                       nrow(dataset_i_from)))}
        
      dataset_i_to <- 
        dataset_i_to %>% select(all_of(var_to_i)) %>%
        add_index() %>%
        arrange(pick(all_of("output_value"))) %>%
        mutate(across(everything(), as.character))
      
      dataset_i_from <- 
        dataset_i_from %>%
        slice(as_any_integer(dataset_i_to$index)) %>%
        mutate(across(everything(), as.character))
      
      dataset_i_to <- dataset_i_to %>% select(-index)
        
      #   - generate subdata dict (input and output)
      data_dict_i_from <- 
        data_dict_i_from %>%
        data_dict_filter(paste0('name %in% c("',paste0(c(vars_from_i),collapse = '","'),'")'))
      
      data_dict_i_to <- 
        data_dict_i_to %>%
        data_dict_filter(paste0('name %in% c("',paste0(c(var_to_i),collapse = '","'),'")'))
        
      #   - add class of each observation (either valid value, missing, 
      #     NA or categorical). Use preprocess function for that
        
      preprocess_from_i <-
        dataset_preprocess(
          distinct(dataset_i_from %>% setNames(vars_from_i)),
          data_dict_i_from) %>%
        mutate(cat_label = ifelse(!is.na(cat_label),paste0("[",cat_label,"]"),cat_label)) %>%
        unite("value_var_lab", c("value_var","cat_label"),sep = ' ',na.rm = TRUE,remove = FALSE) %>%
        mutate(valid_class = ifelse(valid_class == "1_Valid values"," {cat}",valid_class)) %>%
        mutate(valid_class = ifelse(valid_class == "2_Missing values"," {missing} {cat}",valid_class)) %>%
        mutate(valid_class = ifelse(valid_class == "3_Valid other values","",valid_class)) %>%
        mutate(valid_class = ifelse(valid_class == "4_NA values","NA {missing}",valid_class)) %>%
        unite("value_var_lab", c("value_var_lab","valid_class"),sep = '',na.rm = TRUE) %>%
        left_join(tibble(name = vars_from_i,name_input = names(vars_from_i)),by = c("name")) %>%
        select(name = "name_input","value_var","value_var_lab") %>%
        distinct %>% 
        group_split(pick(all_of('name'))) %>% as.list() %>%
        setNames(names(vars_from_i)) %>%
        lapply(function(x) x %>% 
                 rename_with(.cols = "value_var",.fn = ~ unique(x$name)) %>% 
                 rename_with(.cols = "value_var_lab",.fn = ~ paste0(unique(x$name),"_lab")) %>% 
                 select(-'name'))
      
      for(k in seq_along(preprocess_from_i)){
        # stop()}

        dataset_i_from <- 
          dataset_i_from %>% 
          left_join(preprocess_from_i[[k]],by = names(preprocess_from_i[k]))
        
      }
      
      preprocess_to_i <-
        dataset_preprocess(
          distinct(dataset_i_to %>% setNames(var_to_i)),
          data_dict_i_to) %>%
        mutate(cat_label = ifelse(!is.na(cat_label),paste0("[",cat_label,"]"),cat_label)) %>%
        unite("value_var_lab", c("value_var","cat_label"),sep = ' ',na.rm = TRUE,remove = FALSE) %>%
        mutate(valid_class = ifelse(valid_class == "1_Valid values"," {cat}",valid_class)) %>%
        mutate(valid_class = ifelse(valid_class == "2_Missing values"," {missing} {cat}",valid_class)) %>%
        mutate(valid_class = ifelse(valid_class == "3_Valid other values","",valid_class)) %>%
        mutate(valid_class = ifelse(valid_class == "4_NA values","NA {missing}",valid_class)) %>%
        unite("value_var_lab", c("value_var_lab","valid_class"),sep = '',na.rm = TRUE) %>%
        left_join(tibble(name = var_to_i,name_output = names(var_to_i)),by = c("name")) %>%
        select(name = "name_output","value_var","value_var_lab") %>%
        distinct %>% 
        rename(output_value = "value_var",
               output_value_lab = "value_var_lab") %>% 
        select(-"name")
        
      #   - combine them (output on the left, then input)      
      beforafter_i <- 
        dataset_i_to %>%
        left_join(preprocess_to_i,by = c('output_value')) %>%
        bind_cols(dataset_i_from) 
      
      # Unite columns dynamically based on suffixes
      for(k in names(vars_from_i)) {
        # stop()}
        beforafter_i <- 
          beforafter_i %>% 
          mutate(!! as_any_symbol(names(vars_from_i[k])) := !!as_any_symbol(paste0(names(vars_from_i[k]),"_lab"))) %>%
          select(-paste0(names(vars_from_i[k]),"_lab"))
      }

      beforafter_i <- 
        beforafter_i %>% mutate(output_value = output_value_lab) %>%
        select(-'output_value_lab')
            
      beforafter_i <- 
        beforafter_i %>% 
        mutate(
          input_dataset = dpe_i$input_dataset,
          dataschema_variable = var_to_i,
          `Mlstr_harmo::rule_category` = `Mlstr_harmo::rule_category`,
          `Mlstr_harmo::algorithm` = `Mlstr_harmo::algorithm`, 
          cut = '||',
          input_variables = 
            paste0(paste0(names(vars_from_i)," = ", vars_from_i),collapse = ' ; \n')
        ) %>%
        select(
          input_dataset,
          dataschema_variable,
          input_variables, 
          `Mlstr_harmo::rule_category`,
          `Mlstr_harmo::algorithm`,
          ` ` = cut,
          starts_with('output_value'),
          `  ` = cut,
          starts_with('input_value')) %>%
        distinct
        
        # summarise information for each case determined by the rule category
        if(`Mlstr_harmo::rule_category` == 'id_creation') {
          beforafter_i <- 
            beforafter_i %>% 
            mutate(
              output_value = paste0(var_to_i,'(s)'),
              input_value_1 = paste0(vars_from_i,'(s)')) %>%
            distinct
        }
        
        if(`Mlstr_harmo::rule_category` %in% c('paste','impossible','undetermined')) {
          beforafter_i <- 
            beforafter_i %>% 
            mutate(input_value_1 = '__BLANK__') %>%
            distinct
        }
      
      if(`Mlstr_harmo::rule_category` %in% c('recode','direct_mapping')) {
        
        beforafter_i <- 
          beforafter_i %>% 
          # select(` `:last_col()) %>%
          mutate(
            output_value = ifelse(str_detect(input_value_1,"\\{cat\\}|\\{missing\\}"),
                                  output_value,
                                  ifelse(output_value == input_value_1,'identical',output_value)
            ))
        
        if(summarize_output == TRUE){
         
          beforafter_i <- 
            beforafter_i %>% 
            group_by(pick(-c('input_value_1'))) %>% slice(c(1,n())) %>%
            distinct() %>%
            reframe(input_value_1 = paste0(input_value_1,collapse = " [...] ")) %>%
            mutate(input_value_1 = ifelse(str_detect(input_value_1,"\\{cat\\} \\[...\\] "),
                                          str_replace_all(input_value_1,"\\{cat\\} \\[...\\] ","{cat}{split}"),input_value_1)) %>%
            separate_longer_delim(input_value_1,delim = "{split}")
           
        }
      }
      
      beforafter_i <-
        beforafter_i %>%
        mutate(across(c("output_value",starts_with("input_value")), 
                      ~ str_remove_all(.," \\{cat\\}")))
        
      }, silent = TRUE)
      
      err <- ifelse((class(err)[1] == 'try-error'),'**failed**','')
      message(paste0(str_sub(paste0(
        str_trunc(paste0(
          "    ",i,"/",nrow(data_proc_elem)," : ",
          var_to_i," (",dpe_i$input_dataset,")"),width = 49,ellipsis = '[...]'),
        "                                       "),1,50)),bold(err))

      beforafter_all <- bind_rows(beforafter_all,distinct(beforafter_i))
  }
  
  # clean elements.
  #  - if one dataset, the column is deleted
  if(length(unique(beforafter_all$input_dataset)) == 1 ) 
    beforafter_all['input_dataset'] <- NULL 
  
  table_split <-
    beforafter_all %>% group_by(pick(all_of(split_by))) %>% ungroup() %>%
    group_split(pick(all_of(split_by))) %>%
    as.list() %>%
    lapply(function(x) remove_empty(x,'cols'))
  
  names(table_split) <- 
    if(toString(split_by) %in% "") "all" else sort(unique(pull(beforafter_all,all_of(split_by))))
  
  for(i in seq_along(table_split)){
    # stop()}
    
    # clean elements. if the input variable is unique, replace name of the 
    # column input_value by the actual name of the variable
    
    var_from_i <- 
      distinct(table_split[[i]]['input_variables']) %>%
      separate_longer_delim(input_variables, delim = " ; \n") %>%
      separate_wider_delim(input_variables, delim = " = ",names = c("input_variables","name")) %>%
      mutate(input_variables = ifelse(input_variables == "input_value_1","input_value",input_variables))

    names_from_i        <- c(var_from_i$input_variables)
    names(names_from_i) <- var_from_i$name
    
    if(toString(names(names_from_i)) == "__BLANK__") names(names_from_i) <- "input_value"

    #  - if one input value per input variable, rename the input_value_1 into input_value
    if(length(unique(names_from_i)) == 1){
      table_split[[i]] <-
        table_split[[i]] %>%
        rename_with(.cols = starts_with('input_value'),.fn = ~ "input_value") %>%
        mutate(input_variables = str_replace(input_variables,"input_value_1 =","input_value ="))
    } 
    
    #  - if one variables, the column is renamed/deleted
    if(length(unique(table_split[[i]]$input_variables)) == 1){
      table_split[[i]] <-
        table_split[[i]] %>%
        rename_with(.cols = starts_with('input_value'),.fn = ~ c(names(names_from_i))) %>%
        select(-input_variables)
    } 
    
    #  - if one output variable, the column is renamed/deleted
    if(length(unique(table_split[[i]]$dataschema_variable)) == 1){
      table_split[[i]] <- 
        table_split[[i]] %>%
        rename_with(.cols = starts_with('output_value'),.fn = ~ c(unique(table_split[[i]]$dataschema_variable))) %>%
        select(-dataschema_variable)
    } 
    
  }
  
  # if only one table for all processing, unlist it
  if(length(names(table_split)) == 1) table_split <- table_split[[1]]

  return(table_split)
}

# run tests on DEMO files

`dataset_MELBOURNE` <-
  data_dict_apply(madshapR_DEMO$dataset_MELBOURNE,madshapR_DEMO$data_dict_MELBOURNE) %>%
  valueType_adjust(from = madshapR_DEMO$data_dict_MELBOURNE, to = .)
`dataset_PARIS` <-
  data_dict_apply(madshapR_DEMO$dataset_PARIS,madshapR_DEMO$data_dict_PARIS) %>%
  valueType_adjust(from = madshapR_DEMO$data_dict_PARIS, to = .)
`dataset_TOKYO` <-
  data_dict_apply(madshapR_DEMO$dataset_TOKYO,madshapR_DEMO$data_dict_TOKYO) %>%
  valueType_adjust(from = madshapR_DEMO$data_dict_TOKYO, to = .)

# create the inputs
dossier        <- dossier_create(list(`dataset_MELBOURNE`,`dataset_PARIS`,`dataset_TOKYO`))
data_proc_elem <- Rmonize_DEMO$`data_processing_elements - final`
dataschema     <- Rmonize_DEMO$`dataschema - final`

# process harmonization
harmonized_dossier <- harmo_process(dossier,dataschema,data_proc_elem)

dossier                = dossier
harmonized_dossier     = harmonized_dossier
split_by               = "Mlstr_harmo::rule_category"
data_proc_elem         = data_proc_elem
dataset                = NULL
data_dict              = NULL
harmonized_dataset     = NULL
dataschema             = dataschema
col_dataset            = names(dataset)
col_harmonized_dataset = names(harmonized_dataset)
summarize_output       = FALSE


# avant apres harmo pour tous les dpe
test_avant_apres <- 
  avant_apres_harmo(dossier,harmonized_dossier,split_by)


# avant apres harmo pour tous les dpe choisis parmi
avant_apres_harmo(
  dossier,
  harmonized_dossier,
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



### ARCHIVES
avant_apres_harmo_old <- function(
    dossier = NULL, 
    harmonized_dossier = NULL, 
    split_by = NULL,
    data_proc_elem = attributes(harmonized_dossier)$`Rmonize::Data Processing Elements`,
    dataschema = attributes(harmonized_dossier)$`Rmonize::DataSchema`,
    dataset = NULL,
    data_dict = NULL,
    harmonized_dataset = NULL,
    harmonized_data_dict = NULL,
    col_dataset = names(dataset),
    col_harmonized_dataset = names(harmonized_dataset),
    summarize_output = FALSE
){
  
  # internal function to extract names of objects in dpe
  extract_var <- function(x){
    x <- x %>%
      str_replace_all('"',"`") %>%
      str_replace_all("'","`") %>%
      str_remove_all("`") 
    x = x[!is.na(x)]
    
    return(x)}
  
  if(!is.null(dossier) & !is.null(dataset)) 
    stop('no dossier AND dataset')
  if(!is.null(harmonized_dossier) & !is.null(harmonized_dataset)) 
    stop('no harmonized dossier AND harmonized dataset')
  
  if(!split_by %in%
     c("dataschema_variable","input_dataset","Mlstr_harmo::rule_category"))
    stop(call. = FALSE, "
Possible values for `split_by`:\n 
'dataschema_variable','input_dataset' or 'Mlstr_harmo::rule_category'")
  
  # case if user provides dataset and harmo dataset
  if(!is.null(dataset) & !is.null(harmonized_dataset)){
    
    dataset = as_dataset(dataset)
    harmonized_dataset = as_dataset(harmonized_dataset)
    
    if(is.null(data_proc_elem)){
      
      if(length(col_harmonized_dataset) != 1) 
        stop('col_harmonized_dataset must be unique')
      
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
      
      beforafter_all <- bind_cols(dataset_to,dataset_from)
      
      if(ncol(beforafter_all) == 0) return(tibble())
      
      beforafter_all <- 
        beforafter_all %>%
        distinct() %>%
        arrange(across(everything())) %>%
        mutate(
          `  ` = '||',
        ) %>%
        select(1,`  `,everything())
      
      return(beforafter_all) 
      
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
      split_by = split_by,
      dataschema = dataschema,
      split_by = split_by,
      summarize_output = summarize_output)
    )
  }
  
  # case when user provides dossiers 
  # extract global objects: dataschema and dpe
  # dataschema <- 
  #   attributes(harmonized_dossier)$`Rmonize::DataSchema`
  
  name_datasets <- 
    data_proc_elem %>% arrange(input_dataset) %>% 
    pull(input_dataset) %>% unique %>% extract_var()
  
  # reduce complexity
  dossier <- dossier[intersect(names(dossier), name_datasets)]
  
  # make sure the names of the datasets are properly written in the dpe
  data_proc_elem <- 
    data_proc_elem %>% 
    filter(extract_var(data_proc_elem$input_dataset) %in% name_datasets) %>%
    mutate(input_dataset = extract_var(input_dataset))
  
  # split by study
  dpe_dataset <- 
    data_proc_elem %>%
    # group_split(pick(all_of(split_by))) %>%
    group_split(input_dataset) %>%
    set_names(nm = sort(name_datasets)) %>%
    as.list()
  
  # initialize
  beforafter_all = tibble(
    input_dataset = as.character(),
    dataschema_variable = as.character(),
    input_variables = as.character(),
    class_output = as.character(),
  )
  
  # for each dataset
  for(dataset_i in names(dpe_dataset)){
    # stop()}
    
    message(str_sub(paste0("\n",
                           "--before/after of : ",
                           crayon::bold(dataset_i)," -----------------------------------------------------"),1,81))
    
    # extract the involved dpe_dataset lines
    dpe_dataset_i <- dpe_dataset[[dataset_i]]
    
    # extract the dataset input
    dataset_i_from <- dossier[[dataset_i]]
    
    # extract the data_dict input
    data_dict_i_from <- data_dict_extract(dataset_i_from)
    
    # extract the dataset output
    dataset_i_to <- harmonized_dossier[[dataset_i]]
    
    # # initialize
    beforafter_i = tibble()
    
    for(proc_j in seq_len(nrow(dpe_dataset_i))){
      # stop()}
      
      # for each line of the dpe :
      #   - read the rule, 
      
      var_to_j  <- 
        dpe_dataset_i[proc_j,] %>% 
        pull(.data$`dataschema_variable`) %>%
        extract_var %>%
        set_names('output_value')
      
      # dataset_from_j <- 
      #   datasets_from[[
      #     dpe_dataset_i[proc_j,] %>% 
      #       pull(.data$`input_dataset`) %>%
      #   extract_var]]
      
      # data_dict_from_j <- 
      #   data_dicts_from[[
      #     dpe_dataset_i[proc_j,] %>% 
      #       pull(.data$`input_dataset`) %>%
      #       extract_var]]
      
      # dataset_to_j <- 
      #   datasets_to[[
      #     dpe_dataset_i[proc_j,] %>% 
      #       pull(.data$`input_dataset`) %>%
      #       extract_var]]
      
      err = try({
        
        `Mlstr_harmo::rule_category` = 
          dpe_dataset_i[proc_j,] %>% 
          pull(.data$`Mlstr_harmo::rule_category`)
        
        `Mlstr_harmo::algorithm` = 
          dpe_dataset_i[proc_j,] %>% 
          pull(.data$`Mlstr_harmo::algorithm`)
        
        #   - extract variables (input and output) 
        vars_from_j <-     
          str_squish(unlist(strsplit(
            dpe_dataset_i[proc_j,] %>% 
              pull(.data$`input_variables`),split = ";"))) %>%
          extract_var %>% 
          set_names(paste0('input_value_',c(seq_len(length(.)))))
        
        #   - generate subdataset (input and output)
        if(toString(vars_from_j) %in% '__BLANK__') {
          dataset_ij_from  <- dataset_i_from %>% select(any_of(vars_from_j))
        }else{
          dataset_ij_from  <- dataset_i_from %>% select(all_of(vars_from_j))
        }
        
        if(ncol(dataset_ij_from) == 0){
          dataset_ij_from <- 
            tibble(
              `Rmonize::col` = 
                rep(extract_var(dpe_dataset_i[proc_j,] %>% 
                                  pull(.data$`Mlstr_harmo::algorithm`)),
                    nrow(dataset_ij_from)))}
        
        dataset_ij_to <- 
          dataset_i_to %>% 
          select(all_of(var_to_j))
        
        #   - generate subdata dict (input and output)
        data_dict_ij_from <- 
          data_dict_i_from %>%
          data_dict_filter(paste0('name %in% c("',paste0(c(vars_from_j),collapse = '","'),'")'))
        
        data_dict_ij_to <- 
          dataschema %>%
          data_dict_filter(paste0('name %in% c("',paste0(c(var_to_j),collapse = '","'),'")'))
        
        #   - add class of each observation (either valid value, missing, 
        #     NA or categorical). Use preprocess function for that
        
        preprocess_from_j <-
          madshapR::dataset_preprocess(
            distinct(dataset_ij_from %>% setNames(vars_from_j)),
            data_dict_ij_from) %>%
          select(-index_in_dataset,-index_value) %>%
          distinct
        
        preprocess_to_j <-
          madshapR::dataset_preprocess(
            distinct(dataset_ij_to %>% setNames(var_to_j)),
            data_dict_ij_to) %>%
          select(-index_in_dataset,-index_value) %>%
          distinct
        
        #   - combine them (output on the left, then input)      
        beforafter_j <- 
          dataset_ij_to %>%
          mutate(across(everything(), as.character)) %>%
          # left_join(preprocess_to_j,by = c('output_value' ='value_var')) %>% 
          # select(output_value, class_output = valid_class) %>%
          bind_cols(dataset_ij_from) %>%
          mutate(across(everything(), as.character)) %>%
          set_names(c(names(var_to_j),names(vars_from_j))) %>%
          mutate(output_value = replace_na(output_value,'NA')) %>%
          mutate(across(starts_with('input_value_'), ~ replace_na(.,'NA'))) %>%
          mutate(
            input_dataset = dataset_i,
            dataschema_variable = var_to_j,
            `Mlstr_harmo::rule_category` = `Mlstr_harmo::rule_category`,
            `Mlstr_harmo::algorithm` = `Mlstr_harmo::algorithm`, 
            cut = '||',
            input_variables = 
              paste0(paste0(names(vars_from_j)," = ", vars_from_j),collapse = ' ; ')
          ) %>%
          select(
            input_dataset,
            dataschema_variable,
            input_variables, 
            `Mlstr_harmo::rule_category`,
            `Mlstr_harmo::algorithm`,
            ` ` = cut,
            output_value,
            `  ` = cut,
            starts_with('input_value_')) %>%
          distinct
        
        # summarise information for each case determined by the rule category
        if(`Mlstr_harmo::rule_category` == 'id_creation') {
          beforafter_j <- 
            beforafter_j %>% 
            mutate(
              output_value = paste0(var_to_j,'(s)'),
              input_value_1 = paste0(vars_from_j,'(s)')) %>%
            distinct
        }
        
        if(`Mlstr_harmo::rule_category` == 'paste') {
          beforafter_j <- 
            beforafter_j %>% 
            mutate(input_value_1 = '__BLANK__') 
        }
        
        if(`Mlstr_harmo::rule_category` == 'impossible') {
          beforafter_j = beforafter_j %>% 
            mutate(input_value_1 = '__BLANK__')
        }
        
        if(`Mlstr_harmo::rule_category` == 'undetermined') {
          beforafter_j = beforafter_j %>% 
            mutate(input_value_1 = '__BLANK__')
        }
        
        if(`Mlstr_harmo::rule_category` == 'recode') {
          
          beforafter_j <- 
            beforafter_j %>%
            distinct() 
        }
        
        if(`Mlstr_harmo::rule_category` == 'case_when') {
          
          beforafter_j <- beforafter_j %>%
            distinct() 
        }
        
        
        if(`Mlstr_harmo::rule_category` == 'operation') {
          
          beforafter_j <- 
            beforafter_j %>%
            arrange(across(c('output_value',starts_with('input_value_')))) %>% 
            distinct() %>%
            select(
              'input_dataset','dataschema_variable','class_output',
              'input_variables','Mlstr_harmo::rule_category'," ",'Mlstr_harmo::algorithm','output_value',"  ",
              starts_with('input_value_'))
        }
        
        if(`Mlstr_harmo::rule_category` == 'other') {
          
          beforafter_j <- beforafter_j %>%
            arrange(across(c('output_value',starts_with('input_value_')))) %>% 
            distinct() %>%
            select(
              'input_dataset','dataschema_variable','class_output',
              'input_variables','Mlstr_harmo::rule_category'," ",'Mlstr_harmo::algorithm','output_value',"  ",
              starts_with('input_value_'))
        }
        
        
        if(`Mlstr_harmo::rule_category` == 'direct_mapping') {
          
          beforafter_j <- 
            beforafter_j %>%
            distinct() %>%
            arrange(across(c('class_output','output_value'))) 
        }
        
      }, silent = TRUE)
      
      err <- ifelse((class(err)[1] == 'try-error'),'**failed**','')
      message(paste0(str_sub(paste0(
        str_trunc(paste0(
          "    ",proc_j,"/",nrow(dpe_dataset_i)," : ",
          var_to_j),width = 49,ellipsis = '[...]'),
        "                                       "),1,50)),bold(err))
      
      beforafter_i <- bind_rows(beforafter_i,distinct(beforafter_j))
    }
    
    beforafter_i <- 
      beforafter_i %>%
      select(
        'dataschema_variable','class_output',
        'input_variables','output_value','input_value_1') %>%
      left_join(preprocess_from_j %>% 
                  select(input_cat_label = 'cat_label',
                         input_value_1 = 'value_var'), 
                by = c('input_value_1')) %>%
      left_join(preprocess_to_j %>% 
                  select(output_cat_label = 'cat_label',
                         output_value = 'value_var'), 
                by = c('output_value')) %>%
      mutate(
        input_value_1 = ifelse(!is.na(input_cat_label) ,
                               paste0(input_value_1,' [',input_cat_label,']'),input_value_1),
        output_value  = ifelse(!is.na(output_cat_label),
                               paste0(output_value,' [',output_cat_label,']'),output_value)) %>% 
      select(-c("input_cat_label","output_cat_label"))
    
    
    
    if(summarize_output == TRUE){
      
      
      
      
      beforafter_j <- 
        beforafter_j %>%
        mutate(output_value = ifelse(
          output_value == input_value_1 & 
            class_output == "3_Valid other values",
          "[identical]",output_value)) %>%
        group_by(pick(-c('input_value_1'))) %>%
        reframe(across(c("input_value_1"), ~ paste0(.,collapse = " ; "))) %>%
        mutate(input_value_1 = str_squish(str_trunc(
          .data$`input_value_1`, width = 50, ellipsis = ' [...] ',side = "center")))
    }
    # bind elements
    beforafter_all <- bind_rows(beforafter_all,distinct(beforafter_i))
  }
  
  table_split <-
    beforafter_all %>% 
    group_split(pick(all_of(split_by))) %>%
    set_names(nm = sort(unique(pull(beforafter_all,all_of(split_by))))) %>%
    as.list()
  
  
  for(i in 1:length(table_split)){
    # stop()}
    
    # clean elements. if the input variable is unique, replace name of the 
    # column input_value by the actual name of the variable
    
    var_from_i <- unique(table_split[[i]]$input_variables)
    
    table_split[[i]] =
      table_split[[i]] %>%
      mutate(
        input_variables = ifelse(!'input_value_2' %in% names(table_split[[i]]), var_from_i,input_variables),
        class_output = str_sub(class_output, 3,-1)) %>%
      distinct
    
    #  - if one output variable, the column is renamed/deleted
    if(length(unique(table_split[[i]]$dataschema_variable)) == 1){
      table_split[[i]] <- 
        table_split[[i]] %>%
        mutate(output_value = na_if(output_value,'NA')) %>%
        mutate(across(starts_with('input_value_'), ~na_if(.,'NA'))) %>%
        rename_with(.cols = starts_with('output_value'),.fn = ~ c(unique(table_split[[i]]$dataschema_variable))) %>%
        select(-dataschema_variable)
    } 
    
  }
  # clean elements.
  #  - if one dataset, the column is deleted
  if(length(unique(beforafter_all$input_dataset)) == 1 ) 
    beforafter_all['input_dataset'] <- NULL 
  
  
  #  - if one input variable combo, the column is renamed/deleted
  if(length(unique(beforafter_all$input_variables)) == 1)
    beforafter_all <- beforafter_all %>%
    rename_with(.cols = starts_with('input_value_'),.fn = ~ c(vars_from_j)) %>%
    select(-input_variables) 
  
  #  - if one class of variable, the column is deleted
  if(length(unique(beforafter_all$class_output)) == 1)
    beforafter_all <- beforafter_all %>%
    select(-class_output)
  
  # beforafter_all <- distinct(beforafter_all) %>% arrange(pick(1))
  
  return(beforafter_all)
}





