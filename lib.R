# libraries  -----------------------------------------------------------------

library(readr)
library(generic)
library(plotly)
library(dplyr)
library(zoo)

# functions  ---------------------------------------------------------------

# a function that accepts 2 arguments (a string and a vector of strings)
# and returns a formula ( y ~ x1 + x2 ) 
build_formula = function(dv, ivs) {
  f = formula(paste0("`",
                     dv,
                     "` ~ `",
                     paste0(ivs,
                            collapse = "` + `"),
                     "`"))
}

# a function that accpets 1 argument (a vector of strings)
# and returns a table (model_table)
build_model_table = function(ivs){
  
  # number of variables
  n = length(ivs)
  
  # build model table
  model_table = tibble(
    variables = ivs,
    decay = rep(0,n),
    dim_rets = rep(0,n),
    lag = rep(0,n),
    ma = rep(0,n)
  )
  
  return(model_table)
  
}

apply_normalisation = function(raw_data = NULL,
                               meta_data = NULL,
                               model_table = NULL,
                               dv = NULL,
                               verbose=T){
  
  # if no raw_data is provided, end by returning NULL
  if (is.null(raw_data)) {
    if(verbose){
      cat("\n Error: No raw_data provided. Returning NULL.")
    }
    return(NULL)
  }
  # if raw_data is not a dataframe
  if(!is.data.frame(raw_data)){
    if(verbose){
      cat("\n Error: raw_data must be a data.frame. Returning NULL.")
    }
    return(NULL)
  }
  
  
  # if no meta_data is provided, end by returning raw_data
  if (is.null(meta_data)) {
    if(verbose){
      cat("\n Error: No meta_data provided. Returning original raw_data.")
    }
    return(raw_data)
  }
  # if meta_data is not a dataframe
  if(!is.data.frame(meta_data)){
    if(verbose){
      cat("\n Error: meta_data must be a data.frame. Returning original raw_data.")
    }
    return(raw_data)
  }
  
  
  # if the meta_data table doesnt contain a variables column, end by returning raw_data
  if(!("variables" %in% colnames(meta_data))){
    if(verbose){
      cat("\n Error: meta_data does not contain a 'variables' column. Returning original raw_data.")
    }
    return(raw_data)
  }
  # get all variables in meta_data
  variables = meta_data$variables
  
  # get data column names
  data_variables = colnames(raw_data)
  
  # get the pool variable
  pool_variable = TRY({
    variables[toupper(meta_data$meta) == "POOL"]
  })
  
  # check if a pool variable is provided
  ## RETURNING RAW_DATA IF NO POOL_VARIABLE provided??
  if(is.null(pool_variable) | length(pool_variable) == 0) {
    if(verbose){
      cat("\n Warning: no POOL variable found in meta_data. A 'total_pool' variable was added.")
    }
    
    # if not provided create a unique one
    pool_variable = "total_pool"
    
    # add the pool variable to data
    raw_data = cbind(raw_data, total_pool = pool_variable)
    
    # get new data column names
    data_variables = colnames(raw_data)
  }
  
  # check if more than 1 pool variable is provided
  if (length(pool_variable) > 1) {
    if(verbose){
      cat("\n Error: More than 1 pool variable provided")
      }
    return(raw_data)
  }
  
  # check if pool variable in data variables
  if (!(pool_variable %in% data_variables)) {
    if(verbose){
      cat("\n Error: POOL variable not found in raw_data. Returning original raw_data.")
      }
    return(raw_data)
  }
  
  # check if ivs and dv are provided to select only relevant variables
  if(is.null(model_table)){
    if(verbose){
      cat("\n Warning: Normalising all raw_data as no model_table is provided.")
    }
  }else{
    if(!is.data.frame(model_table)){
      if(verbose){
        cat("\n Warning: model_table must be a data.frame. Normalising all raw_data.")
      }
    }else{
      if(!is.null(dv)){
        
        ivs = model_table$variables
        
        # check if ivs dv are in data
        if(all(c(ivs) %in% colnames(raw_data))){
          if(dv %in% colnames(raw_data)){
            
            # keep only relevant variables
            raw_data = raw_data[,c(pool_variable,dv,ivs)]
            
          }else{
            if(verbose){
              cat("\n Warning: dependent variable (dv) not found in raw_data. Normalising all raw_data.")
            }
          }
        }else{
          if(verbose){
            cat("\n Warning: variables from model_table not found in raw_data. Normalising all raw_data.")
          }
        } 
      }else{
        if(verbose){
          cat("\n Warning: dependent variable (dv) not provided. Normalising all raw_data.")
        }
      } 
    }
  }

  # in raw data, check for and drop NAs
  if(any(complete.cases(raw_data))) {
    if(verbose){
      cat("\n Warning: NA's found in raw_data will be dropped.")
      }
    raw_data = raw_data[complete.cases(raw_data), ]
  }
  
  # remove the pool variable from the others
  variables = variables[variables != pool_variable]
  
  # for each variable
  for (var in variables) {
    # get that variable's transformation
    transformation = meta_data$meta[meta_data$variables == var] %>% toupper()
    
    # if STA apply pooled sta
    if (transformation == "STA") {
      raw_data = raw_data %>%
        group_by(!!sym(pool_variable)) %>%
        mutate(var = !!sym(var) / mean(!!sym(var), na.rm = T)) %>%
        mutate(var = if_else(is.na(var), 0, var))
      
      # replace the raw variable with the STAed variable
      raw_data[, var] = raw_data[, "var"]
      raw_data[, "var"] = NULL
      
    }
    
  }
  
  return(raw_data)
  
}

apply_transformation = function(raw_data,
                                model_table = NULL,
                                meta_data = NULL,
                                verbose=T) {
  
  # check model table provided (not NULL)
  if(is.null(model_table)){
    if(verbose)print("Info: No model table provided for transformations. Returning raw data.")
    return(raw_data)
  }
  
  # get variables from model table
  variables = model_table$variables
  transformations = c("decay", "dim_rets", "lag", "ma")
  
  # if a meta table is provided try to extract the POOL variable
  if (!is.null(meta_data)) {
    pool = TRY({
      meta_data %>%
        filter(meta == "POOL") %>%
        pull(variables)
    })
    
    # if there is a pool variable in the meta data...
    # ...but has zero
    if (!is.null(pool)) {
      if (length(pool) == 0) {
        if(verbose)print("Warning: no pool variable found in meta_data")
        pool = "total_pool"
        
        # if default pool variable already in use replace it
        if(pool %in% colnames(raw_data)){
          if(verbose)print("Warning: 'total_pool' variable will be added/replaced from data")   
          
          raw_data$total_pool = NULL
          
        }
        
        raw_data = tibble(raw_data, total_pool = pool)
      }
    } else{
      # else create a pool variable "total"...
      if(verbose)print("Warning: no pool variable found in meta_data")
      
      # if default pool variable already in use replace it
      if(pool %in% colnames(raw_data)){
        if(verbose)print("Warning: 'total_pool' variable will be added/replaced from data")   
        
        raw_data$total_pool = NULL
        
      }
      
      pool = "total_pool"
      # ...and add it to the data
      raw_data = tibble(raw_data, total_pool = pool)
      
    }
  } else{
    # else create a pool variable "total"...
    if(verbose)print("Info: no meta_data provided")
    pool = "total_pool"
    # ...and add it to the data
    
    # if default pool variable already in use replace it
    if(pool %in% colnames(raw_data)){
      if(verbose)print("Warning: 'total_pool' variable will be added/replaced from data")   
      
      raw_data$total_pool = NULL
      
    }
    
    raw_data = tibble(raw_data, total_pool = pool)
    
  }
  
  for (var in variables) {
    var_val = raw_data %>%
      select(!!sym(pool), !!sym(var))
    
    dim_rets = model_table$dim_rets[model_table$variables == var]
    decay = model_table$decay[model_table$variables == var]
    lag = model_table$lag[model_table$variables == var]
    ma = model_table$ma[model_table$variables == var]
    
    var_val = var_val %>%
      group_by(!!sym(pool)) %>%
      mutate(new_var = diminish(v = !!sym(var), dim_rets))
    
    var_val = var_val %>%
      group_by(!!sym(pool)) %>%
      mutate(new_var = decay(v = new_var, decay))
    
    var_val = var_val %>%
      group_by(!!sym(pool)) %>%
      mutate(new_var = generic::lag(v = new_var, lag))
    
    var_val = var_val %>%
      group_by(!!sym(pool)) %>%
      mutate(new_var = ma(v = new_var, ma))
    
    raw_data[, var] = var_val$new_var
    
  }
  
  return(raw_data)
  
}


run_model = function(data, dv, ivs = NULL, meta_data = NULL, model_table = NULL,verbose=F) {
  
  # if model table is provided
  if(!is.null(model_table)){
    if(!is.null(ivs)){
      if(verbose)print("Info: will use variables from model_table and disregard ivs argument.")
    }
    # use model table variables as ivs
    ivs = model_table$variables
  }
  
  # build formula object
  formula = build_formula(dv = dv, ivs = ivs)
  
  # generate norm_data
  norm_data = apply_normalisation(raw_data = data,
                                  meta_data = meta_data,
                                  verbose = verbose)
  
  # generate trans_data
  trans_data = apply_transformation(raw_data = norm_data,
                                    model_table = model_table,
                                    verbose = verbose)
  
  # run model on norm_data
  model = lm(formula = formula, data = trans_data)
  
  # add meta_data to mdoel object
  model$meta_data = meta_data
  
  # return model object
  return(model)
}

decomping = function(model,
                     de_normalise = T,
                     raw_data = NULL,
                     categories = NULL,
                     id_var = NULL,
                     verbose=T) {
  # get the coefficients from the model object
  coef = model$coefficients
  
  # get the modeled data from the model object
  data = model$model
  
  # extract dependent variable name
  dv = colnames(data)[1]
  
  # get the dependent variable from the data object
  actual = data[, dv]
  
  # initiate variable to confirm if correct raw data is provided
  raw_actual_supplied = F
  
  
  # in raw data is supplied check for and drop NAs
  if(!is.null(raw_data)){
    if(any(complete.cases(raw_data))) {
      if(verbose)print("Warning: NA's found in raw data will be dropped.")
      raw_data = raw_data[complete.cases(raw_data), ]
    }
  }
  
  # get raw dependent variable if supplied
  if (de_normalise) {
    # if no raw data provided
    if (is.null(raw_data)) {
      if(verbose)print("Warning: you must provide a raw_data to de normalise the data")
    }
    
    # if the raw data is supplied
    else{
      # try to get the dependent variable from the raw data
      raw_actual = TRY({
        raw_data %>%
          select(!!sym(dv))
      })
      
      # if the dependent variable is found in the raw data provided
      if (!is.null(raw_actual)) {
        # switch raw_actual_supplied variable to TRUE
        raw_actual_supplied = T
        
      } else{
        # else if(verbose)print warning
        if(verbose)print("Warning: dependent variable not found in raw data supplied")
        
      }
    }
  }
  
  
  # get the intercept value from the coef object
  intercept = coef[1]
  # keep the other coefficients
  coef = coef[2:length(coef)]
  
  # generate an id variable if one is not provided
  if (is.null(id_var)) {
    if(verbose)print(paste0(
      "Info: no id variable supplied. New id variable generated as 1 to ",
      nrow(data)
    ))
    id_var = "id"
    id_var_values = 1:nrow(data)
  } else{
    # if and id_var is provided, check that raw data is provided
    if (is.null(raw_data)) {
      # if raw data not provided if(verbose)print warning and generate id_var_values
      if(verbose)print(
        paste0(
          "Warning: ID variable provided, but no raw data provided. New id variable generated as 1 to ",
          nrow(data)
        )
      )
      id_var_values = 1:nrow(data)
    }
    else{
      # if raw data is provided, check that raw data contains the id variable
      if (id_var %in% colnames(raw_data)) {
        id_var_values = raw_data[, id_var] %>% pull()
      } else{
        # if raw data doesnt contain the id_var, if(verbose)print warning and generate id_variable
        if(verbose)print(
          paste0(
            "Warning: ID variable provided not found in raw data provided. New id variable generated as 1 to ",
            nrow(data)
          )
        )
        id_var_values = 1:nrow(data)
      }
    }
  }
  
  
  
  # try to get the normalisation table
  meta_data = TRY({
    model$meta_data
  })
  
  # if no meta_data is provided
  if (is.null(meta_data)) {
    if(verbose)print(
      "Info: no normalisation table (meta_data) found in model object. A pool variable ('total') will be generated."
    )
    
    pool = tibble("total_pool")
  } else{
    # if a norm table is provided extract the pool variable
    pool_variable = meta_data$variables[toupper(meta_data$meta) == "POOL"]
    
    # if no raw data provided if(verbose)print a warning and generate a pool variable
    if (is.null(raw_data)) {
      if(verbose)print(
        "Warning: no raw_data found to extract pool variable found in model's meta_data. A pool variable ('total') will be generated."
      )
      
      pool = tibble("total_pool")
    } else if (length(pool_variable) > 0) {
      # if raw data is provided check if it contains the pool variable
      if (pool_variable %in% colnames(raw_data)) {
        pool = raw_data[, pool_variable]
      } else{
        # if not, if(verbose)print warning and geterate pool variable
        if(verbose)print(
          "Warning: pool variable from model's meta_data not found in raw_data. A pool variable ('total_pool') will be generated."
        )
        pool = tibble("total_pool")
      }
    }else{
      # if not, if(verbose)print warning and geterate pool variable
      if(verbose)print(
        "Warning: pool variable from model's meta_data not found in raw_data. A pool variable ('total_pool') will be generated."
      )
      pool = tibble("total_pool")
    }
  }
  
  # generate the fitted values dataframe
  fitted_values = tibble(
    actual = c(actual),
    residual = model$residuals,
    predicted = model$fitted.values,
    id = id_var_values,
    pool = pool %>% pull()
  ) %>%
    reshape2::melt(id.vars = c("id", "pool"))
  
  # get the independent variables decomp
  independendent_variables =  data[, 2:ncol(data)]
  if (length(coef) == 1) {
    # multiply independent variable by coefficient
    variable_decomp = data.frame(independendent_variables * coef)
    colnames(variable_decomp) = names(coef)
  } else{
    # multiply independent variables data frame by coefficient vector
    variable_decomp = data.frame(mapply(
      FUN = `*`,
      independendent_variables,
      coef,
      SIMPLIFY = FALSE
    ))
  }
  
  # rename variable decomp using coef names
  colnames(variable_decomp) = names(coef)
  
  # generate tibble df using the variable decomp, intercept and id variable
  variable_decomp = tibble(
    "(Intercept)" = intercept,
    variable_decomp,
    id = id_var_values,
    pool = pool %>% pull()
  ) %>%
    reshape2::melt(id.vars = c("id", "pool")) %>%
    rename(contrib = value)
  
  # if an id variable name is provided use it
  if (id_var != "id") {
    # fitted values
    col_names = colnames(fitted_values)
    col_names[col_names == "id"] = id_var
    colnames(fitted_values) = col_names
    
    # decomp
    col_names = colnames(variable_decomp)
    col_names[col_names == "id"] = id_var
    colnames(variable_decomp) = col_names
    
  }
  
  # if a raw actual is provided and de-normalise is TRUE, check if dv is STAed
  if (raw_actual_supplied & de_normalise) {
    # check if meta_data is provided
    if (is.null(meta_data)) {
      if(verbose)print("Warning: meta_data not found in model, but required to de-normalised.")
    } else{
      # else check if the dv is not in meta_data
      if (!(dv %in% meta_data$variables)) {
        # if the dv is not found in the norm table if(verbose)print warning
        ### STA could work if the de_normalise is true
        print("Warning: dv not found in meta_data.")
        
      } else{
        pool_mean = tibble(raw_actual = raw_actual %>% pull(),
                           pool = pool %>% pull()) %>%
          group_by(pool) %>%
          summarise(pool_mean = mean(raw_actual))
        
        
        variable_decomp = variable_decomp %>%
          left_join(pool_mean, by = "pool") %>%
          mutate(contrib = contrib * pool_mean) %>%
          select(-pool_mean)
        
        
        fitted_values = fitted_values %>%
          left_join(pool_mean, by = "pool") %>%
          mutate(value = value * pool_mean) %>%
          select(-pool_mean)
      }
    }
  }
  
  
  if (is.null(categories)) {
    category_decomp = variable_decomp
  } else{
    # generate category decomp using categories df input
    category_decomp = variable_decomp %>%
      # add a categories and calc column to the variables decomp table
      left_join(categories, by = "variable") %>%
      # if no category is found for a variable assign it "Other" as a category
      mutate(category  = if_else(is.na(category),
                                 "Other",
                                 category)) %>%
      # assign the "Base" category to the intercept variable
      mutate(category  = if_else(variable == "(Intercept)",
                                 "Base",
                                 category)) %>%
      # if no calc is found for a variable/category assign it "none" as a calc
      mutate(calc  = if_else(is.na(calc),
                             "none",
                             calc)) %>%
      # group and sum the table by id and category
      group_by(!!sym(id_var_name), category) %>%
      summarise(contrib = sum(contrib)) %>%
      rename(variable = category)
    
    # extract minned variables
    minned_vars = categories %>%
      filter(calc == "min") %>%
      pull(category) %>%
      unique()
    
    # extract maxed variables
    maxed_vars = categories %>%
      filter(calc == "max") %>%
      pull(category) %>%
      unique()
    
    # extract the initial (pre calc) base value
    based_value = category_decomp[category_decomp$variable == "Base", "contrib"]
    
    # for each minned variable
    for (cat in minned_vars) {
      # get the category values
      cat_val = category_decomp %>%
        filter(variable == cat) %>%
        pull(contrib)
      
      # get the minimum of each category
      min_val = cat_val %>%
        min()
      
      # replace the category values with the minned variable
      category_decomp[category_decomp$variable == cat, "contrib"] = cat_val - min_val
      
      # replace the base value with the base plus min value
      based_value = based_value + min_val
    }
    # for each maxed variable
    for (cat in maxed_vars) {
      # get the category values
      cat_val = category_decomp %>%
        filter(variable == cat) %>%
        pull(contrib)
      
      # get maximum of each category
      max_val = cat_val %>%
        max()
      
      # replace the category values with the mixed variable
      category_decomp[category_decomp$variable == cat, "contrib"] = cat_val - max_val
      
      # replace the base value with the base plus max value
      based_value = based_value + max_val
    }
    
    # replace the base value with the minned and maxed base value
    category_decomp[category_decomp$variable == "Base", "contrib"] = based_value
    
  }
  
  # return a list of category, variable tables, and fitted values
  l = list(
    category_decomp = category_decomp,
    variable_decomp = variable_decomp,
    fitted_values = fitted_values
  )
  
  return(l)
}

decomp_chart = function(decomp_list,
                        pool = NULL,
                        variable_decomp = F) {
  # get decomp
  if (variable_decomp) {
    # get variable decomp table
    decomp = decomp_list$variable_decomp
  } else{
    # get category decomp table
    decomp = decomp_list$category_decomp
  }
  
  # get actual dependent variable table
  fitted_values = decomp_list$fitted_values
  fitted_values = fitted_values[fitted_values$variable %in% c("actual", "residual"), ]
  
  # filter by pool if provided
  if (!is.null(pool)) {
    decomp = decomp[decomp$pool == pool, ]
    fitted_values = fitted_values[fitted_values$pool == pool, ]
  }
  
  # the id variable name is the first column name
  id_var = colnames(decomp)[1]
  
  # plot
  plot_ly() %>%
    add_bars(
      data = decomp,
      x = ~ get(id_var),
      y = ~ contrib,
      color = ~ variable
    ) %>%
    add_lines(
      data = fitted_values,
      x = ~ get(id_var),
      y = ~ value,
      color = ~ variable
    ) %>%
    layout(barmode = "relative")
  
}

fit_chart = function(decomp_list,
                     pool = NULL) {
  # get actual dependent variable table
  fitted_values = decomp_list$fitted_values
  
  # filter by pool if provided
  if (!is.null(pool)) {
    fitted_values = fitted_values[fitted_values$pool == pool, ]
  }
  
  # the id variable name is the first column name
  id_var = colnames(fitted_values)[1]
  
  # plot
  plot_ly() %>%
    add_lines(
      data = fitted_values,
      x = ~ get(id_var),
      y = ~ value,
      color = ~ variable
    ) %>%
    layout(barmode = "relative")
  
}


what_next = function(raw_data, ivs, dv, test_ivs, meta_data = NULL){
  
  
  # define output table to fill with loop
  output = tibble(
    variable = "0",
    adj_R2 = 0,
    t_stat = 0,
    coef = 0
  )
  
  for(i in 1:length(test_ivs)){
    
    # get test variable
    var = test_ivs[i]
    
    # check if in data
    if(!(var %in% colnames(raw_data))){
      print(
        paste0(
          "Warning: variable ",
          var,
          " not found in data supplied."
        )
      )
      
      # fill row with empty
      output[i,] = list(var,NA,NA,NA)
      
    }else{
      # run model
      model = TRY({run_model(data = raw_data,dv = dv,ivs = c(ivs,var),meta_data = meta_data)})
      
      
      # if model failed
      if(is.null(model)){
        
        # fill row with empty
        output[i,] = list(var,NA,NA,NA)
        
      }else{
        
        # get model summary
        ms = summary(model)
        
        # generate row
        adj_R2 = ms$adj.r.squared
        coef = ms$coefficients[i,"Estimate"]
        t_value = ms$coefficients[i,"t value"]
        
        output[i,] = list(var,adj_R2,t_value,coef)
        
      }
    }
  }
  
  return(output)
  
}

what_trans = function(raw_data,
                      dv,
                      test_table,
                      ivs = NULL,
                      model_table = NULL,
                      meta_data = NULL) {
  
  test_table_t = test_table %>%
    reshape2::dcast(variables ~ transformation, fill = 0)
  
  # if duplicate_var=T keep any trans variable in current model spec
  # and add the transformed one
  ## FOR NOW - replace
  
  # define output table to fill with loop
  output = tibble(
    variable = "0",
    transformation = "",
    value = 0,
    adj_R2 = 0,
    t_stat = 0,
    coef = 0
  )
  
  if (is.null(model_table)) {
    # print("Warning: no model_table provided.")
    if (!all(ivs %in% colnames(raw_data))) {
      print(paste0(
        "Warning: raw_data does not contain variables ",
        paste0(ivs = ivs[!(ivs %in% colnames(raw_data))],
               collapse = ";")
      ))
      
      ivs = ivs[ivs %in% colnames(raw_data)]
    }
    
    model_table = build_model_table(ivs)
  } else{
    ivs = model_table$variables
    
    if (!all(ivs %in% colnames(raw_data))) {
      print(
        "Warning: these variables from the model_table were not found in the raw_data and will be disregarded:"
      )
      print(ivs[!(ivs %in% colnames(raw_data))])
      
      model_table = model_table[ivs %in% colnames(raw_data), ]
      ivs = model_table$variables
    }
  }
  
  for (i in 1:nrow(test_table)) {
    # get test variable
    # row = test_table_t[i, ]
    var = test_table$variables[i]
    trans = test_table$transformation[i]
    val = test_table$value[i]
    
    row = test_table[i,]%>%
      reshape2::dcast(variables ~ transformation, fill = 0)
    
    temp_model_table = model_table %>%
      bind_rows(row) %>% 
      mutate_all(~replace(., is.na(.), 0))
    
    # run model
    model = TRY({
      run_model(
        data = raw_data,
        dv = dv,
        meta_data = meta_data,
        model_table = temp_model_table
      )
    })
    
    
    # if model failed
    if (is.null(model)) {
      # fill row with empty
      output[i, ] = list(var, "0",0,0,0,0)
      
    } else{
      # get model summary
      ms = summary(model)
      
      # generate row
      adj_R2 = ms$adj.r.squared
      coef = ms$coefficients[var, "Estimate"]
      t_value = ms$coefficients[var, "t value"]
      
      output[i, ] = list(var %>% as.character(),
                         trans %>% as.character(),
                         val %>% as.numeric(),
                         adj_R2 %>% as.numeric(), 
                         t_value %>% as.numeric(), 
                         coef %>% as.numeric())
      
    }
  }
  
  return(output)
  
}

read_xcsv = function(file) {
  # if the file eds with .csv read as csv
  if (endsWith(x = tolower(file) , suffix = ".csv")) {
    data = readr::read_csv(file = file)
  }
  else if (endsWith(x = tolower(file) , suffix = ".xls") |
           endsWith(x = tolower(file) , suffix = ".xlsx") |
           endsWith(x = tolower(file) , suffix = ".xlsm")) {
    data = readr::read_excel(file = file)
  }
  else{
    print("Error: file path must refer to csv or Excel (.xls, ,xlsm, .xlsx) file")
    return(NULL)
  }
  return(data)
}
