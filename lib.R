# libraries  -----------------------------------------------------------------

library(dplyr)
library(readr)
library(cpLIB)
library(plotly)
library(zoo)

# functions  ---------------------------------------------------------------

build_formula = function(dv, ivs) {
  f = formula(paste0("`",
                     dv,
                     "` ~ `",
                     paste0(ivs,
                            collapse = "` + `"),
                     "`"))
}

apply_normalisation = function(data, norm_table = NULL) {
  # if no norm table is provided, end by returning data
  if (is.null(norm_table)) {
    return(data)
  }
  
  # get all variables in norm_table
  variables = norm_table$variables
  
  # get data column names
  data_variables = colnames(data)
  
  # get the pool variable
  pool_variable = TRY({
    variables[toupper(norm_table$transformation) == "POOL"]
  })
  
  # check if a pool variable is provided
  if (is.null(pool_variable)) {
    # if not provided create a unique one
    pool_variable = "total"
    
    # add the pool variable to data
    data = cbind(data, total = pool_variable)
    
  }
  
  
  # check if more than 1 pool variable is provided
  if (length(pool_variable) > 1) {
    print("Error: More than 1 pool variable provided")
    return(data)
  }
  
  #check if pool variable in data variables
  if (!(pool_variable %in% data_variables)) {
    print("Error: pool variable not found in data")
    return(data)
  }
  
  # remove the pool variable from the others
  variables = variables[variables != pool_variable]
  
  # for each variable
  for (var in variables) {
    # get that variable's transformation
    transformation = norm_table$transformation[norm_table$variables == var] %>% toupper()
    
    # if STA apply pooled sta
    if (transformation == "STA") {
      data = data %>%
        group_by(!!sym(pool_variable)) %>%
        mutate(var = !!sym(var) / mean(!!sym(var))) %>%
        mutate(var = if_else(is.na(var), 0, var))
      
      # replace the raw variable with the STAed variable
      data[, var] = data[, "var"]
      data[, "var"] = NULL
      
    }
    
  }
  
  return(data)
  
}

run_model = function(data, dv, ivs, norm_table = NULL) {
  # build formula object
  formula = build_formula(dv = dv, ivs = ivs)
  
  # generate norm_data
  norm_data = apply_normalisation(data = data,
                                  norm_table = norm_table)
  
  # run model on norm_data
  model = lm(formula = formula, data = norm_data)
  
  # add norm_table to mdoel object
  model$norm_table = norm_table
  
  # return model object
  return(model)
}

decomping = function(model,
                     raw_data,
                     de_normalise = F,
                     categories = NULL,
                     id_var = NULL) {
  
  # get the coefficients from the model object
  coef = model$coefficients
  # get the modeled data from the model object
  data = model$model
  
  # extract dependent variable name
  dv = colnames(data)[1]
  
  # get the dependent variable from the data object
  actual = data[,dv]
  
  # initiate variable to confirm if correct raw data is provided
  raw_actual_supplied = F
  
  # get raw dependent variable if supplied
  if (de_normalise) {
    
    # if no raw data provided
    if(is.null(raw_data)){
      print("Warning: you must provide a raw_data to de normalise the data")
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
        
      }else{
        
        # else print warning
        print("Warning: dependent variable not found in raw data supplied")
        
      }
    }
  }
  
  
  # get the intercept value from the coef object
  intercept = coef[1]
  # keep the other coefficients
  coef = coef[2:length(coef)]
  
  # generate an id variable if one is not provided
  if(is.null(id_var)) {
    print(
      paste0("Info: no id variable supplied. New id variable generated as 1 to ",
             nrow(data))
      )
    id_var = "id"
    id_var_values = 1:nrow(data)
  }else{
    
    # if and id_var is provided, check that raw data is provided 
    if(is.null(raw_data)){
      
      # if raw data not provided print warning and generate id_var_values
      print(paste0("Warning: ID variable provided, but no raw data provided. New id variable generated as 1 to ",
                   nrow(data)))
      id_var_values = 1:nrow(data)
    }
    else{
      # if raw data is provided, check that raw data contains the id variable
      if(id_var %in% colnames(raw_data)){
        id_var_values = raw_data[,id_var] %>% pull()
      }else{
        # if raw data doesnt contain the id_var, print warning and generate id_variable
        print(paste0("Warning: ID variable provided not found in raw data provided. New id variable generated as 1 to ",
                     nrow(data)))
        id_var_values = 1:nrow(data)
      }
    }
  }
  
  # try to get the normalisation table
  ## (in the "de_normalise" section ?)
  ## a pool variable is useful for consistency (if a filter needsa to be applied)
  norm_table = TRY({model$norm_table})

  # if no norm_table is provided
  if(is.null(norm_table)){
    print("Info: no normalisation table (norm_table) found in model object. A pool variable ('total') will be generated.")
    
    pool = tibble("total")
  }else{
    
    # if a norm table is provided extract the pool variable
    pool_variable = norm_table$variables[toupper(norm_table$transformation) == "POOL"]
    
    # if no raw data provided print a warning and generate a pool variable
    if(is.null(raw_data)){
      print("Warning: no raw_data found to extract pool variable found in model's norm_table. A pool variable ('total') will be generated.")
      
      pool = tibble("total")
    }else{
      # if raw data is provided check if it contains the pool variable
      if(pool_variable %in% colnames(raw_data)){
        pool = raw_data[,pool_variable]
      }else{
        # if not, print warning and geterate pool variable
        print("Warning: pool variable from model's norm_table not found in raw_data. A pool variable ('total') will be generated.")
        pool = tibble("total")
      }
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
  
  # get the independent variables
  independendent_variables =  data[, 2:ncol(data)]
  
  
  if (length(coef) == 1) {
    # multiply independent variable by coefficient
    variable_decomp = data.frame(independendent_variables * coef)
    colnames(variable_decomp) = names(coef)
  } else{
    # multiply independent variables data frame by coefficient vector
    variable_decomp = data.frame(mapply(FUN = `*`,
                                        independendent_variables, 
                                        coef,
                                        SIMPLIFY = FALSE))
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
  if(raw_actual_supplied & de_normalise){
    
    # check if norm_table is provided
    if(is.null(norm_table)){
      print("Warning: norm_table not found in model, but required to de-normalised.")
    }else{
      # else check if the dv is not in norm_table
      if(!(dv %in% norm_table$variables)){
        
        # if the dv is not found in the norm table print warning
        ### STA could work if the de_normalise is true
        print("Warning: dv not found in norm_table.")
        
      }else{
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
  fitted_values = fitted_values[fitted_values$variable %in% c("actual", "residual"),]
  
  # filter by pool if provided
  if (!is.null(pool)) {
    decomp = decomp[decomp$pool == pool,]
    fitted_values = fitted_values[fitted_values$pool == pool,]
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
    fitted_values = fitted_values[fitted_values$pool == pool,]
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

decay = function(v,decay){
  if (decay == 0) {
    return(v)
  }
  else {
    stats::filter(v, decay, method = "recursive")
  }
}
diminish = function(v,m,abs=T){
  m = as.numeric(as.character(m))
  if (m == 0) {
    return(v)
  }
  else {
    1 - base::exp(-v/m)
  }
}
lag = function(v,l,zero=T){
  
  # if lag is zero, return original vector
  if(l == 0){
    return(v)
  }
  
  # get the length of the vector
  n = length(v)
    
  # if the lag is positive
  if(l > 0){
    
    #move forward 
    
    # cut forward extremities
    v = v[1:(n-l)]
    
    # if zero is TRUE
    if(zero == T){
      
      v = c(rep(0,l),v)
      
    }else if(zero == F){
      
      v = c(rep(v[1],l),v)
      
    }
  }
  
  if(l < 0){
    
    l = l*-1
    
    v = v[(l+1):n]
    
    if(zero == T){
      
      v = c(v,rep(0,l))
      
    }
    else{
      
      v = c(v,rep(v[length(v)],l))
      
    }
  }
  
  return(v)
  
}
ma = function(v,width,align="center",fill=1){
  
  if(width == 0){
    return(v)
  }
  
  else{
    
    # fill options:
    ## 0
    ## 1 (extremes)
    
    if(fill == 0){
      v = rollmean(v,width,fill = fill,align = align)
    }
    else if(fill == 1){
      
      fill = mean(v)
      v = rollmean(v,width,fill = fill,align = align)
      
      #if right, left, center
      # if odd or even
      # if(align == "center"){}
      # if(align == "left"){}
      # if(align == "right"){}
      # test_roll_mean = function(width,fill){
      #   
      #   df = data.frame(
      #     v0 = mtcars$mpg,
      #     vcenter = rollmean(mtcars$mpg,width,fill = fill,align = "center"),
      #     vright = rollmean(mtcars$mpg,width,fill = fill,align = "right"),
      #     vleft = rollmean(mtcars$mpg,width,fill = fill,align = "left"),
      #     id = 1:32
      #   ) %>% reshape2::melt(id.vars = "id")
      #   
      #   plot_ly() %>% 
      #     add_lines(data = df,x = ~id,y = ~value,color = ~variable)
      #   
      # }
      # 
      # test_roll_mean(width = 5,fill = NA)
      # 
      
    }
  }
  
  return(v)
}
