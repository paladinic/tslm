# funcs    ####

source("c:/Users/44751/Desktop/modelling lib/lib.R")

# test 1   ------------------------------------------------------------------

raw_data = read_xcsv(file = "c:/Users/44751/Desktop/modelling lib/data/pooled data.csv")

meta_data = tibble(variables = c("amazon", "rakhi","country","Week"),
                    meta = c("STA", "STA","POOL","ID"))
dv = "amazon"
ivs = c("rakhi", "christmas", "diwali")
id_var = "Week"

model_table = tibble(
  variables = ivs,
  decay = c(0,0,0),
  dim_rets = c(0,0,20),
  lag = c(0,52,0),
  ma = c(0,0,0)
)

norm_data = apply_normalisation(data = raw_data,meta_data =  meta_data)

trans_data = apply_transformation(model_table = model_table,
                                  meta_data =  meta_data,
                                  raw_data = norm_data)

model = run_model(data = trans_data,dv = dv,ivs = ivs,meta_data =  meta_data)

decomp_list = decomping(
  model = model,
  de_normalise = T,
  raw_data = raw_data
)

decomp_chart(decomp_list,pool = "India")
fit_chart(decomp_list = decomp_list,pool = "India")

# test 2   --------------------------------------------------------------------

raw_data = mtcars

dv = "mpg"
ivs = c("wt", "cyl")

model_table = tibble(
  variables = ivs,
  decay = c(0,0),
  dim_rets = c(0,20),
  lag = c(0,0),
  ma = c(0,0)
)

model = run_model(dv = dv,model_table = model_table, data = raw_data)

decomp_list = decomping(
  model = model,
  raw_data = raw_data
)

decomp_chart(decomp_list = decomp_list)
fit_chart(decomp_list = decomp_list)
# test 3   ####

raw_data_0 = read_xcsv("c:/Users/44751/Desktop/Data/eqq.csv")

raw_data = raw_data_0[1:(nrow(raw_data_0)-50),]
raw_data_1 = raw_data_0[(nrow(raw_data_0)-49):nrow(raw_data_0),]

dv = "Open_eqqq"
ivs = colnames(raw_data)[grepl(pattern = "Open",x = colnames(raw_data))]
ivs = ivs[!(ivs %in% c(dv))]
ivs = ivs[!(ivs %in% c(dv,"Open_TSLA"))]
id_var = "Date"

meta_data = tibble(
  variables = c(dv,ivs),
  meta = rep("STA",7)
)

model = run_model(dv = dv,ivs = ivs,data = raw_data,meta_data = meta_data)
d = decomping(model = model,raw_data = raw_data)

decomp_chart(decomp_list = d)

test_ivs = colnames(raw_data)[grepl(pattern = "Close",x = colnames(raw_data))]

what_next(raw_data = raw_data,ivs = ivs,dv = dv,test_ivs = test_ivs)


###########

test_table = tibble(
  variable = c("Close_eqqq","Close_AAPL","Close_AMZN","Close_MSFT","Close_PYPL"),
  transformation = c("diminish","diminish","decay","diminish","decay"),
  value = c(50,40,.5,100,.8)
)

what_trans = function(raw_data,ivs,dv,test_table,meta_data = NULL){
  
  # if duplicate_var=T keep any trans variable in current model spec
  # and add the transformed one
  ## FOR NOW - replace
  
  # define output table to fill with loop
  output = tibble(
    variable = "0",
    transformation = "",
    value = "",
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
        coef = ms$coefficients[var,"Estimate"]
        t_value = ms$coefficients[var,"t value"]
        
        output[i,] = list(var,adj_R2,t_value,coef)
        
      }
    }
  }
  
  return(output)
  
}