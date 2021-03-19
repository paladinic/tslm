# Library           ####
source("c:/Users/44751/Desktop/R/modelling lib/lib.R")
###NOTES            ####

# normalization without pool variable?

# NAs in apply_norm and apply_trans  

###Apply Norm       ---------------------------------------------------------------
# SETUP             ####

raw_data = read_xcsv(file = "c:/Users/44751/Desktop/R/modelling lib/data/pooled data.csv")

dv = "amazon"
ivs = c("rakhi", "christmas", "diwali")
id_var = "Week"

meta_data = tibble(
  variables = c("amazon", "rakhi", "country", "Week"),
  meta = c("STA", "STA", "POOL", "ID")
)

model_table = tibble(
  variables = ivs,
  decay = c(0, 0, 0),
  dim_rets = c(0, 0, 20),
  lag = c(0, 52, 0),
  ma = c(0, 0, 0)
)



# TESTS             ####
# 1 NO ARGUMENTS    ####
# no data provided
df = apply_normalisation(meta_data = meta_data,model_table = model_table,dv = dv,verbose = T)
# no meta_data provided
df = apply_normalisation(raw_data = raw_data,model_table = model_table,dv = dv,verbose = T)
# no model_table provided
df = apply_normalisation(raw_data = raw_data,dv = dv,verbose = T,meta_data = meta_data)
# no dv provided
df = apply_normalisation(raw_data = raw_data,model_table = model_table,verbose = T,meta_data = meta_data)
# verbose
df = apply_normalisation(raw_data = raw_data,model_table = model_table,verbose = F,meta_data = meta_data)

# 2 WRONG ARGUMENTS ####
# raw_data not a data.frame
raw_data_2 = "not a DF"
df = apply_normalisation(raw_data = raw_data_2,model_table = model_table,verbose = T,meta_data = meta_data)
# meta_data not a data.frame
meta_data_2 = "not a DF"
df = apply_normalisation(raw_data = raw_data,model_table = model_table,verbose = T,meta_data = meta_data_2)
# model_table not a data.frame
model_table_2 = "not a DF"
df = apply_normalisation(raw_data = raw_data,model_table = model_table_2,verbose = T,meta_data = meta_data,dv=dv)
# dv not a string
dv2 = 100
df = apply_normalisation(raw_data = raw_data,model_table = model_table,verbose = T,meta_data = meta_data,dv=dv2)
# verbose not bool
df = apply_normalisation(raw_data = raw_data,model_table = model_table,verbose = "T",meta_data = meta_data,dv=dv)


###Apply Trans      ---------------------------------------------------------------
# SETUP             ####

raw_data = read_xcsv(file = "c:/Users/44751/Desktop/R/modelling lib/data/pooled data.csv")

dv = "amazon"
ivs = c("rakhi", "christmas", "diwali")
id_var = "Week"

meta_data = tibble(
  variables = c("amazon", "rakhi", "country", "Week"),
  meta = c("STA", "STA", "POOL", "ID")
)

model_table = tibble(
  variables = ivs,
  decay = c(0.3, 0, 0),
  dim_rets = c(0, 0, 20),
  lag = c(0, 52, 0),
  ma = c(0, 0, 7)
)


# TESTS             ####
# 1 NO ARGUMENTS    ####

# raw_data
df = apply_transformation(model_table = model_table,meta_data = meta_data)
# model_table
df = apply_transformation(raw_data = raw_data,meta_data = meta_data)
# meta_data
df = apply_transformation(raw_data = raw_data,model_table = model_table)
# verbose
df = apply_transformation(raw_data = raw_data,meta_data = meta_data,verbose = F)


# 2 WRONG ARGUMENTS ####

# raw_data
raw_data_2
df = apply_transformation(raw_data = raw_data_2,model_table = model_table,meta_data = meta_data)
# model_table
model_table_2
df = apply_transformation(model_table = model_table_2,raw_data = raw_data,meta_data = meta_data)
# meta_data
meta_data_2
df = apply_transformation(meta_data = meta_data_2,raw_data = raw_data,model_table = model_table)
# verbose
verbose2 = "not a bool"
df = apply_transformation(raw_data = raw_data,meta_data = meta_data,verbose = verbose2)


###Run Model        ---------------------------------------------------------------
# SETUP             ####

raw_data = read_xcsv(file = "c:/Users/44751/Desktop/R/modelling lib/data/pooled data.csv")

dv = "amazon"
ivs = c("rakhi", "christmas", "diwali")
id_var = "Week"

meta_data = tibble(
  variables = c("amazon", "rakhi", "country", "Week"),
  meta = c("STA", "STA", "POOL", "ID")
)

model_table = tibble(
  variables = ivs,
  decay = c(0.3, 0, 0),
  dim_rets = c(0, 0, 20),
  lag = c(0, 52, 0),
  ma = c(0, 0, 7)
)


# TESTS             ####
# 1 NO ARGUMENTS    ####

# raw_data
df = run_model(model_table = model_table,meta_data = meta_data,dv = dv)
# model_table
df = run_model(data = raw_data,meta_data = meta_data,dv = dv)
df = run_model(data = raw_data,meta_data = meta_data,dv = dv,ivs = ivs)
# dv
df = run_model(data = raw_data,model_table = model_table)
# ivs
df = run_model(data = raw_data,dv = dv,meta_data = meta_data)
df = run_model(data = raw_data,dv = dv,meta_data = meta_data,model_table = model_table)
# meta_data
df = run_model(data = raw_data,model_table = model_table, dv = dv)
# verbose
df = run_model(data = raw_data,meta_data = meta_data,verbose = F)


# 2 WRONG ARGUMENTS ####

# raw_data
raw_data_2
df = run_model(data = raw_data_2,model_table = model_table,meta_data = meta_data,dv = dv)
# model_table
model_table_2
df = run_model(model_table = model_table_2,data = raw_data,meta_data = meta_data,dv = dv)
 # meta_data
meta_data_2
df = run_model(meta_data = meta_data_2,data = raw_data,model_table = model_table,dv = dv)
# verbose
verbose2 = "not a bool"
df = run_model(data = raw_data,meta_data = meta_data,verbose = verbose2,dv = dv,ivs = ivs)


###What ifs         ---------------------------------------------------------------

# SETUP             ####

raw_data = read_xcsv(file = "c:/Users/44751/Desktop/R/modelling lib/data/pooled data.csv")

dv = "amazon"
ivs = c("christmas")
id_var = "Week"

meta_data = tibble(
  variables = c("amazon", "rakhi", "country", "Week"),
  meta = c("STA", "STA", "POOL", "ID")
)

model_table = tibble(
  variables = ivs,
  decay = c(0.3),
  dim_rets = c(0),
  lag = c(0),
  ma = c(7)
)


test_table = tibble(
  variables = c("christmas","rakhi"),
  transformation = c("dim_rets", "dim_rets"),
  value = c(50, 40)
)

test_ivs = c("rakhi", "diwali")

# what var next     ####

# NO ARGUMENT       ####
# raw_data
what_next(ivs = ivs,dv = dv,test_ivs = test_ivs,meta_data = meta_data)
# model_table
what_next(raw_data = raw_data,ivs = ivs,dv = dv,test_ivs = test_ivs,meta_data = meta_data)
what_next(raw_data = raw_data,dv = dv,test_ivs = test_ivs,meta_data = meta_data)
# dv
what_next(raw_data = raw_data,ivs = ivs,test_ivs = test_ivs,meta_data = meta_data,dv = dv2)
# ivs
what_next(raw_data = raw_data,dv = dv,test_ivs = test_ivs,meta_data = meta_data)
what_next(raw_data = raw_data,dv = dv,test_ivs = test_ivs,meta_data = meta_data,model_table = model_table)
# meta_data
what_next(raw_data = raw_data,dv = dv,test_ivs = test_ivs,ivs = ivs)
# verbose
what_next(raw_data = raw_data,ivs = ivs,dv = dv,test_ivs = test_ivs,verbose = F)

# WRONG ARGUMENT    ####
# raw_data
what_next(ivs = ivs,dv = dv,test_ivs = test_ivs,meta_data = meta_data,raw_data = raw_data_2)
# model_table
what_next(raw_data = raw_data,ivs = ivs,dv = dv,test_ivs = test_ivs,meta_data = meta_data,model_table = model_table_2)
# dv
what_next(raw_data = raw_data,ivs = ivs,test_ivs = test_ivs,meta_data = meta_data,dv = dv2)
# ivs
what_next(raw_data = raw_data,dv = dv,test_ivs = test_ivs,meta_data = meta_data,ivs = 100)
# meta_data
what_next(raw_data = raw_data,dv = dv,test_ivs = test_ivs,ivs = ivs, meta_data = meta_data_2)
# verbose
what_next(raw_data = raw_data,ivs = ivs,dv = dv,test_ivs = test_ivs,verbose = verbose2)



# what trans on vars

# Done --------------------------------------------------------------------


###Build Charts     ---------------------------------------------------------------

# decomp

# decomp chart

# avm chart

# SETUP             ####

raw_data = read_xcsv(file = "c:/Users/44751/Desktop/R/modelling lib/data/pooled data.csv")

dv = "amazon"
ivs = c("christmas")
id_var = "Week"

meta_data = tibble(
  variables = c("amazon", "country", "Week"),
  meta = c("STA", "POOL", "ID")
)

categories = tibble(
  variable = "christmas",
  category = "seasonality",
  calc = "min",
)

model_table = tibble(
  variables = ivs,
  decay = c(0.3),
  dim_rets = c(0),
  lag = c(3),
  ma = c(0)
)

test_ivs = c("rakhi","diwali")


p = run_model(
  data = raw_data,
  dv = dv,
  model_table = model_table,
  meta_data = meta_data
) %>%
  decomping(
    de_normalise = T,
    categories = categories,
    raw_data = raw_data,
    id_var = id_var
  ) %>%
  decomp_chart(pool = "UK",
               variable_decomp = T)

# NO ARGUMENT       ####
# raw_data
decomping(model = model,de_normalise = T,categories = categories,raw_data = raw_data,id_var = id_var)
# model_table
what_next(raw_data = raw_data,ivs = ivs,dv = dv,test_ivs = test_ivs,meta_data = meta_data)
what_next(raw_data = raw_data,dv = dv,test_ivs = test_ivs,meta_data = meta_data)
# dv
what_next(raw_data = raw_data,ivs = ivs,test_ivs = test_ivs,meta_data = meta_data,dv = dv2)
# ivs
what_next(raw_data = raw_data,dv = dv,test_ivs = test_ivs,meta_data = meta_data)
what_next(raw_data = raw_data,dv = dv,test_ivs = test_ivs,meta_data = meta_data,model_table = model_table)
# meta_data
what_next(raw_data = raw_data,dv = dv,test_ivs = test_ivs,ivs = ivs)
# verbose
what_next(raw_data = raw_data,ivs = ivs,dv = dv,test_ivs = test_ivs,verbose = F)

# WRONG ARGUMENT    ####
# raw_data
what_next(ivs = ivs,dv = dv,test_ivs = test_ivs,meta_data = meta_data,raw_data = raw_data_2)
# model_table
what_next(raw_data = raw_data,ivs = ivs,dv = dv,test_ivs = test_ivs,meta_data = meta_data,model_table = model_table_2)
# dv
what_next(raw_data = raw_data,ivs = ivs,test_ivs = test_ivs,meta_data = meta_data,dv = dv2)
# ivs
what_next(raw_data = raw_data,dv = dv,test_ivs = test_ivs,meta_data = meta_data,ivs = 100)
# meta_data
what_next(raw_data = raw_data,dv = dv,test_ivs = test_ivs,ivs = ivs, meta_data = meta_data_2)
# verbose
what_next(raw_data = raw_data,ivs = ivs,dv = dv,test_ivs = test_ivs,verbose = verbose2)



# what trans on vars

# SETUP             ####

raw_data = read_xcsv(file = "c:/Users/44751/Desktop/R/modelling lib/data/pooled data.csv")

dv = "amazon"
ivs = c("rakhi", "christmas", "diwali")
id_var = "Week"

meta_data = tibble(
  variables = c("amazon", "rakhi", "country", "Week"),
  meta = c("STA", "STA", "POOL", "ID")
)

model_table = tibble(
  variables = ivs,
  decay = c(0.3, 0, 0),
  dim_rets = c(0, 0, 20),
  lag = c(0, 52, 0),
  ma = c(0, 0, 7)
)

