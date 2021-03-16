# Library           ####

source("c:/Users/44751/Desktop/R/modelling lib/lib.R")

# Apply Norm        ---------------------------------------------------------------
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



# TESTS ####
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


# Apply Trans       ---------------------------------------------------------------
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


# TEST ####
# 1 NO ARGUMENTS    ####

# raw_data
df = apply_transformation(model_table = model_table,meta_data = meta_data)

# model_table
# meta_data
# verbose


# 2 WRONG ARGUMENTS ####

# Run Model         ---------------------------------------------------------------
# Build Charts      ---------------------------------------------------------------
# What ifs          ---------------------------------------------------------------