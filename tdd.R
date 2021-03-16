# Library       ####

source("c:/Users/44751/Desktop/R/modelling lib/lib.R")

# Apply Norm    ---------------------------------------------------------------
# SETUP ####

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
# 1
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



# Apply Trans   ---------------------------------------------------------------
# Run Model     ---------------------------------------------------------------
# Build Charts  ---------------------------------------------------------------
# What ifs      ---------------------------------------------------------------