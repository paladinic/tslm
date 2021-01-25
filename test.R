# funcs    ####

source("c:/Users/44751/Desktop/modelling lib/lib.R")

# test 1   ------------------------------------------------------------------

raw_data = read_csv(file = "c:/Users/44751/Desktop/pooled amazon searches - multiTimeline (8).csv")

raw_data = raw_data %>%
  mutate(christmas = if_else(christmas == "<1", "0", christmas)) %>%
  mutate(christmas = as.numeric(christmas)) %>%
  mutate(rakhi = if_else(rakhi == "<1", "0", rakhi)) %>%
  mutate(rakhi = as.numeric(rakhi)) %>%
  mutate(diwali = if_else(diwali == "<1", "0", diwali)) %>%
  mutate(diwali = as.numeric(diwali))

write.csv(x = raw_data,
          file = "c:/Users/44751/Desktop/pooled data.csv",
          row.names = F)

norm_table = tibble(variables = c("amazon", "rakhi","country"),
                    transformation = c("STA", "STA","POOL"))

dv = "amazon"
ivs = c("rakhi", "christmas", "diwali")
id_var = "Week"

model_table = tibble(
  variables = ivs,
  decay = c(0,0,0),
  dim_rets = c(),
  lag = c(),
  ma = c()
)

model = run_model(data = raw_data,dv = dv,ivs = ivs,norm_table = norm_table)

decomp_list = decomping(
  model = model,
  de_normalise = F,
  raw_data = raw_data,
  id_var = id_var
)

decomp_chart(decomp_list,pool = "UK")
fit_chart(decomp_list = decomp_list,pool = "UK")

# test 2   --------------------------------------------------------------------

data_table = mtcars

dv = "mpg"
ivs = c("wt", "cyl")

model = run_model(dv = dv,ivs = ivs, data = data_table)

decomp_list = decomping(
  model = model,
  raw_data = data_table
)

decomp_chart(decomp_list = decomp_list)
fit_chart(decomp_list = decomp_list)
