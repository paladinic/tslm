# funcs    ####

source("c:/Users/44751/Desktop/R/modelling lib/lib.R")

# test 1   ------------------------------------------------------------------

raw_data = read_xcsv(file = "c:/Users/44751/Desktop/R/modelling lib/data/pooled data.csv")
data = raw_data

meta_data = tibble(
  variables = c("amazon", "rakhi", "country", "Week"),
  meta = c("STA", "STA", "POOL", "ID")
)
dv = "amazon"
ivs = c("rakhi", "christmas", "diwali")
id_var = "Week"

model_table = tibble(
  variables = ivs,
  decay = c(0, 0, 0),
  dim_rets = c(0, 0, 20),
  lag = c(0, 52, 0),
  ma = c(0, 0, 0)
)


model = run_model(
  data = raw_data,
  dv = dv,
  model_table = model_table,
  meta_data =  meta_data
)

decomp_list = decomping(model = model,
                        de_normalise = T,
                        raw_data = raw_data)

decomp_chart(decomp_list, pool = "India")
fit_chart(decomp_list = decomp_list, pool = "India")

# test 2   --------------------------------------------------------------------

raw_data = mtcars

dv = "mpg"
ivs = c("wt", "cyl")

model_table = tibble(
  variables = ivs,
  decay = c(0, 0),
  dim_rets = c(0, 20),
  lag = c(0, 0),
  ma = c(0, 0)
)

model = run_model(dv = dv,
                  model_table = model_table,
                  data = raw_data)

decomp_list = decomping(model = model,
                        raw_data = raw_data)

decomp_chart(decomp_list = decomp_list)
fit_chart(decomp_list = decomp_list)
# test 3   ####

raw_data = read_xcsv("c:/Users/44751/Desktop/Data/eqq.csv")

# fx = raw_data %>% select(`Close_GBPUSD=X`)
# id = raw_data %>% select("Date")
# raw_data = raw_data %>% select(-Date) * fx %>% pull() 
# raw_data = raw_data %>% bind_cols(fx)

# raw_data_0 = raw_data_0[1:(nrow(raw_data) - 50), ]
# raw_data_1 = raw_data_0[(nrow(raw_data) - 49):nrow(raw_data), ]

dv = "Open_eqqq"
ivs = colnames(raw_data)[grepl(pattern = "Open", x = colnames(raw_data))]
ivs = ivs[!(ivs %in% c(dv))]
# ivs = ivs[!(ivs %in% c(dv, "Open_TSLA"))]
id_var = "Date"

meta_data = tibble(variables = c(dv, ivs),
                   meta = rep("STA", 9))

model_table = build_model_table(ivs = ivs)


## WHY DOES THIS WORK WITH 2 messages...
model = run_model(
  dv = dv,
  ivs = ivs,
  data = raw_data,
  meta_data = meta_data
)
## ...AND THIS WITH 3 messages???
model = run_model(
  dv = dv,
  model_table = model_table,
  data = raw_data,
  meta_data = meta_data
)

d = decomping(model = model, raw_data = raw_data,id_var = "Date")

decomp_chart(decomp_list = d)
fit_chart(decomp_list = d)

test_ivs = colnames(raw_data)[grepl(pattern = "Close", x = colnames(raw_data))]

what_next(
  raw_data = raw_data,
  ivs = ivs,
  dv = dv,
  test_ivs = test_ivs
)


###########

test_table = tibble(
  variables = c(
    "Close_eqqq",
    "Close_AAPL",
    "Close_AMZN",
    "Close_MSFT",
    "Close_PYPL"
  ),
  transformation = c("dim_rets", "dim_rets", "decay", "lag", "ma"),
  value = c(50, 40, .5, 3, 5)
)
what_trans(
  raw_data = raw_data,
  dv = dv,
  ivs = ivs,
  test_table = test_table,
  meta_data = meta_data
)



# auto save####

schedule = list(
  year = list(as.POSIXct("00:00:00 2021-03-05",format="%H:%M:%S %Y-%m-%d"),60*24*365,NA),
  month = list(as.POSIXct("00:00:00 2021-03-05",format="%H:%M:%S %Y-%m-%d"),60*24*30,NA),
  week = list(as.POSIXct("00:00:00 2021-03-05",format="%H:%M:%S %Y-%m-%d"),60*24*7,NA),
  day = list(as.POSIXct("00:00:00 2021-03-05",format="%H:%M:%S %Y-%m-%d"),60*24,NA),
  hour = list(as.POSIXct("00:00:00 2021-03-05",format="%H:%M:%S %Y-%m-%d"),60,NA),
  mins15 = list(as.POSIXct("00:00:00 2021-03-05",format="%H:%M:%S %Y-%m-%d"),15,NA)
)

for(i in 1:length(schedule)){
  
  item = schedule[[i]]
  
  if(item[1] < Sys.time()){
    print("save")
    item[[1]] = Sys.time() + 60*item[[2]]
  }
  
  schedule[[i]] = item
}
