library(plotly)
library(htmlwidgets)

data = mtcars

model_1 = lm(formula = mpg ~ wt,data = data)
model_2 = lm(formula = mpg ~ wt + cyl,data = data)
model_3 = lm(formula = mpg ~ wt + cyl + gear,data = data)
model_4 = lm(formula = mpg ~ wt + cyl + gear + hp,data = data)
model_5 = lm(formula = mpg ~ wt + cyl + gear + hp + disp,data = data)
model_6 = lm(formula = mpg ~ wt + cyl + gear + hp + disp + qsec,data = data)

chart_data_0 = data.frame(
  index = 1:nrow(data),
  mpg = data$mpg,
  predicted = mean(data$mpg),
  variables = 0
) %>% mutate(error = mpg - predicted)

chart_data_1 = data.frame(
  index = 1:nrow(data),
  mpg = data$mpg,
  predicted = model_1$fitted.values,
  variables = 1
) %>% mutate(error = mpg - predicted)

chart_data_2 = data.frame(
  index = 1:nrow(data),
  mpg = data$mpg,
  predicted = model_2$fitted.values,
  variables = 2
) %>% mutate(error = mpg - predicted)

chart_data_3 = data.frame(
  index = 1:nrow(data),
  mpg = data$mpg,
  predicted = model_3$fitted.values,
  variables = 3
) %>% mutate(error = mpg - predicted)

chart_data_4 = data.frame(
  index = 1:nrow(data),
  mpg = data$mpg,
  predicted = model_4$fitted.values,
  variables = 4
) %>% mutate(error = mpg - predicted)

chart_data_5 = data.frame(
  index = 1:nrow(data),
  mpg = data$mpg,
  predicted = model_5$fitted.values,
  variables = 5
) %>% mutate(error = mpg - predicted)

chart_data_6 = data.frame(
  index = 1:nrow(data),
  mpg = data$mpg,
  predicted = model_6$fitted.values,
  variables = 6
) %>% mutate(error = mpg - predicted)

chart_data = rbind(chart_data_0,
                   chart_data_1,
                   chart_data_2,
                   chart_data_3,
                   chart_data_4,
                   chart_data_5,
                   chart_data_6)

p1 = plot_ly(data = chart_data) %>% 
  add_trace(
    type = "scatter",
    x = ~index,
    y = ~mpg,
    frame = ~variables,
    name = "dependent variable",
    marker = list(color = "white",
                  size = 10,
                  line = list(
                    color = "black",
                    width = 3
                  ))
  ) %>% 
  add_trace(
    type = "scatter",
    x = ~index,
    y = ~predicted,
    frame = ~variables,
    name = "predicted",
    marker = list(color = "blue")
  ) %>% 
  add_trace(
    type = "scatter",
    x = ~index,
    y = ~error,
    frame = ~variables,
    name = "error",
    marker = list(color = "red")
  ) %>% 
  layout(yaxis = list(title = "dependent variables"))


p1

diminish_data = data.frame(
  predicted = generic::diminish(v = 0:10,m = 2,abs = F)*10 ,
  index = 0:10
)

p2 = plot_ly(diminish_data) %>% 
  add_trace(type = "scatter",
            mode = "lines+markers",
            x = ~index,
            y = ~predicted) %>% 
  layout(xaxis = list(title = "explanatory variable"))

p2

saveWidget(p1, "c:/Users/44751/Desktop/R/documentation-html-template/images/plots/html/lm_animation.html", selfcontained = F, libdir = "lib")
saveWidget(p2, "diminish.html", selfcontained = F, libdir = "lib")