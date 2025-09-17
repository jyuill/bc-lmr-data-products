
library(tidyverse)
library(ggtext)
library(plotly)
library(scales)
library(lubridate)
library(here)

plot_data <- beer_annual_test

scale_set <- list('comma', 1e-4, 'T')
labelz <- "label_currency(scale = 1e-6, suffix = 'M')"
labelz <- "label_currency(scale = scale_set[[2]], suffix = scale_set[[3]])"
labelz <- "label_comma(scale = scale_set[[2]], suffix = scale_set[[3]])"

y_labels <- switch(scale_set[[1]],
                   'curr' = label_currency(scale = scale_set[[2]], suffix = scale_set[[3]]),
                   'comma' = label_comma(scale = scale_set[[2]], suffix = scale_set[[3]]),
                   'percent' = label_percent(accuracy = 0.1)
)

scale_set2 <- c('num','B')
scale_set3 <- 'B'
y_labels <- make_y_labels(c('comma','K'))
y_labels <- make_y_labels('M')
plot_data %>% group_by(cyr) %>% 
  summarize(netsales = sum(netsales),
            litres = sum(litres)) %>% 
  ggplot(aes(x=cyr, y=netsales)) +
  geom_col(fill=bar_col) +
  #geom_text(aes(label=scales::dollar(netsales, suffix = "M")), 
  #        vjust=-0.5, size=3)+
  #scale_y_continuous(labels = label_currency(scale = 1e-6, suffix = "M"))
  #scale_y_continuous(labels = eval(parse(text=labelz)))
  #scale_y_continuous(labels = label_currency(scale = scale_set[[2]], 
  #                  suffix = scale_set[[3]]))
  scale_y_continuous(labels = y_labels)

make_y_labels <- function(scale_params) {
  if(scale_params[1] == 'M') {
    scale_val <- 1e-6
  } else if (scale_params[1] == 'B') {
    scale_val <- 1e-9
  } else if (scale_params[1] == 'K') {
    scale_val <- 1e-3
  } else {
    scale_val <- 1
  }
  print(scale_val)
  y_labels <- switch(scale_params[2],
                     'netsales' = label_currency(scale = scale_val, suffix = scale_params[1]),
                     'litres' = label_comma(scale = scale_val, suffix = scale_params[1]),
                     'percent' = label_percent(accuracy = 0.1),
                     is.na = label_number(suffix = scale_params[1])
  )
  return(y_labels)
}
  