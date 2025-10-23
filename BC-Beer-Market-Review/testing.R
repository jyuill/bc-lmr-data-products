# Various testing, scratch code, etc.

library(tidyverse)
library(plotly)
library(lubridate)
library(scales)
library(here)
library(RColorBrewer)

# testing ----
# using variables in data processing
test_data <- beer_data
group_var <- "category"

test_data %>% 
  group_by(!!sym(group_var)) %>% 
  summarise(litres = sum(litres))

# create function that accepts data and group variable
test_data_group <- function(data, group_var) {
  data %>% 
    group_by(!!sym(group_var)) %>% 
    summarise(litres = sum(litres))
}
# test function
groupon <- test_data_group(test_data, group_var)
groupon

# testing different levels of grouping -> are redundant groups needed?
test_data %>% filter(end_qtr_dt >= '2024-03-31') %>%
  group_by(end_qtr_dt, end_qtr, qtr, category) %>% 
  summarise(litres = sum(litres))

td_lag1 <- test_data %>%
    # convert cyr to number for lag calculation
    mutate(cyr = as.numeric(as.character(cyr))) %>%
    # group by category, subcategory for yoy calcs
    group_by(category, subcategory) %>% ungroup() %>%
    mutate(yoy_sales = (netsales - lag(netsales, n=11))/lag(netsales, n=11),
           yoy_litres = (litres - lag(litres, n=11))/lag(litres, n=11)
    ) %>% select(category, subcategory, cyr, end_qtr, netsales, yoy_sales)

n_catt <- test_data %>% distinct(category) %>% nrow()
n_subcatt <- test_data %>% distinct(subcategory) %>% nrow()
n_lag <- n_subcatt

td_lag <- test_data %>% group_by(cyr, category, subcategory) %>% 
  summarise(netsales = sum(netsales)) %>% ungroup() %>%
  mutate(
    yoy_sales = netsales - lag(netsales, n=n_lag)
  )