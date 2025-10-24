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

# works with hard-coded lag number
td_lag1 <- test_data %>%
    # convert cyr to number for lag calculation
    mutate(cyr = as.numeric(as.character(cyr))) %>%
    # group by category, subcategory for yoy calcs
    group_by(cyr, category, subcategory) %>% 
    # summarize for annual sales
    summarise(netsales = sum(netsales)) %>% ungroup() %>%
    mutate(
      yoy_sales_pc = (netsales - lag(netsales, n=11))/lag(netsales, n=11)
    ) %>% select(category, subcategory, cyr, netsales)

# works with variable lag number based on number of subcategories
n_catt <- test_data %>% distinct(category) %>% nrow()
n_subcatt <- test_data %>% distinct(subcategory) %>% nrow()
n_lag <- n_subcatt

td_lag <- test_data %>% group_by(cyr, category, subcategory) %>% 
  summarise(netsales = sum(netsales)) %>% ungroup() %>% # lag doesn't work with grouped data
  mutate(
    yoy_sales = netsales - lag(netsales, n=n_lag),
    yoy_sales_pc = (netsales - lag(netsales, n=n_lag))/lag(netsales, n=n_lag)
  )

test_bc <- test_data %>% filter(category == "BC")
n_lag <- length(unique(test_bc$subcategory))
td_lag_bc <- test_bc %>% group_by(cyr, category, subcategory) %>% 
  summarise(netsales = sum(netsales)) %>% ungroup() %>%
  mutate(
    yoy_sales = netsales - lag(netsales, n=n_lag),
    yoy_sales_pc = (netsales - lag(netsales, n=n_lag))/lag(netsales, n=n_lag)
  )


high_cat <- 'category'
low_cat <- 'subcategory'
n_lag <- length(unique(test_bc[[low_cat]]))
td_lag_bc2 <- test_bc %>% group_by(cyr, !!sym(high_cat), !!sym(low_cat)) %>% 
  summarise(netsales = sum(netsales)) %>% ungroup() %>% 
  mutate(
    yoy_sales = netsales - lag(netsales, n=n_lag),
    yoy_sales_pc = (netsales - lag(netsales, n=n_lag))/lag(netsales, n=n_lag)
  )

## convert to function
cat_data <- function(data_cat, high_cat, low_cat) {
  n_lag <- length(unique(data_cat[[low_cat]]))
  data_cat %>% 
    group_by(cyr, !!sym(high_cat), !!sym(low_cat)) %>% 
    summarise(netsales = sum(netsales)) %>% ungroup() %>% 
    mutate(
      yoy_sales = netsales - lag(netsales, n=n_lag),
      yoy_sales_pc = (netsales - lag(netsales, n=n_lag))/lag(netsales, n=n_lag)
    )
}

td_lag_cat <- cat_data(test_data, 'cat_type', 'category')
td_lag_bc3 <- cat_data(test_bc, 'category', 'subcategory')
