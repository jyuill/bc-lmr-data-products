# getting test data if needed for bug fixes / troubleshooting

library(tidyverse)
library(lmrtools)

# query database with lmrtools pkg
# all beer data
beer_data <- fetch_lmr_complete_filter(replace=TRUE, cat_type='Beer')

# annual beer data
annual_data <- lmrtools::aggregate_annual_cat_type(beer_data, beer_data)

# qtr beer data
qtr_data <- lmrtools::aggregate_qtr_cat_type(beer_data, 4)

