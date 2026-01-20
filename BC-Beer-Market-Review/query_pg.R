# Query cloud db for LMR dataset
# ENTIRE ORIGINAL WORKFLOW CAN BE REPLACED WITH lmrtools
# lmrtools - see lmrtools documentation if any issues
# DON'T EVEN NEED THIS FILE! can call function from global.R
library(lmrtools)
beer_data <- fetch_lmr_complete_filter(replace=TRUE, cat_type='Beer')
# DONE!
# -----------------everything below is deprecated--
library(RPostgres)
library(tidyverse) 
library(here)

# cat("load config \n")
# # Using config.yml for credentials
# # - (alternative to .Renviron file)
# # - system will automatically look for config.yml in parent folder
# # NOTE: config.yml is in .gitignore for security
# db_config <- config::get()
# print('connecting to pg db...')
# # connect to the database
# con_aws <- dbConnect(RPostgres::Postgres(),
#                      host=db_config$db_pg$endpt,
#                      dbname=db_config$db_pg$adb,
#                      user=db_config$db_pg$user,
#                      password=db_config$db_pg$apwd,
#                      port=db_config$db_pg$aport)
# # check underlying tables - for testing
# #dbListTables(con_aws)
# # main query - filter for 'beer' -> raw data joined with date dimensions
# # - everything from lmr, everything BUT fy_qtr from qtr to avoid duplication
# lmr_data_db <- dbGetQuery(con_aws, "SELECT 
#                            lmr.*
#                           , qtr.fyr
#                           , qtr.qtr
#                           , qtr.end_qtr
#                           , qtr.end_qtr_dt
#                           , qtr.cyr
#                           , qtr.season
#                           , qtr.cqtr 
#                           FROM public.lmr_data lmr 
#                           LEFT JOIN public.lmr_quarters qtr 
#                           ON lmr.fy_qtr = qtr.fy_qtr
#                           WHERE lmr.cat_type = 'Beer';")

# # close connection
# dbDisconnect(con_aws)

