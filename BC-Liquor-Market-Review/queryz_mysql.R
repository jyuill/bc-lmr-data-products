# Query cloud db for LMR dataset
# MySQL database -> DEPRECATED AUG 2025
# Preserved for archival purposes
library(tidyverse) 
library(RMariaDB) ## best way to access MySQL from R
library(dotenv) # not sure if this actually does anything in this setup
library(here)

cat("load config \n")
# Using config.yml for credentials
# - (alternative to .Renviron file)
# - system will automatically look for config.yml in parent folder
# - needed for each app
# NOTE: config.yml is in .gitignore for security
db_config <- config::get(config = "db")
endpt <- db_config$db$endpt
apwd <- db_config$db$apwd
aport <- db_config$db$aport
user <- db_config$db$user

print('connecting to db...')
# connect to the database
con_aws <- dbConnect(RMariaDB::MariaDB(),
                     host=endpt,
                     user='admin',
                     password=apwd,
                     port=aport)
# check underlying tables
#t_data <- dbGetQuery(con_aws, "SELECT * FROM bcbg.tblLDB_lmr LIMIT 10")
#q_data <- dbGetQuery(con_aws, "SELECT * FROM bcbg.tblLDB_quarter LIMIT 10")
# get all - if needed
#lmr_all <- dbGetQuery(con_aws, "SELECT * FROM bcbg.tblLDB_lmr")
#qtr_all <- dbGetQuery(con_aws, "SELECT * FROM bcbg.tblLDB_quarter")
# main query - all the data -> raw data joined with date dimensions
# - everything from lmr, everything BUT fy_qtr from qtr to avoid duplication
lmr_data_db <- dbGetQuery(con_aws, "SELECT 
                           lmr.*
                          , qtr.fyr
                          , qtr.qtr
                          , qtr.end_qtr
                          , qtr.end_qtr_dt
                          , qtr.cyr
                          , qtr.season
                          , qtr.cqtr 
                          FROM bcbg.tblLDB_lmr lmr 
                          LEFT JOIN bcbg.tblLDB_quarter qtr 
                          ON lmr.fy_qtr = qtr.fy_qtr;")

# close connection
dbDisconnect(con_aws)

# cleanup: convert from integer64 to numeric, etc
lmr_data <- lmr_data_db
#print(head(lmr_data))
lmr_data$netsales <- as.numeric(lmr_data$netsales)
lmr_data$litres <- as.numeric(lmr_data$litres)
lmr_data$cat_type <- as.factor(lmr_data$cat_type)
lmr_data$cqtr <- as.factor(lmr_data$cqtr)
lmr_data$cyr <- as.factor(lmr_data$cyr) # also saved as numeric at end below
lmr_data <- lmr_data %>% mutate(
  cyr_qtr = paste(str_sub(cyr, start = 3, end = 4), cqtr, sep = "-")
  )
lmr_data <- lmr_data %>% mutate(
  cat_type = str_replace(cat_type, "Refreshment Beverages", "Refresh Bev"))
# save cyr as numerical value for filtering
lmr_data$cyr_num <- as.numeric(as.character(lmr_data$cyr))

# save data
#write_csv(lmr_data, "data/lmr-data.csv")

# save in top-level data folder - if needed
# clean-up for postgresql upload
#lmr_all$netsales <- as.numeric(lmr_all$netsales)
#lmr_all$litres <- as.numeric(lmr_all$litres)
#lmr_all$created_at <- Sys.time()
#write_csv(lmr_all, here('data', 'tblLDB_lmr.csv'), na = "tblLDB_lmr.csv")
#qtr_all$created_at <- Sys.time()
#write_csv(qtr_all, here('data', 'tblLDB_quarter.csv'), na = "tblLDB_quarter.csv")


