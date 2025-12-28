# Query cloud db for LMR dataset
library(RPostgres)
library(tidyverse) 
library(here)

cat("load config \n")
# Using config.yml for credentials
# - (alternative to .Renviron file)
# - system will automatically look for config.yml in parent folder
# NOTE: config.yml is in .gitignore for security
db_config <- config::get()
print('connecting to pg db...')
# connect to the database
con_aws <- dbConnect(RPostgres::Postgres(),
                     host=db_config$db_pg$endpt,
                     dbname=db_config$db_pg$adb,
                     user=db_config$db_pg$user,
                     password=db_config$db_pg$apwd,
                     port=db_config$db_pg$aport)
# check underlying tables - for testing
#dbListTables(con_aws)
# main query - filter for 'beer' -> raw data joined with date dimensions
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
                          FROM public.lmr_data lmr 
                          LEFT JOIN public.lmr_quarters qtr 
                          ON lmr.fy_qtr = qtr.fy_qtr
                          WHERE lmr.cat_type = 'Beer';")

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

## RENAME categories ----
  # rename categories for brevity
  beer_data <- lmr_data %>% mutate(
    category = case_when(
      category == "Domestic - BC Beer" ~ "BC",
      category == "Domestic - Other Province Beer" ~ "Other Prov",
      category == "Import Beer" ~ "Import"
    )
  )
  # rename bc subcategories for brevity
  cat("rename bc subcategories for brevity \n")
  beer_data <- beer_data %>% mutate(
    subcategory = case_when(
      subcategory == "Domestic - BC Commercial Beer" ~ "BC Major",
      subcategory == "Domestic - BC Regional Beer" ~ "BC Regional",
      subcategory == "Domestic - BC Micro Brew Beer" ~ "BC Micro",
      subcategory == "Domestic - Other Province Commercial Beer" ~ "Other Prov Major",
      subcategory == "Domestic - Other Province Regional Beer" ~ "Other Prov Reg.",
      subcategory == "Domestic - Other Province Micro Brew Beer" ~ "Other Prov Micro",
      str_detect(subcategory,  "Asia") ~ "Asia",
      str_detect(subcategory, "Europe") ~ "Europe",
      str_detect(subcategory, "Mexico") ~ "Mex/Carib",
      str_detect(subcategory,"USA") ~ "USA",
      str_detect(subcategory, "Other Country") ~ "Other Ctry"
    )
  )
  print(head(beer_data))
