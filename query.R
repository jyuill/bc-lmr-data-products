
library(tidyverse) 
library(RMariaDB) ## best way to access MySQL from R
library(dotenv)

# create a .env file in the root directory of the project
# and add the following lines
# ENDPT="...rds.amazonaws.com"
# APWD="A...KOCX"
# APORT=3..6  

## Load the .env file
#dotenv::load_dot_env("../.env") # for top-level .env
#dotenv::load_dot_env("lmr-data/.env") # for local .env
#print("loading .env")
#dotenv::load_dot_env() # for top-level .env
## Load the environment variables
#endpt <- Sys.getenv("ENDPT")
#apwd <- Sys.getenv("APWD")
#aport <- as.numeric(Sys.getenv("APORT"))

# from config.yml
# Load the config.yml from the shiny_app subfolder
#config_file_path <- file.path("lmr-data", "config.yml")
#db_config <- config::get(file = config_file_path, config = "db")
#endpt <- db_config$db$endpt
#apwd <- db_config$db$apwd
#aport <- db_config$db$aport
#user <- db_config$db$user

cat("get db_config \n")
db_config <- config::get(config = "db")
#print(db_config)
endpt <- db_config$db$endpt
apwd <- db_config$db$apwd
aport <- db_config$db$aport
user <- db_config$db$user

print('connecting to db')
# connect to the database
con_aws <- dbConnect(RMariaDB::MariaDB(),
                     host=endpt,
                     user='admin',
                     password=apwd,
                     port=aport)
# test query
#t_data <- dbGetQuery(con_aws, "SELECT * FROM bcbg.tblLDB_lmr LIMIT 10")
#q_data <- dbGetQuery(con_aws, "SELECT * FROM bcbg.tblLDB_quarter LIMIT 10")
# main query - all the data -> raw data joined with date dimensions
lmr_data_db <- dbGetQuery(con_aws, "SELECT * FROM bcbg.tblLDB_lmr lmr
                           RIGHT JOIN bcbg.tblLDB_quarter qtr ON lmr.fy_qtr = qtr.fy_qtr;")
lmr_data <- lmr_data_db
#print(head(lmr_data))
# close connection
print('closing connection')
dbDisconnect(con_aws)

# cleanup: convert from integer64 to numeric, etc
lmr_data$netsales <- as.numeric(lmr_data$netsales)
lmr_data$litres <- as.numeric(lmr_data$litres)
lmr_data$cat_type <- as.factor(lmr_data$cat_type)
lmr_data$cqtr <- as.factor(lmr_data$cqtr)
lmr_data$cyr <- as.factor(lmr_data$cyr) # also saved as numeric at end below
# appears that there is process in qtrly update that results in duplicating fy_qtr column
# - prior to Dec 2024, showed up as fy_qtr..8 and was removed below
# lmr_data <- lmr_data %>% select(-c(fy_qtr..8))
lmr_data <- lmr_data %>% select(-8)
lmr_data <- lmr_data %>% mutate(
  cyr_qtr = paste(str_sub(cyr, start = 3, end = 4), cqtr, sep = "-")
  )
lmr_data <- lmr_data %>% mutate(
  cat_type = str_replace(cat_type, "Refreshment Beverages", "Refresh Bev"))
# save cyr as numerical value for filtering
lmr_data$cyr_num <- as.numeric(as.character(lmr_data$cyr))

# save data
#write_csv(lmr_data, "data/lmr-data.csv")

return(lmr_data)


