# Working with PostgreSQL database
# 1. table setup - needed only once, when copying from local to AWS RDS
# 2. check on connection, etc
# installed on macbook via homebrew: (brew install postgresql)
#install.packages("RPostgres")
# for local db use pgAdmin to manage the database and make sure server is running

library(RPostgres)
library(tidyverse)
library(glue)

# local connection to postgresql
con <- dbConnect( 
  RPostgres::Postgres(),
  dbname = "fig4_db",
  host = "localhost", 
  port = 5432,
  user = "jy")
# check tables
dbListTables(con)
# disconnect
dbDisconnect(con)

# fetch local data from db for upload 
# open con if previously closed
lmr_pg <- dbGetQuery(con, 
  "SELECT * FROM public.lmr_data;")
lmr_qtr_pg <- dbGetQuery(con,
  "SELECT * FROM public.lmr_quarters;")
dbDisconnect(con)

## upload table data
# Connect to AWS RDS
# get credentials from config.yml
db_config <- config::get()
endpt <- db_config$db_pg$endpt
apwd <- db_config$db_pg$apwd
aport <- db_config$db_pg$aport
user <- db_config$db_pg$user
# make the connection
con_aws <- dbConnect( 
  RPostgres::Postgres(),
  host = endpt, 
  dbname = "fig4_db",
  port = aport,
  user = user,
  password = apwd)

# used to set up tables
#dbWriteTable(con_aws, 
#            "lmr_data", 
#            lmr_pg,
#            append = TRUE)
#dbWriteTable(con_aws, 
#            "lmr_quarters", 
#            lmr_qtr_pg,
#            append = TRUE)
# check data
dbListTables(con)
lmr_pgt <- dbGetQuery(con_aws, 
  "SELECT * FROM public.lmr_data LIMIT 10;")

dbDisconnect(con_aws)
