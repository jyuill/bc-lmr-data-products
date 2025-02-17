# DUCK DB TEST QUERIES

library(DBI) # needed for duckdb
library(duckdb) # needed for duckdb
library(duckplyr) # used for duckdb

## get src data ----
data_src <- source('query.R')
data <- data_src$value

# DuckDB connection
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = TRUE)  # Use in-memory DB
# disconnect when done! also at bottom
#dbDisconnect(con)

# DuckDB table
# set up duckdb table
duckdb::dbWriteTable(con, "duckdb_test", data, overwrite = TRUE)

## test queries ----
test_db <- dbGetQuery(con, "SELECT * FROM duckdb_test LIMIT 10")
test_db2 <- dbGetQuery(con, "SELECT * FROM duckdb_test 
                       WHERE cyr IN (2023,2024) 
                       AND cat_type = 'Beer';")
test_db3 <- dbGetQuery(con, "SELECT cat_type, cyr, SUM(netsales) as sales
                        FROM duckdb_test 
                        WHERE cyr IN (2023,2024) 
                        AND cat_type = 'Beer'
                        GROUP BY cat_type, cyr;")
test_db4 <- dbGetQuery(con, "SELECT cat_type, AVG(sales) as avg_sales 
                        FROM (SELECT cat_type, SUM(netsales) as sales 
                              FROM duckdb_test 
                              WHERE cyr IN (2023,2024)
                              AND cat_type = 'Beer'
                              GROUP BY cat_type, cyr)
                        GROUP BY cat_type;")
test_db5 <- dbGetQuery(con, "WITH yr_sales AS (
                        SELECT cat_type, cyr, SUM(netsales) as sales 
                        FROM duckdb_test 
                        WHERE cyr IN (2023,2024)
                        AND cat_type = 'Beer'
                        GROUP BY cat_type, cyr
                        )
                        SELECT cat_type, AVG(sales) as avg_sales 
                        FROM yr_sales
                        GROUP BY cat_type;")


# DISCONNECT! ----
dbDisconnect(con)