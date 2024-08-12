# test out different table aggregation / filtering options

data <- lmr_data 
# table for sales by year
data_yr <- data %>% group_by(cyr) %>% summarize(netsales = sum(netsales))
# filter yr
yr <- 2024
data_f <- data %>% filter(cyr == yr)
data_yr_f <- data_yr %>% filter(cyr == yr)

q_test <- lmr_data %>% group_by(cyr, cqtr, end_qtr_dt) %>% 
  summarize(netsales = sum(netsales)) %>% ungroup() %>%
  mutate(qoq = (netsales - lag(netsales))/lag(netsales))

# yoy chg depending on number of cats
cats <- c("Beer", "Wine")
yr_test_cat <- lmr_data %>% group_by(cyr, cat_type) %>% 
  summarize(netsales = sum(netsales)) %>% ungroup() %>%
  filter(cat_type %in% cats) %>%
  mutate(yoy = (netsales - lag(netsales, n=length(cats)))/lag(netsales, n=length(cats)))

# save data 
write_csv(lmr_data, "lmr-data.csv")

# ui.R spare code -> original filter setup
# crashed because lmr_data not available for filters at load time
pickerInput(
  inputId = "cyr_picker",
  label = "Select Year(s):",
  choices = unique(lmr_data$cyr),
  selected = unique(lmr_data$cyr),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `selected-text-format` = "count > 3",
    `count-selected-text` = "{0} years selected",
    `live-search` = TRUE
  )
)
# filter for quarters
checkboxGroupInput(inputId = "qtr_check", "Select a quarter", 
                   choices = sort(unique(lmr_data$cqtr)), 
                   selected = unique(lmr_data$cqtr),
                   inline = FALSE
)
# filter for categories
checkboxGroupInput(inputId = "cat_check", "Select a Category", 
                   choices = unique(lmr_data$cat_type), 
                   selected = unique(lmr_data$cat_type),
                   inline = FALSE
)

# db query code
# connect to the database
con_aws <- dbConnect(RMariaDB::MariaDB(),
                     host=endpt,
                     user='admin',
                     password=apwd,
                     port=aport)
lmr_data <- dbGetQuery(con_aws, "SELECT * FROM bcbg.tblLDB_lmr lmr
                           RIGHT JOIN bcbg.tblLDB_quarter qtr ON lmr.fy_qtr = qtr.fy_qtr;")
print(head(lmr_data))
# close connection
dbDisconnect(con_aws)