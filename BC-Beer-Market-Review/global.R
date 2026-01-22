# global.R - for global variables accessed by both ui.R and server.R

library(shiny)
library(tidyverse)
library(shinyjs)
library(shinyWidgets) # For pickerInput
library(lmrtools) # custom pkg with lmr functions for data fetch etc

# 1. Load Data from Database and Pre-process ----
# query database with lmrtools pkg
# postgresql as of Jun 2025
beer_data <- fetch_lmr_complete_filter(replace=TRUE, cat_type='Beer')

## pre-process/set up ----
  # recent data - apply to yr filter as default to avoid over-crowding
  yr_max <- max(beer_data$cyr_num) # get current latest yr
  yrs_back <- 6 # determine how many yrs back to go
  data_recent <- beer_data %>% filter(cyr_num > yr_max-yrs_back)
  max_date <- max(beer_data$end_qtr_dt)
  # for top of sidebar on pg, set in dynamic sidebar
  max_date_note <- paste0("Data as of: ", format(max_date, "%b %d %Y"))
  # bc subcategories
  bc_subcats <- unique(beer_data$subcategory[beer_data$category == "BC"])

# functions & support files ----
# load functions used: data manipulation and plots
# - if running server.R will load files from relative path
# - if running manually, need to adjust full path
files <- c('functions_data.R', 'functions_plots.R', 'support_vars.R')
for (f in files) {
  if(file.exists(f)) {
    source(f)
  } else {
    source(here::here("BC-Beer-Market-Review", f))
  }
}
#source('functions_data.R')
#source('functions_plots.R')
# load support variables for plots etc
#source('support_vars.R')

# 2. Define Static UI Elements ----
# (with placeholder choices = NULL)
# These objects are now available in ui.R and server.R

## annual vs qtr grain filter ----
dynamic_grain <- radioButtons(inputId = "grain_check", 
                                label = "Select Grain:", 
                                choices = c("Annual", "Quarterly"), 
                                selected = "Annual",
                                inline = FALSE
)
## year filter ----
dynamic_cyr <- pickerInput(
    inputId = "cyr_picker",
    label = "Select Calendar Year(s):",
    choices = NULL, # Placeholder
    multiple = TRUE,
    options = list(
      `actions-box` = TRUE,
      `selected-text-format` = "count > 3",
      `count-selected-text` = "{0} years selected",
      `live-search` = TRUE
    )
)
## qtr filters ----
dynamic_qtr <- checkboxGroupInput(inputId = "qtr_check", 
                                    label = "Select Quarter:", 
                                    choices = NULL, # Placeholder
                                    inline = FALSE
)
## source/cat filters ----
dynamic_beer_cat <- checkboxGroupInput(inputId = "beer_cat_check", 
                                    label = "Select Source:", 
                                    choices = NULL, # Placeholder
                                    inline = FALSE
)
## BC subcategory filters ----
dynamic_beer_bc_subcat <- checkboxGroupInput(inputId = "beer_bc_subcat_check", 
                                             label = "Select BC category (select only BC above):", 
                                             choices = NULL, # Placeholder
                                             inline = FALSE
)
