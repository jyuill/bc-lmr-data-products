# Functions for patterns that recur over the categories
# For data manipulation
# DEPRECATED -MAINTAINED FOR REFERENCE: functions moved to lmrtools
# Summary data ----
# -- use for: annual data by category type (beer only in this case)
# -- - includes year-over-year changes in sales and litres
# -- MOVED to lmrtools::aggregate_annual_cat_type
AnnualCatTypeData <- function(dataset, dataset_all=beer_data) {
  cat("fn: AnnualCatTypeData\n")
  # summarize higher level data for % of ttl calculations
  dataset_yr <- dataset_all %>% group_by(cyr) %>% 
    summarize(ttl_sales = sum(netsales),
              ttl_litres = sum(litres),
              max_qtr = max(as.character(cqtr)) # for partial yr flag
            ) %>% 
    mutate(# two yr flags - one for lines, one for points
           yr_flag = ifelse(max_qtr == "Q4", "full", "partial"),
           yr_flag_line = ifelse(yr_flag == "partial", "partial", lead(yr_flag))) %>% 
    ungroup() 
  dataset_yr <- dataset_yr %>% 
    mutate(yr_flag_line = ifelse(is.na(yr_flag_line), "full", yr_flag_line), 
  )
  # summarize current level (cat_type)
  dataset <- dataset %>% group_by(cat_type, cyr) %>% 
              summarize(netsales = sum(netsales),
                        litres = sum(litres)) %>% 
                #ungroup() %>%
              mutate(yoy_sales = (netsales - lag(netsales))/lag(netsales),
                    yoy_litres = (litres - lag(litres))/lag(litres))
  # add percent of totals for each category type
  # - join totals to category data set and calculate percentages
  dataset <- left_join(dataset, dataset_yr, by=c("cyr")) %>%
              mutate(pct_ttl_sales = netsales/ttl_sales,
                     pct_ttl_litres = litres/ttl_litres)
  # add yoy chg calculations for % of total
  dataset <- dataset %>% 
    # convert cyr to number for lag calculation
    mutate(cyr = as.numeric(as.character(cyr))) %>%
    group_by(cat_type) %>% # only need to group for cat_type
    # multiply by 100 to get point values - avoid confusion with %
    mutate(yoy_pcp_ttl_sales = (pct_ttl_sales - lag(pct_ttl_sales))*100,
           yoy_pcp_ttl_litres = (pct_ttl_litres - lag(pct_ttl_litres))*100,
           # creating dummy variable to enable color fill in facet plot
           dummy_fill = 'x') %>% 
      ungroup()
  # reset cyr to factor for plotting
  dataset$cyr <- as.factor(dataset$cyr)
  return(dataset)
}
# Qtr smry data
# -- MOVED to lmrtools::aggregate_qtr_cat_type
QtrData <- function(dataset, n_qtr) {
  cat("fn: Qtr data \n")
  # takes n_qtr from number of quarters selected in input selector for calc yoy lag
  dataset <- dataset %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt) %>%
    summarize(netsales = sum(netsales),
              litres = sum(litres)) %>% ungroup() %>%
    mutate(qoq_sales = (netsales - lag(netsales))/lag(netsales),
           qoq_litres = (litres - lag(litres))/lag(litres),
           # for same qtr prev yr comparisons
           yoy_qoq_sales = (netsales - lag(netsales, n=n_qtr))/lag(netsales, n=n_qtr),
           yoy_qoq_litres = (litres - lag(litres, n=n_qtr))/lag(litres, n=n_qtr),
           yr_qtr = paste(cyr, cqtr, sep = "-")
    )
  return(dataset)
}

# annual category/subcategory data
# use for: 
# - annual data by category within category type
# - subcategory data also -> just needs highest level variable for totals and lowest level for other calcs
# -- MOVED to lmrtools::aggregate_annual_cat_subcat
AnnualCatData <- function(dataset, high_cat, low_cat, dataset_all) {
  cat("fn: AnnualCatData \n")
  # get totals for yr to use in % of total calculations
  # - should not change based on cat filters, since should be consistent % of total
  dataset_yr <- dataset_all %>% group_by(cyr, !!sym(high_cat)) %>% 
    summarize(ttl_sales = sum(netsales),
              ttl_litres = sum(litres),
              max_qtr = max(as.character(cqtr)) # for partial yr flag
            ) %>% mutate(
              yr_flag = ifelse(max_qtr == "Q4", "full", "partial"),
              ) %>% 
    ungroup()
  # summarize current level (category)
  dataset <- dataset %>% 
    group_by(cyr, !!sym(high_cat), !!sym(low_cat)) %>%  
    summarize(netsales = sum(netsales),
              litres = sum(litres)) %>% 
    ungroup()
  # get yoy calculations
  n_lag <- length(unique(dataset[[low_cat]]))
  dataset <- dataset %>% 
    # convert cyr to number for lag calculation
    mutate(cyr = as.numeric(as.character(cyr))) %>% 
    group_by(cyr, !!sym(high_cat), !!sym(low_cat)) %>% ungroup() %>%
    mutate(yoy_sales = (netsales - lag(netsales, n=n_lag))/lag(netsales, n=n_lag),
           yoy_litres = (litres - lag(litres, n=n_lag))/lag(litres, n=n_lag),
          # order cat_type by sales
           category = reorder(category, netsales, FUN = sum)
    ) %>% ungroup()
  # restore cyr to factor
  dataset$cyr <- as.factor(dataset$cyr)
  # add percent of totals for each category
  # - join totals to category data set and calculate percentages
  dataset <- left_join(dataset, dataset_yr, by=c("cyr", high_cat)) %>%
    mutate(pct_ttl_sales = netsales/ttl_sales,
           pct_ttl_litres = litres/ttl_litres)
  # add yoy chg calculations for % of total
  dataset <- dataset %>% 
    group_by(!!sym(low_cat)) %>%
    # multiply by 100 to get point values - avoid confusion with %
    mutate(yoy_pcp_ttl_sales = (pct_ttl_sales - lag(pct_ttl_sales))*100,
           yoy_pcp_ttl_litres = (pct_ttl_litres - lag(pct_ttl_litres))*100) %>% 
    ungroup()
  #print(head(dataset))
  #print(colnames(dataset))
  return(dataset)
}
# Qtr category summary data
# revised Qtr category version with % of total calculations
# - use for: categories, subcategories
# -- MOVED to lmrtools::aggregate_qtr_cat_subcat
QtrCatData2 <- function(dataset, high_cat, low_cat) {
  cat("fn: QtrCatData2 \n")
  # get totals for qtr to use in % of total calculations
  # - should not change based on cat filters, since should be consistent % of total
  dataset_qtr <- dataset %>% group_by(cyr, cyr_qtr, cqtr, !!sym(high_cat)) %>% 
    summarize(ttl_sales = sum(netsales),
              ttl_litres = sum(litres)
            )  %>% ungroup()
  # summarize current level (category)
  n_lag <- length(unique(dataset[[low_cat]]))
  n_qtr <- length(unique(dataset$cqtr))
  dataset <- dataset %>% 
    group_by(cyr, cyr_qtr, cqtr, !!sym(high_cat), !!sym(low_cat),) %>% 
    summarize(netsales = sum(netsales),
              litres = sum(litres)) %>% 
    ungroup() %>%
    mutate(qoq_sales = (netsales - lag(netsales, n=n_lag))/lag(netsales, n=n_lag),
           qoq_litres = (litres - lag(litres, n=n_lag))/lag(litres, n_lag),
           yoy_qoq_sales = (netsales - lag(netsales, n=n_lag*n_qtr))/lag(netsales, n=n_lag*n_qtr),
           yoy_qoq_litres = (litres - lag(litres, n=n_lag*n_qtr))/lag(litres, n=n_lag*n_qtr),
           yr_qtr = paste(cyr, cqtr, sep = "-")
    )
  
  # add percent of totals for each category
  # - join totals to category data set and calculate percentages
  dataset <- left_join(dataset, dataset_qtr, by=c("cyr","cqtr","cyr_qtr", high_cat)) %>%
    mutate(pct_ttl_sales = netsales/ttl_sales,
           pct_ttl_litres = litres/ttl_litres)
  # add qoq chg calculations for change in % of total (market share)
  dataset <- dataset %>% 
    group_by(!!sym(low_cat)) %>% 
    # multiply by 100 to get point values - avoid confusion with %
    # NOT USED at QTR level - may need revising to take acct qtrs filtering
    # not sure how this even works (category but not year/qtr?)
    mutate(qoq_pcp_ttl_sales = (pct_ttl_sales - lag(pct_ttl_sales))*100,
           qoq_pcp_ttl_litres = (pct_ttl_litres - lag(pct_ttl_litres))*100) %>% 
    ungroup()
  #print(head(dataset))
  #print(colnames(dataset))
  return(dataset)
}

# subcategories data
# CONVERT ANY DATA SOURCES USING THIS: SUPERSEDED by AnnualCatData with appropriate parameters
# - use for: annual data by category type and subcategory
# - yoy calcs for subcategory account for multiple categories
AnnualSubCatData <- function(dataset, n_cats, n_subcats, dataset_all) {
  cat("fn: AnnualSubCatData \n")
  # get category totals for yr to use in % of total calculations
  # - should NOT change based on cat filters, since should be consistent % of total
   dataset_yr <- dataset_all %>% group_by(cyr, category) %>% 
                 summarize(ttl_sales = sum(netsales),
                           ttl_litres = sum(litres),
                           max_qtr = max(max_qtr)
                         )  %>% 
                 mutate(
                   yr_flag = ifelse(max_qtr == "Q4", "full", "partial")
                 ) %>% ungroup()
  
  # summarize by subcategory by year
  dataset <- dataset %>% group_by(category, subcategory, cyr) %>% 
    summarize(netsales = sum(netsales),
              litres = sum(litres)) %>% 
      ungroup() 
  # add yoy chg calculations with lag for number of subcategories
  n_lag <- n_subcats
  dataset <- dataset %>%
    # convert cyr to number for lag calculation
    mutate(cyr = as.numeric(as.character(cyr))) %>%
    # group by cat_type, category, subcategory for yoy calcs
    group_by(category, subcategory) %>% ungroup() %>%
    mutate(yoy_sales = (netsales - lag(netsales, n=n_lag))/lag(netsales, n=n_lag),
           yoy_litres = (litres - lag(litres, n=n_lag))/lag(litres, n=n_lag),
           subcategory = reorder(subcategory, netsales, FUN = sum)
    ) %>% ungroup()
  # restore cyr as factor
  dataset$cyr <- as.factor(dataset$cyr)
  # add percent of totals for each category
  # - join totals to category data set and calculate percentages
  dataset <- left_join(dataset, dataset_yr, by=c("cyr","category","subcategory")) %>%
    mutate(pct_ttl_sales = netsales/ttl_sales,
           pct_ttl_litres = litres/ttl_litres) %>% ungroup()
  # add yoy chg calculations for % of total
  dataset <- dataset %>% 
    # convert cyr to number for lag calculation
    mutate(cyr = as.numeric(as.character(cyr))) %>%
    # group by subcategory for yoy calcs
    group_by(subcategory) %>%
    # multiply by 100 to get point values - avoid confusion with %
    mutate(yoy_pcp_ttl_sales = (pct_ttl_sales - lag(pct_ttl_sales, n=1))*100,
           yoy_pcp_ttl_litres = (pct_ttl_litres - lag(pct_ttl_litres, n=1))*100) %>% 
    ungroup()
  #print(head(dataset))
  return(dataset)
}

# CONVERT DATA SOURCES: Superseded by using QtrCatData2 with appropriate parameters
QtrSubCatData <- function(dataset, n_cats = 1, n_subcats = 3, n_qtr = 4, dataset_all) {
  cat("fn: QtrSubCatData \n")
  # get category totals for qtr to use in % of total calculations
  # - data is pre-filtered for category = 'BC' 
  # - should not change based on cat filters, since should be consistent % of total
  dataset_qtr <- dataset_all %>% group_by(category, cyr, end_qtr_dt, cyr_qtr) %>% 
    summarize(ttl_sales = sum(netsales),
              ttl_litres = sum(litres)
            )  %>% ungroup()
  # summarize current level (subcategory)
  n_lag <- n_subcats
  dataset <- dataset %>% 
    group_by(cyr, cyr_qtr, end_qtr_dt, category, subcategory) %>% 
    summarize(netsales = sum(netsales),
              litres = sum(litres)) %>% 
    ungroup() %>%
    mutate(qoq_sales = (netsales - lag(netsales, n=n_lag))/lag(netsales, n=n_lag),
           qoq_litres = (litres - lag(litres, n=n_lag))/lag(litres, n_lag),
           yoy_qoq_sales = (netsales - lag(netsales, n=n_lag*n_qtr))/lag(netsales, n=n_lag*n_qtr),
           yoy_qoq_litres = (litres - lag(litres, n=n_lag*n_qtr))/lag(litres, n=n_lag*n_qtr),
           yr_qtr = paste(cyr, cqtr, sep = "-")
    )
  
  # add percent of totals for each category
  # - join totals to category data set and calculate percentages
  dataset <- left_join(dataset, dataset_qtr, by=c("category","cyr","cyr_qtr","end_qtr_dt")) %>%
    mutate(pct_ttl_sales = netsales/ttl_sales,
           pct_ttl_litres = litres/ttl_litres)
  # add yoy chg calculations for % of total (market share)
  dataset <- dataset %>% 
    group_by(category, subcategory) %>%
    # multiply by 100 to get point values - avoid confusion with %
    # NOT USED at QTR level -> may need revising to take acct qtrs filtering
    mutate(qoq_pcp_ttl_sales = (pct_ttl_sales - lag(pct_ttl_sales))*100,
           qoq_pcp_ttl_litres = (pct_ttl_litres - lag(pct_ttl_litres))*100) %>% 
    ungroup()
  #print(head(dataset))
  #print(colnames(dataset))
  return(dataset)
}


