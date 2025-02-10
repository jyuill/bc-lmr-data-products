#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
cat("load libraries \n")
library(shiny)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)
library(here)
library(bslib)
library(RColorBrewer)
library(treemap)
library(treemapify)

scipen <- options(scipen=999) # suppress scientific notation

### colors etc ----
# set plot theme
# - under 'plots' below
# set color palette
# - bar and line colors
bar_col <- brewer.pal(n=9, name='YlGnBu')[9] # #081D58
# - palette for use with categories
bpal <- brewer.pal(n=9, name="YlOrRd")
cat_type_color <- c("Beer"=bpal[6], "Refresh Bev"=bpal[3], "Spirits"=bpal[4], "Wine"=bpal[8])
# - palette for quarters
qpal <- brewer.pal(n=9, name="Blues")
qtr_color <- c("Q1"=qpal[5], "Q2"=qpal[7], "Q3"=qpal[8], "Q4"=qpal[9])
# palette for beer categories
beer_pal <- brewer.pal(n=11, name="RdYlGn")
beer_cat_color <- c("BC"=beer_pal[11], "Other Prov"=beer_pal[7], "Import"=beer_pal[9])
beer_bc_cat_color <- c("BC Major"=beer_pal[11], "BC Regional"=beer_pal[10], "BC Micro"=beer_pal[9])
refresh_cat_color <- c("Cider"=beer_pal[10], "Coolers"=beer_pal[11])
spirits_cat_color <- brewer.pal(n=12, name="Paired")
# for wine categories, generate a custom color palette with 24 colors
wine_cat_color <- colorRampPalette(brewer.pal(n=12, name="Paired"))(24)

# drop incomplete calendar year at start
#tbl_yq <- table(lmr_data$cyr, lmr_data$cqtr)
#if(any(tbl_yq[1,] == 0)) {
#  lmr_data <- lmr_data %>% filter(cyr != rownames(tbl_yq)[1])
#}

# load functions used - mostly plots
source('functions.R')

# Define server logic
function(input, output, session) {
  # experiment with different bs themes
  #bslib::bs_themer()
  # toggle sidebar ----
  # for toggling sidebar, using shinyjs 
  # - NOT USED - clunky
  # - makes sidebar disappear / appear but mainPanel doesn't expand - pointless
  observeEvent(input$toggleSidebar, {
    toggle("sidebar")
  })
  # get data ----
  # query database via separate file for tidyness
  ## all data ----
  source('query.R')
  ## recent data ----
  # apply to yr filter as default to avoid over-crowding
  lmr_max <- max(lmr_data$cyr_num) # get current latest yr
  lmr_yrs <- 6 # determine how many yrs back to go
  lmr_recent <- lmr_data %>% filter(cyr_num > lmr_max-lmr_yrs)
  lmr_max_date <- max(lmr_data$end_qtr_dt)
  # for top of sidebar on pg, set in dynamic sidebar
  lmr_max_note <- paste0("Data as of: ", format(lmr_max_date, "%b %d %Y"))
  ## RENAME categories ----
  ## beer ----
  beer_data <- lmr_data %>% filter(cat_type == "Beer")
  # rename categories for brevity
  beer_data <- beer_data %>% mutate(
    category = case_when(
      category == "Domestic - BC Beer" ~ "BC",
      category == "Domestic - Other Province Beer" ~ "Other Prov",
      category == "Import Beer" ~ "Import"
    )
  )
  # rename bc subcategories for brevity
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
  ## refresh bev ----
  refresh_data <- lmr_data %>% filter(cat_type == "Refresh Bev")
  # rename categories for brevity
  refresh_data <- refresh_data %>% mutate(
    subcategory = case_when(
      str_detect(subcategory,  "Domestic") ~ "Domestic",
      str_detect(subcategory, "Import") ~ "Import",
      str_detect(subcategory, "Malt") ~ "Malt-Based",
      str_detect(subcategory,"Spirit") ~ "Spirit",
      str_detect(subcategory, "Wine") ~ "Wine/Fruit"
    )
  )
  ## spirits ----
  spirits_data <- lmr_data %>% filter(cat_type == "Spirits")
  # rename categories for brevity
  spirits_data <- spirits_data %>% mutate(
    category = case_when(
      str_detect(category,  "Asian") ~ "Asian",
      str_detect(category, "Other") ~ "Other",
      str_detect(category, "Grape and Fruit") ~ "Brandy",
      str_detect(category,"Liqueurs") ~ "Liqueur",
      # default
      TRUE ~ category
    )
  )
  
  ## wine ----
  wine_data <- lmr_data %>% filter(cat_type == "Wine")
  # rename categories for brevity
  wine_data <- wine_data %>% mutate(
    category = case_when(
      str_detect(category,  "New Zealand") ~ "NZ",
      str_detect(category, "Australia") ~ "Austrl",
      str_detect(category, "Canada") ~ str_replace(category, "Canada","Can"),
      str_detect(category, "Other") ~ "Other",
      str_detect(category, "Wine") ~ str_replace(category, "Wine", ""),
      # default
      TRUE ~ category
    )
  )
  
  # setup filters ----------------------------------------------------
  # Dynamically generate UI filters based on lmr_data
  # otherwise, app will crash because lmr_data not available for filters in ui.R
  # CHATGPT suggestion as alt to below
  ## year filter ----
  dynamic_cyr <- pickerInput(
    inputId = "cyr_picker",
    label = "Select Calendar Year(s):",
    choices = unique(lmr_data$cyr),
    selected = unique(lmr_recent$cyr),
    multiple = TRUE,
    options = list(
      `actions-box` = TRUE,
      `selected-text-format` = "count > 3",
      `count-selected-text` = "{0} years selected",
      `live-search` = TRUE
    )
  )
  ## qtr filters ----
  dynamic_qtr <- checkboxGroupInput(inputId = "qtr_check", "Select a quarter", 
                                    choices = sort(unique(lmr_data$cqtr)), 
                                    selected = unique(lmr_data$cqtr),
                                    inline = FALSE
  )
  ## cat filters ----
  dynamic_cat <- checkboxGroupInput(inputId = "cat_check", "Select a Category", 
                                    choices = unique(lmr_data$cat_type), 
                                    selected = unique(lmr_data$cat_type),
                                    inline = FALSE
  )
  dynamic_beer_cat <- checkboxGroupInput(inputId = "beer_cat_check", "Select a Category", 
                                    choices = unique(beer_data$category), 
                                    selected = unique(beer_data$category),
                                    inline = FALSE
  )
  dynamic_refresh_cat <- checkboxGroupInput(inputId = "refresh_cat_check", "Select a Category", 
                                         choices = unique(refresh_data$category), 
                                         selected = unique(refresh_data$category),
                                         inline = FALSE
  )
  dynamic_spirits_cat <- checkboxGroupInput(inputId = "spirits_cat_check", "Select a Category", 
                                            choices = unique(spirits_data$category), 
                                            selected = unique(spirits_data$category),
                                            inline = FALSE
  )
  dynamic_wine_cat <- checkboxGroupInput(inputId = "wine_cat_check", "Select a Category", 
                                         choices = unique(wine_data$category), 
                                         selected = unique(wine_data$category),
                                         inline = FALSE
  )
  # drop-down picker: too many categories to shown in checkbox
  dynamic_wine_cat_picker <- pickerInput(
    inputId = "wine_cat_picker",
    label = "Select a Category:",
    choices = unique(wine_data$category),
    selected = unique(wine_data$category),
    multiple = TRUE,
    options = list(
      `actions-box` = TRUE,
      `selected-text-format` = "count > 3",
      `count-selected-text` = "{0} categories selected",
      `live-search` = TRUE
    )
    )
    ## additional drop-down for showing category charts
    ## clunky but used to limit category charts while allowing for 
    #  totals to be filtered by all categories
    # get top 10 wine categories by sales
    wine_cat_top <- wine_data %>% group_by(category) %>% 
     summarize(netsales = sum(netsales)) %>% ungroup() %>%
     arrange(desc(netsales)) %>% head(8)
    
    dynamic_wine_cat_chart_picker <- pickerInput(
      inputId = "wine_cat_chart_picker",
      label = "Select for Cat. Charts:",
      choices = unique(wine_data$category),
      selected = unique(wine_cat_top$category),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 3",
        `count-selected-text` = "{0} cat. selected",
        `live-search` = TRUE
      )
    )
  
  # CHATGPT: apply dynamic filters as needed to different tabs, based on selection
  # Dynamic Sidebar ----
  output$dynamic_sidebar <- renderUI({
    if (input$tabselected == 1) {
      tagList(
        tags$p(lmr_max_note, class="note"),
        dynamic_cyr,
        dynamic_qtr,
        dynamic_cat,
        tags$h4("Contents"),
        tags$a(href="#ttl_sales", "Ttl Sales by Yr & Qtr"),tags$br(),
        tags$a(href="#cat_sales", "Category Sales: Yr & Qtr"),tags$br(),
        tags$br(),
        tags$h4("Notes"),
        tags$p("Years & Quarters are calendar yr, not LDB fiscal year")
      )
    } else if (input$tabselected == 2) {
      tagList(
        tags$p(lmr_max_note, class="note"),
        dynamic_cyr,
        dynamic_qtr,
        dynamic_beer_cat,
        tags$h4("Contents"),
          tags$a(href="#beer_sales", "Ttl Sales by Yr & Qtr"),tags$br(),
          tags$a(href="#bsrc_sales", "Sales by Source"), tags$br(),
          tags$a(href="#bcat_sales", "BC Beer by Category"), tags$br(),
          tags$a(href="#bimp_sales","Import Sales by Ctry"), tags$br()
      )
    } else if (input$tabselected == 3) {
      tagList(
        tags$p(lmr_max_note, class="note"),
        dynamic_cyr,
        dynamic_qtr,
        dynamic_refresh_cat,
        tags$h4("Contents"),
          tags$a(href="#refresh_sales", "Ttl Sales by Yr & Qtr"),tags$br(),
          tags$a(href="#refresh_cat_sales", "Category Sales"), tags$br()
      )
    } else if (input$tabselected == 4) {
      tagList(
        tags$p(lmr_max_note, class="note"),
        dynamic_cyr,
        dynamic_qtr,
        dynamic_spirits_cat,
        tags$h4("Contents"),
          tags$a(href="#spirits_sales", "Ttl Sales by Yr & Qtr"),tags$br(),
          tags$a(href="#spirits_cat_sales", "Category Sales"), tags$br()
      )
    } else if (input$tabselected == 5) {
      tagList(
        tags$p(lmr_max_note, class="note"),
        dynamic_cyr,
        dynamic_qtr,
        dynamic_wine_cat_picker,
        dynamic_wine_cat_chart_picker,
        tags$h4("Contents"),
          tags$a(href="#wine_sales", "Ttl Sales by Yr & Qtr"),tags$br(),
          tags$a(href="#wine_cat_sales", "Category Sales"), tags$br(),
          tags$a(href="#wine_treemaps", "Regions & Types"), tags$br()
      )
    } else if (input$tabselected == 6) {
      tagList(
        tags$h4("Contents"),
          tags$a(href="#about", "What is this?"),tags$br(),
          tags$a(href="#who", "Who did this?"),tags$br(),
          tags$a(href="#release", "Release Notes"),tags$br(),
          tags$br()
      )
    }
  })
  # TOTALS --------------------------------------------------------------
  # apply filters to data ---------------------------------------------------
  cat("01 apply filters \n")
  # Filter the data set based on the selected categories
  filtered_data <- reactive({
    req(input$cyr_picker, input$qtr_check, input$cat_check)
    lmr_data %>% filter(cyr %in% input$cyr_picker) %>%
      filter(cqtr %in% input$qtr_check) %>%
      filter(cat_type %in% input$cat_check)
  })
  cat("02 aggregate annual & qtr totals \n")
  # Aggregate data ----
  # annual and qtr totals ---------------------------------------------------
  annual_data <- reactive({
    filtered_data() %>% group_by(cyr) %>%
    summarize(netsales = sum(netsales),
              litres = sum(litres)) %>%
    mutate(yoy_sales = (netsales - lag(netsales))/lag(netsales),
           yoy_litres = (litres - lag(litres))/lag(litres))
  })
  qtr_data <- reactive({
    filtered_data() %>% group_by(cyr, cqtr, cyr_qtr, end_qtr_dt) %>%
      summarize(netsales = sum(netsales)) %>% ungroup() %>%
      mutate(qoq = (netsales - lag(netsales))/lag(netsales),
             yr_qtr = paste(cyr, cqtr, sep = "-")
      )
    # for testing
    #qtr_data <- lmr_data %>% group_by(cyr, cqtr, cyr_qtr, end_qtr_dt) %>% 
    #  summarize(netsales = sum(netsales)) %>% ungroup() %>%
    #  mutate(qoq = (netsales - lag(netsales))/lag(netsales))
  })
  # agg by cat -------------------------------------------------------------------
  ## annual data by cat
  cat('03 aggregate annual data by cat \n')
  annual_data_cat <- reactive({
    AnnualCatTypeData(filtered_data())
  })
  # test
  #ancattype <- AnnualCatTypeData(lmr_data)
  
  qtr_data_cat <- reactive({
    # need to base the qoq on the number of cats chosen in filter
    n_qtr <- length(input$qtr_check)
    n_cats <- length(input$cat_check)
    filtered_data() %>% group_by(cyr, cqtr, cyr_qtr, end_qtr_dt, cat_type) %>%
      summarize(netsales = sum(netsales)) %>% ungroup() %>%
      mutate(qoq = (netsales - lag(netsales, n = n_cats))/lag(netsales, n = n_cats))
  })
  # PLOT THEMES--------------------------------------------------------------------
  ## ggplot themes ----
  theme_set(theme_light()+theme(panel.grid.minor = element_blank(),
                                panel.grid.major = element_line(color = 'grey90', linewidth=0.1)))
  # x-axis text - set angle and other formats
  theme_xax <- theme(axis.ticks.x = element_blank(),
                   axis.text.x = element_text(angle = 90, hjust = 1))
  theme_xaxq <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
  # no legend
  theme_nleg <- theme(legend.position = "none")
  # facet chart spacing 
  theme_facet <- theme(panel.spacing.y = unit(0.1,"lines"))
  # customize tooltip format
  text_bn <- aes(text=paste0(cyr, ": ", scales::dollar(netsales, scale = 1e-9, suffix = "B")))
  text_bn <- aes(text = paste0(cyr, ": ", label_currency(scale = 1e-9, suffix = "B")(netsales)))
  text_m <- aes(text=paste0(cyr, ": ", scales::dollar(netsales, scale = 1e-6, suffix = "M")))
  text_mq <- aes(text=paste0(cqtr, ": ", scales::dollar(netsales, scale = 1e-6, suffix = "M")))
  text_mcat <- aes(text=paste0(cat_type, ": ", scales::dollar(netsales, scale = 1e-6, suffix = "M")))
  text_pc <- aes(text = paste0(cyr, ": ", label_percent(accuracy = 0.1)(yoy_sales)))
  ## plotly layouts ----
  layout_legend_hc <- list(
    orientation = "h",     # Horizontal legend
    x = 0.5,               # Center legend horizontally
    xanchor = "center",    # Align legend center with x position
    y = 1,                 # Place legend at the top
    yanchor = "bottom",    # Align legend bottom with y position
    title = list(text = "")  # Remove legend title
  )
  layout_legend_vr <- list(orientation = "v",  # Vertical legend
                       x = 1.2,           # Position on the x-axis
                       y = 1,
                       yanchor='top',
                       itemheight = 15)          # Position on the y-axis
  ## plot titles ----
  # consistent titles to apply across same charts
  yr_sales_cat <-  "Yrly Sales by Category"
  yr_sales_pc_cat <-  "% of Ttl Sales by Category"
  yr_sales_pc_chg_cat <- "Yrly % Chg Sales by Category"
  yr_sales_pcpt_chg_cat <-  "Yrly % Pt Chg Sales % of Ttl"
  # litre sales
  yr_litre_cat <- "Yrly Litres by Category"
  yr_litre_pc_cat <- "% of Ttl Litres by Category"
  yr_litre_pc_chg_cat <- "Yrly % Chg Litres by Category"
  yr_litre_pcpt_chg_cat <- "Yrly % Pt Chg Litres % of Ttl"
  
    # $ SALES ---------------------------------------------------------------
    ## beer: apply filters to data ---------------------------------------------------
    cat("01 apply beer filters \n")
    ## 1. Filter the data set based on the selected categories ----
    beer_filtered_data <- reactive({
      req(input$cyr_picker, input$qtr_check, input$beer_cat_check)
      beer_data
      beer_data %>% filter(cyr %in% input$cyr_picker) %>%
        filter(cqtr %in% input$qtr_check) %>%
        filter(category %in% input$beer_cat_check)
    })
    cat("02 aggregate annual & qtr totals \n")
    ## 2. annual and qtr totals ---------------------------------------------------
    #beer_annual_data <- reactive({
    #   beer_filtered_data() %>% group_by(cat_type, cyr) %>%
    #    summarize(netsales = sum(netsales)) %>%
    #    mutate(yoy = (netsales - lag(netsales))/lag(netsales))
    #})
    beer_annual_data <- reactive({
      AnnualCatTypeData(beer_filtered_data())
    })
    beer_qtr_data <- reactive({
      #beer_filtered_data() %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt) %>%
      #  summarize(netsales = sum(netsales)) %>% ungroup() %>%
      #  mutate(qoq = (netsales - lag(netsales))/lag(netsales),
      #         yr_qtr = paste(cyr, cqtr, sep = "-")
      #  )
      QtrData(beer_filtered_data(), length(input$qtr_check))
    })
    ## PLOTS ----
    ### sales - yr, qtr ----
    # similar to overview but using functions to get plot, providing:
    # - chart_title, dataset, bar col variable, list of theme modifications
    # - for themes, can list joined by '+'
    output$beer_sales_yr <- renderPlotly({
      TtlChart("Net Beer $ Sales by Year", 
               beer_annual_data(), 'cyr', 'netsales', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M")
    })
    ## plot sales by quarter
    output$beer_sales_qtr <- renderPlotly({
      TtlChart("Net Beer $ Sales by Qtr", 
               beer_qtr_data(), 'cyr_qtr', 'netsales', 'cqtr', qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "M")
    })
    
    ### change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$beer_sales_yoy <- renderPlotly({
      PoPChart("% Chg Beer Sales - Yr", beer_annual_data(), "cyr", "yoy_sales", "cat_type", bar_col, 
               theme_xax+theme_nleg, "%")
    })
    output$beer_sales_qoq <- renderPlotly({
      PoPChart("% Chg Beer Sales - Qtr", beer_qtr_data(), "cyr_qtr", "qoq_sales", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    ## CAT - origin ----
    ## data by cat ----
    ## yoy = $ SALES ONLY
    beer_annual_data_cat <- reactive({
      n_cats <- length(input$beer_cat_check)
      AnnualCatData(beer_filtered_data(), beer_data)
    })
    # test annual category data
    n_cats <- length(unique(beer_data$category))
    beer_annual_test <- AnnualCatData(beer_data, beer_data)
    
    # qtr
    beer_qtr_data_cat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_qtr <- length(input$qtr_check)
      n_cats <- length(input$beer_cat_check)
      QtrCatData(beer_filtered_data(), n_cats, n_qtr)
      #beer_filtered_data() %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt, category) %>%
      #  summarize(netsales = sum(netsales)) %>% ungroup() %>%
      #  mutate(qoq = (netsales - lag(netsales, n = n_cats))/lag(netsales, n = n_cats))
    })
    ## PLOTS by category (source / origin) ----
    output$beer_sales_yr_cat <- renderPlotly({
      CatChart("Yrly Beer Sales by Source", 
               beer_annual_data_cat(), "cyr", "netsales","category", 
               beer_cat_color, "stack",
               theme_xax,"M")
    })
    output$beer_sales_yr_cat_pc <- renderPlotly({
      CatChart("Yrly % Sales by Source", 
               beer_annual_data_cat(), "cyr", "pct_ttl_sales","category", 
               beer_cat_color, "fill",
               theme_xax,tunits="%")
    })
    # qtr - abandoned in favour of % of annual ttl
    output$beer_sales_qtr_cat <- renderPlotly({
      CatChart("Qtrly Beer Sales by Source", 
               beer_qtr_data_cat(), "cyr_qtr", "netsales", "category", beer_cat_color, "stack",
               theme_xax+theme_xaxq, "M")
    })
    ### facet: % change by source ----
    output$beer_sales_yoy_cat_chg <- renderPlotly({
      x <- beer_annual_data_cat()
      CatChgChart("Yrly % Chg $ Sales by Src.", 
               x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales",
               fill_var = "cat_type", 
               facet_var = "category",
               fill_color = bar_col, 
               strp_color = bar_col,
               theme_xax+theme_nleg)
    })
    
    # % point chg by src yoy
    output$beer_sales_yoy_cat_chg_pt <- renderPlotly({
      x <- beer_annual_data_cat()
      CatChgChart("Yrly % Pt Chg in Share", 
               x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales", 
               fill_var = "cat_type", 
               facet_var = "category",
               fill_color = bar_col, 
               strp_color = bar_col,
               theme_xax+theme_nleg, tunits = "num")
    })
    # qtr % chg by src - abandoned in favour of % of annual total
    output$beer_sales_qoq_cat_chg <- renderPlotly({
      x <- beer_qtr_data_cat()
      CatChgChart("Qtrly % Chg Beer Sales by Source", 
               x, x_var = "cyr_qtr", y_var = "qoq_sales", sort_var = "yoy_pcp_ttl_sales", 
               fill_var = "cqtr", 
               facet_var = "category",
               fill_color = qtr_color, 
               strp_color = bar_col,
               theme_xax+theme_xaxq+theme_nleg)
    })
    
    ## SUBCAT data ----
    # annual
    beer_yr_data_subcat <- reactive({
      cat("beer_subcat \n")
      n_cats <- length(unique(beer_filtered_data()$category))
      n_subcats <- length(unique(beer_filtered_data()$subcategory))
      AnnualSubCatData(beer_filtered_data(), n_cats, n_subcats, beer_annual_data_cat())
    })
    # test
    n_cats <- length(unique(beer_data$category))
    n_subcats <- length(unique(beer_data$subcategory))
    beer_annual_test <- AnnualSubCatData(beer_data, n_cats, n_subcats, beer_data)
    ## bc beer subcat ----
    ## PLOTS by BC subcategory ----
    output$beer_sales_yr_bc_cat <- renderPlotly({
      #data <- beer_bc_yr_subcat()
      data <- beer_yr_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChart("Beer Sales $ by BC Category", 
               data, "cyr", "netsales","subcategory", beer_bc_cat_color, "stack", theme_xax, "M")
    })
    ## plot % of total sales by bc subcategory
    output$beer_sales_yr_bc_cat_pc <- renderPlotly({
      data <- beer_yr_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      #data <- beer_bc_yr_subcat()
      CatChart("Beer Sales % by BC Category", 
               data, "cyr", "pct_ttl_sales","subcategory", beer_bc_cat_color, "fill", 
               theme_xax, "%")
    })
    ## plots for bc beer subcategory yoy change in facets
    output$beer_sales_yoy_bc_cat_chg <- renderPlotly({
      x <- beer_yr_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChgChart("Yrly % Chg Beer Sales by BC Category", 
               x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales", 
               fill_var = "cat_type", 
               facet_var = "subcategory",
               fill_color = bar_col, 
               strp_color = bar_col,
               theme_xax+theme_nleg)
    })
    ## % point chg by bc subcat yoy
    output$beer_sales_yoy_bc_cat_chg_pt <- renderPlotly({
      x <- beer_yr_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChgChart("Yrly % Pt Chg in Share", 
               x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales",
               fill_var = "cat_type", 
               facet_var = "subcategory",
               fill_color = bar_col, 
               strp_color = bar_col,
               theme_xax+theme_nleg, tunits = "num")
    })
    ## import beer subcat ----
    
    ## plots by import country/region
    output$beer_sales_yr_import_cat <- renderPlotly({
      cat('beer_import chart \n')
      data <- beer_yr_data_subcat() %>% filter(category=='Import')
      print(data)
      CatChart("Beer $ by Import Ctry/Reg", 
               data, "cyr", "netsales","subcategory", beer_pal, "stack", 
               theme_xax, "M") %>%
        layout(legend = layout_legend_vr)
    }) 

    output$beer_sales_yr_import_cat_pc <- renderPlotly({
      cat('beer_import_pc chart \n')
      data <- beer_yr_data_subcat() %>% filter(category=='Import')
      CatChart("Beer $ % by Import Ctry/Reg", 
               data, "cyr", "pct_ttl_sales","subcategory", beer_pal, "fill", 
               theme_xax, "%") %>%
        layout(legend = layout_legend_vr)
    }) 
    
    ## plots for import beer subcategory yoy change in facets
    output$beer_sales_yoy_import_cat_chg <- renderPlotly({
      x <- beer_yr_data_subcat() %>% filter(category=='Import')
      CatChgChart(yr_sales_pc_chg_cat, 
               x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales", 
               fill_var = "cat_type", 
               facet_var = "subcategory",
               fill_color = bar_col, 
               strp_color = bar_col,
               theme_xax+theme_nleg)
    })
    ## % point chg by import subcat yoy
    output$beer_sales_yoy_import_cat_chg_pt <- renderPlotly({
      x <- beer_yr_data_subcat() %>% filter(category=='Import')
      CatChgChart(yr_sales_pcpt_chg_cat, 
               x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales", 
               fill_var = "cat_type", 
               facet_var = "subcategory",
               fill_color = bar_col, 
               strp_color = bar_col,
               theme_xax+theme_nleg, tunits = "num")
    })
    
    ## LITRES ----
    ## beer: apply filters to data ---------------------------------------------------
    cat("01 apply beer filters \n")
    ## 1. Filter the data set based on the selected categories ----
    beer_filtered_data <- reactive({
      req(input$cyr_picker, input$qtr_check, input$beer_cat_check)
      beer_data
      beer_data %>% filter(cyr %in% input$cyr_picker) %>%
        filter(cqtr %in% input$qtr_check) %>%
        filter(category %in% input$beer_cat_check)
    })
    cat("02 aggregate annual & qtr totals \n")
    ## 2. annual and qtr totals ---------------------------------------------------
    beer_annual_data <- reactive({
      AnnualCatTypeData(beer_filtered_data())
    })
    beer_qtr_data <- reactive({
      QtrData(beer_filtered_data(), length(input$qtr_check))
    })
    ## PLOTS ----
    ### sales - yr, qtr ----
    # similar to overview but using functions to get plot, providing:
    # - chart_title, dataset, bar col variable, list of theme modifications
    # - for themes, can list joined by '+'
    output$litre_sales_yr <- renderPlotly({
      TtlChart("Litre Sales by Year", 
               beer_annual_data(), 'cyr', 'litres', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M")
    })
    ## plot sales by quarter
    output$litre_sales_qtr <- renderPlotly({
      TtlChart("Net Beer $ Sales by Qtr", 
               beer_qtr_data(), 'cyr_qtr', 'litres', 'cqtr', qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "M")
    })
    
    ### change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$litre_sales_yoy <- renderPlotly({
      PoPChart("% Chg Beer Sales - Yr", beer_annual_data(), "cyr", 
               "yoy_litres", "cat_type", bar_col, 
               theme_xax+theme_nleg, "%")
    })
    output$litre_sales_qoq <- renderPlotly({
      PoPChart("% Chg Beer Sales - Qtr", beer_qtr_data(), "cyr_qtr", 
               "qoq_litres", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    
    ## WINE ---------------------------------------------------------------
    ## PLOTLY treemap wine countries, categories ----
    output$wine_sales_country_treemap_plot <- renderPlotly({
      cat("wine_country \n")
      data <- wine_filtered_data()
      # filter for most recent year in data set
      #data <- data %>% filter(end_qtr_dt == max(data$end_qtr_dt))
      data$cyr <- as.integer(as.character(data$cyr)) # need to convert cyr from factor
      data <- data %>% filter(cyr == max(data$cyr))
      # get most recent qtr for title
      dynamic_title <- paste0(as.character(max(data$end_qtr_dt)), ": Most Recent Yr/Qtr (based on filters applied)")
      # create shape needed for plotly treemap, based on 3 levels of hierarchy
      # - each level, starting with cat_type is considered a child subcategory of a parent category
      # - first level (cat_type) has blank parent category
      # - everything else has category of one level above as parent
      plotly_data <- bind_rows(
        cat_type_data <- data %>% group_by(cat_type) %>% summarize(netsales = sum(netsales)) %>%
          mutate(subcategory = cat_type, category = "") %>% select(subcategory, category, netsales),
        cat_data <- data %>% group_by(cat_type, category) %>% summarize(netsales = sum(netsales)) %>%
          mutate(subcategory = category, 
                 category = cat_type) %>% ungroup() %>% select(subcategory, category, netsales),
        subcat_data <- data %>% group_by(category, subcategory) %>% summarize(netsales = sum(netsales)) %>%
          select(subcategory, category, netsales)
      )
      
      # Create a treemap
        plot_ly(
          plotly_data,
          type = "treemap",
          labels = ~subcategory,        # Labels for subcategories
          parents = ~category,          # Parent categories
          values = ~netsales,              # Size of each rectangle
          #textinfo = "label+values+percent parent", # Information displayed - overridden by texttemplate
          branchvalues = "total",  # critical for % to add up to parent category
          texttemplate = "%{label}<br>%{value:$,.0f} (% of cat/overall: %{percentParent:0.0%} / %{percentRoot:0.1%})",
          hovertemplate = 
            "<b>%{label}</b> Sales: $%{value:,} (% of cat./overall: %{percentParent:.0%} / %{percentRoot:0.1%})<extra></extra>" # Value in dollar format
        ) %>% layout(
            title = list(
              text = dynamic_title,  # Chart title - set above to reflect most recent quarter selected
              font = list(size = 16, color = "black"),             # Customize font size and color
              x = 0,                                             # Center-align the title
              xanchor = "left"                                   # Ensure proper alignment
            )
          )
      
    }) # end first treemap 
    ## add comparison with earliest quarter in filter -> same chart, different filter
    output$wine_sales_country_treemap_plot_earliest <- renderPlotly({
      cat("wine_country \n")
      data <- wine_filtered_data()
      # filter for earliest quarter in data set
      #data <- data %>% filter(end_qtr_dt == min(data$end_qtr_dt))
      data$cyr <- as.integer(as.character(data$cyr)) # need to convert cyr from factor
      data <- data %>% filter(cyr == min(data$cyr))
      # get end_qtr_dt for title
      dynamic_title <- paste0(as.character(max(data$end_qtr_dt)), " for Comparison (earliest yr based on filters applied)")
      # create shape needed for plotly treemap, based on 3 levels of hierarchy
      # - each level, starting with cat_type is considered a child subcategory of a parent category
      # - first level (cat_type) has blank parent category
      # - everything else has category of one level above as parent
      plotly_data <- bind_rows(
        cat_type_data <- data %>% group_by(cat_type) %>% summarize(netsales = sum(netsales)) %>%
          mutate(subcategory = cat_type, category = "") %>% select(subcategory, category, netsales),
        cat_data <- data %>% group_by(cat_type, category) %>% summarize(netsales = sum(netsales)) %>%
          mutate(subcategory = category, 
                 category = cat_type) %>% ungroup() %>% select(subcategory, category, netsales),
        subcat_data <- data %>% group_by(category, subcategory) %>% summarize(netsales = sum(netsales)) %>%
          select(subcategory, category, netsales)
      )
      
      # Create a second treemap
        plot_ly(
          plotly_data,
          type = "treemap",
          labels = ~subcategory,        # Labels for subcategories
          parents = ~category,          # Parent categories
          values = ~netsales,              # Size of each rectangle
          branchvalues = "total",  # critical for % to add up to parent category
          texttemplate = "%{label}<br>%{value:$,.0f} (% of cat/overall: %{percentParent:0.0%} / %{percentRoot:0.1%})",
          hovertemplate = 
            "<b>%{label}</b> Sales: $%{value:,} (% of cat./overall: %{percentParent:.0%} / %{percentRoot:0.1%})<extra></extra>" # Value in dollar format
        ) %>% layout(
          title = list(
            text = dynamic_title,  # title set above to reflect earliest quarter selected
            font = list(size = 16, color = "black"),             # Customize font size and color
            x = 0,                                             # Center-align the title
            xanchor = "left"                                   # Ensure proper alignment
          )
        )
    }) # end second treemap
    # treemap for biggest gainers / losers in market share
    output$wine_sales_country_treemap_plot_chg <- renderPlotly({
      cat("wine_country \n")
      data <- wine_filtered_data()
      # filter for most recent year in data set
      #data <- data %>% filter(end_qtr_dt == max(data$end_qtr_dt))
      data <- data %>% filter(cyr_num == max(data$cyr_num) | cyr == min(data$cyr_num))
      data <- data %>% group_by(cyr_num, cat_type, category, subcategory) %>% 
        summarize(
          end_qtr_dt = max(end_qtr_dt),
          netsales = sum(netsales)) %>% ungroup()
      # calculate % of total for each subcategory for each year
      data <- data %>% group_by(cat_type, cyr_num) %>% 
        mutate(pct_ttl_sales = netsales/sum(netsales)) %>% ungroup()
      # test - each yr should add up to 1
      #data %>% group_by(cyr_num) %>% summarize(pct_ttl_sales = sum(pct_ttl_sales, na.rm=TRUE))
      #data %>% group_by(cyr_num, category) %>% summarize(pct_ttl_sales = sum(pct_ttl_sales, na.rm=TRUE))
      data_chg_all <- data.frame()
      for(s in 1:length(unique(data$subcategory))) {
        subcat <- unique(data$subcategory)[s]
        data_chg <- data %>% filter(subcategory == subcat) %>% 
          mutate(pct_ttl_sales_chg = pct_ttl_sales - lag(pct_ttl_sales, n = 1))
        data_chg_all <- bind_rows(data_chg_all, data_chg)
      }
      data_chg_all <- data_chg_all %>% filter(cyr_num == max(data_chg_all$cyr_num))
      
      # get most recent qtr for title
      dynamic_title <- paste0("Rel. Chg in Mkt Share Pts ",
                              as.character(max(data$end_qtr_dt)),
                              " v ",
                              as.character(min(data$end_qtr_dt))," (based on filters applied)")
      # create shape needed for plotly treemap, based on 3 levels of hierarchy
      # - each level, starting with cat_type is considered a child subcategory of a parent category
      # - first level (cat_type) has blank parent category
      # - everything else has category of one level above as parent
      plotly_data <- bind_rows(
        cat_type_data <- data_chg_all %>% group_by(cat_type) %>% 
          summarize(pct_ttl_sales_chg = sum(pct_ttl_sales_chg, na.rm = TRUE)) %>%
          mutate(subcategory = cat_type, category = "") %>% 
          select(subcategory, category, pct_ttl_sales_chg),
        cat_data <- data_chg_all %>% group_by(cat_type, category) %>% 
          summarize(pct_ttl_sales_chg = sum(pct_ttl_sales_chg, na.rm = TRUE)) %>%
          mutate(subcategory = category, 
                 category = cat_type) %>% ungroup() %>% select(subcategory, category, pct_ttl_sales_chg),
        subcat_data <- data_chg_all %>% group_by(category, subcategory) %>% 
          summarize(pct_ttl_sales_chg = sum(pct_ttl_sales_chg, na.rm = TRUE)) %>%
          select(subcategory, category, pct_ttl_sales_chg)
      )
      plotly_data <- plotly_data %>% mutate(
        pct_ttl_sales_chg_up = pct_ttl_sales_chg*100
      )
      # need to rescale 0-1 -> treemap can't deal with giving size to negative number?
      # - calls into question whether treemap is the right chart for this
      # - maybe a heat map would be better?
      plotly_data <- plotly_data %>% mutate(
        pct_ttl_sales_chg_rs = scales::rescale(pct_ttl_sales_chg, to = c(0, 1))
      )
      # Create treemap
      color_scale <- brewer.pal(6, "RdYlGn")
      plot_ly(
        plotly_data,
        type = "treemap",
        labels = ~subcategory,        # Labels for subcategories
        parents = ~category,          # Parent categories
        values = ~pct_ttl_sales_chg_rs,              # Size of each rectangle
        text = ~paste0(round(pct_ttl_sales_chg_up, 1),"pts"), # Text to display on hover
        textinfo = "label+text",
        marker = list(colors = ~pct_ttl_sales_chg_rs, colorscale = list(
          c(0, color_scale[1]),
          c(0.25, color_scale[2]),
            c(0.5, color_scale[3]),
            c(0.75, color_scale[4]),
            c(1, color_scale[5]))),
        #branchvalues = "remainder",  # critical for % to add up to parent category
        #texttemplate = "%{label}<br>%{value:.1%}", # Value in dollar format
        hovertemplate = 
          paste("<b>%{label}</b> Mkt Share % Pts:", round(plotly_data$pct_ttl_sales_chg_up, 1),"<extra></extra>")
      ) %>% layout(
        title = list(
          text = dynamic_title,  # Chart title - set above to reflect most recent quarter selected
          font = list(size = 16, color = "black"),             # Customize font size and color
          x = 0,                                             # Center-align the title
          xanchor = "left"                                   # Ensure proper alignment
        )
      )
      
    })
} # end server


