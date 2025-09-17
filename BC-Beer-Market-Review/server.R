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

# load functions used: data manipulation and plots ----
source('functions_data.R')
source('functions_plots.R')
# load support variables for plots etc
source('support_vars.R')

# Define server logic ----
function(input, output, session) {
  # experiment with different bs themes
  #bslib::bs_themer()
  # xtoggle sidebar ----
  # for toggling sidebar, using shinyjs 
  # - NOT USED - clunky
  # - makes sidebar disappear / appear but mainPanel doesn't expand - pointless
  #observeEvent(input$toggleSidebar, {
  #  toggle("sidebar")
  #})
# get data ----
# query database via separate file for tidyness
# postgresql as of Jun 2025
## all data ----
  source('query_pg.R')
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
  print(head(beer_data))
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
  
  # CHATGPT: apply dynamic filters as needed to different tabs, based on selection
  # Dynamic Sidebar ----
  # in ui: sidebarPanel(id = "sidebar", ...)
  # hrefs below refer to ids of h2 tags in ui 
  output$dynamic_sidebar <- renderUI({
    if (input$tabselected == 1) {
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
    } else if (input$tabselected == 2) {
      tagList(
        tags$p(lmr_max_note, class="note"),
        dynamic_cyr,
        dynamic_qtr,
        dynamic_beer_cat,
        tags$h4("Contents"),
          tags$a(href="#litre_sales", "Ttl Litres by Yr & Qtr"),tags$br(),
          tags$a(href="#bsrc_sales_litre", "Litres by Source"), tags$br(),
          tags$a(href="#bcat_sales_litre", "BC Litres by Category"), tags$br(),
          tags$a(href="#bimp_sales_litre","Import Litres by Ctry"), tags$br()
      )
    } else if (input$tabselected == 3) {
      tagList(
        tags$h4("Contact"),
          # Placeholder span for the email address
          tags$span(id = "email-container"),
          # JavaScript to dynamically build and insert the email link
          tags$script(HTML("
            const user = 'john';
            const domain = 'bcbeer.ca';
            const email = `${user}@${domain}`;
            const link = document.createElement('a');
            link.href = `mailto:${email}`;
            link.textContent = email;
            document.getElementById('email-container').appendChild(link);
          ")),
        tags$p("LinkedIn: ",
               tags$a(href="https://www.linkedin.com/in/johnyuill/", "John Yuill")),
        tags$br()
          )
        } 
      })
  
    # $ SALES ---------------------------------------------------------------
    ## beer: apply filters to data ---------------------------------------------------
    cat("297: 01 apply beer filters \n")
    ## 1. Filter the data set based on the selected categories ----
    beer_filtered_data <- reactive({
      req(input$cyr_picker, input$qtr_check, input$beer_cat_check)
      #beer_data
      beer_data %>% filter(cyr %in% input$cyr_picker) %>%
        filter(cqtr %in% input$qtr_check) %>%
        filter(category %in% input$beer_cat_check)
    })
  #beer_filtered_test <- beer_data %>% filter(cyr_num < 2025)
  #beer_annual_test <- AnnualCatTypeData(beer_data, beer_data)
  #beer_filter_annual <- AnnualCatTypeData(beer_filtered_test, beer_filtered_test)
    cat("306: 02 aggregate annual & qtr totals \n")
    ## 2. annual and qtr totals ---------------------------------------------------
    cat("308: annual data \n")
    beer_annual_data <- reactive({
      AnnualCatTypeData(beer_filtered_data(), beer_filtered_data())
    })
    cat("312: qtr data \n")
    beer_qtr_data <- reactive({
      QtrData(beer_filtered_data(), length(input$qtr_check))
    })
    ## PLOTS ----
    ### $ sales - yr, qtr ----
    # similar to overview but using functions to get plot, providing:
    # - chart_title, dataset, bar col variable, list of theme modifications
    # - for themes, can list joined by '+'
    output$beer_sales_yr <- renderPlotly({
      TtlChart("Net $", yr_sales, 
               beer_annual_data(),
               #beer_annual_test,
               #beer_filter_annual,
                'cyr', 'netsales', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M", yr_flag_color, lwidth, lpointsize)
    })
    ## plot sales by quarter
    output$beer_sales_qtr <- renderPlotly({
      QtrChart("Net $", qtr_sales, 
               beer_qtr_data(), 'cyr_qtr', 'netsales', 'cqtr', qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "M", lwidth, lpointsize)
    })
    
    ### change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$beer_sales_yoy <- renderPlotly({
      PoPChart("", pop_chg_sales, beer_annual_data(), "cyr", "yoy_sales", "yr_flag", yr_flag_color, 
               theme_xax+theme_nleg, "%")
    })
    output$beer_sales_qoq <- renderPlotly({
      PoPChart("", pop_chg_sales_qtr, beer_qtr_data(), "cyr_qtr", "qoq_sales", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    ## CAT - source / origin ----
    ## data by cat ----
    ## yoy = $ SALES ONLY
    cat("346: annual data \n")
    beer_annual_data_cat <- reactive({
      n_cats <- length(input$beer_cat_check)
      AnnualCatData(beer_filtered_data(), beer_filtered_data())
    })
    # test annual category data
    #beer_annual_test <- AnnualCatData(beer_data, beer_data)
    
    # qtr
    cat("356: quarterly data \n")
    beer_qtr_data_cat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_qtr <- length(input$qtr_check)
      n_cats <- length(input$beer_cat_check)
      QtrCatData(beer_filtered_data(), n_cats, n_qtr)
    })
    ## PLOTS by category (source / origin) ----
    output$beer_sales_yr_cat <- renderPlotly({
      CatChart("Net $", yr_source, 
               beer_annual_data_cat(), "cyr", "netsales","category", 
               beer_cat_color, "stack",
               theme_xax,"M")
    })
    output$beer_sales_yr_cat_pc <- renderPlotly({
      CatChart("Net $", yr_source_pc, 
               beer_annual_data_cat(), "cyr", "pct_ttl_sales","category", 
               beer_cat_color, "fill",
               theme_xax, tunits="%")
    })
    # ABANDONED:qtr - abandoned in favour of % of annual ttl
    # output$beer_sales_qtr_cat <- renderPlotly({
    #   CatChart("Net $"," Qtrly Beer Sales by Source", 
    #            beer_qtr_data_cat(), "cyr_qtr", "netsales", "category", beer_cat_color, "stack",
    #            theme_xax+theme_xaxq, "M")
    # })
    ### facet: % change by source ----
    output$beer_sales_yoy_cat_chg <- renderPlotly({
      x <- beer_annual_data_cat()
      CatChgChart("", yr_source_pc_chg, 
               x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales",
               fill_var = "yr_flag", 
               facet_var = "category",
               fill_color = yr_flag_color, 
               strp_color = strp_col,
               theme_xax+theme_nleg)
    })
    
    # % point chg by src yoy
    output$beer_sales_yoy_cat_chg_pt <- renderPlotly({
      x <- beer_annual_data_cat()
      CatChgChart("", yr_source_pcpt_chg, 
               x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales", 
               fill_var = "cat_type", 
               facet_var = "category",
               fill_color = bar_col, 
               strp_color = strp_col,
               theme_xax+theme_nleg, tunits = "num")
    })
    
    ## SUBCAT data ----
    # annual
    beer_yr_data_subcat <- reactive({
      cat("409: beer_subcat \n")
      n_cats <- length(unique(beer_filtered_data()$category))
      n_subcats <- length(unique(beer_filtered_data()$subcategory))
      AnnualSubCatData(beer_filtered_data(), n_cats, n_subcats, beer_annual_data_cat())
    })
    # test
    #n_cats <- length(unique(beer_data$category))
    #n_subcats <- length(unique(beer_data$subcategory))
    #beer_annual_subcat_test <- AnnualSubCatData(beer_data, n_cats, n_subcats, beer_annual_test)
    ## BC subcat ----
    ## PLOTS by BC subcategory ----
    output$beer_sales_yr_bc_cat <- renderPlotly({
      data <- beer_yr_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChart("Net $",yr_sales_cat, 
               data, "cyr", "netsales","subcategory", 
               beer_bc_cat_color, "stack", theme_xax, "M")
    })
    ## plot % of total sales by bc subcategory
    output$beer_sales_yr_bc_cat_pc <- renderPlotly({
      data <- beer_yr_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChart("Net $", yr_sales_cat_pc, 
               data, "cyr", "pct_ttl_sales","subcategory", beer_bc_cat_color, "fill", 
               theme_xax, "%")
    })
    ## plots for bc beer subcategory yoy change in facets
    output$beer_sales_yoy_bc_cat_chg <- renderPlotly({
      x <- beer_yr_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChgChart("",yr_sales_cat_pc_chg, 
               x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales", 
               fill_var = "yr_flag", 
               facet_var = "subcategory",
               fill_color = yr_flag_color, 
               strp_color = strp_col,
               theme_xax+theme_nleg)
    })
    ## % point chg by bc subcat yoy
    output$beer_sales_yoy_bc_cat_chg_pt <- renderPlotly({
      x <- beer_yr_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChgChart("",yr_sales_cat_pcpt_chg, 
               x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales",
               fill_var = "cat_type", 
               facet_var = "subcategory",
               fill_color = bar_col, 
               strp_color = strp_col,
               theme_xax+theme_nleg, tunits = "num")
    })
    ## IMPORT beer subcat ----
    
    ## plots by import country/region
    output$beer_sales_yr_import_cat <- renderPlotly({
      cat('beer_import chart \n')
      data <- beer_yr_data_subcat() %>% filter(category=='Import')
      print(data)
      CatChart("Net $",yr_sales_imp, 
               data, "cyr", "netsales","subcategory", beer_imp_color, "stack", 
               theme_xax, "M") %>%
        layout(legend = layout_legend_vr)
    }) 

    output$beer_sales_yr_import_cat_pc <- renderPlotly({
      cat('beer_import_pc chart \n')
      data <- beer_yr_data_subcat() %>% filter(category=='Import')
      CatChart("Net $",yr_sales_imp_pc, 
               data, "cyr", "pct_ttl_sales","subcategory", beer_imp_color, "fill", 
               theme_xax, "%") %>%
        layout(legend = layout_legend_vr)
    }) 
    
    ## plots for import beer subcategory yoy change in facets
    output$beer_sales_yoy_import_cat_chg <- renderPlotly({
      x <- beer_yr_data_subcat() %>% filter(category=='Import')
      CatChgChart("",yr_sales_imp_pc_chg, 
               x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales", 
               fill_var = "yr_flag", 
               facet_var = "subcategory",
               fill_color = yr_flag_color, 
               strp_color = strp_col,
               theme_xax+theme_nleg+theme_facet)
    })
    ## % point chg by import subcat yoy
    output$beer_sales_yoy_import_cat_chg_pt <- renderPlotly({
      x <- beer_yr_data_subcat() %>% filter(category=='Import')
      CatChgChart("",yr_sales_imp_pcpt_chg, 
               x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales", 
               fill_var = "cat_type", 
               facet_var = "subcategory",
               fill_color = bar_col, 
               strp_color = strp_col,
               theme_xax+theme_nleg+theme_facet, tunits = "num")
    })

    # LITRES ----
    ## beer: apply filters to data ---------------------------------------------------
    cat("503: 01 litres apply beer filters \n")
    ## 1. Filter the data set based on the selected categories ----
    beer_filtered_data <- reactive({
      req(input$cyr_picker, input$qtr_check, input$beer_cat_check)
      beer_data
      beer_data %>% filter(cyr %in% input$cyr_picker) %>%
        filter(cqtr %in% input$qtr_check) %>%
        filter(category %in% input$beer_cat_check)
    })
    cat("513: 02 litres aggregate annual & qtr totals \n")
    ## 2. annual and qtr totals ---------------------------------------------------
    beer_annual_data <- reactive({
      AnnualCatTypeData(beer_filtered_data(), beer_filtered_data())
    })
    beer_qtr_data <- reactive({
      QtrData(beer_filtered_data(), length(input$qtr_check))
    })
    ## PLOTS ----
    ### litre sales - yr, qtr ----
    # similar to overview but using functions to get plot, providing:
    # - chart_title, dataset, bar col variable, list of theme modifications
    # - for themes, can list joined by '+'
    metric <- "Litres"
    output$litre_sales_yr <- renderPlotly({
      TtlChart(metric, yr_sales, 
               beer_annual_data(), 'cyr', 'litres', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M", yr_flag_color, lwidth, lpointsize)
    })
    ## plot sales by quarter
    output$litre_sales_qtr <- renderPlotly({
      QtrChart(metric, qtr_sales, 
               beer_qtr_data(), 'cyr_qtr', 'litres', 'cqtr', qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "M", lwidth, lpointsize)
    })
    
    ### change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$litre_sales_yoy <- renderPlotly({
      PoPChart("", pop_chg_sales, beer_annual_data(), "cyr", 
               "yoy_litres", "yr_flag", yr_flag_color, 
               theme_xax+theme_nleg, "%")
    })
    output$litre_sales_qoq <- renderPlotly({
      PoPChart("", pop_chg_sales_qtr,beer_qtr_data(), "cyr_qtr", 
               "qoq_litres", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    ## CAT - source / origin ----
    ## data by cat ----
    cat("552: yoy cat annual data \n")
    beer_annual_data_cat <- reactive({
      n_cats <- length(input$beer_cat_check)
      AnnualCatData(beer_filtered_data(), beer_filtered_data())
      })
    # test annual category data
    #n_cats <- length(unique(beer_data$category))
    #beer_annual_test <- AnnualCatData(beer_data, beer_data)
    
    # qtr
    beer_qtr_data_cat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_qtr <- length(input$qtr_check)
      n_cats <- length(input$beer_cat_check)
      QtrCatData(beer_filtered_data(), n_cats, n_qtr)
    })
    ## PLOTS by category (source / origin) ----
    output$litre_sales_yr_cat <- renderPlotly({
      CatChart(metric, yr_source, 
               beer_annual_data_cat(), "cyr", "litres","category", 
               beer_cat_color, "stack",
               theme_xax,"M")
    })
    output$litre_sales_yr_cat_pc <- renderPlotly({
      CatChart(metric, yr_source_pc, 
               beer_annual_data_cat(), "cyr", "pct_ttl_litres","category", 
               beer_cat_color, "fill",
               theme_xax,tunits="%")
    })
    # ABANDONED:qtr - abandoned in favour of % of annual ttl
    # output$litre_sales_qtr_cat <- renderPlotly({
    #   CatChart("Qtrly Litres by Source", 
    #            beer_qtr_data_cat(), "cyr_qtr", "litres", "category", beer_cat_color, "stack",
    #            theme_xax+theme_xaxq, "M")
    # })
    ### facet: % change by source ----
    output$litre_sales_yoy_cat_chg <- renderPlotly({
      x <- beer_annual_data_cat()
      CatChgChart("",yr_source_pc_chg, 
                  x, x_var = "cyr", y_var = "yoy_litres", sort_var = "litres",
                  fill_var = "yr_flag", 
                  facet_var = "category",
                  fill_color = yr_flag_color, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg)
    })
    
    # % point chg by src yoy
    output$litre_sales_yoy_cat_chg_pt <- renderPlotly({
      x <- beer_annual_data_cat()
      CatChgChart("", yr_source_pcpt_chg, 
                  x, x_var = "cyr", y_var = "yoy_pcp_ttl_litres", 
                  sort_var = "litres", 
                  fill_var = "cat_type", 
                  facet_var = "category",
                  fill_color = bar_col, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg, tunits = "num")
    })
    # ABANDONED:qtr % chg by src - abandoned in favour of % of annual total
    # output$litre_sales_qoq_cat_chg <- renderPlotly({
    #   x <- beer_qtr_data_cat()
    #   CatChgChart("Qtrly % Chg Beer Sales by Source", 
    #               x, x_var = "cyr_qtr", y_var = "qoq_litres", 
    #               sort_var = "yoy_pcp_ttl_litres", 
    #               fill_var = "cqtr", 
    #               facet_var = "category",
    #               fill_color = qtr_color, 
    #               strp_color = bar_col,
    #               theme_xax+theme_xaxq+theme_nleg)
    # })
    ## SUBCAT data ----
    # annual
    beer_yr_data_subcat <- reactive({
      cat("beer_subcat \n")
      n_cats <- length(unique(beer_filtered_data()$category))
      n_subcats <- length(unique(beer_filtered_data()$subcategory))
      AnnualSubCatData(beer_filtered_data(), n_cats, n_subcats, beer_annual_data_cat())
    })
    # test
    #n_cats <- length(unique(beer_data$category))
    #n_subcats <- length(unique(beer_data$subcategory))
    #beer_annual_test <- AnnualSubCatData(beer_data, n_cats, n_subcats, beer_data)
    ## BC subcat ----
    ## PLOTS by BC subcategory ----
    output$litre_sales_yr_bc_cat <- renderPlotly({
      #data <- beer_bc_yr_subcat()
      data <- beer_yr_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChart(metric, yr_sales_cat, 
               data, "cyr", "litres","subcategory", 
               beer_bc_cat_color, "stack", theme_xax, "M")
    })
    ## plot % of total sales by bc subcategory
    output$litre_sales_yr_bc_cat_pc <- renderPlotly({
      data <- beer_yr_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChart(metric, yr_sales_cat_pc, 
               data, "cyr", "pct_ttl_litres","subcategory", beer_bc_cat_color, "fill", 
               theme_xax, "%")
    })
    ## plots for bc beer subcategory yoy change in facets
    output$litre_sales_yoy_bc_cat_chg <- renderPlotly({
      x <- beer_yr_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChgChart("", yr_sales_cat_pc_chg, 
                  x, x_var = "cyr", y_var = "yoy_litres", sort_var = "litres", 
                  fill_var = "yr_flag", 
                  facet_var = "subcategory",
                  fill_color = yr_flag_color, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg)
    })
    ## % point chg by bc subcat yoy
    output$litre_sales_yoy_bc_cat_chg_pt <- renderPlotly({
      x <- beer_yr_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChgChart("", yr_sales_cat_pcpt_chg, 
                  x, x_var = "cyr", y_var = "yoy_pcp_ttl_litres", sort_var = "litres",
                  fill_var = "cat_type", 
                  facet_var = "subcategory",
                  fill_color = bar_col, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg, tunits = "num")
    })
    
    ## IMPORT beer subcat ----
    ## plots by import country/region
    output$litre_sales_yr_import_cat <- renderPlotly({
      cat('beer_import chart \n')
      data <- beer_yr_data_subcat() %>% filter(category=='Import')
      print(data)
      CatChart(metric, yr_sales_imp, 
               data, "cyr", "litres","subcategory", beer_pal, "stack", 
               theme_xax, "M") %>%
        layout(legend = layout_legend_vr)
    }) 
    
    output$litre_sales_yr_import_cat_pc <- renderPlotly({
      cat('beer_import_pc chart \n')
      data <- beer_yr_data_subcat() %>% filter(category=='Import')
      CatChart(metric, yr_sales_imp_pc, 
               data, "cyr", "pct_ttl_litres","subcategory", beer_pal, "fill", 
               theme_xax, "%") %>%
        layout(legend = layout_legend_vr)
    }) 
    
    ## plots for import beer subcategory yoy change in facets
    output$litre_sales_yoy_import_cat_chg <- renderPlotly({
      x <- beer_yr_data_subcat() %>% filter(category=='Import')
      CatChgChart("", yr_sales_imp_pc_chg,
                  x, x_var = "cyr", y_var = "yoy_sales", 
                  sort_var = "litres", 
                  fill_var = "yr_flag", 
                  facet_var = "subcategory",
                  fill_color = yr_flag_color, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg)
    })
    ## % point chg by import subcat yoy
    output$litre_sales_yoy_import_cat_chg_pt <- renderPlotly({
      x <- beer_yr_data_subcat() %>% filter(category=='Import')
      CatChgChart("", yr_sales_imp, 
                  x, x_var = "cyr", y_var = "yoy_pcp_ttl_litres", 
                  sort_var = "litres", 
                  fill_var = "cat_type", 
                  facet_var = "subcategory",
                  fill_color = bar_col, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg, tunits = "num")
    })
} # end server


