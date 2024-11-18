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
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)
library(here)
library(bslib)
library(RColorBrewer)

# colors etc ----
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
  # get data ----
  # query database via separate file for tidyness
  ## all data ----
  source('query.R')
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
    subcat = case_when(
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
  # rename subcategories for brevity
  refresh_data <- refresh_data %>% mutate(
    subcat = case_when(
      str_detect(subcategory,  "Domestic") ~ "Domestic",
      str_detect(subcategory, "Import") ~ "Import",
      str_detect(subcategory, "Malt") ~ "Malt-Based",
      str_detect(subcategory,"Spirit") ~ "Spirit",
      str_detect(subcategory, "Wine") ~ "Wine/Fruit"
    )
  )
  ## spirits ----
  spirits_data <- lmr_data %>% filter(cat_type == "Spirits")
  ## wine ----
  wine_data <- lmr_data %>% filter(cat_type == "Wine")
  
  # setup dynamic filters ----------------------------------------------------
  # Dynamically generate UI filters based on lmr_data
  # otherwise, app will crash because lmr_data not available for filters in ui.R
  # CHATGPT suggestion as alt to below
  ## year filter ----
  dynamic_cyr <- pickerInput(
    inputId = "cyr_picker",
    label = "Select Calendar Year(s):",
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
  ## qtr filters ----
  dynamic_qtr <- checkboxGroupInput(inputId = "qtr_check", "Select a quarter", 
                                    choices = sort(unique(lmr_data$cqtr)), 
                                    selected = unique(lmr_data$cqtr),
                                    inline = FALSE
  )
  ## overall category filter ----
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
  # CHATGPT: apply dynamic filters as needed to different tabs, based on selection
  # dynamic sidebar ----
  output$dynamic_sidebar <- renderUI({
    if (input$tabselected == 1) {
      tagList(
        dynamic_cyr,
        dynamic_qtr,
        dynamic_cat,
        tags$h4("Contents"),
        tags$a(href="#ttl_sales", "Ttl Sales by Yr & Qtr"),tags$br(),
        tags$a(href="#cat_sales", "Category Sales: Yr & Qtr"),tags$br(),
      )
    } else if (input$tabselected == 2) {
      tagList(
        dynamic_cyr,
        dynamic_qtr,
        dynamic_beer_cat,
        tags$h4("Contents"),
          tags$a(href="#beer_sales", "Ttl Sales by Yr & Qtr"),tags$br(),
          tags$a(href="#bsrc_sales", "Sales by Source"), tags$br(),
          tags$a(href="#bcat_sales", "BC Sales by Category"), tags$br(),
          tags$a(href="#bimp_sales","Import Sales by Ctry"), tags$br()
      )
    } else if (input$tabselected == 3) {
      tagList(
        dynamic_cyr,
        dynamic_qtr,
        dynamic_refresh_cat,
        tags$h4("Contents"),
          tags$a(href="#refresh_sales", "Ttl Sales by Yr & Qtr"),tags$br(),
          tags$a(href="#refresh_cat_sales", "Category Sales"), tags$br(),
          tags$a(href="#refresh_subcat_sales","Subcategory Sales"), tags$br()
      )
    }
  })
  
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
  # annual and qtr totals ---------------------------------------------------
  annual_data <- reactive({
    filtered_data() %>% group_by(cyr) %>%
    summarize(netsales = sum(netsales)) %>%
    mutate(yoy = (netsales - lag(netsales))/lag(netsales))
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
  # by cat -------------------------------------------------------------------
  ## annual data by cat
  cat('03 aggregate annual data by cat \n')
  annual_data_cat <- reactive({
    n_cats <- length(input$cat_check)
    filtered_data() %>% group_by(cyr, cat_type) %>%
      summarize(netsales = sum(netsales)) %>% ungroup() %>%
      mutate(yoy = (netsales - lag(netsales, n=n_cats))/lag(netsales, n=n_cats),
             cat_type = reorder(cat_type, netsales, FUN = sum))
  })
  
  qtr_data_cat <- reactive({
    # need to base the qoq on the number of cats chosen in filter
    n_qtr <- length(input$qtr_check)
    n_cats <- length(input$cat_check)
    filtered_data() %>% group_by(cyr, cqtr, cyr_qtr, end_qtr_dt, cat_type) %>%
      summarize(netsales = sum(netsales)) %>% ungroup() %>%
      mutate(qoq = (netsales - lag(netsales, n = n_cats))/lag(netsales, n = n_cats))
  })
  # PLOTS --------------------------------------------------------------------
  # plot theme
  theme_set(theme_light()+theme(panel.grid.minor = element_blank(),
                                panel.grid.major = element_line(color = 'grey90', linewidth=0.1)))
  # x-axis text - set angle and other formats
  theme_xax <- theme(axis.ticks.x = element_blank(),
                   axis.text.x = element_text(angle = 90, hjust = 1))
  theme_xaxq <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
  # no legend
  theme_nleg <- theme(legend.position = "none")
  # customize tooltip format
  text_bn <- aes(text=paste0(cyr, ": ", scales::dollar(netsales, scale = 1e-9, suffix = "B")))
  text_bn <- aes(text = paste0(cyr, ": ", label_currency(scale = 1e-9, suffix = "B")(netsales)))
  text_m <- aes(text=paste0(cyr, ": ", scales::dollar(netsales, scale = 1e-6, suffix = "M")))
  text_mq <- aes(text=paste0(cqtr, ": ", scales::dollar(netsales, scale = 1e-6, suffix = "M")))
  text_mcat <- aes(text=paste0(cat_type, ": ", scales::dollar(netsales, scale = 1e-6, suffix = "M")))
  text_pc <- aes(text = paste0(cyr, ": ", label_percent(accuracy = 0.1)(yoy)))
  
    # TTL SALES ------------------------------------------------------------------
    ## ttl sales ----
    # plot for sales by year
    output$sales_yr <- renderPlotly({
      cat("sales data by year \n")
        # get filtered, aggregated data
        x <- annual_data()
        # add tooltip column formatted to data
        x <- x %>% tooltip_fmt(dim = 'cyr', units = 'B', y_var = 'netsales') 
        # plot
        ch_title <- "Net $ Sales by Year"
        p <- x %>%
          ggplot(aes(x = cyr, y = netsales, text=tooltip_text)) +
          geom_col(fill=bar_col) +
          scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                             expand = expansion(mult=c(0,0.05))) +
          labs(title=ch_title, x="", y="")+
          theme_xax #+
          #function to set tooltip format, based on dimension, units, and metric
          #tooltip_fmt('cyr', 'B', 'netsales')
          
        ggplotly(p, tooltip = "text") # specify tooltip to avoid default showing
    })
    # plot sales by quarter
    output$sales_qtr <- renderPlotly({
      x <- qtr_data()
      x <- x %>% tooltip_fmt(dim = 'cyr_qtr', units = 'M', y_var = 'netsales') 
      ch_title <- "Net $ Sales by Qtr"
      p <- x %>%
        ggplot(aes(x = cyr_qtr, y = netsales, fill = cqtr, text = tooltip_text)) +
        geom_col() +
        scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                           expand = expansion(mult=c(0,0.05))) +
        scale_fill_manual(values=qtr_color)+
        labs(title=ch_title, x="", y="")+
        theme_xax+theme_xaxq+theme_nleg #+
        #tooltip_fmt('cyr_qtr', "M", 'netsales')
      
      ggplotly(p, tooltip = "text")
    })
    ## change in sales --------------------------------------------------------
    # plot for year-over-year change in sales
    output$sales_yoy <- renderPlotly({
      x <- annual_data()
      # add tooltip column formatted to data
      x <- x %>% tooltip_fmt(dim = 'cyr', units = '%', y_var = 'yoy')
      max_y <- max(x$yoy, na.rm = TRUE)
      min_y <- min(x$yoy, na.rm = TRUE)
      max_val <- max(abs(min_y), abs(max_y))
      ch_title <- "% Chg Net $ Sales by Yr"
      p <- x %>% 
        ggplot(aes(x = cyr, y = yoy, text = tooltip_text)) +
        geom_col(fill=bar_col) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        scale_y_continuous(labels = scales::label_percent(accuracy = 0.1), 
                           expand = expansion(mult=c(0,0.05)),
                           limits = c(0 - max_val, max_val)) +
        labs(title=ch_title, x="", y="")+
        theme_xax 
      ggplotly(p, tooltip = "text")
    })
    
    # plot for quarter-over-quarter change in sales
    output$sales_qoq <- renderPlotly({
      x <- qtr_data()
      x <- x %>% tooltip_fmt(dim = 'cyr_qtr', units = '%', y_var = 'qoq')
      max_y <- max(x$qoq, na.rm = TRUE)
      min_y <- min(x$qoq, na.rm = TRUE)
      max_val <- max(abs(min_y), abs(max_y))
      ch_title <- "% Chg Net $ Sales by Qtr"
      p <- x %>% 
        ggplot(aes(x = cyr_qtr, y = qoq, fill = cqtr, text = tooltip_text)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        scale_y_continuous(labels = scales::percent,
                           expand = expansion(mult=c(0,0.05)),
                           limits = c(0 - max_val, max_val)) +
        scale_fill_manual(values=qtr_color)+
        labs(title=ch_title, x="", y="")+
        theme_xax+theme_xaxq+theme_nleg #+
        #tooltip_fmt('cyr_qtr', "%", 'qoq')
      ggplotly(p, tooltip = "text")
    })
    
    ## sales by category --------------------------------------
    ## plot for annual sales by category
    output$sales_yr_cat <- renderPlotly({
      x <- annual_data_cat()
      #x <- x %>% 
      #  mutate(cat_type = fct_reorder(cat_type, netsales, .fun = sum, .desc = FALSE))
      # add formatted tooltip and sort categories
      x <- x %>% tooltip_fmt(dim = 'cat_type', units = 'M', y_var = 'netsales') %>%
        mutate(cat_type = fct_reorder(cat_type, netsales, .fun = sum, .desc = FALSE))
      print(head(x))
      
      print(unique(x$cat_type))
      ch_title <- "Net $ Sales by Cat by Yr"
      p <- x %>%
        ggplot(aes(x = cyr, y = netsales, fill = cat_type, text=tooltip_text)) +
        geom_col(position = "stack") +
        scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                           expand = expansion(mult=c(0,0.05))) +
        scale_fill_manual(values=cat_type_color)+
        labs(title=ch_title, x="", y="")+
        theme(axis.ticks.x = element_blank(),
              legend.position = "top",
              legend.title = element_blank(),
              plot.margin = unit(c(1, 0, 0, 0), "cm"))+
        theme_xax #+
        #tooltip_fmt('cat_type', "M", 'netsales')
      #p_plotly <- ggplotly(p)
      p_plotly <- ggplotly(p, tooltip = "text")
      # Customize legend in plotly
      p_plotly <- p_plotly %>% layout(
        legend = list(
          orientation = "h",     # Horizontal legend
          x = 0.5,               # Center legend horizontally
          xanchor = "center",    # Align legend center with x position
          y = 1,                 # Place legend at the top
          yanchor = "bottom",    # Align legend bottom with y position
          title = list(text = "")  # Remove legend title
        )
      )
      p_plotly
    })
    
    ## plot for qtr sales by category 
    output$sales_qtr_cat <- renderPlotly({
      x <- qtr_data_cat()
      x <- x %>% tooltip_fmt(dim = 'cat_type', units = 'M', y_var = 'netsales') %>%
        mutate(cat_type = fct_reorder(cat_type, netsales, .fun = sum, .desc = FALSE))
      
      ch_title <- "Net $ Sales by Cat by Qtr"
      p <- x %>%
        ggplot(aes(x = cyr_qtr, y = netsales, fill = cat_type, text = tooltip_text)) +
        geom_col(position = "stack") +
        scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                           expand = expansion(mult=c(0,0.05))) +
        scale_fill_manual(values=cat_type_color)+
        labs(title=ch_title, x="", y="")+
        theme(axis.ticks.x = element_blank(),
              legend.position = "top",
              legend.title = element_blank(),
              plot.margin = unit(c(1, 0, 0, 0), "cm"))+
        theme_xax+theme_xaxq 
      
      p_plotly <- ggplotly(p, tooltip = "text")
      # Customize legend in plotly
      p_plotly <- p_plotly %>% layout(
        legend = list(
          orientation = "h",     # Horizontal legend
          x = 0.5,               # Center legend horizontally
          xanchor = "center",    # Align legend center with x position
          y = 1,                 # Place legend at the top
          yanchor = "bottom",    # Align legend bottom with y position
          title = list(text = "")  # Remove legend title
        )
      )
      p_plotly
    })

    ## facet plots --------------------------------------------
    ## plot yoy chg by cat
    output$sales_yoy_cat <- renderPlotly({
      x <- annual_data_cat()
      max_y <- max(x$yoy, na.rm = TRUE)
      min_y <- min(x$yoy, na.rm = TRUE)
      max_val <- max(abs(min_y), abs(max_y))
      x <- x %>% tooltip_fmt(dim = 'cyr', units = '%', y_var = 'yoy')
      
      ch_title <- "% Chg Net $ Sales by Cat"
      p <- x %>%
        ggplot(aes(x = cyr, y = yoy, text = tooltip_text)) +
        geom_col(fill=bar_col) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        facet_grid(cat_type~.) +
        scale_y_continuous(labels = scales::label_percent(),
                           expand = expansion(mult=c(0,0.05)),
                           limits = c(0 - max_val, max_val)) +
        theme(strip.background = element_rect(fill = bar_col)) +
        theme(strip.text=element_text(color='white'))+
        labs(title=ch_title, x="", y="")+
        theme_xax 
      ggplotly(p, tooltip = "text")
    })
    ## plot qoq chg by cat
    output$sales_qoq_cat <- renderPlotly({
      x <- qtr_data_cat()
      max_y <- max(x$qoq, na.rm = TRUE)
      min_y <- min(x$qoq, na.rm = TRUE)
      max_val <- max(abs(min_y), abs(max_y))
      x <- x %>% tooltip_fmt(dim = 'cyr_qtr', units = '%', y_var = 'qoq')
      
      ch_title <- "% Chg Net $ Sales by Cat by Qtr"
      p <- x %>%
        ggplot(aes(x = cyr_qtr, y = qoq, fill = cqtr, text = tooltip_text)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        facet_grid(cat_type~.) +
        scale_y_continuous(labels = scales::percent,
                           expand = expansion(mult=c(0,0.05)),
                           limits = c(0 - max_val, max_val)) +
        scale_fill_manual(values=qtr_color)+
        theme_light()+
        theme(strip.background = element_rect(fill = bar_col)) +
        theme(strip.text=element_text(color='white'))+
        labs(title=ch_title, x="", y="")+
        theme_xax+theme_xaxq+theme_nleg 
      ggplotly(p, tooltip = "text")
    })
    
    # BEER ---------------------------------------------------------------
    ## beer: apply filters to data ---------------------------------------------------
    cat("01 apply beer filters \n")
    # Filter the data set based on the selected categories
    beer_filtered_data <- reactive({
      req(input$cyr_picker, input$qtr_check, input$beer_cat_check)
      beer_data
      beer_data %>% filter(cyr %in% input$cyr_picker) %>%
        filter(cqtr %in% input$qtr_check) %>%
        filter(category %in% input$beer_cat_check)
    })
    cat("02 aggregate annual & qtr totals \n")
    ## annual and qtr totals ---------------------------------------------------
    beer_annual_data <- reactive({
       beer_filtered_data() %>% group_by(cat_type, cyr) %>%
        summarize(netsales = sum(netsales)) %>%
        mutate(yoy = (netsales - lag(netsales))/lag(netsales))
    })
    beer_qtr_data <- reactive({
      beer_filtered_data() %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt) %>%
        summarize(netsales = sum(netsales)) %>% ungroup() %>%
        mutate(qoq = (netsales - lag(netsales))/lag(netsales),
               yr_qtr = paste(cyr, cqtr, sep = "-")
        )
    })
    ## beer plots ----
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
      PoPChart("% Chg Beer Sales - Yr", beer_annual_data(), "cyr", "yoy", "cat_type", bar_col, 
               theme_xax+theme_nleg, "%")
    })
    output$beer_sales_qoq <- renderPlotly({
      PoPChart("% Chg Beer Sales - Qtr", beer_qtr_data(), "cyr_qtr", "qoq", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    ## beer - cat - origin ----
    ## data by cat ----
    beer_annual_data_cat <- reactive({
      n_cats <- length(input$beer_cat_check)
      beer_filtered_data() %>% group_by(cat_type, cyr, category) %>%
        summarize(netsales = sum(netsales)) %>% ungroup() %>%
        mutate(yoy = (netsales - lag(netsales, n=n_cats))/lag(netsales, n=n_cats),
               cat_type = reorder(cat_type, netsales, FUN = sum))
    })
    beer_qtr_data_cat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_qtr <- length(input$qtr_check)
      n_cats <- length(input$beer_cat_check)
      beer_filtered_data() %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt, category) %>%
        summarize(netsales = sum(netsales)) %>% ungroup() %>%
        mutate(qoq = (netsales - lag(netsales, n = n_cats))/lag(netsales, n = n_cats))
    })
    ## plots by category (source / origin) ----
    output$beer_sales_yr_cat <- renderPlotly({
      CatChart("Yrly Beer Sales by Source", 
               beer_annual_data_cat(), "cyr", "netsales","category", beer_cat_color, 
               theme_xax,"M")
    })
    output$beer_sales_qtr_cat <- renderPlotly({
      CatChart("Qtrly Beer Sales by Source", 
               beer_qtr_data_cat(), "cyr_qtr", "netsales", "category", beer_cat_color, 
               theme_xax+theme_xaxq, "M")
    })
    ### facet: change by source ----
    output$beer_sales_yoy_cat_chg <- renderPlotly({
      x <- beer_annual_data_cat()
      CatChgChart("Yrly % Chg Beer Sales by Source", 
               x, x_var = "cyr", y_var = "yoy", fill_var = "cat_type", 
               fill_color = bar_col, 
               strp_color = bar_col,
               theme_xax+theme_nleg)
    })
    output$beer_sales_qoq_cat_chg <- renderPlotly({
      x <- beer_qtr_data_cat()
      CatChgChart("Qtrly % Chg Beer Sales by Source", 
               x, x_var = "cyr_qtr", y_var = "qoq", fill_var = "cqtr", 
               fill_color = qtr_color, 
               strp_color = bar_col,
               theme_xax+theme_xaxq+theme_nleg)
    })
    ## beer - bc subcategory ----
    ## data by subcat ----
    beer_yr_data_subcat <- reactive({
      n_subcats <- length(unique(beer_filtered_data()$subcat))
      beer_filtered_data() %>% group_by(cat_type, cyr, subcat) %>%
        summarize(netsales = sum(netsales),
                  litres = sum(litres)) %>% ungroup() %>%
        mutate(yoy_ns = (netsales - lag(netsales, n=n_subcats))/lag(netsales, n=n_subcats),
               yoy_l = (litres - lag(litres, n=n_subcats))/lag(litres, n=n_subcats)
      )
    })
    beer_qtr_data_subcat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_qtr <- length(input$qtr_check)
      #n_cats <- length(input$beer_cat_check)
      n_cats <- length(unique(beer_filtered_data()$subcat))
      beer_filtered_data() %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt, subcat) %>%
        summarize(netsales = sum(netsales)) %>% ungroup() %>%
        mutate(qoq = (netsales - lag(netsales, n = n_cats))/lag(netsales, n = n_cats))
    })
    ## bc subcat ----
    beer_bc_yr_subcat <- reactive({
      data <- beer_yr_data_subcat() %>% filter(str_detect(subcat, "BC"))
      data_yr <- data %>% group_by(cyr) %>%
        summarize(ttl_netsales = sum(netsales),
                  ttl_litres = sum(litres)) %>% ungroup() 
      data <- left_join(data, data_yr, by='cyr') %>%
        mutate(pct_sales = netsales/ttl_netsales,
               pct_litres = litres/ttl_litres)
    })
    
    ## plots by bc subcategory ----
    output$beer_sales_yr_bc_cat <- renderPlotly({
      data <- beer_bc_yr_subcat()
      CatChart2("Beer Sales $ by BC Category", 
               data, "cyr", "netsales","subcat", beer_bc_cat_color, "stack", theme_xax, "M")
    })
    output$beer_sales_yr_bc_cat_pc <- renderPlotly({
      #data <- beer_annual_data_subcat() %>% filter(str_detect(subcat, "BC"))
      data <- beer_bc_yr_subcat()
      CatChart2("Beer Sales % by BC Category", 
               data, "cyr", "pct_sales","subcat", beer_bc_cat_color, "fill", theme_xax, "%")
    })
    
  # REFRESH BEV ---------------------------------------------------------------
  ## apply filters to data ---------------------------------------------------
    cat("10 apply filters \n")
    # Filter the data set based on the selected categories
    refresh_filtered_data <- reactive({
      req(input$cyr_picker, input$qtr_check, input$refresh_cat_check)
      refresh_data %>% filter(cyr %in% input$cyr_picker) %>%
        filter(cqtr %in% input$qtr_check) %>%
        filter(category %in% input$refresh_cat_check)
    })  
    cat("11 aggregate annual & qtr totals \n")
    ## annual and qtr totals ---------------------------------------------------
    refresh_annual_data <- reactive({
      AnnualData(refresh_filtered_data())
    })
    
    refresh_qtr_data <- reactive({
      QtrData(refresh_filtered_data(), length(input$qtr_check))
    })
    ## plots ----
    ### sales - yr, qtr ----
    # similar to overview but using functions to get plot, providing:
    # - chart_title, dataset, bar col variable, list of theme modifications
    # - for themes, can list joined by '+'
    output$refresh_sales_yr <- renderPlotly({
      TtlChart("Net Refresh Bev $ Sales by Year", 
               refresh_annual_data(), 'cyr', 'netsales', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M")
    })
    ## plot sales by quarter
    output$refresh_sales_qtr <- renderPlotly({
      TtlChart("Net Refresh Bev $ Sales by Qtr", 
               refresh_qtr_data(), 'cyr_qtr', 'netsales', 'cqtr', qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "M")
    })
    ### change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$refresh_sales_yoy <- renderPlotly({
      PoPChart("% Chg Sales - Yr", refresh_annual_data(), "cyr", "yoy_sales", "cat_type", bar_col, 
               theme_xax+theme_nleg, "%")
    })
    output$refresh_sales_qoq <- renderPlotly({
      PoPChart("% Chg Sales - Qtr", refresh_qtr_data(), "cyr_qtr", "qoq_sales", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    ## refresh - cat ----
    ## data by cat ----
    refresh_annual_data_cat <- reactive({
      n_cats <- length(input$refresh_cat_check)
      AnnualCatData(refresh_filtered_data(), n_cats)
      # refresh_filtered_data() %>% group_by(cat_type, cyr, category) %>%
      #   summarize(netsales = sum(netsales)) %>% ungroup() %>%
      #   mutate(yoy = (netsales - lag(netsales, n=n_cats))/lag(netsales, n=n_cats),
      #          cat_type = reorder(cat_type, netsales, FUN = sum))
    })
    refresh_qtr_data_cat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_cats <- length(input$refresh_cat_check)
      n_qtr <- length(input$qtr_check)
      QtrCatData(refresh_filtered_data(), n_cats, n_qtr)
      #refresh_filtered_data() %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt, category) %>%
      #  summarize(netsales = sum(netsales)) %>% ungroup() %>%
      #  mutate(qoq = (netsales - lag(netsales, n = n_cats))/lag(netsales, n = n_cats))
    })
    ## plots by category ----
    output$refresh_sales_yr_cat <- renderPlotly({
      CatChart("Yrly Sales by Category", 
               refresh_annual_data_cat(), "cyr", "netsales","category", refresh_cat_color, 
               theme_xax,"M")
    })
    output$refresh_sales_qtr_cat <- renderPlotly({
      CatChart("Qtrly Sales by Category", 
               refresh_qtr_data_cat(), "cyr_qtr", "netsales", "category", refresh_cat_color, 
               theme_xax+theme_xaxq, "M")
    })
} # end server


