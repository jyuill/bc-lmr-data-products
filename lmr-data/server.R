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
# drop incomplete calendar year at start
#tbl_yq <- table(lmr_data$cyr, lmr_data$cqtr)
#if(any(tbl_yq[1,] == 0)) {
#  lmr_data <- lmr_data %>% filter(cyr != rownames(tbl_yq)[1])
#}

# load functions used - mostly plots
source('functions.R')

# Define server logic required to draw a histogram
function(input, output, session) {
  # experiment with different bs themes
  #bslib::bs_themer()
  # get data ----
  # query database via separate file for tidyness
  source('query.R')
  beer_data <- lmr_data %>% filter(cat_type == "Beer")
  beer_data <- beer_data %>% mutate(
    category = case_when(
      category == "Domestic - BC Beer" ~ "BC",
      category == "Domestic - Other Province Beer" ~ "Other Prov",
      category == "Import Beer" ~ "Import"
    )
  )
  refresh_data <- lmr_data %>% filter(cat_type == "Refresh Bev")
  spirits_data <- lmr_data %>% filter(cat_type == "Spirits")
  wine_data <- lmr_data %>% filter(cat_type == "Wine")
  
  # setup dynamic filters ----------------------------------------------------
  # Dynamically generate UI filters based on lmr_data
  # otherwise, app will crash because lmr_data not available for filters in ui.R
  # CHATGPT suggestion as alt to below
  ## year filter ----
  dynamic_cyr <- pickerInput(
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
  # CHATGPT: apply dynamic filters as needed to different tabs, based on selection
  output$dynamic_sidebar <- renderUI({
    if (input$tabselected == 1) {
      tagList(
        dynamic_cyr,
        dynamic_qtr,
        dynamic_cat
      )
    } else if (input$tabselected == 2) {
      tagList(
        dynamic_cyr,
        dynamic_qtr,
        dynamic_beer_cat
      )
    } else if (input$tabselected == 3) {
      tagList(
        dynamic_cyr
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
  annual_data_cat <- reactive({
    n_cats <- length(input$cat_check)
    filtered_data() %>% group_by(cyr, cat_type) %>%
      summarize(netsales = sum(netsales)) %>% ungroup() %>%
      mutate(yoy = (netsales - lag(netsales, n=n_cats))/lag(netsales, n=n_cats))
  })
  qtr_data_cat <- reactive({
    # need to base the qoq on the number of cats chosen in filter
    n_qtr <- length(input$qtr_check)
    n_cats <- length(input$cat_check)
    filtered_data() %>% group_by(cyr, cqtr, cyr_qtr, end_qtr_dt, cat_type) %>%
      summarize(netsales = sum(netsales)) %>% ungroup() %>%
      mutate(qoq = (netsales - lag(netsales, n = n_cats))/lag(netsales, n = n_cats))
  })
  # plots --------------------------------------------------------------------
  # plot theme
  theme_set(theme_light()+theme(panel.grid.minor = element_blank(),
                                panel.grid.major = element_line(color = 'grey90', linewidth=0.1)))
  # x-axis text - set angle and other formats
  theme_xax <- theme(axis.ticks.x = element_blank(),
                   axis.text.x = element_text(angle = 90, hjust = 1))
  theme_xaxq <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
  # no legend
  theme_nleg <- theme(legend.position = "none")
    ## sales ------------------------------------------------------------------
    # plot for sales by year
    output$sales_yr <- renderPlotly({
        # get filtered, aggregated data
        x <- annual_data()
        # plot
        ch_title <- "Net $ Sales by Year"
        p <- x %>%
          ggplot(aes(x = cyr, y = netsales)) +
          geom_col(fill=bar_col) +
          scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                             expand = expansion(mult=c(0,0.05))) +
          labs(title=ch_title, x="", y="")+
          theme_xax
        ggplotly(p)
    })
    # plot sales by quarter
    output$sales_qtr <- renderPlotly({
      x <- qtr_data()
      ch_title <- "Net $ Sales by Qtr"
      p <- x %>%
        ggplot(aes(x = cyr_qtr, y = netsales, fill = cqtr)) +
        geom_col() +
        scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                           expand = expansion(mult=c(0,0.05))) +
        scale_fill_manual(values=qtr_color)+
        labs(title=ch_title, x="", y="")+
        theme_xax+theme_xaxq+theme_nleg
      ggplotly(p)
    })
    ## change in sales --------------------------------------------------------
    # plot for year-over-year change in sales
    output$sales_yoy <- renderPlotly({
      x <- annual_data()
      max_y <- max(x$yoy, na.rm = TRUE)
      min_y <- min(x$yoy, na.rm = TRUE)
      max_val <- max(abs(min_y), abs(max_y))
      ch_title <- "% Chg Net $ Sales by Yr"
      p <- x %>% 
        ggplot(aes(x = cyr, y = yoy)) +
        geom_col(fill=bar_col) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        scale_y_continuous(labels = scales::percent,
                           expand = expansion(mult=c(0,0.05)),
                           limits = c(0 - max_val, max_val)) +
        labs(title=ch_title, x="", y="")+
        theme_xax
    })
    
    # plot for quarter-over-quarter change in sales
    output$sales_qoq <- renderPlotly({
      x <- qtr_data()
      max_y <- max(x$qoq, na.rm = TRUE)
      min_y <- min(x$qoq, na.rm = TRUE)
      max_val <- max(abs(min_y), abs(max_y))
      ch_title <- "% Chg Net $ Sales by Qtr"
      p <- x %>% 
        ggplot(aes(x = cyr_qtr, y = qoq, fill = cqtr)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        scale_y_continuous(labels = scales::percent,
                           expand = expansion(mult=c(0,0.05)),
                           limits = c(0 - max_val, max_val)) +
        scale_fill_manual(values=qtr_color)+
        labs(title=ch_title, x="", y="")+
        theme_xax+theme_xaxq+theme_nleg
    })
    
    ## sales by category --------------------------------------
    ## plot for annual sales by category
    output$sales_yr_cat <- renderPlotly({
      x <- annual_data_cat()
      x$cat_type <- reorder(x$cat_type, x$netsales, FUN = sum)
      ch_title <- "Net $ Sales by Cat by Yr"
      p <- x %>%
        ggplot(aes(x = cyr, y = netsales, fill = cat_type)) +
        geom_col(position = "stack") +
        scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                           expand = expansion(mult=c(0,0.05))) +
        scale_fill_manual(values=cat_type_color)+
        labs(title=ch_title, x="", y="")+
        theme(axis.ticks.x = element_blank(),
              legend.position = "top",
              legend.title = element_blank(),
              plot.margin = unit(c(1, 0, 0, 0), "cm"))+
        theme_xax
      p_plotly <- ggplotly(p)
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
      x$cat_type <- reorder(x$cat_type, x$netsales, FUN = sum)
      ch_title <- "Net $ Sales by Cat by Qtr"
      p <- x %>%
        ggplot(aes(x = cyr_qtr, y = netsales, fill = cat_type)) +
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
      p_plotly <- ggplotly(p)
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
      x$cat_type <- reorder(x$cat_type, x$yoy, FUN = sum)
      ch_title <- "% Chg Net $ Sales by Cat"
      p <- x %>%
        ggplot(aes(x = cyr, y = yoy)) +
        geom_col(fill=bar_col) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        facet_grid(cat_type~.) +
        scale_y_continuous(labels = scales::percent,
                           expand = expansion(mult=c(0,0.05))) +
        theme(strip.background = element_rect(fill = bar_col)) +
        theme(strip.text=element_text(color='white'))+
        labs(title=ch_title, x="", y="")+
        theme_xax
      ggplotly(p)
    })
    ## plot qoq chg by cat
    output$sales_qoq_cat <- renderPlotly({
      x <- qtr_data_cat()
      x$cat_type <- reorder(x$cat_type, x$qoq, FUN = sum)
      ch_title <- "% Chg Net $ Sales by Qtr"
      p <- x %>%
        ggplot(aes(x = cyr_qtr, y = qoq, fill = cqtr)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        facet_grid(cat_type~.) +
        scale_y_continuous(labels = scales::percent,
                           expand = expansion(mult=c(0,0.05))) +
        scale_fill_manual(values=qtr_color)+
        theme_light()+
        theme(strip.background = element_rect(fill = bar_col)) +
        theme(strip.text=element_text(color='white'))+
        labs(title=ch_title, x="", y="")+
        theme_xax+theme_xaxq+theme_nleg
      ggplotly(p)
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
      # test
      #beer_annual_data <- beer_data %>% group_by(cyr) %>% 
      #  summarize(netsales = sum(netsales)) %>% 
      #  mutate(yoy = (netsales - lag(netsales))/lag(netsales))
    })
    beer_qtr_data <- reactive({
      beer_filtered_data() %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt) %>%
        summarize(netsales = sum(netsales)) %>% ungroup() %>%
        mutate(qoq = (netsales - lag(netsales))/lag(netsales),
               yr_qtr = paste(cyr, cqtr, sep = "-")
        )
      # for testing
      #beer_qtr_data <- beer_data %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt) %>% 
      #  summarize(netsales = sum(netsales)) %>% ungroup() %>%
      #  mutate(qoq = (netsales - lag(netsales))/lag(netsales))
    })
    ## beer plots ----
    ### sales - yr, qtr ----
    # similar to overview but using functions to get plot, providing:
    # - chart_title, dataset, bar col variable, list of theme modifications
    # - for themes, can list joined by '+'
    output$beer_sales_yr <- renderPlotly({
      AnnualChart("Net Beer $ Sales by Year", beer_annual_data(), bar_col, theme_xax)
    })
    ## plot sales by quarter
    output$beer_sales_qtr <- renderPlotly({
      QtrChart("Net Beer $ Sales by Qtr", 
               beer_qtr_data(), qtr_color, theme_xax+theme_xaxq+theme_nleg)
    })
    
    ### change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$beer_sales_yoy <- renderPlotly({
      YoYChart("% Chg Beer Sales - Yr", beer_annual_data(), "cyr", bar_col, theme_xax)
    })
    output$beer_sales_qoq <- renderPlotly({
      QoQChart("% Chg Beer Sales - Qtr", beer_qtr_data(), qtr_color, theme_xax+theme_xaxq+theme_nleg)
    })
    ## beer - origin ----
    ### annual data by cat 
    beer_annual_data_cat <- reactive({
      n_cats <- length(input$beer_cat_check)
      beer_filtered_data() %>% group_by(cat_type, cyr, category) %>%
        summarize(netsales = sum(netsales)) %>% ungroup() %>%
        mutate(yoy = (netsales - lag(netsales, n=n_cats))/lag(netsales, n=n_cats))
    })
    beer_qtr_data_cat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_qtr <- length(input$qtr_check)
      n_cats <- length(input$beer_cat_check)
      beer_filtered_data() %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt, category) %>%
        summarize(netsales = sum(netsales)) %>% ungroup() %>%
        mutate(qoq = (netsales - lag(netsales, n = n_cats))/lag(netsales, n = n_cats))
    })
    ### sales by category (source / origin) ----
    output$beer_sales_yr_cat <- renderPlotly({
      CatChart("Yrly Beer Sales by Source", 
               beer_annual_data_cat(), "cyr", beer_cat_color, theme_xax)
    })
    output$beer_sales_qtr_cat <- renderPlotly({
      CatChart("Qtrly Beer Sales by Source", 
               beer_qtr_data_cat(), "cyr_qtr", beer_cat_color, theme_xax+theme_xaxq)
    })
    ### facet: change by source ----
    output$beer_sales_yoy_cat_chg <- renderPlotly({
      x <- beer_annual_data_cat()
      x$category <- reorder(x$category, x$yoy, FUN = sum)
      CatChgChart("Yrly % Chg Beer Sales by Source", 
               x, "cyr", "yoy", "cat_type", bar_col, theme_xax+theme_nleg)
    })
    output$beer_sales_qoq_cat_chg <- renderPlotly({
      x <- beer_qtr_data_cat()
      x$category <- reorder(x$category, x$qoq, FUN = sum)
      CatChgChart("Qtrly % Chg Beer Sales by Source", 
               x, "cyr_qtr", "qoq", "cqtr", bar_col, theme_xax+theme_xaxq+theme_nleg)
    })
}


