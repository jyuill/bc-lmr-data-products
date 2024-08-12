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

# set plot theme
theme_set(theme_classic())

# drop incomplete calendar year at start
#tbl_yq <- table(lmr_data$cyr, lmr_data$cqtr)
#if(any(tbl_yq[1,] == 0)) {
#  lmr_data <- lmr_data %>% filter(cyr != rownames(tbl_yq)[1])
#}

# Define server logic required to draw a histogram
function(input, output, session) {
  # query database via separate file for tidyness
  source('query.R')
  
  # setup dynamic filters ----------------------------------------------------
  # Dynamically generate UI filters based on lmr_data
  # otherwise, app will crash because lmr_data not available for filters in ui.R
  output$dynamic_cyr <- renderUI({
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
  })
  output$dynamic_qtr <- renderUI({
    checkboxGroupInput(inputId = "qtr_check", "Select a quarter", 
                        choices = sort(unique(lmr_data$cqtr)), 
                        selected = unique(lmr_data$cqtr),
                        inline = FALSE
    )
  })
  output$dynamic_cat <- renderUI({
    checkboxGroupInput(inputId = "cat_check", "Select a Category", 
                        choices = unique(lmr_data$cat_type), 
                        selected = unique(lmr_data$cat_type),
                        inline = FALSE
    )
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
    filtered_data() %>% group_by(cyr, cqtr, end_qtr_dt) %>%
      summarize(netsales = sum(netsales)) %>% ungroup() %>%
      mutate(qoq = (netsales - lag(netsales))/lag(netsales))
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
    filtered_data() %>% group_by(cyr, cqtr, end_qtr_dt, cat_type) %>%
      summarize(netsales = sum(netsales)) %>% ungroup() %>%
      mutate(qoq = (netsales - lag(netsales, n = n_cats))/lag(netsales, n = n_cats))
  })
  # plots --------------------------------------------------------------------
    # plot for sales by year
    output$sales_yr <- renderPlotly({
        # get filtered, aggregated data
        x <- annual_data()
        # plot
        p <- x %>%
          ggplot(aes(x = cyr, y = netsales)) +
          geom_col() +
          scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                             expand = expansion(mult=c(0,0.05))) +
          labs(title="Net $ Sales by Year", x="", y="")+
          theme(axis.ticks.x = element_blank())
        ggplotly(p)
    })
    # plot sales by quarter
    output$sales_qtr <- renderPlotly({
      x <- qtr_data()
      p <- x %>%
        ggplot(aes(x = end_qtr_dt, y = netsales)) +
        geom_col() +
        scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                           expand = expansion(mult=c(0,0.05))) +
        labs(title="Net $ Sales by Quarter", x="", y="")+
        theme(axis.ticks.x = element_blank())
      ggplotly(p)
    })
    
    # plot for year-over-year change in sales
    output$sales_yoy <- renderPlotly({
      x <- annual_data()
      max_y <- max(x$yoy, na.rm = TRUE)
      min_y <- min(x$yoy, na.rm = TRUE)
      max_val <- max(abs(min_y), abs(max_y))
      p <- x %>% 
        ggplot(aes(x = cyr, y = yoy)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        scale_y_continuous(labels = scales::percent,
                           expand = expansion(mult=c(0,0.05)),
                           limits = c(0 - max_val, max_val)) +
        labs(title='% Change in Net $ Sales', x="", y="")+
        theme(axis.ticks.x = element_blank())
    })
    
    # plot for quarter-over-quarter change in sales
    output$sales_qoq <- renderPlotly({
      x <- qtr_data()
      max_y <- max(x$qoq, na.rm = TRUE)
      min_y <- min(x$qoq, na.rm = TRUE)
      max_val <- max(abs(min_y), abs(max_y))
      p <- x %>% 
        ggplot(aes(x = end_qtr_dt, y = qoq)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        scale_y_continuous(labels = scales::percent,
                           expand = expansion(mult=c(0,0.05)),
                           limits = c(0 - max_val, max_val)) +
        labs(title='% Change in Net $ Sales by Qtr', x="", y="")+
        theme(axis.ticks.x = element_blank())
    })
    
    ## plot for annual sales by category --------------------------------------
    output$sales_yr_cat <- renderPlotly({
      x <- annual_data_cat()
      x$cat_type <- reorder(x$cat_type, x$netsales, FUN = sum)
      p <- x %>%
        ggplot(aes(x = cyr, y = netsales, fill = cat_type)) +
        geom_col(position = "stack") +
        scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                           expand = expansion(mult=c(0,0.05))) +
        labs(title="Net $ Sales by Year and Category", x="", y="")+
        theme(axis.ticks.x = element_blank(),
              legend.position = "top",
              legend.title = element_blank())
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
    
    ## plot for qtr sales by category -----------------------------------------
    output$sales_qtr_cat <- renderPlotly({
      x <- qtr_data_cat()
      x$cat_type <- reorder(x$cat_type, x$netsales, FUN = sum)
      p <- x %>%
        ggplot(aes(x = end_qtr_dt, y = netsales, fill = cat_type)) +
        geom_col(position = "stack") +
        scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                           expand = expansion(mult=c(0,0.05))) +
        labs(title="Net $ Sales by Qtr and Category", x="", y="")+
        theme(axis.ticks.x = element_blank(),
              legend.position = "top",
              legend.title = element_blank())
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

    ## plot yoy chg by cat
    output$sales_yoy_cat <- renderPlotly({
      x <- annual_data_cat()
      x$cat_type <- reorder(x$cat_type, x$yoy, FUN = sum)
      p <- x %>%
        ggplot(aes(x = cyr, y = yoy)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        facet_grid(cat_type~.) +
        scale_y_continuous(labels = scales::percent,
                           expand = expansion(mult=c(0,0.05))) +
        labs(title='% Change in Net $ Sales by Category', x="", y="")+
        theme(axis.ticks.x = element_blank())
      ggplotly(p)
    })
    
    output$sales_qoq_cat <- renderPlotly({
      x <- qtr_data_cat()
      x$cat_type <- reorder(x$cat_type, x$qoq, FUN = sum)
      p <- x %>%
        ggplot(aes(x = end_qtr_dt, y = qoq)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        facet_grid(cat_type~.) +
        scale_y_continuous(labels = scales::percent,
                           expand = expansion(mult=c(0,0.05))) +
        labs(title='% Change in Net $ Sales by Qtr and Category', x="", y="")+
        theme(axis.ticks.x = element_blank())
      ggplotly(p)
    })
}


