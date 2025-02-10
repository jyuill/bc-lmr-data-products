#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(bslib)
library(RColorBrewer)
library(shinyjs)
library(ellmer)
library(lubridate)
library(scales)
library(plotly)
library(here)

# set color palette
# - bar and line colors
bar_col <- brewer.pal(n=9, name='YlGnBu')[9] # #081D58
# - palette for use with categories
bpal <- brewer.pal(n=9, name="YlOrRd")
cat_type_color <- c("Beer"=bpal[6], "Refresh Bev"=bpal[3], "Spirits"=bpal[4], "Wine"=bpal[8])
# - palette for quarters
qpal <- brewer.pal(n=9, name="Blues")
qtr_color <- c("Q1"=qpal[5], "Q2"=qpal[7], "Q3"=qpal[8], "Q4"=qpal[9])

# load functions used - mostly plots
source('functions.R')

## GPT integration -----------------------------------------------------------
# gpt-4o does much better than gpt-4o-mini, especially at interpreting plots
openai_model <- "gpt-4o"

# Dynamically create the system prompt, based on the real data. For an actually
# large database, you wouldn't want to retrieve all the data like this, but
# instead either hand-write the schema or write your own routine that is more
# efficient than system_prompt().
system_prompt_str <- system_prompt(dbGetQuery(conn, "SELECT * FROM tips"), "tips")

# This is the greeting that should initially appear in the sidebar when the app
# loads.
greeting <- paste(readLines(here("greeting.md")), collapse = "\n")


# Define server logic -----
function(input, output, session) {

  # get data ----
  # query database via separate file for tidyness
  ## all data ----
  source('query.R')
  # apply to yr filter as default to avoid over-crowding
  lmr_max <- max(lmr_data$cyr_num) # get current latest yr
  lmr_yrs <- 6 # determine how many yrs back to go
  lmr_recent <- lmr_data %>% filter(cyr_num > lmr_max-lmr_yrs)
  lmr_max_date <- max(lmr_data$end_qtr_dt)
  # for top of sidebar on pg, set in dynamic sidebar
  lmr_max_note <- paste0("Data as of: ", format(lmr_max_date, "%b %d %Y"))
  
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
  
  # dynamic sidebar ----
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
      )
    } else if (input$tabselected == 3) {
      tagList(
        
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
  
  # TTL PLOTS ------------------------------------------------------------------
  ## ttl sales ----
  # plot for sales by year
  # NOTE: functions were later created after the fact 
  # to avoid repeating code for these plots; see functions.R
  # could be applied here but did not go back to refactor
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
    x <- x %>% tooltip_fmt(dim = 'cyr', units = '%', y_var = 'yoy_sales')
    max_y <- max(x$yoy_sales, na.rm = TRUE)
    min_y <- min(x$yoy_sales, na.rm = TRUE)
    max_val <- max(abs(min_y), abs(max_y))
    ch_title <- "% Chg Net $ Sales by Yr"
    p <- x %>% 
      ggplot(aes(x = cyr, y = yoy_sales, text = tooltip_text)) +
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
    x <- x %>% tooltip_fmt(dim = 'cat_type', units = 'M', y_var = 'netsales') %>%
      mutate(cat_type = fct_reorder(cat_type, netsales, .fun = sum, .desc = FALSE))
    print(head(x))
    
    print(unique(x$cat_type))
    ch_title <- "Net $ Sales by Cat. by Yr"
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
  
  # plot for % of total sales by category type
  output$sales_yr_cat_pct <- renderPlotly({
    x <- annual_data_cat()
    x <- x %>% tooltip_fmt(dim = 'cat_type', units = '%', y_var = 'pct_ttl_sales') %>%
      mutate(cat_type = fct_reorder(cat_type, netsales, .fun = sum, .desc = FALSE))
    ch_title <- "% of Total Sales by Cat by Yr"
    p <- x %>%
      ggplot(aes(x = cyr, y = pct_ttl_sales, fill = cat_type, text=tooltip_text)) +
      geom_col(position = "stack") +
      scale_y_continuous(labels = scales::percent,
                         expand = expansion(mult=c(0,0.05))) +
      scale_fill_manual(values=cat_type_color)+
      labs(title=ch_title, x="", y="")+
      theme(axis.ticks.x = element_blank(),
            legend.position = "top",
            legend.title = element_blank(),
            plot.margin = unit(c(1, 0, 0, 0), "cm"))+
      theme_xax #+
    #tooltip_fmt('cat_type', "%", 'pct_ttl_sales')
    p_plotly <- ggplotly(p, tooltip = "text")
    # Customize legend in plotly
    p_plotly <- p_plotly %>% layout(
      legend = layout_legend_hc
    )
    p_plotly
  })
  
  ## plot for qtr sales by category 
  ## - REPLACE with % of total above
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
      legend = layout_legend_hc
    )
    p_plotly
  })
  
  ## facet plots --------------------------------------------
  ## plot yoy chg by cat
  output$sales_yoy_cat <- renderPlotly({
    x <- annual_data_cat()
    CatChgChart(yr_sales_pc_chg_cat, 
                x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales",
                fill_var = "dummy_fill", 
                facet_var = "cat_type",
                fill_color = bar_col, 
                strp_color = bar_col,
                theme_xax+theme_nleg, tunits="%")
  })
  ## plot for % point change by category type
  output$sales_yoy_cat_pcp <- renderPlotly({
    x <- annual_data_cat()
    CatChgChart(yr_sales_pcpt_chg_cat, 
                x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales",
                fill_var = "dummy_fill", 
                facet_var = "cat_type",
                fill_color = bar_col, 
                strp_color = bar_col,
                theme_xax+theme_nleg, tunits="num")
  })
  
  ## plot qoq chg by cat - abandoned for % point change above
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

}
