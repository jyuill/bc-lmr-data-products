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
          tags$a(href="#wine_cat_sales", "Category Sales"), tags$br()
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
    
    # BEER ---------------------------------------------------------------
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
    ## BEER PLOTS ----
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
    ## beer - cat - origin ----
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
    ## plots by category (source / origin) ----
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
    # test
    # x <- beer_annual_test
    # CatChgChart("Yrly % Chg Sales by Source", 
    #            x, x_var = "cyr", y_var = "yoy_sales", fill_var = "cat_type", 
    #            facet_var = "category",
    #            fill_color = bar_col, 
    #            strp_color = bar_col,
    #            theme_xax+theme_nleg)
    
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
    
    ## data by subcat ----
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
    ## qtr - abandoned in favour of % of total
    beer_qtr_data_subcat <- reactive({
      cat("bc beer subcat \n")
      # need to base the qoq on the number of cats chosen in filter
      n_qtr <- length(input$qtr_check)
      #n_cats <- length(input$beer_cat_check)
      n_cats <- length(unique(beer_filtered_data()$subcategory))
      beer_filtered_data() %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt, subcategory) %>%
        summarize(netsales = sum(netsales)) %>% ungroup() %>%
        mutate(qoq = (netsales - lag(netsales, n = n_cats))/lag(netsales, n = n_cats))
    })
    ## bc beer subcat ----
    ## plots by bc subcategory ----
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
      AnnualCatTypeData(refresh_filtered_data())
    })
    
    refresh_qtr_data <- reactive({
      QtrData(refresh_filtered_data(), length(input$qtr_check))
    })
    ## PLOTS ----
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
    ### categories ----
    #### data by cat ----
    refresh_annual_data_cat <- reactive({
      n_cats <- length(input$refresh_cat_check)
      AnnualCatData(refresh_filtered_data(), refresh_data)
    })
    refresh_qtr_data_cat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_cats <- length(input$refresh_cat_check)
      n_qtr <- length(input$qtr_check)
      QtrCatData(refresh_filtered_data(), n_cats, n_qtr)
    })
    #### plots by category ----
    output$refresh_sales_yr_cat <- renderPlotly({
      CatChart(yr_sales_cat, 
               refresh_annual_data_cat(), "cyr", "netsales","category", refresh_cat_color, 
               "stack", theme_xax,"M")
    })
    output$refresh_sales_yr_cat_pc <- renderPlotly({
      CatChart(yr_sales_pc_cat, 
               refresh_annual_data_cat(), "cyr", "pct_ttl_sales", "category", refresh_cat_color, 
               "stack",theme_xax, "%")
    })
    #### facet: chg category ----
    output$refresh_sales_yoy_cat_chg <- renderPlotly({
      x <- refresh_annual_data_cat()
      CatChgChart(yr_sales_pc_chg_cat, 
               x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales",
               fill_var = "cat_type", 
               facet_var = "category",
               fill_color = bar_col, 
               strp_color = bar_col,
               theme_xax+theme_nleg)
    })
    # % point chg in % of total yoy
    output$refresh_sales_yoy_cat_chg_pcp <- renderPlotly({
      x <- refresh_annual_data_cat()
      CatChgChart(yr_sales_pcpt_chg_cat,
                  x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales",
                  fill_var = "cat_type",
                  facet_var = "category",
                  fill_color = bar_col, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg, tunits = "num")
    })
    
  # SPIRITS ---------------------------------------------------------------
    cat("process spirits \n")
  ## apply filters to data ---------------------------------------------------
    cat("20 apply filters \n")
    # Filter the data set based on the selected categories
    spirits_filtered_data <- reactive({
      req(input$cyr_picker, input$qtr_check, input$spirits_cat_check)
      spirits_data %>% filter(cyr %in% input$cyr_picker) %>%
        filter(cqtr %in% input$qtr_check) %>%
        filter(category %in% input$spirits_cat_check)
    })  
    cat("21 aggregate annual & qtr totals \n")
    ## annual and qtr totals ---------------------------------------------------
    spirits_annual_data <- reactive({
      AnnualCatTypeData(spirits_filtered_data())
    })
    
    spirits_qtr_data <- reactive({
      QtrData(spirits_filtered_data(), length(input$qtr_check))
    })
    ## PLOTS ----
    ### sales - yr, qtr ----
    # similar to overview but using functions to get plot, providing:
    # - chart_title, dataset, bar col variable, list of theme modifications
    # - for themes, can list joined by '+'
    output$spirits_sales_yr <- renderPlotly({
      TtlChart("Net Spirits $ Sales by Year", 
               spirits_annual_data(), 'cyr', 'netsales', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M")
    })
    ## plot sales by quarter
    output$spirits_sales_qtr <- renderPlotly({
      TtlChart("Net Spirits $ Sales by Qtr", 
               spirits_qtr_data(), 'cyr_qtr', 'netsales', 'cqtr', qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "M")
    })
    ### change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$spirits_sales_yoy <- renderPlotly({
      PoPChart("% Chg Sales - Yr", spirits_annual_data(), "cyr", "yoy_sales", "cat_type", bar_col, 
               theme_xax+theme_nleg, "%")
    })
    output$spirits_sales_qoq <- renderPlotly({
      PoPChart("% Chg Sales - Qtr", spirits_qtr_data(), "cyr_qtr", "qoq_sales", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    ### categories ----
    #### data by cat ----
    spirits_annual_data_cat <- reactive({
      n_cats <- length(input$spirits_cat_check)
      AnnualCatData(spirits_filtered_data(), spirits_data)
    })
    spirits_qtr_data_cat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_cats <- length(input$spirits_cat_check)
      n_qtr <- length(input$qtr_check)
      QtrCatData(spirits_filtered_data(), n_cats, n_qtr)
    })
    #### plots by category ----
    output$spirits_sales_yr_cat <- renderPlotly({
      CatChart("Yrly Sales by Category", 
               spirits_annual_data_cat(), "cyr", "netsales","category", spirits_cat_color, 
               "stack", theme_xax,"M") %>%
      layout(legend = layout_legend_vr)          # Position on the y-axis
    })
    output$spirits_sales_yr_cat_pc <- renderPlotly({
      CatChart("% of Ttl Sales by Category", 
               spirits_annual_data_cat(), "cyr", "pct_ttl_sales", "category", spirits_cat_color, 
               "stack",theme_xax+theme_xax, "%") %>%
        layout(legend = layout_legend_vr)          # Position on the y-axis
    })
    ### facet: change by category ----
    output$spirits_sales_yoy_cat <- renderPlotly({
      x <- spirits_annual_data_cat()
      CatChgChart("Yrly % Chg Sales by Category", 
               x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales",
               fill_var = "cat_type", 
               facet_var = "category",
               fill_color = bar_col, 
               strp_color = bar_col,
               theme_xax+theme_nleg+theme_facet)
    })
    # % point chg in % of total yoy
    output$spirits_sales_yoy_cat_chg_pcp <- renderPlotly({
      x <- spirits_annual_data_cat()
      CatChgChart("Yrly % Pt Chg Sales % of Ttl.", 
                  x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales",
                  fill_var = "cat_type", 
                  facet_var = "category",
                  fill_color = bar_col, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg+theme_facet, tunits = "num")
    })
    
  # WINE ---------------------------------------------------------------
    cat("process wine \n")
  ## apply filters to data ---------------------------------------------------
    cat("30 apply filters \n")
    # Filter the data set based on the selected categories
    wine_filtered_data <- reactive({
      req(input$cyr_picker, input$qtr_check, input$wine_cat_picker)
      wine_data %>% filter(cyr %in% input$cyr_picker) %>%
        filter(cqtr %in% input$qtr_check) %>%
        filter(category %in% input$wine_cat_picker)
    })  
    ### cat duplicate -----
    # duplicates of the above for wine categories -> using different cat filter
    # filtered
    wine_filtered_data_cat <- reactive({
      req(input$cyr_picker, input$qtr_check, input$wine_cat_chart_picker)
      wine_data %>% filter(cyr %in% input$cyr_picker) %>%
        filter(cqtr %in% input$qtr_check) %>%
        filter(category %in% input$wine_cat_chart_picker)
    })
    
    cat("31 aggregate annual & qtr totals \n")
    ## annual and qtr totals ---------------------------------------------------
    wine_annual_data <- reactive({
      AnnualCatTypeData(wine_filtered_data())
    })
    
    wine_qtr_data <- reactive({
      QtrData(wine_filtered_data(), length(input$qtr_check))
    })
    
    ## PLOTS ----
    ### sales - yr, qtr ----
    # similar to overview but using functions to get plot, providing:
    # - chart_title, dataset, bar col variable, list of theme modifications
    # - for themes, can list joined by '+'
    output$wine_sales_yr <- renderPlotly({
      TtlChart("Net Wine $ Sales by Year", 
               wine_annual_data(), 'cyr', 'netsales', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M")
    })
    ## plot sales by quarter
    output$wine_sales_qtr <- renderPlotly({
      TtlChart("Net Wine $ Sales by Qtr", 
               wine_qtr_data(), 'cyr_qtr', 'netsales', 'cqtr', qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "M")
    })
    ### change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$wine_sales_yoy <- renderPlotly({
      PoPChart("% Chg Sales - Yr", wine_annual_data(), "cyr", "yoy_sales", "cat_type", bar_col, 
               theme_xax+theme_nleg, "%")
    })
    output$wine_sales_qoq <- renderPlotly({
      PoPChart("% Chg Sales - Qtr", wine_qtr_data(), "cyr_qtr", "qoq_sales", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    ### categories ----
    ## - need to find a way to show top 5 or so categories only
    #### data by cat ----
    wine_annual_data_cat <- reactive({
      n_cats <- length(input$wine_cat_chart_picker)
      AnnualCatData(wine_filtered_data_cat(), wine_data)
    })
    wine_qtr_data_cat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_cats <- length(input$wine_cat_chart_picker)
      n_qtr <- length(input$qtr_check)
      QtrCatData(wine_filtered_data_cat(), n_cats, n_qtr)
    })
    #### plots by category ----
    output$wine_sales_yr_cat <- renderPlotly({
      CatChart("Yrly Sales by Category", 
               wine_annual_data_cat(), "cyr", "netsales","category", wine_cat_color, 
               "stack", theme_xax,"M") %>%
      layout(legend = layout_legend_vr)          # Position on the y-axis
    })
    output$wine_sales_yr_cat_pc <- renderPlotly({
      CatChart("Qtrly Sales by Category", 
               wine_annual_data_cat(), "cyr", "pct_ttl_sales", "category", wine_cat_color, 
               "stack",theme_xax+theme_xax, "%") %>%
        layout(legend = layout_legend_vr)          # Position on the y-axis
    })
    ### facet: change by category ----
    output$wine_sales_yoy_cat <- renderPlotly({
      x <- wine_annual_data_cat()
      CatChgChart("Yrly % Chg Sales by Category", 
                  x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales",
                  fill_var = "cat_type", 
                  facet_var = "category",
                  fill_color = bar_col, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg+theme_facet)
    })
    # % point chg in % of total yoy
    output$wine_sales_yoy_cat_chg_pcp <- renderPlotly({
      x <- wine_annual_data_cat()
      CatChgChart("Yrly % Pt Chg Sales % of Ttl.", 
                  x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales",
                  fill_var = "cat_type", 
                  facet_var = "category",
                  fill_color = bar_col, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg+theme_facet, tunits = "num")
    })
    ## treemap wine countries, categories ----
    output$wine_sales_country_treemap <- renderPlot({
      cat("wine_country \n")
      # wine_filtered_data()
      data <- wine_data
      data <- data %>% filter(end_qtr_dt == max(data$end_qtr_dt))
      # Create a treemap
      ggplot(data, aes(
        area = netsales, 
        fill = category,
        subgroup = category,
        label = paste(subcategory, "\n$", netsales)
      )) +
        geom_treemap() +
        geom_treemap_subgroup_border(color = "black", size = 1.5) + # Border for categories
        geom_treemap_text(
          color = "white", place = "center",
          grow = TRUE
        ) +
        labs(title = "Sales by Category and Subcategory") +
        theme_minimal()+
        theme(legend.position = "none")
    })
} # end server


