# BC Liquor Market Review - server logic for Shiny web app
# - LMR dashboard
# You can run the
# application by clicking 'Run App' above.
#
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
#max_qtr_color <- c("Q4"=bar_col, "Q3"=qpal[5], "Q2"=qpal[5], "Q1"=qpal[5])
# for full vs partial year in annual charts
yr_flag_color <- c("full"=bar_col, "partial"='grey70')
# palette for beer categories
beer_pal <- brewer.pal(n=11, name="RdYlGn")
beer_cat_color <- c("BC"=beer_pal[11], "Other Prov"=beer_pal[7], "Import"=beer_pal[9])
beer_bc_cat_color <- c("BC Major"=beer_pal[11], "BC Regional"=beer_pal[10], "BC Micro"=beer_pal[9])
refresh_cat_color <- c("Cider"=beer_pal[10], "Coolers"=beer_pal[11])
spirits_cat_color <- brewer.pal(n=12, name="Paired")
# for wine categories, generate a custom color palette with 24 colors
wine_cat_color <- colorRampPalette(brewer.pal(n=12, name="Paired"))(24)
# linewidth for line charts
lwidth <- 1
# point size for line charts
lpointsize <- 2.2

# get data ----
# query database via separate file for tidyness
# load here - only needed once per session
# returns lmr_data frm cloud db with some cleaning 
cat("00 query db from query.R \n")
#source(here("BC-Liquor-Market-Review","query_pg.R")) # local testing
source('query_pg.R')

# drop incomplete calendar year at start
#tbl_yq <- table(lmr_data$cyr, lmr_data$cqtr)
#if(any(tbl_yq[1,] == 0)) {
#  lmr_data <- lmr_data %>% filter(cyr != rownames(tbl_yq)[1])
#}

# load functions used - mostly plots
#source(here("BC-Liquor-Market-Review","functions.R")) # local testing
source('functions.R')

# Define server logic ----
function(input, output, session) {
  # experiment with different bs themes
  #bslib::bs_themer()
  # toggle sidebar ----
  # for toggling sidebar, using shinyjs 
  # - NOT USED - clunky
  # - makes sidebar disappear / appear but mainPanel doesn't expand - pointless
  #observeEvent(input$toggleSidebar, {
  #  toggle("sidebar")
  #})
  
  ## recent data ----
  # apply to yr filter as default to avoid over-crowding
  lmr_max <- max(lmr_data$cyr_num) # get current latest yr
  lmr_yrs <- 6 # determine how many yrs back to go
  lmr_recent <- lmr_data %>% filter(cyr_num > lmr_max-lmr_yrs)
  # get max date for note at top of sidebar
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
  dynamic_wine_cat <- checkboxGroupInput(inputId = "wine_cat_check", "Select a Country", 
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
      `count-selected-text` = "{0} countries selected",
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
      label = "Select for Ctry Charts:",
      choices = unique(wine_data$category),
      selected = unique(wine_cat_top$category),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 3",
        `count-selected-text` = "{0} countries selected",
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
          tags$a(href="#wine_cat_sales", "Country Sales"), tags$br(),
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
              litres = sum(litres),
              max_qtr = max(as.character(cqtr)) # for yr flag
            ) %>%
    mutate(yoy_sales = (netsales - lag(netsales))/lag(netsales),
           yoy_litres = (litres - lag(litres))/lag(litres),
           # two yr flags - one for lines, one for points
           yr_flag = ifelse(max_qtr == "Q4", "full", "partial"),
           yr_flag_line = ifelse(yr_flag == "partial", "partial", lead(yr_flag))
          )
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
    AnnualCatTypeData(filtered_data(), filtered_data())
  })
  # test
  #ancattype <- AnnualCatTypeData(lmr_data)
  
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
  yr_sales <- "Net $ Sales by Yr (grey = partial yr)"
  qtr_sales <- "Net $ Sales by Quarter"
  pop_chg_sales <- "% Chg $ Sales - by Year (grey = partial yr)"
  pop_chg_sales_qtr <- "% Chg $ Sales by Qtr"
  yr_sales_cat <-  "Sales by Category"
  yr_sales_pc_cat <-  "% Share of Ttl Sales"
  yr_sales_pc_chg_cat <- "% Chg in Sales"
  yr_sales_pcpt_chg_cat <-  "Point Chg Share of Ttl Sales"
  
    # TTL PLOTS ------------------------------------------------------------------
    ## ttl sales ----
    # plot for sales by year
  # NOTE: functions were later created after the fact 
  # to avoid repeating code for these plots; see functions.R
  # could be applied here but did not go back to refactor
    output$sales_yr <- renderPlotly({
      cat("sales data by year \n")
        ch_title <- yr_sales
        # get filtered, aggregated data
        x <- annual_data()
        # add tooltip column formatted to data
        x <- x %>% tooltip_fmt(dim = 'cyr', units = 'B', y_var = 'netsales') 
        # plot
        
        # separate out partial year data for line overlay - if exists; otherwise use full dataset
        if("partial" %in% unique(x$yr_flag_line)) {
              x_partial <- x %>% filter(yr_flag_line == "partial")
          } else {
              x_partial <- x 
          }
        # plot
        p <- x %>%
          ggplot(aes(x = cyr, y = netsales, text=tooltip_text, group=1)) +
          # main line
          geom_line(linewidth = lwidth, color=bar_col) +
          # additional ovelay for partial year
          geom_line(data=x_partial, linewidth=lwidth, aes(color=yr_flag_line)) +
          geom_point(aes(color = yr_flag), size=lpointsize) + # points colored by full/partial yr
          scale_color_manual(values=yr_flag_color) +
          scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                             expand = expansion(mult=c(0,0.05)),
                             limits = c(0, max(x$netsales, na.rm = TRUE))) +
          labs(title=ch_title, x="", y="")+
          theme_xax + theme_nleg
      
        ggplotly(p, tooltip = "text") # specify tooltip to avoid default showing
    })
    # plot sales by quarter
    output$sales_qtr <- renderPlotly({
      x <- qtr_data()
      x <- x %>% tooltip_fmt(dim = 'cyr_qtr', units = 'M', y_var = 'netsales') 
      ch_title <- qtr_sales
      p <- x %>%
        ggplot(aes(x = cyr_qtr, y = netsales, text = tooltip_text, group = 1)) +
        geom_line(size=lwidth, color=bar_col) +
        geom_point(size=lpointsize, aes(color=cqtr)) +
        scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                           expand = expansion(mult=c(0,0.05)),
                           limits = c(0, max(x$netsales, na.rm = TRUE))) +
        scale_color_manual(values=qtr_color)+
        labs(title=ch_title, x="", y="") +
        theme_xax+theme_xaxq+theme_nleg 
      
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
      ch_title <- pop_chg_sales
      
      p <- x %>% 
        ggplot(aes(x = cyr, y = yoy_sales, fill = yr_flag, text = tooltip_text)) +
        geom_col() +
        scale_fill_manual(values=yr_flag_color) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        scale_y_continuous(labels = scales::label_percent(accuracy = 0.1), 
                           expand = expansion(mult=c(0,0.05)),
                           limits = c(0 - max_val, max_val)) +
        labs(title=ch_title, x="", y="")+
        theme_xax+theme_nleg 
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
    beer_annual_data <- reactive({
      AnnualCatTypeData(beer_filtered_data())
    })
    beer_qtr_data <- reactive({
      QtrData(beer_filtered_data(), length(input$qtr_check))
    })
    ## BEER PLOTS ----
    ### sales - yr, qtr ----
    # similar to overview but using functions to get plot, providing:
    # - chart_title, dataset, bar col variable, list of theme modifications
    # - for themes, can list joined by '+'
    output$beer_sales_yr <- renderPlotly({
      TtlChart(yr_sales, 
               beer_annual_data(), 'cyr', 'netsales', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M", yr_flag_color, lwidth, lpointsize)
    })
    ## plot sales by quarter
    output$beer_sales_qtr <- renderPlotly({
      QtrChart(qtr_sales, 
               beer_qtr_data(), 'cyr_qtr', 'netsales', 'cqtr', qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "M", lwidth, lpointsize)
    })
    
    ### change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$beer_sales_yoy <- renderPlotly({
      PoPChart(pop_chg_sales, beer_annual_data(), "cyr", "yoy_sales", "yr_flag", yr_flag_color, 
               theme_xax+theme_nleg, "%")
    })
    output$beer_sales_qoq <- renderPlotly({
      PoPChart(pop_chg_sales_qtr, beer_qtr_data(), "cyr_qtr", "qoq_sales", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    ## beer - cat - origin ----
    ## data by cat ----
    ## yoy = $ SALES ONLY
    beer_annual_data_cat <- reactive({
      n_cats <- length(input$beer_cat_check)
      AnnualCatData(beer_filtered_data(), beer_filtered_data())
    })
    # test annual category data
    n_cats <- length(unique(beer_data$category))
    beer_annual_test <- AnnualCatData(beer_data, beer_data)
    
    # # qtr - not used
    # beer_qtr_data_cat <- reactive({
    #   # need to base the qoq on the number of cats chosen in filter
    #   n_qtr <- length(input$qtr_check)
    #   n_cats <- length(input$beer_cat_check)
    #   QtrCatData(beer_filtered_data(), n_cats, n_qtr)
    #   #beer_filtered_data() %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt, category) %>%
    #   #  summarize(netsales = sum(netsales)) %>% ungroup() %>%
    #   #  mutate(qoq = (netsales - lag(netsales, n = n_cats))/lag(netsales, n = n_cats))
    # })
    ## plots by category (source / origin) ----
    output$beer_sales_yr_cat <- renderPlotly({
      CatChart("Sales by Source", 
               beer_annual_data_cat(), "cyr", "netsales","category", 
               beer_cat_color, "stack",
               theme_xax,"M")
    })
    output$beer_sales_yr_cat_pc <- renderPlotly({
      CatChart("% Share of Total by Source", 
               beer_annual_data_cat(), "cyr", "pct_ttl_sales","category", 
               beer_cat_color, "fill",
               theme_xax,tunits="%")
    })
    
    ### facet: % change by source ----
    output$beer_sales_yoy_cat_chg <- renderPlotly({
      x <- beer_annual_data_cat()
      CatChgChart(yr_sales_pc_chg_cat, 
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
      CatChgChart(yr_sales_pcpt_chg_cat, 
               x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales", 
               fill_var = "cat_type", 
               facet_var = "category",
               fill_color = bar_col, 
               strp_color = bar_col,
               theme_xax+theme_nleg, tunits = "num")
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
      CatChgChart(yr_sales_pc_chg_cat, 
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
      CatChgChart(yr_sales_pcpt_chg_cat, 
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
      CatChart("% Share of Import $ by Ctry/Reg", 
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
      TtlChart(yr_sales, 
               refresh_annual_data(), 'cyr', 'netsales', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M", yr_flag_color, lwidth, lpointsize)
    })
    ## plot sales by quarter
    output$refresh_sales_qtr <- renderPlotly({
      QtrChart(qtr_sales, 
               refresh_qtr_data(), 'cyr_qtr', 'netsales', 'cqtr', qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "M", lwidth, lpointsize)
    })
    ### change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$refresh_sales_yoy <- renderPlotly({
      PoPChart(pop_chg_sales, refresh_annual_data(), "cyr", "yoy_sales", "yr_flag", yr_flag_color, 
               theme_xax+theme_nleg, "%")
    })
    output$refresh_sales_qoq <- renderPlotly({
      PoPChart(pop_chg_sales_qtr, refresh_qtr_data(), "cyr_qtr", "qoq_sales", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    ### categories ----
    #### data by cat ----
    refresh_annual_data_cat <- reactive({
      n_cats <- length(input$refresh_cat_check)
      AnnualCatData(refresh_filtered_data(), refresh_filtered_data())
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
               refresh_annual_data_cat(), "cyr", "pct_ttl_sales", "category", 
               refresh_cat_color, 
               "fill",theme_xax, "%")
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
      TtlChart(yr_sales, 
               spirits_annual_data(), 'cyr', 'netsales', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M", yr_flag_color, lwidth, lpointsize)
    })
    ## plot sales by quarter
    output$spirits_sales_qtr <- renderPlotly({
      QtrChart(qtr_sales, 
               spirits_qtr_data(), 'cyr_qtr', 'netsales', 'cqtr', qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "M", lwidth, lpointsize)
    })
    ### change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$spirits_sales_yoy <- renderPlotly({
      PoPChart(pop_chg_sales, spirits_annual_data(), "cyr", "yoy_sales", "yr_flag", yr_flag_color, 
               theme_xax+theme_nleg, "%")
    })
    output$spirits_sales_qoq <- renderPlotly({
      PoPChart(pop_chg_sales_qtr, spirits_qtr_data(), "cyr_qtr", "qoq_sales", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    ### categories ----
    #### data by cat ----
    spirits_annual_data_cat <- reactive({
      n_cats <- length(input$spirits_cat_check)
      AnnualCatData(spirits_filtered_data(), spirits_filtered_data())
    })
    # spirits_qtr_data_cat <- reactive({
    #   # need to base the qoq on the number of cats chosen in filter
    #   n_cats <- length(input$spirits_cat_check)
    #   n_qtr <- length(input$qtr_check)
    #   QtrCatData(spirits_filtered_data(), n_cats, n_qtr)
    # })
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
      TtlChart(yr_sales, 
               wine_annual_data(), 'cyr', 'netsales', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M", yr_flag_color, lwidth, lpointsize)
    })
    ## plot sales by quarter
    output$wine_sales_qtr <- renderPlotly({
      QtrChart(qtr_sales, 
               wine_qtr_data(), 'cyr_qtr', 'netsales', 'cqtr', qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "M", lwidth, lpointsize)
    })
    ### change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$wine_sales_yoy <- renderPlotly({
      PoPChart(pop_chg_sales, wine_annual_data(), "cyr", "yoy_sales", "yr_flag", yr_flag_color, 
               theme_xax+theme_nleg, "%")
    })
    output$wine_sales_qoq <- renderPlotly({
      PoPChart(pop_chg_sales_qtr, wine_qtr_data(), "cyr_qtr", "qoq_sales", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    ### categories ----
    ## - need to find a way to show top 5 or so categories only
    #### data by cat ----
    wine_annual_data_cat <- reactive({
      n_cats <- length(input$wine_cat_chart_picker)
      AnnualCatData(wine_filtered_data_cat(), wine_filtered_data_cat())
    })
    wine_qtr_data_cat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_cats <- length(input$wine_cat_chart_picker)
      n_qtr <- length(input$qtr_check)
      QtrCatData(wine_filtered_data_cat(), n_cats, n_qtr)
    })
    #### plots by category ----
    output$wine_sales_yr_cat <- renderPlotly({
      CatChart("Sales by Country", 
               wine_annual_data_cat(), "cyr", "netsales","category", wine_cat_color, 
               "stack", theme_xax,"M") %>%
      layout(legend = layout_legend_vr)          # Position on the y-axis
    })
    output$wine_sales_yr_cat_pc <- renderPlotly({
      CatChart("% Share by Country", 
               wine_annual_data_cat(), "cyr", "pct_ttl_sales", "category", wine_cat_color, 
               "fill",theme_xax+theme_xax, "%") %>%
        layout(legend = layout_legend_vr)          # Position on the y-axis
    })
    ### facet: change by category ----
    output$wine_sales_yoy_cat <- renderPlotly({
      x <- wine_annual_data_cat()
      CatChgChart(yr_sales_pc_chg_cat, 
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
      CatChgChart(yr_sales_pcpt_chg_cat, 
                  x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales",
                  fill_var = "cat_type", 
                  facet_var = "category",
                  fill_color = bar_col, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg+theme_facet, tunits = "num")
    })
    ## treemaps wine countries, categories ----
    ## ggplot works nicely, but...plotly (below) MUCH better!
    ## - this version NOT shown in app (no section in ui.R)
    output$wine_sales_country_treemap <- renderPlot({
      cat("wine_country \n")
      # wine_filtered_data()
      data <- wine_data
      #data <- data %>% filter(end_qtr_dt == max(data$end_qtr_dt))
      data <- data %>% filter(cyr == max(data$cyr))
      # Create a treemap
      ggplot(data, aes(
        area = netsales, 
        fill = category,
        subgroup = category,
        label = paste(subcategory, "\n", scales::dollar(netsales, scale=1e-3, suffix = "k", accuracy = 1))
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


