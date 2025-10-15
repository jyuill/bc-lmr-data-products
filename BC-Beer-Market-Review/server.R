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
  ### for testing ----
  #beer_annual_test <- AnnualCatTypeData(beer_data, beer_data)
  #beer_annual_cat_test <- AnnualCatData(beer_data, beer_data)
  # setup filters ----------------------------------------------------
  # Dynamically generate UI filters based on lmr_data
  # otherwise, app will crash because lmr_data not available for filters in ui.R
  # CHATGPT suggestion as alt to below
  ## annual vs qtr grain filter ----
  dynamic_grain <- radioButtons(inputId = "grain_check", "Select Grain:", 
                                choices = c("Annual", "Quarterly"), 
                                selected = "Quarterly",
                                inline = FALSE
  )
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
  dynamic_qtr <- checkboxGroupInput(inputId = "qtr_check", "Select Quarter:", 
                                    choices = sort(unique(lmr_data$cqtr)), 
                                    selected = unique(lmr_data$cqtr),
                                    inline = FALSE
  )
  ## source/cat filters ----
  dynamic_beer_cat <- checkboxGroupInput(inputId = "beer_cat_check", "Select Source:", 
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
        dynamic_grain,
        dynamic_cyr,
        dynamic_qtr,
        dynamic_beer_cat,
        tags$h4("Contents"),
        tags$a(href="#overview_comparison", "Qtr Net $ & Litre Sales"),tags$br(),
        tags$a(href="#multi_year_summary", "Multi-Yr Summary"),tags$br(),
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
        tags$a(href="#beer_sales", "$ Sales by Yr & Qtr"),tags$br(),
        tags$a(href="#bsrc_sales", "$ Sales by Source"), tags$br(),
        tags$a(href="#bcat_sales", "BC Beer by Category"), tags$br(),
        tags$a(href="#bimp_sales","Import Sales by Ctry"), tags$br()
      )
    } else if (input$tabselected == 3) {
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
    } else if (input$tabselected == 4) {
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
  
    
    # Data processing ---------------------------------------------------
    cat("297: 01 apply beer filters \n")
    ## 1. Filter the data set based on the selected categories ----
    beer_filtered_data <- reactive({
      req(input$cyr_picker, input$qtr_check, input$beer_cat_check)
      #beer_data
      beer_data %>% filter(cyr %in% input$cyr_picker) %>%
        filter(cqtr %in% input$qtr_check) %>%
        filter(category %in% input$beer_cat_check)
    })
    # test
    #beer_filtered_test <- beer_data %>% filter(cyr_num < 2025)
    #beer_filter_annual_test <- AnnualCatTypeData(beer_filtered_test, beer_filtered_test)
    cat("306: 02 aggregate annual & qtr totals \n")
    ## 2. annual and qtr totals ---------------------------------------------------
    cat("308: annual data \n")
    beer_annual_data <- reactive({
      AnnualCatTypeData(beer_filtered_data(), beer_filtered_data())
    })
    cat("312: qtr data \n")
    # test - qtr data
    #beer_qtr_test <- QtrData(beer_data, length(unique(beer_data$cqtr)))
    beer_qtr_data <- reactive({
      QtrData(beer_filtered_data(), length(input$qtr_check))
    })
    ## 3. by category (source / origin) ----
    cat("346: annual data \n")
    # annual
    beer_annual_data_cat <- reactive({
      n_cats <- length(input$beer_cat_check)
      AnnualCatData(beer_filtered_data(), beer_filtered_data())
    })
    # test annual category data
    #beer_annual_cat_test <- AnnualCatData(beer_data, beer_data)
  
    # qtr
    cat("356: quarterly data \n")
    beer_qtr_data_cat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_qtr <- length(input$qtr_check)
      n_cats <- length(input$beer_cat_check)
      QtrCatData(beer_filtered_data(), n_cats, n_qtr)
    })
    # test qtr category data
    #beer_qtr_cat_test <- QtrCatData(beer_data)
    
  # by subcategory
    ## 4. subcategory data ----
    cat("375: subcategory data \n")
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
    
    # Overview tab -----------------------------------------------------
    ### Qtr $ sales & litres ----
    output$overview_sales_qtr <- renderPlotly({
      req(input$grain_check)
      if(input$grain_check == "Annual") {
        TtlChart("Net $", yr_sales, 
               beer_annual_data(),
                'cyr', 'netsales', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M", yr_flag_color, lwidth, lpointsize)
      } else if(input$grain_check == "Quarterly") {
        QtrChart("Net $", qtr_sales,
               beer_qtr_data(), 'cyr_qtr', 'netsales', 'cqtr', qtr_color,
               theme_xax+theme_xaxq+theme_nleg, "M", lwidth, lpointsize)  
      }
    })

    output$overview_litres_qtr <- renderPlotly({
      req(input$grain_check)
      if(input$grain_check == "Annual") {
        TtlChart("Litres", yr_sales, 
               beer_annual_data(),
                'cyr', 'litres', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M", yr_flag_color, lwidth, lpointsize)
      } else if(input$grain_check == "Quarterly") {
        QtrChart("Litres", qtr_sales,
               beer_qtr_data(), 'cyr_qtr', 'litres', 'cqtr', qtr_color,
               theme_xax+theme_xaxq+theme_nleg, "M", lwidth, lpointsize)  
      }
    })
    ### Qtr % chg - yoy ----
    output$overview_sales_yoy <- renderPlotly({
      req(input$grain_check)
      if(input$grain_check == "Annual") {
        PoPChart("", pop_chg_sales, beer_annual_data(), 
              "cyr", "yoy_sales", "yr_flag", yr_flag_color, 
               theme_xax+theme_nleg, "%")
      } else if(input$grain_check == "Quarterly") {
        PoPChart("Net $", "% Chg - same Qtr Prev Yr",  
                beer_qtr_data(), "cyr_qtr", "yoy_qoq_sales", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
      }
    })

    output$overview_litres_yoy <- renderPlotly({
      req(input$grain_check)
      if(input$grain_check == "Annual") {
        PoPChart("", pop_chg_sales, beer_annual_data(), "cyr", "yoy_litres", "yr_flag", yr_flag_color, 
               theme_xax+theme_nleg, "%")
      } else if(input$grain_check == "Quarterly") {
        PoPChart("Litres", "% Chg - same Qtr Prev Yr", beer_qtr_data(), 
                "cyr_qtr", "yoy_qoq_litres", "cqtr", qtr_color,
               theme_xax+theme_xaxq+theme_nleg, "%")
      } 
    })

    ### Multi-year summary chart data ---- 
    overview_summary_data <- reactive({
      req(beer_annual_data(), beer_qtr_data(), input$grain_check)
      
      if(input$grain_check == "Annual") {
        data <- beer_annual_data()
        # Create chart with year over year % change over each of 1 to n yrs based on number of yrs in filtered data
        # Get the most recent complete yr (assuming not qtr filters applied)
        # identify unique quarters in data -> filter for yrs that have those qtrs
        max_qtr_yr <- max(data$max_qtr, na.rm = TRUE) # identifies max qtr in each yr
        data <- data %>% filter(max_qtr == max_qtr_yr) # yrs with max max_qtr only
        max_year <- max(as.numeric(as.character(data$cyr)), na.rm = TRUE)
        min_year <- min(as.numeric(as.character(data$cyr)), na.rm = TRUE)
        
        # get most recent yr row to calculate % chg 
        most_recent <- data %>% filter(cyr == max_year)
        if(nrow(most_recent) == 0) return(NULL) # need yr to calculate from
        recent_year <- as.numeric(as.character(most_recent$cyr[1]))
        
        # Get all years in dataset for comparison
        available_years <- sort(unique(as.numeric(as.character(data$cyr))))
        years_back <- available_years[available_years < recent_year]

        if(length(years_back) == 0) return(NULL) # Need at least 2 years for comparison

        # Create chart data in long format
        chart_data <- data.frame()

        # For each year back, calculate the percent change from same qtr in that yr
        for(i in seq_along(years_back)) {
          year_back <- years_back[length(years_back) - i + 1]  # Start from most recent
          years_diff <- recent_year - year_back
          period_label <- paste0(years_diff, ifelse(years_diff == 1, " Year", " Years"))

          # Find the comparison year
          comparison_data <- data %>%
            filter(cyr == year_back)

          if(nrow(comparison_data) > 0) {
            sales_pct <- round(((most_recent$netsales[1] - comparison_data$netsales[1]) / comparison_data$netsales[1]) * 100, 1)
            litres_pct <- round(((most_recent$litres[1] - comparison_data$litres[1]) / comparison_data$litres[1]) * 100, 1)

            # Add rows for both metrics
            chart_data <- rbind(chart_data,
                                data.frame(
                                  Recent_per = as.character(most_recent$cyr[1]),
                                  Period = period_label,
                                  Metric = "Net $ Sales",
                                  Percent_Change = sales_pct,
                                  stringsAsFactors = FALSE
                                ),
                                data.frame(
                                  Recent_per = as.character(most_recent$cyr[1]),
                                  Period = period_label,
                                  Metric = "Litre Sales",
                                  Percent_Change = litres_pct,
                                  stringsAsFactors = FALSE
                                ))
            }
          } # end annual calc
      } else if(input$grain_check == "Quarterly") {
        data <- beer_qtr_data()
        # Get the most recent quarter in the filtered data
        max_date <- max(data$end_qtr_dt, na.rm = TRUE)
        most_recent <- data %>% filter(end_qtr_dt == max_date)

        if(nrow(most_recent) == 0) return(NULL)

        recent_qtr <- most_recent$cqtr[1]
        recent_year <- as.numeric(as.character(most_recent$cyr[1]))

        # Get all years in dataset for comparison
        available_years <- sort(unique(as.numeric(as.character(data$cyr))))
        years_back <- available_years[available_years < recent_year]

        if(length(years_back) == 0) return(NULL)

        # Create chart data in long format
        chart_data <- data.frame()

        # For each year back, calculate the percent change from same qtr in that yr
        for(i in seq_along(years_back)) {
          year_back <- years_back[length(years_back) - i + 1]  # Start from most recent
          years_diff <- recent_year - year_back
          period_label <- paste0(years_diff, ifelse(years_diff == 1, " Year", " Years"))

          # Find the comparison quarter

          comparison_data <- data %>%
            filter(cyr == year_back, cqtr == recent_qtr)

          if(nrow(comparison_data) > 0) {
            sales_pct <- round(((most_recent$netsales[1] - comparison_data$netsales[1]) / comparison_data$netsales[1]) * 100, 1)
            litres_pct <- round(((most_recent$litres[1] - comparison_data$litres[1]) / comparison_data$litres[1]) * 100, 1)

            # Add rows for both metrics
            chart_data <- rbind(chart_data,
                                data.frame(
                                  Recent_per = most_recent$cqtr[1],
                                  Period = period_label,
                                  Metric = "Net $ Sales",
                                  Percent_Change = sales_pct,
                                  stringsAsFactors = FALSE
                                ),
                                data.frame(
                                  Recent_per = most_recent$cqtr[1],
                                  Period = period_label,
                                  Metric = "Litre Sales",
                                  Percent_Change = litres_pct,
                                  stringsAsFactors = FALSE
                                ))
            }
          }
      } # end qtr calc 
      
      # Order periods properly (1 Year, 2 Years, etc.)
      if(nrow(chart_data) > 0) {
        chart_data$Period <- factor(chart_data$Period, levels = unique(chart_data$Period))
        # to control order of metrics shown in bar chart - matches charts above
        chart_data$Metric <- factor(chart_data$Metric, levels = c("Net $ Sales", "Litre Sales"))
      }

      return(chart_data)
    })

    output$overview_summary_chart <- renderPlotly({
      data <- overview_summary_data()
      req(data, input$grain_check)

      # Define subtle, neutral colors for the two metrics
      metric_colors <- c("Net $ Sales" = "#708090", "Litre Sales" = "#A0A0A0")

      # Create tooltip text and label text
      data <- data %>%
        mutate(tooltip_text = paste(Metric, "<br>",
                                   Period, " Change: ",
                                   ifelse(Percent_Change >= 0, "+", ""),
                                   Percent_Change, "%"),
               label_text = paste0(ifelse(Percent_Change >= 0, "+", ""), Percent_Change, "%"))
      if(input$grain_check == "Annual") {
        chart_title <- paste0("% Change over Extended Periods, using Most Recent Complete Year (", max(data$Recent_per), ")")
      } else if(input$grain_check == "Quarterly") {
        chart_title <- "% Change vs Same Quarter in Previous Years, over Extended Periods"
      }
      p <- data %>%
        ggplot(aes(x = Period, y = Percent_Change, fill = Metric, text = tooltip_text)) +
        geom_col(position = "dodge", width = 0.7) +
        geom_text(aes(label = label_text, y = Percent_Change / 2),
                  position = position_dodge(width = 0.7),
                  size = 3,
                  color = "white",
                  fontface = "bold") +
        geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
        scale_fill_manual(values = metric_colors) +
        scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1),
                          expand = expansion(mult = c(0.05, 0.05))) +
        labs(title = chart_title,
             x = "", y = "", fill = "Metric") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 8),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          legend.position = "bottom"
        )

      return(ggplotly(p, tooltip = "text"))
    })
  
  ## category by quarter ----
  # line trend
    output$beer_sales_qtr_cat_line <- renderPlotly({
      CatChartLine("Net $","Qtrly by Source", 
                beer_qtr_data_cat(), "cyr_qtr", "netsales", "category", beer_cat_color,
                theme_xax+theme_xaxq, "M", lwidth, lpointsize)
    })
    output$beer_litre_qtr_cat_line <- renderPlotly({
      CatChartLine("Litres","Qtrly by Source", 
               beer_qtr_data_cat(), "cyr_qtr", "litres", "category", beer_cat_color,
               theme_xax+theme_xaxq, "M", lwidth, lpointsize)
    })
  # stacked bar charts %
    output$beer_sales_qtr_cat <- renderPlotly({
       CatChart("Net $","Qtrly by Source % of Total", 
                beer_qtr_data_cat(), "cyr_qtr", "netsales", "category", beer_cat_color, "fill",
                theme_xax+theme_xaxq, "M")
     })
    output$beer_litre_qtr_cat <- renderPlotly({
      CatChart("Litres","Qtrly by Source % of Total", 
               beer_qtr_data_cat(), "cyr_qtr", "litres", "category", beer_cat_color, "fill",
               theme_xax+theme_xaxq, "M")
    })
  ## facet charts yoy % chg ----
  # cat <- "category"
  #  beer_qtr_cat_test %>% ggplot(aes(x=cyr_qtr, y=yoy_qoq_sales, fill=cqtr)) +
  #    geom_col() +
  #    facet_grid(as.formula(paste(cat,'~.')), scales = "free_y")+
  #    scale_fill_manual(values = qtr_color) +
  # scale_y_continuous(labels = label_percent(accuracy=1),
  #                      expand = expansion(mult=c(0,0.05)),
  #                      limits = c(0 - max_val, max_val))
  # % chg yoy by source: same qtr prev yr, faceted by source
    output$sales_qtr_cat_yoy <- renderPlotly({
      CatChgChart("","Net $ Sales % Chg - same Qtr Prev Yr", 
               beer_qtr_data_cat(), 
               x_var = "cyr_qtr", y_var = "yoy_qoq_sales", 
               sort_var = "netsales", 
               fill_var = "cqtr", 
               facet_var = "category",
               fill_color = qtr_color, 
               strp_color = strp_col,
               theme_xax+theme_nleg, "%",
               f_scales = "fixed")
    })
    output$litres_qtr_cat_yoy <- renderPlotly({
        CatChgChart("","Litre Sales % Chg - same Qtr Prev Yr", 
                beer_qtr_data_cat(), 
                x_var = "cyr_qtr", y_var = "yoy_qoq_litres", 
                sort_var = "netsales", 
                fill_var = "cqtr", 
                facet_var = "category",
                fill_color = qtr_color, 
                strp_color = strp_col,
                theme_xax+theme_nleg,
                f_scales = "free_y")
    })
  
  # $ SALES ---------------------------------------------------------------
    ## $ sales - yr, qtr ----
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
    
    ## change in sales ----
    # - uses x_var to set x variable - in this case, 'cyr'
    output$beer_sales_yoy <- renderPlotly({
      PoPChart("", pop_chg_sales, beer_annual_data(), "cyr", "yoy_sales", "yr_flag", yr_flag_color, 
               theme_xax+theme_nleg, "%")
    })
    output$beer_sales_qoq <- renderPlotly({
      PoPChart("", pop_chg_sales_qtr, beer_qtr_data(), "cyr_qtr", "qoq_sales", "cqtr", qtr_color, 
               theme_xax+theme_xaxq+theme_nleg, "%")
    })
    
    ## category (source / origin) ----
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
    
    ## facet: % change by source ----  
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
    ## Data sourced from above process ----
    # filters apply the same across tabs
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
    #beer_annual_cat_test <- AnnualCatData(beer_data, beer_data)
    
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
    #beer_annual_subcat_test <- AnnualSubCatData(beer_data, n_cats, n_subcats, beer_data)
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


