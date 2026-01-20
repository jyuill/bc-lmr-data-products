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
library(lmrtools) # custom pkg with lmr functions for data fetch etc

scipen <- options(scipen=999) # suppress scientific notation

# START: global.R - initial data fetch, pre-processing, set-up ----
# - global.R runs automatically - no need to source()
# - variables available for use in ui.R and server.R

# Define server logic ----
function(input, output, session) {
  
  cat("180: start server \n")

  # Dynamic Sidebar ----
  # - content in ui.R 
  # - process here sets up initial filter choices & determines 
  #   which content to show in sidebar based on active tab
  # populate initial filter choices after session starts
  observeEvent(session, {
    # Hide all tab-specific sidebar content initially
    shinyjs::hide("sidebar_tab1_content")
    shinyjs::hide("sidebar_tab2_content")
    shinyjs::hide("sidebar_tab3_content")
    shinyjs::hide("sidebar_tab4_content")

    # Update choices for filters in sidebar
    updateRadioButtons(session, "grain_check",
                       choices = c("Annual", "Quarterly"),
                       selected = "Annual"
    )
    updatePickerInput(session, "cyr_picker",
                      choices = unique(beer_data$cyr),
                      selected = unique(data_recent$cyr)
    )
    updateCheckboxGroupInput(session, "qtr_check",
                             choices = sort(unique(beer_data$cqtr)),
                             selected = unique(beer_data$cqtr)
    )
    updateCheckboxGroupInput(session, "beer_cat_check",
                             choices = unique(beer_data$category),
                             selected = unique(beer_data$category)
    )
    updateCheckboxGroupInput(session, "beer_bc_subcat_check",
                             choices = bc_subcats,
                             selected = bc_subcats
    )
  }, once = TRUE) # Run only once at session start

  # Use observeEvent to show/hide the correct static sidebar content defined in ui.R
  observeEvent(input$tabselected, {
      # Hide all possible sidebar containers first
      shinyjs::hide("sidebar_tab1_content")
      shinyjs::hide("sidebar_tab2_content")
      shinyjs::hide("sidebar_tab3_content")
      shinyjs::hide("sidebar_tab4_content")

      # Show grain_check only on tab 1 
      if (input$tabselected %in% c(1, 2, 3)) {
        shinyjs::show("sidebar_filters")
        # show content for active tab
        if (input$tabselected == 1) {
          shinyjs::show("grain_check")
          shinyjs::show("sidebar_tab1_content")
        } else if (input$tabselected == 2) {
          shinyjs::show("sidebar_tab2_content")
          shinyjs::hide("grain_check")
        } else if (input$tabselected == 3) {
          shinyjs::show("sidebar_tab3_content")
          shinyjs::hide("grain_check")
        }
      } else if (input$tabselected == 4) {
        shinyjs::hide("sidebar_filters")
        shinyjs::show("sidebar_tab4_content")
      }
  }, ignoreInit = FALSE) # Run on initial load too

  
  # code for activating/deactivating subcat filter based on cat filter selection
    observe({
      # req(input$tabselected == 1)
      is_only_bc_selected <- length(input$beer_cat_check) == 1 && input$beer_cat_check[1] == "BC"
      if(is_only_bc_selected) {
        shinyjs::enable("beer_bc_subcat_check")
      } else {
        shinyjs::disable("beer_bc_subcat_check")
      }
  #    # Optional: reset the selections when it gets disabled
  #    #updateCheckboxGroupInput(session, "beer_bc_subcat_check", selected = character(0))
    })
    
    # Data processing ---------------------------------------------------
    cat("192: 01 apply beer filters to data \n")
    ## 1. Filter the data set based on the selected categories ----
    beer_filtered_data <- reactive({
      req(input$cyr_picker, input$qtr_check, input$beer_cat_check)
      # req(input$beer_bc_subcat_check)
      #beer_data
      beer_data %>% filter(cyr %in% input$cyr_picker) %>%
        filter(cqtr %in% input$qtr_check) %>%
        filter(category %in% input$beer_cat_check) %>%
        filter( # apply subcategory filter only if BC category is only selection
          if(length(input$beer_cat_check) == 1 && input$beer_cat_check[1] == "BC") {
            subcategory %in% input$beer_bc_subcat_check
          } else {
            TRUE
            }
        )
    })
  
    #beer_filtered_test <- beer_data %>% filter(cyr_num < 2025)
    #beer_filter_annual_test <- AnnualCatTypeData(beer_filtered_test, beer_filtered_test)
    cat("204: 02 aggregate annual & qtr totals \n")
    ## 2. annual and qtr totals ---------------------------------------------------
    cat("206: annual data \n")
    beer_annual_data <- reactive({
      AnnualCatTypeData(beer_filtered_data(), beer_filtered_data())
    })
    cat("210: qtr data \n")
    
    beer_qtr_data <- reactive({
      QtrData(beer_filtered_data(), length(input$qtr_check))
    })
    ## 3. by category (source / origin) ----
    cat("217: annual cat/src data \n")
    ### annual cat ----
    beer_annual_data_cat <- reactive({
      #n_cats <- length(input$beer_cat_check) # calc in function based on data passed
      AnnualCatData(beer_filtered_data(), 'cat_type', 'category', beer_data)
    })
    
    ### qtr cat ----
    cat("227: quarterly cat/src data \n")
    beer_qtr_data_cat <- reactive({
      # need to base the qoq on the number of cats chosen in filter
      n_qtr <- length(input$qtr_check)
      n_cats <- length(input$beer_cat_check)
      QtrCatData(beer_filtered_data(), n_cats, n_qtr)
    })
    beer_qtr_data_cat2 <- reactive({
      # NO need to base the qoq on the number of cats chosen in filter - factored into data
      #n_qtr <- length(input$qtr_check)
      #n_cats <- length(input$beer_cat_check)
      QtrCatData2(beer_filtered_data(), "cat_type", "category")
    })
    
  # by subcategory
    ## 4. bc subcategory data ----
    cat("239: subcategory data \n")
    # annual
    beer_annual_data_subcat <- reactive({
      cat("242: beer_subcat \n")
      beer_bc <- beer_filtered_data() %>% filter(category== "BC")
      AnnualCatData(beer_bc, 'category', 'subcategory', beer_data)
    })
    # test
    #n_cats <- length(unique(beer_data$category))
    #n_subcats <- length(unique(beer_data$subcategory))
    #beer_annual_subcat_test <- AnnualSubCatData(beer_data, n_cats, n_subcats, beer_annual_cat_test)
    # qtr
    beer_qtr_data_subcat <- reactive({
      cat("255: beer_qtr_subcat \n")
      beer_bc <- beer_filtered_data() %>% filter(category== "BC")
      QtrCatData2(beer_bc, 'category', 'subcategory')
    })
    # test - qtr
    #beer_test_qtr_subcat <- QtrCatData2(beer_bc_data, 'category', 'subcategory')
  
    ## 5. import country data ----
    cat("239: import subcategory data \n")
    # annual
    beer_annual_data_subcat_imp <- reactive({
      cat("242: imp_subcat \n")
      beer_imp <- beer_filtered_data() %>% filter(category== "Import")
      AnnualCatData(beer_imp, 'category', 'subcategory', beer_imp)
    })
    # Overview tab -----------------------------------------------------
    ### Ttl sales & litres ----
    output$overview_sales_qtr <- renderPlotly({
      req(input$grain_check)
      if(input$grain_check == "Annual") {
        TtlChart("Net $:", yr_sales, 
               beer_annual_data(),
                'cyr', 'netsales', 'cat_type', bar_col, 
               theme_xax+theme_nleg, "M", yr_flag_color, lwidth, lpointsize)
      } else if(input$grain_check == "Quarterly") {
        QtrChart("Net $:", qtr_sales,
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
        PoPChart("Net $:", "% Chg - same Qtr Prev Yr",  
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

    ### Multi-year summary ---- 
    #### data processing ----
    overview_summary_data <- reactive({
      req(beer_annual_data(), beer_qtr_data(), input$grain_check)
      # NOTE: a LOT of duplications between annual and qtr calcs - could be refactored as function
      
      if(input$grain_check == "Annual") {
        # CHANGING THIS FROM CAL YR COMPS TO ROLLING 4 QTRS COMPS
        # get aggregated qtr data
        data <- beer_qtr_data()
        # Get the most recent yr & qtr
        most_rec_yr <- data %>% filter(cyr == max(as.character(data$cyr))) # yr with max qtr
        most_rec_qtr <- max(as.character(most_rec_yr$cqtr, na.rm = TRUE))
        # aggregate data based on matching qtr periods starting from most recent yr and qtr
        # starting with most recent yr and quarter, aggregate back to the same quarter prev yr
        # mark the rows that have the same qtr with index number
        # get list of yrs and qtrs
        data_qtr <- data %>% filter(cqtr == most_rec_qtr)
        data_qtr <- data_qtr %>% arrange(desc(cyr))
        data_qtr$index <- 0:(nrow(data_qtr) - 1)
        data_qtrs_index <- data_qtr %>% select(cyr, cqtr, index)
        # join data qtr indexes with qtr data
        data_join <- left_join(data, data_qtrs_index, by = c("cyr","cqtr"))
        # fill in the NA values in index column with next available value
        # - allows for grouping based on qtr periods, even if cross-over yrs
        data_index <- data_join %>% mutate(
          index_yr = case_when(
            !is.na(index) ~ index,
            !is.na(lead(index, n=1)) ~ lead(index, n=1),
            !is.na(lead(index, n=2)) ~ lead(index, n=2),
            !is.na(lead(index, n=3)) ~ lead(index, n=3),
            !is.na(lead(index, n=4)) ~ lead(index, n=4),
            TRUE ~ 0
          )
        )
        data_index$index_yr <- as.integer(data_index$index_yr) # set to integer
        # ensure complete set for each yr before aggregating and comparing
        # how many qtrs in year 0
        qtrs_0 <- data_index %>% filter(index_yr == 0) %>% nrow()
        # filter out index yr with less qtrs than yr 0
        data_ind_count <- data_index %>% group_by(index_yr) %>% 
          summarize(qtrs = n()) %>% ungroup() 
        data_yr_under <- data_ind_count%>% filter(qtrs < qtrs_0)
        data_index <- data_index %>% filter(index_yr != data_yr_under$index_yr)
        # aggregate data by index yr
        data <- data_index %>% group_by(cat_type,index_yr) %>% 
          summarise(netsales = sum(netsales), 
                    litres = sum(litres))
        # arrange for calculations
        data <- data %>% arrange(desc(index_yr))
        # calculate % chg yoy for reference (not actually used here)
        # - could be used for year-over-year patterns on rolling basis 
        data <- data %>% mutate(
          yoy_sales = (netsales - lag(netsales))/lag(netsales),
          yoy_litres = (litres - lag(litres))/lag(litres)
        )
        # loop through to calc percent change from year 0 to each prev
        # set empty cols to hold calculated values
        data$Period <- NA
        data$Percent_Change_sales <- NA
        data$Percent_Change_litres <- NA
        # loop through each row
        for(i in seq_along(data$index_yr)) {
          data$Period[i] = ifelse(data$index_yr[i] == 1, "1 Year", paste0(data$index_yr[i], " Years"))
          data$Percent_Change_sales[i] = round(data$netsales[data$index_yr==0]/data$netsales[i]-1,4)*100
          data$Percent_Change_litres[i] = round(data$litres[data$index_yr==0]/data$litres[i]-1,4)*100
        }
        # pivot dataframe so metrics are in a column
        chart_data <- data %>% select(index_yr, Period,Percent_Change_sales, Percent_Change_litres)
        chart_data <- chart_data %>% pivot_longer(cols = c(Percent_Change_sales, Percent_Change_litres), names_to = "Metric", values_to = "Percent_Change")
        chart_data <- chart_data %>% mutate(
          Metric = case_when(
            Metric == "Percent_Change_sales" ~ "Net $ Sales",
            Metric == "Percent_Change_litres" ~ "Litre Sales"
          )
        )
        # drop the yr 0 row
        chart_data <- chart_data %>% filter(index_yr != 0)
        # reverse the order of rows based on index yr low to high
        chart_data <- chart_data %>% arrange(index_yr)
        
        # return chart data
        if(nrow(chart_data) == 0) return(NULL)
        
        # end annual calc
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
    #### plot ----
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
      # set title depending on grain
      if(input$grain_check == "Annual") {
        chart_title <- paste0("% Change over Extended Periods, rolling 4 qtrs from most recent qtr shown")
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
  
  ## category / origin ----
  ### line trend ----
    output$beer_sales_qtr_cat_line <- renderPlotly({
      req(input$grain_check)
      if(input$grain_check == "Annual") {
        CatChartLine("Net $:",yr_source, 
                beer_annual_data_cat(), "cyr", "netsales", "category", beer_cat_color,
                theme_xax, "M", lwidth, lpointsize)
      } else if(input$grain_check == "Quarterly") {
        CatChartLine("Net $:",yr_source, 
                beer_qtr_data_cat2(), "cyr_qtr", "netsales", "category", beer_cat_color,
                theme_xax+theme_xaxq, "M", lwidth, lpointsize)
      }
    })
    output$beer_litre_qtr_cat_line <- renderPlotly({
      req(input$grain_check)
      if(input$grain_check == "Annual") {
        #return(NULL) # no annual line chart
        CatChartLine("Litres",yr_source, 
                beer_annual_data_cat(), "cyr", "litres", "category", beer_cat_color,
                theme_xax, "M", lwidth, lpointsize)
      } else if(input$grain_check == "Quarterly") {
        CatChartLine("Litres",yr_source, 
               beer_qtr_data_cat2(), "cyr_qtr", "litres", "category", beer_cat_color,
               theme_xaxq, "M", lwidth, lpointsize)
      }
    })
  ### stacked bar charts % ----
    output$beer_sales_qtr_cat <- renderPlotly({
      req(input$grain_check)
      if(input$grain_check == "Annual") {
        #return(NULL) # no annual chart
        CatChart("Net $:", yr_source_pc, 
               beer_annual_data_cat(), "cyr", "pct_ttl_sales","category", 
               beer_cat_color, "stack", # stack instead of fill - % values known
               theme_xax, tunits="%")
      } else if(input$grain_check == "Quarterly") {
        CatChart("Net $:",yr_source_pc, 
                beer_qtr_data_cat2(), "cyr_qtr", "pct_ttl_sales", "category", 
                beer_cat_color, "stack",
                theme_xaxq, "%") 
      }
     })
  
    output$beer_litre_qtr_cat <- renderPlotly({
      req(input$grain_check)
      if(input$grain_check == "Annual") {
        CatChart("Litres:",yr_source_pc, 
                beer_annual_data_cat(), "cyr", "pct_ttl_litres", "category", 
                beer_cat_color, "stack",
                theme_xax, "%")
      } else if(input$grain_check == "Quarterly") {
        CatChart("Litres:",yr_source_pc, 
                beer_qtr_data_cat2(), "cyr_qtr", "pct_ttl_litres", "category", 
                beer_cat_color, "stack",
                theme_xaxq, "%")
      }
    })
  ### facet charts yoy % chg (qtr, yr) ----
    output$sales_qtr_cat_yoy <- renderPlotly({
      req(input$grain_check)
      if(input$grain_check == "Annual") {
        x <- beer_annual_data_cat()
        CatChgChart("Net $:", yr_source_pc_chg, x, 
               x_var = "cyr", y_var = "yoy_sales", 
               sort_var = "netsales",
               fill_var = "yr_flag", 
               facet_var = "category",
               fill_color = yr_flag_color, 
               strp_color = strp_col,
               theme_xax+theme_nleg, "%", 
               f_scales = "fixed")
      } else if(input$grain_check == "Quarterly") {
        x <- beer_qtr_data_cat2()
        CatChgChart("Net $:",qtr_source_pc_chg, x,
               x_var = "cyr_qtr", y_var = "yoy_qoq_sales", 
               sort_var = "netsales", 
               fill_var = "cqtr", 
               facet_var = "category",
               fill_color = qtr_color, 
               strp_color = strp_col,
               theme_xaxq+theme_nleg, "%",
               f_scales = "fixed")
      }
    })
  
    output$litres_qtr_cat_yoy <- renderPlotly({
      req(input$grain_check)
      if(input$grain_check == "Annual") {
        #return(NULL) # no annual chart
        x <- beer_annual_data_cat()
        CatChgChart("Litres:", yr_source_pc_chg, x, 
               x_var = "cyr", y_var = "yoy_litres", 
               sort_var = "netsales",
               fill_var = "yr_flag", 
               facet_var = "category",
               fill_color = yr_flag_color, 
               strp_color = strp_col,
               theme_xax+theme_nleg, "%", 
               f_scales = "fixed")
      } else if(input$grain_check == "Quarterly") {
        CatChgChart("Litres:",qtr_source_pc_chg, 
                beer_qtr_data_cat2(), 
                x_var = "cyr_qtr", y_var = "yoy_qoq_litres", 
                sort_var = "netsales", 
                fill_var = "cqtr", 
                facet_var = "category",
                fill_color = qtr_color, 
                strp_color = strp_col,
                theme_xaxq+theme_nleg,
                f_scales = "free_y")
        }
      })
  
  ## BC subcategories by yr and qtr ----
  # use for: annual data by category type and subcategory
  ### line trend ----
      output$beer_sales_yq_subcat_line <- renderPlotly({
      req(input$grain_check)
        safe_plotly({
          if(input$grain_check == "Annual") {
            CatChartLine("Net $:",yr_sales_cat, 
                    beer_annual_data_subcat(), "cyr", "netsales", "subcategory", beer_bc_cat_color,
                    theme_xax, "M", lwidth, lpointsize)
          } else if(input$grain_check == "Quarterly") {
            #return(NULL) # no qtr subcat line chart yet - modify code below when ready
            CatChartLine("Net $:",yr_sales_cat, 
                    beer_qtr_data_subcat(), "cyr_qtr", "netsales", "subcategory", beer_bc_cat_color,
                    theme_xaxq, "M", lwidth, lpointsize)
          }
        })
      })
    output$beer_litre_yq_subcat_line <- renderPlotly({
      req(input$grain_check)
      safe_plotly({
        if(input$grain_check == "Annual") {
          #return(NULL) # no annual line chart
          CatChartLine("Litres:",yr_sales_cat, 
                  beer_annual_data_subcat(), "cyr", "litres", "subcategory", beer_bc_cat_color,
                  theme_xax, "M", lwidth, lpointsize)
        } else if(input$grain_check == "Quarterly") {
          #return(NULL) # no qtr subcat line chart yet - modify code below when ready
          CatChartLine("Litres:",yr_sales_cat, 
                beer_qtr_data_subcat(), "cyr_qtr", "litres", "subcategory", beer_bc_cat_color,
                theme_xaxq, "M", lwidth, lpointsize)
        }
      })
    })
  ### stacked bar charts % share by yr and qtr ----
    # MOVED to bottom of Overview tab for better flow
    output$beer_sales_yq_subcat_stacked <- renderPlotly({
      req(input$grain_check)
      safe_plotly({
        if(input$grain_check == "Annual") {
          CatChart("Net $:",yr_sales_cat_pc, 
                  beer_annual_data_subcat(), "cyr", "pct_ttl_sales", "subcategory", 
                  beer_bc_cat_color, "stack",
                  theme_xax, "%")
        } else if(input$grain_check == "Quarterly") {
          #return(NULL) # no qtr subcat stacked chart yet - modify code below when ready
          CatChart("Net $:",yr_sales_cat_pc, 
                  beer_qtr_data_subcat(), "cyr_qtr", "netsales", "subcategory", 
                  beer_bc_cat_color, "stack",
                  theme_xaxq, "%")
        }
      })
    })
    output$beer_litre_yq_subcat_stacked <- renderPlotly({
      req(input$grain_check)
      safe_plotly({
        if(input$grain_check == "Annual") {
          CatChart("Litres:",yr_sales_cat_pc, 
                  beer_annual_data_subcat(), "cyr", "pct_ttl_litres", "subcategory", 
                  beer_bc_cat_color, "stack",
                  theme_xax, "%")
        } else if(input$grain_check == "Quarterly") {
          #return(NULL) # no qtr subcat stacked chart yet - modify code below when ready
          CatChart("Litres:",yr_sales_cat_pc, 
                  beer_qtr_data_subcat(), "cyr_qtr", "pct_ttl_litres", "subcategory", 
                  beer_bc_cat_color, "stack",
                  theme_xaxq, "%")
        }
      })
    })
    ### facet charts yoy % chg by subcategory ----
    # % chg yoy by source: same qtr prev yr, faceted by source
    # MOVED above % share stacked charts for better flow
    output$sales_subcat_yoy <- renderPlotly({
      req(input$grain_check)
      safe_plotly({
        if(input$grain_check == "Annual") {
          x <- beer_annual_data_subcat()
          CatChgChart("Net $:", yr_sales_cat_pc_chg, x, 
                x_var = "cyr", y_var = "yoy_sales", 
                sort_var = "netsales",
                fill_var = "yr_flag", 
                facet_var = "subcategory",
                fill_color = yr_flag_color, 
                strp_color = strp_col,
                theme_xax+theme_nleg, "%", 
                f_scales = "fixed")
        } else if(input$grain_check == "Quarterly") {
          #return(NULL) # no qtr subcat yoy chart yet - modify code below when ready
          x <- beer_qtr_data_subcat()
          CatChgChart("Net $:", qtr_sales_cat_pc_chg, x,
                  x_var = "cyr_qtr", y_var = "yoy_qoq_sales", 
                  sort_var = "netsales", 
                  fill_var = "cqtr", 
                  facet_var = "subcategory",
                  fill_color = qtr_color, 
                  strp_color = strp_col,
                  theme_xaxq+theme_nleg, "%",
                  f_scales = "fixed")
        }
      })
    })
  
    output$litres_subcat_yoy <- renderPlotly({
      # NEXT: add in annual views
      req(input$grain_check)
      safe_plotly({
        if(input$grain_check == "Annual") {
          #return(NULL) # no annual chart
          x <- beer_annual_data_subcat()
          CatChgChart("Litres:", yr_sales_cat_pc_chg, x, 
                x_var = "cyr", y_var = "yoy_litres", 
                sort_var = "netsales",
                fill_var = "yr_flag", 
                facet_var = "subcategory",
                fill_color = yr_flag_color, 
                strp_color = strp_col,
                theme_xax+theme_nleg, "%", 
                f_scales = "fixed")
        } else if(input$grain_check == "Quarterly") {
          #return(NULL) # no qtr subcat yoy chart yet - modify code below when ready
          CatChgChart("Litres:", qtr_sales_cat_pc_chg, 
                  beer_qtr_data_subcat(), 
                  x_var = "cyr_qtr", y_var = "yoy_qoq_litres", 
                  sort_var = "netsales", 
                  fill_var = "cqtr", 
                  facet_var = "subcategory",
                  fill_color = qtr_color, 
                  strp_color = strp_col,
                  theme_xaxq+theme_nleg,
                  f_scales = "free_y")
          }
      })
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
      safe_plotly({
        CatChart("Net $", yr_source, 
                beer_annual_data_cat(), "cyr", "netsales","category", 
                beer_cat_color, "stack",
                theme_xax,"M")
      })
    })
    output$beer_sales_yr_cat_pc <- renderPlotly({
      safe_plotly({
        CatChart("Net $", yr_source_pc, 
                beer_annual_data_cat(), "cyr", "pct_ttl_sales","category", 
                beer_cat_color, "stack",
                theme_xax, tunits="%")
      })
    })
    
    ## facet: % change by source ----  
    output$beer_sales_yoy_cat_chg <- renderPlotly({
      safe_plotly({
        x <- beer_annual_data_cat()
        CatChgChart("", yr_source_pc_chg, 
                x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales",
                fill_var = "yr_flag", 
                facet_var = "category",
                fill_color = yr_flag_color, 
                strp_color = strp_col,
                theme_xax+theme_nleg)
      })
    })
    
    # % point chg by src yoy
    output$beer_sales_yoy_cat_chg_pt <- renderPlotly({
      safe_plotly({
        x <- beer_annual_data_cat()
        CatChgChart("", yr_source_pcpt_chg, 
                x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales", 
                fill_var = "yr_flag", 
                facet_var = "category",
                fill_color = yr_flag_color, 
                strp_color = strp_col,
                theme_xax+theme_nleg, tunits = "num")
      })
    })
    
    ## BC subcat ----
    ## PLOTS by BC subcategory ----
    output$beer_sales_yr_bc_cat <- renderPlotly({
      safe_plotly({
        data <- beer_annual_data_subcat() %>% filter(str_detect(subcategory, "BC"))
        CatChart("Net $",yr_sales_cat, 
                data, "cyr", "netsales","subcategory", 
                beer_bc_cat_color, "stack", theme_xax, "M")
      })
    })
    ## plot % of total sales by bc subcategory
    output$beer_sales_yr_bc_cat_pc <- renderPlotly({
      safe_plotly({
        data <- beer_annual_data_subcat() %>% filter(str_detect(subcategory, "BC"))
        CatChart("Net $", yr_sales_cat_pc, 
                data, "cyr", "pct_ttl_sales","subcategory", 
                beer_bc_cat_color, "stack", 
                theme_xax, "%")
      })
    })
    ## plots for bc beer subcategory yoy change in facets  
    output$beer_sales_yoy_bc_cat_chg <- renderPlotly({
      safe_plotly({
        x <- beer_annual_data_subcat() %>% filter(str_detect(subcategory, "BC"))
        CatChgChart("",yr_sales_cat_pc_chg, 
                x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales", 
                fill_var = "yr_flag", 
                facet_var = "subcategory",
                fill_color = yr_flag_color, 
                strp_color = strp_col,
                theme_xax+theme_nleg)
        })
      })
    ## % point chg by bc subcat yoy
    output$beer_sales_yoy_bc_cat_chg_pt <- renderPlotly({
      safe_plotly({
        x <- beer_annual_data_subcat() %>% filter(str_detect(subcategory, "BC"))
        CatChgChart("",yr_sales_cat_pcpt_chg, 
                x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales",
                fill_var = "yr_flag", 
                facet_var = "subcategory",
                fill_color = yr_flag_color, 
                strp_color = strp_col,
                theme_xax+theme_nleg, tunits = "num")
        })
      })
    ## IMPORT beer subcat ----
    
    ## plots by import country/region
    output$beer_sales_yr_import_cat <- renderPlotly({
      safe_plotly({
        cat('beer_import chart \n')
        data <- beer_annual_data_subcat_imp()
        print(data)
        CatChart("Net $",yr_sales_imp, 
                data, "cyr", "netsales","subcategory", beer_imp_color, "stack", 
                theme_xax, "M") %>%
          layout(legend = layout_legend_vr)
      })
    }) 

    output$beer_sales_yr_import_cat_pc <- renderPlotly({
      safe_plotly({
        cat('beer_import_pc chart \n')
        data <- beer_annual_data_subcat_imp()
        CatChart("Net $",yr_sales_imp_pc, 
                data, "cyr", "pct_ttl_sales","subcategory", 
                beer_imp_color, "fill", 
                theme_xax, "%") %>%
          layout(legend = layout_legend_vr)
      })
    }) 
    
    ## plots for import beer subcategory yoy change in facets
    output$beer_sales_yoy_import_cat_chg <- renderPlotly({
      safe_plotly({
        x <- beer_annual_data_subcat_imp() 
        CatChgChart("",yr_sales_imp_pc_chg, 
                x, x_var = "cyr", y_var = "yoy_sales", sort_var = "netsales", 
                fill_var = "yr_flag", 
                facet_var = "subcategory",
                fill_color = yr_flag_color, 
                strp_color = strp_col,
                theme_xax+theme_nleg+theme_facet)
      })
    })
    ## % point chg by import subcat yoy
    output$beer_sales_yoy_import_cat_chg_pt <- renderPlotly({
      safe_plotly({
        x <- beer_annual_data_subcat_imp()
        CatChgChart("",yr_sales_imp_pcpt_chg, 
                x, x_var = "cyr", y_var = "yoy_pcp_ttl_sales", sort_var = "netsales", 
                fill_var = "yr_flag", 
                facet_var = "subcategory",
                fill_color = yr_flag_color, 
                strp_color = strp_col,
                theme_xax+theme_nleg+theme_facet, tunits = "num")
      })
    })

    # LITRES ----
    ## Data sourced from above process ----
    # filters apply the same across tabs
    ## PLOTS ----
    ### litre sales - yr, qtr ----
    # similar to overview but using functions to get plot, providing:
    # - chart_title, dataset, bar col variable, list of theme modifications
    # - for themes, can list joined by '+'
    metric <- "Litres:"
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
               beer_cat_color, "stack",
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
                  fill_var = "yr_flag", 
                  facet_var = "category",
                  fill_color = yr_flag_color, 
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
    # annual REPLACED BY beer_annual_data_subcat()
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
      safe_plotly({
        #data <- beer_bc_yr_subcat()
        data <- beer_annual_data_subcat() %>% filter(str_detect(subcategory, "BC"))
        CatChart(metric, yr_sales_cat, 
                data, "cyr", "litres","subcategory", 
                beer_bc_cat_color, "stack", theme_xax, "M")
      })
    })
    ## plot % of total sales by bc subcategory
    output$litre_sales_yr_bc_cat_pc <- renderPlotly({
      safe_plotly({
      data <- beer_annual_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChart(metric, yr_sales_cat_pc, 
               data, "cyr", "pct_ttl_litres","subcategory", 
               beer_bc_cat_color, "stack", 
               theme_xax, "%")
      })
    })
    ## plots for bc beer subcategory yoy change in facets
    output$litre_sales_yoy_bc_cat_chg <- renderPlotly({
      safe_plotly({
      x <- beer_annual_data_subcat() %>% filter(str_detect(subcategory, "BC"))
      CatChgChart("", yr_sales_cat_pc_chg, 
                  x, x_var = "cyr", y_var = "yoy_litres", sort_var = "litres", 
                  fill_var = "yr_flag", 
                  facet_var = "subcategory",
                  fill_color = yr_flag_color, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg)
      })
    })
    ## % point chg by bc subcat yoy
    output$litre_sales_yoy_bc_cat_chg_pt <- renderPlotly({
      safe_plotly({
        x <- beer_annual_data_subcat() %>% filter(str_detect(subcategory, "BC"))
        CatChgChart("", yr_sales_cat_pcpt_chg, 
                    x, x_var = "cyr", y_var = "yoy_pcp_ttl_litres", sort_var = "litres",
                    fill_var = "yr_flag", 
                    facet_var = "subcategory",
                    fill_color = yr_flag_color, 
                    strp_color = bar_col,
                    theme_xax+theme_nleg, tunits = "num")
      })
    })
    
    ## IMPORT beer subcat ----
    ## plots by import country/region
    output$litre_sales_yr_import_cat <- renderPlotly({
      safe_plotly({
        cat('beer_import chart \n')
        data <- beer_annual_data_subcat_imp()
        print(data)
        CatChart(metric, yr_sales_imp, 
                data, "cyr", "litres","subcategory", beer_pal, "stack", 
                theme_xax, "M") %>%
          layout(legend = layout_legend_vr)
      })
    }) 
    
    output$litre_sales_yr_import_cat_pc <- renderPlotly({
      safe_plotly({
        cat('beer_import_pc chart \n')
        data <- beer_annual_data_subcat_imp()
        CatChart(metric, yr_sales_imp_pc, 
                data, "cyr", "pct_ttl_litres","subcategory", 
                beer_pal, "fill", 
                theme_xax, "%") %>%
          layout(legend = layout_legend_vr)
      })
    }) 
    
    ## plots for import beer subcategory yoy change in facets
    output$litre_sales_yoy_import_cat_chg <- renderPlotly({
      safe_plotly({
        x <- beer_annual_data_subcat_imp()
        CatChgChart("", yr_sales_imp_pc_chg,
                    x, x_var = "cyr", y_var = "yoy_sales", 
                    sort_var = "litres", 
                    fill_var = "yr_flag", 
                    facet_var = "subcategory",
                    fill_color = yr_flag_color, 
                    strp_color = bar_col,
                    theme_xax+theme_nleg)
      })
    })
    ## % point chg by import subcat yoy
    output$litre_sales_yoy_import_cat_chg_pt <- renderPlotly({
      safe_plotly({
      x <- beer_annual_data_subcat_imp() %>% filter(category=='Import')
      CatChgChart("", yr_sales_imp_pcpt_chg, 
                  x, x_var = "cyr", y_var = "yoy_pcp_ttl_litres", 
                  sort_var = "litres", 
                  fill_var = "yr_flag", 
                  facet_var = "subcategory",
                  fill_color = yr_flag_color, 
                  strp_color = bar_col,
                  theme_xax+theme_nleg, tunits = "num")
      })
    })
} # end server


