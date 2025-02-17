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
library(shinyjs)
library(tidyverse)
library(plotly)
library(bslib)
library(RColorBrewer)
library(lubridate)
library(scales)
library(plotly)
library(here)

library(DBI) # needed for duckdb
library(duckdb) # needed for duckdb
library(duckplyr) # used for duckdb
library(glue) # good for parameterized queries of duckdb
library(ellmer) # needed for llm
library(shinychat) # needed for llm
library(promises) # needed for llm

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

## DATA LLM -----------------------------------------------------------
# connection set here and ended at end of session 
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = TRUE)  # Use in-memory DB
# close when done
onStop(function() {
  dbDisconnect(con)
})

# get data ----
# query database via separate file for tidyness
## all data ----
source('query.R')
lmr_data_bu <- lmr_data
# set up duckdb table
duckdb::dbWriteTable(con, "lmr_data_duck", lmr_data, overwrite = TRUE)

# system prompt ----
# Dynamically create the system prompt, based on the real data. For an actually
# large database, you wouldn't want to retrieve all the data like this, but
# instead either hand-write the schema or write your own routine that is more
# efficient than system_prompt().
system_prompt_str <- system_prompt(dbGetQuery(con, "SELECT * FROM lmr_data_duck"), 
                                   "lmr_data_duck")

# greeting ----
# greeting that should initially appear in the sidebar when the app loads.
#greeting <- paste(readLines(here("greeting.md")), collapse = "\n")
greeting <- paste(readLines('greeting.md'), collapse = "\n")

# Server logic -----
function(input, output, session) {
  # duckdb ---
  # connection set here and ended at end of session (bottom)
  #con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")  # Use in-memory DB
  
  # get data ----
  # query database via separate file for tidyness
  ## all data ----
  #source('query.R')
  #lmr_data_bu <- lmr_data
  # set up duckdb table
  #duckdb::dbWriteTable(con, "lmr_data_duck", lmr_data, overwrite = TRUE)
  #lmr_data <- dbReadTable(con, "lmr_data_duck")
  #duckdb::dbDisconnect(con) # leave connection open for session
  # test duckdb
  #lmr_test <- dbGetQuery(con, "SELECT * FROM lmr_data_duck LIMIT 10")
  
  # apply to yr filter as default to avoid over-crowding
  lmr_max <- max(lmr_data$cyr_num) # get current latest yr
  lmr_yrs <- 6 # determine how many yrs back to go
  lmr_recent <- lmr_data %>% filter(cyr_num > lmr_max-lmr_yrs)
  # max date for top of sidebar on pg, set in dynamic sidebar
  lmr_max_date <- max(lmr_data$end_qtr_dt)
  lmr_max_note <- paste0("Data as of: ", format(lmr_max_date, "%b %d %Y"))
  
  # DATA LLM ---- 
  # This object must always be passed as the `.ctx` argument to query(), so that
  # tool functions can access the context they need to do their jobs; in this
  # case, the database connection that query() needs.
  ctx <- list(con = con)
  # initial data setup
  current_title <- reactiveVal(NULL)
  current_query <- reactiveVal("")
  data_llm <- reactive({
    qry <- current_query()
    if (is.null(qry) || qry == "") {
      qry <- "SELECT * FROM lmr_data_duck;"
    }
    dbGetQuery(con, qry)
  })
  
  cat("02 aggregate annual & qtr totals \n")
  # Aggregate data ----
  # annual and qtr totals ---------------------------------------------------
  annual_data <- reactive({
    data_llm() %>% group_by(cyr) %>%
      summarize(netsales = sum(netsales),
                litres = sum(litres)) %>%
      mutate(yoy_sales = (netsales - lag(netsales))/lag(netsales),
             yoy_litres = (litres - lag(litres))/lag(litres))
  })
  qtr_data <- reactive({
    data_llm() %>% group_by(cyr, cqtr, cyr_qtr, end_qtr_dt) %>%
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
    AnnualCatTypeData(data_llm())
  })
  # test
  #ancattype <- AnnualCatTypeData(lmr_data)
  
  qtr_data_cat <- reactive({
    # need to base the qoq on the number of cats chosen in filter
    n_qtr <- length(input$qtr_check)
    n_cats <- length(input$cat_check)
    data_llm() %>% group_by(cyr, cqtr, cyr_qtr, end_qtr_dt, cat_type) %>%
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
  
  # CHAT Header Output -------------------------------------------------------------------
  output$show_title <- renderText({
    current_title()
  })
  output$show_query <- renderText({
    current_query()
  })
  
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

  # CHAT -----------------------------------------------------------
  # Key functions
  # taken directly from original repo:
  # - https://github.com/jcheng5/r-sidebot/blob/main/app.R#L44
  append_output <- function(...) {
    txt <- paste0(...)
    shinychat::chat_append_message(
      "chat",
      list(role = "assistant", content = txt),
      chunk = TRUE,
      operation = "append",
      session = session
    )
  }
  
  #' Modifies the data presented in the data dashboard, based on the given SQL
  #' query, and also updates the title.
  #' @param query A DuckDB SQL query; must be a SELECT statement.
  #' @param title A title to display at the top of the data dashboard,
  #'   summarizing the intent of the SQL query.
  update_dashboard <- function(query, title) {
    append_output("\n```sql\n", query, "\n```\n\n")
    
    tryCatch(
      {
        # Try it to see if it errors; if so, the LLM will see the error
        dbGetQuery(con, query)
      },
      error = function(err) {
        append_output("> Error: ", conditionMessage(err), "\n\n")
        stop(err)
      }
    )
    
    if (!is.null(query)) {
      current_query(query)
    }
    if (!is.null(title)) {
      current_title(title)
    }
  }
  
  #' Perform a SQL query on the data, and return the results as JSON.
  #' @param query A DuckDB SQL query; must be a SELECT statement.
  #' @return The results of the query as a JSON string.
  query <- function(query) {
    # Do this before query, in case it errors
    append_output("\n```sql\n", query, "\n```\n\n")
    
    tryCatch(
      {
        df <- dbGetQuery(con, query)
      },
      error = function(e) {
        append_output("> Error: ", conditionMessage(e), "\n\n")
        stop(e)
      }
    )
    
    tbl_html <- df_to_html(df, maxrows = 5)
    append_output(tbl_html, "\n\n")
    
    df |> jsonlite::toJSON(auto_unbox = TRUE)
  }
  # instructions for chat model and setup
  chat <- chat_openai(model = openai_model, system_prompt = system_prompt_str)
  chat$register_tool(tool(
    update_dashboard,
    "Modifies the data presented in the data dashboard, based on the given SQL query, and also updates the title.",
    query = type_string("A DuckDB SQL query; must be a SELECT statement."),
    title = type_string("A title to display at the top of the data dashboard, summarizing the intent of the SQL query.")
  ))
  chat$register_tool(tool(
    query,
    "Perform a SQL query on the data, and return the results as JSON.",
    query = type_string("A DuckDB SQL query; must be a SELECT statement.")
  ))
  
  # Prepopulate the chat UI with a welcome message that appears to be from the
  # chat model (but is actually hard-coded). This is just for the user, not for
  # the chat model to see.
  chat_append("chat", greeting)
  
  ## Handle user input ----
  observeEvent(input$chat_user_input, {
    # Add user message to the chat history
    # need promises pkg
    chat_append("chat", chat$stream_async(input$chat_user_input)) %...>% {
       print(chat)
    }
  })
  ## end chat -----------------------------------------------------------
  # Ensure duckdb connection is closed when the session ends
  # - using onStop() at top instead
  #session$onSessionEnded(function() {
  #  DBI::dbDisconnect(con, shutdown = TRUE)
  #})

} # END server -----------------------------------------------------------
