#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Define UI for application that draws a histogram
fluidPage(
    # apply bootstrap theme - set using 'preset'
    theme = bslib::bs_theme(version = 4,
                            preset = 'lux',
                            `enable-shadows` = TRUE,
                            `enable-rounded` = TRUE,
                            font_scale = NULL),
    # Link to the external CSS file
    # 2 ways to do this: 2) tags$link in tags$head or 1) includeCSS
    # - same effect; 2 more scalable but 1 allows to see the CSS file in the IDE, without needing browser
    includeCSS("www/style.css"),
    # Application title
    tags$div(class = "title-container",
             tags$h1("BC Liquor Market Report LLM Dashboard", class='title'),
             tags$h3("An (unofficial) consolidated view of quarterly BC Liquor Sales data, 
            compiled from", tags$a(href="https://www.bcldb.com/publications/liquor-market-review", "govt. sources", class='non-tab'),
                     class = "sub")
    ),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      # sidebar panel ----
      sidebarPanel(
        class = "sidebar",
        style = "height: 100%;",
        # CHAT UI ####
        chat_ui("chat", height = "80%", fill = TRUE)
      ), # end sidebarPanel
        
      # main panel with content ----
      mainPanel(
        class = "main",
        # header
        textOutput("show_title", container = h3),
        verbatimTextOutput("show_query") |>
          tagAppendAttributes(style = "max-height: 100px; overflow: auto;"),
                              fluidRow( ## fluidRow 1 ----
                                        tags$h2("Sales by Year and Quarter", 
                                                class='section',
                                                id='ttl_sales'),
                                        column(width = 6,
                                               plotlyOutput("sales_yr")
                                        ),
                                        column(width = 6,
                                               plotlyOutput("sales_qtr")
                                        )
                              ), # end fluidRow 1
                              fluidRow( ## fluidRow 2 ----
                                        column(width = 6,
                                               plotlyOutput("sales_yoy", height = "200px")
                                        ),
                                        column(width = 6,
                                               plotlyOutput("sales_qoq", height = "200px")
                                        )
                              ),
                              fluidRow( ## fluidRow 3 ----
                                        tags$h2("Category Sales by Year and Quarter", class='section',
                                                id='cat_sales'),
                                        column(width = 6,
                                               plotlyOutput("sales_yr_cat")
                                        ),
                                        column(width = 6,
                                               plotlyOutput("sales_yr_cat_pct")
                                        )
                              ), # end fluidRow 3
                              fluidRow( ## fluidRow 4 ----
                                        column(width = 6,
                                               plotlyOutput("sales_yoy_cat", height = "500px")
                                        ),
                                        column(width = 6,
                                               plotlyOutput("sales_yoy_cat_pcp", height = "500px")
                                        )
                              ) # end fluidRow 4
    ) # end mainPanel
    ) # end sidebarLayout  
) # end fluidPage
