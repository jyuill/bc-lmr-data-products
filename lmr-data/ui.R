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
    tags$head(
      #  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),
    
    # Application title
    titlePanel("BC Liquor Market Report (LMR) Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      # sidebar ----
        sidebarPanel(
          class = "sidebar",
          # dynamic sidebar displays filter options depending on tab selected (courtesy of chatGPT)
          # - allows for re-use of same filter setup across multiple tabs
          uiOutput("dynamic_sidebar") 
        ), # end sidebarPanel

        # main panel with content ----
        mainPanel(
          class = "main",
          tabsetPanel( # tabsetPanel ----
            id = "tabselected",
            # tabPanel 1: Overview ----
            tabPanel("Overview", value = 1,
                     fluidRow( ## fluidRow 1 ----
                       tags$h2("Total Sales by Year and Quarter (all categories)", class='section'),
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
                       tags$h2("Category Sales by Year and Quarter", class='section'),
                       column(width = 6,
                              plotlyOutput("sales_yr_cat")
                       ),
                       column(width = 6,
                              plotlyOutput("sales_qtr_cat")
                       )
                     ), # end fluidRow 3
                     fluidRow( ## fluidRow 4 ----
                       column(width = 6,
                              plotlyOutput("sales_yoy_cat", height = "500px")
                       ),
                       column(width = 6,
                              plotlyOutput("sales_qoq_cat", height = "500px")
                       )
                     ), # end fluidRow 4
            ), # end tabPanel 1
            # tabPanel 2: Beer ----
            tabPanel("Beer", value = 2,
                     fluidRow( ## fluidRow 1 ----
                       tags$h2("Beer Sales by Year and Quarter (all categories)", class='section'),
                       column(width = 6
                              ,
                              plotlyOutput("beer_sales_yr")
                       ),
                       column(width = 6
                              ,
                              plotlyOutput("beer_sales_qtr")
                       )
                     ), # end fluidRow 1
                     fluidRow( ## fluidRow 2 ----
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yoy", height = "200px")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qoq", height = "200px")
                        )
                     ), # end fluidRow 2
                     fluidRow( ## fluidRow 3 ----
                       tags$h2("Beer Category Sales by Year and Quarter", class='section'),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yr_cat")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qtr_cat")
                       )
                     ), # end fluidRow 3
                     fluidRow( ## fluidRow 4 ----
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yoy_cat", height = "500px")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qoq_cat", height = "500px")
                       )
                     ) # end fluidRow 4
            ), # end tabPanel 2
            # tabPanel 3: Refresh Bev ----
            tabPanel("Refresh Bev", value = 3,
                     fluidRow( ## fluidRow 1 ----
                        tags$h2("COMING SOON", class='section'),
                       tags$h2("Refreshment Beverage Sales by Year and Quarter (all categories)", class='section'),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yr")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qtr")
                       )
                     ), # end fluidRow 1
                     fluidRow( ## fluidRow 2 ----
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yoy", height = "200px")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qoq", height = "200px")
                       )
                     ), # end fluidRow 2
                     fluidRow( ## fluidRow 3 ----
                       tags$h2("Refreshment Beverage Category Sales by Year and Quarter", class='section'),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yr_cat")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qtr_cat")
                       )
                     ), # end fluidRow 3
                     fluidRow( ## fluidRow 4 ----
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yoy_cat", height = "500px")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qoq_cat", height = "500px")
                       )
                     ) # end fluidRow 4
            ), # end tabPanel 3
            # tabPanel 4: Spirits ----
            tabPanel("Spirits", value = 4,
                     fluidRow( ## fluidRow 1 ----
                        tags$h2("COMING SOON", class='section'),
                       tags$h2("Spirits Sales by Year and Quarter (all categories)", class='section'),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yr")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qtr")
                       )
                     ), # end fluidRow 1
                     fluidRow( ## fluidRow 2 ----
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yoy", height = "200px")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qoq", height = "200px")
                       )
                     ), # end fluidRow 2
                     fluidRow( ## fluidRow 3 ----
                       tags$h2("Spirits Category Sales by Year and Quarter", class='section'),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yr_cat")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qtr_cat")
                       )
                     ), # end fluidRow 3
                     fluidRow( ## fluidRow 4 ----
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yoy_cat", height = "500px")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qoq_cat", height = "500px")
                       )
                     ) # end fluidRow 4
            ), # end tabPanel 4
            # tabPanel 5: Wine ----
            tabPanel("Wine", value = 5,
                     fluidRow( ## fluidRow 1 ----
                      tags$h2("COMING SOON", class='section'),
                       tags$h2("Wine Sales by Year and Quarter (all categories)", class='section'),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yr")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qtr")
                       )
                     ), # end fluidRow 1
                     fluidRow( ## fluidRow 2 ----
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yoy", height = "200px")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qoq", height = "200px")
                       )
                     ), # end fluidRow 2
                     fluidRow( ## fluidRow 3 ----
                       tags$h2("Wine Category Sales by Year and Quarter", class='section'),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yr_cat")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qtr_cat")
                       )
                     ), # end fluidRow 3
                     fluidRow( ## fluidRow 4 ----
                       column(width = 6
                              #,
                              #plotlyOutput("sales_yoy_cat", height = "500px")
                       ),
                       column(width = 6
                              #,
                              #plotlyOutput("sales_qoq_cat", height = "500px")
                       )
                     ) # end fluidRow 4
            ), # end tabPanel 5
            # tabPanel 6: About ----
            tabPanel("About", value = 6,
                     fluidRow( ## fluidRow 1 ----
                       tags$h2("About the BC Liquor Market Report (LMR) Data", class='section'),
                       tags$p("This is a Shiny web application that provides an overview of the BC Liquor Market Report (LMR) data."),
                       tags$p("The data is sourced from the BC Liquor Distribution Branch (LDB) and includes sales data for various categories of alcoholic beverages."),
                       tags$p("The data is updated quarterly and includes sales data for the current fiscal year and previous fiscal years."),
                       tags$p("The data is available for the following categories: Beer, Refreshment Beverages, Spirits, and Wine."),
                       tags$p("The data is available for the following years: 2020, 2021, 2022, 2023, 2024.")
                     ) # end fluidRow 1
              ) # end tabPanel 6
            ) # end tabsetPanel ----
        ) # end mainPanel ----
    ) # end sidebarLayout ----
) # end shinyUI
