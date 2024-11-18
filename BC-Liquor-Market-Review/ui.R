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
    titlePanel("BC Liquor Market Report Dashboard"),
    tags$h3("An (unofficial) consolidated view of quarterly BC Liquor Sales data, 
            compiled from", tags$a(href="https://www.bcldb.com/publications/liquor-market-review", "govt. sources", class='non-tab'),
            class = "sub"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      # sidebar ----
        sidebarPanel(
          class = "sidebar",
          # dynamic sidebar displays filter options depending on tab selected (courtesy of chatGPT)
          # - allows for re-use of same filter setup across multiple tabs
          # - conttrolled by dynamic_sidebar in server.R
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
                       tags$h2("Total Sales by Year and Quarter (all categories)", 
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
                     fluidRow( ## fluidRow 1 ttl sales----
                       tags$h2("Beer Sales by Year and Quarter (all categories)", 
                               class='section',
                               id="beer_sales"),
                       column(width = 6,
                              plotlyOutput("beer_sales_yr")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_sales_qtr")
                       )
                     ), # end fluidRow 1
                     fluidRow( ## fluidRow 2 ----
                       column(width = 6,
                              plotlyOutput("beer_sales_yoy", height = "200px")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_sales_qoq", height = "200px")
                        )
                     ), # end fluidRow 2
                     fluidRow( ## fluidRow 3 src sales ----
                       tags$h2("Beer Sales by Source: Yearly and Quarterly", 
                               class='section',
                               id="bsrc_sales"),
                       column(width = 6,
                              plotlyOutput("beer_sales_yr_cat")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_sales_qtr_cat")
                       )
                     ), # end fluidRow 3
                     fluidRow( ## fluidRow 4 ----
                       column(width = 6,
                              plotlyOutput("beer_sales_yoy_cat_chg", height = "500px")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_sales_qoq_cat_chg", height = "500px")
                       )
                     ), # end fluidRow 4
                     fluidRow( ## fluidRow 5 bc cat ----
                               tags$h2("BC-Produced Beer by Category: $ & %", 
                                       class='section',
                                       id="bcat_sales"),
                               column(width = 6,
                                      plotlyOutput("beer_sales_yr_bc_cat")
                                ),
                               column(width = 6,
                                      plotlyOutput("beer_sales_yr_bc_cat_pc")
                               ),
                     ), # end fluidRow 5
                     fluidRow( ## fluidRow 6 import ----
                       tags$h2("Import Beer Sales by Country: Yrly, Qtrly", 
                               class='section',
                               id="bimp_sales"),
                       tags$p("Coming soon!"),
                       column(width = 6,
                              
                       ),
                       column(width = 6,
                              
                       )
                     ) # end fluidRow 6
            ), # end tabPanel 2
            # tabPanel 3: Refresh Bev ----
            tabPanel("Refresh Bev", value = 3,
                     fluidRow( ## fluidRow 1 ----
                       tags$h2("Refreshment Beverage Sales by Yr & Qtr", class='section'),
                       column(width = 6,
                              plotlyOutput("refresh_sales_yr")
                       ),
                       column(width = 6,
                              plotlyOutput("refresh_sales_qtr")
                       )
                     ), # end fluidRow 1
                     fluidRow( ## fluidRow 2 ----
                       column(width = 6,
                              plotlyOutput("refresh_sales_yoy", height = "200px")
                       ),
                       column(width = 6,
                              plotlyOutput("refresh_sales_qoq", height = "200px")
                       )
                     ), # end fluidRow 2
                     fluidRow( ## fluidRow 3 ----
                       tags$h2("Refreshment Beverage Category Sales by Yr & Qtr", class='section'),
                       column(width = 6,
                              plotlyOutput("refresh_sales_yr_cat")
                       ),
                       column(width = 6,
                              plotlyOutput("refresh_sales_qtr_cat")
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
                       tags$h2("About the BC Liquor Market Review Dashboard", class='section marginb'),
                       tags$p("This collection of data visualizations provides a consolidated view of BC liquor sales, 
                       compiled from the ", 
                              tags$a(href="https://www.bcldb.com/publications/liquor-market-review", 
                                     "BC Liquor Market Review", class="non-tab"), 
                              "published each quarter by the British Columbia Liquor Distribution Branch."),
                       tags$p("Each edition of the Liquor Market Review is published in PDF format and 
                              includes the ", tags$strong("most recent quarterly sales data for the province, 
                              plus previous 4 quarters."), "with limited visualizations. 
                              So the advantage here is that I have:"),
                       tags$ul(tags$li(tags$strong("consolidated data from all quarterly reports since 2015")),
                               tags$li("provided interactive visualizations with filtering, details on mouseover"),
                               tags$li("shown year-over-year and quarter-over-quarter comparisons"),
                               tags$li("consolidated data into total, category-level, and category-specific breakdowns")),
                       tags$p("The Liquor Market Review breaks down",
                              tags$strong("wholesale sales data by net revenue and litres"),
                              " for alcoholic beverages in the following categories:"),
                       tags$ul(tags$li("Beer"), 
                               tags$li("Refreshment Beverages"), 
                               tags$li("Spirits"), 
                               tags$li("Wine")),
                      tags$p("In addition, each category has beverage type and geographic depending on the category. 
                             Not all of the detail available in the official version of the Liquor Market Review may be captured here. 
                             Further, I offer", tags$strong("no guarantees"), "as to the accuracy of the data presented here. 
                             If in doubt or using this information for important purposes, 
                             please refer to the official Liquor Market Review."),
                      tags$strong("Note: This is an unofficial, personal project and is not affiliated with the BC Liquor Distribution Branch."),
                      tags$h3("Who did this?"),
                      tags$p("This dashboard was created by John Yuill, a data analyst and data product developer based in Vancouver, BC. 
                             "),
                      tags$p("For more information, questions, or feedback, you can reach me on", 
                             tags$a(href="https://www.linkedin.com/in/johnyuill/", "LinkedIn.", class="non-tab")
                             )
                     ) # end fluidRow 1
              ) # end tabPanel 6
            ) # end tabsetPanel ----
        ) # end mainPanel ----
    ) # end sidebarLayout ----
) # end shinyUI
