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

# Define UI for application that draws a histogram
fluidPage(
  useShinyjs(), # for toggling side panel
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
      # style for toggle button
      tags$style(
        "#toggleSidebar {margin-bottom: 4px;
        padding: 2px;}"
      )
      ),
    
    # Application title
    titlePanel("BC Beer Sales Dashboard"),
    tags$h3("An (unofficial) consolidated view of quarterly BC Liquor Sales data, 
            compiled from", tags$a(href="https://www.bcldb.com/publications/liquor-market-review", "govt. sources", class='non-tab'),
            class = "sub"),
    # action button to toggle sidebar - abandoned for now
    # - makes sidebar disappear/appear but mainPanel doesn't expand to fill the space
    #column(12, actionButton('toggleSidebar', 'Toggle Sidebar'),),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      # sidebar panel ----
        sidebarPanel(
          class = "sidebar",
          id = "sidebar", # needed for toggling
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
            # tabPanel 1: Net $ ----
            tabPanel("Net $ Sales", value = 1,
                     fluidRow( ## fluidRow 1 ttl sales----
                       tags$h2("Beer Sales by Year and Qtr (all cat.)", 
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
                       tags$h2("Beer Sales by Source", 
                               class='section',
                               id="bsrc_sales"),
                       column(width = 6,
                              plotlyOutput("beer_sales_yr_cat")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_sales_yr_cat_pc")
                       )
                     ), # end fluidRow 3
                     fluidRow( ## fluidRow 4 ----
                       column(width = 6,
                              plotlyOutput("beer_sales_yoy_cat_chg", height = "500px")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_sales_yoy_cat_chg_pt", height = "500px")
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
                     fluidRow( ## fluidRow 6 BC beer subcat % chg ----
                       column(width = 6,
                              plotlyOutput("beer_sales_yoy_bc_cat_chg")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_sales_yoy_bc_cat_chg_pt") 
                       )
                     ), # end fluidRow 6
                     fluidRow( ## fluidRow 7 import ----
                       tags$h2("Import Beer Sales by Ctry/Region", 
                               class='section',
                               id="bimp_sales"),
                       column(width = 6,
                              plotlyOutput("beer_sales_yr_import_cat")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_sales_yr_import_cat_pc") 
                       )
                     ), # end fluidRow 7
                     fluidRow( ## fluidRow 8 import % chg ----
                       column(width = 6,
                              plotlyOutput("beer_sales_yoy_import_cat_chg")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_sales_yoy_import_cat_chg_pt") 
                       )
                     ) # end fluidRow 8
            ), # end tabPanel 1
            # tabPanel 2: Litres $ ----
            tabPanel("Litre Sales", value = 2,
                     fluidRow( ## fluidRow 1 ttl sales----
                               tags$h2("Litre Sales by Year and Qtr (all cat.)", 
                                       class='section',
                                       id="litre_sales"),
                               column(width = 6,
                                      plotlyOutput("litre_sales_yr")
                               ),
                               column(width = 6,
                                      plotlyOutput("litre_sales_qtr")
                               )
                     ), # end fluidRow 1
                     fluidRow( ## fluidRow 2 ----
                               column(width = 6,
                                      plotlyOutput("litre_sales_yoy", height = "200px")
                               ),
                               column(width = 6,
                                      plotlyOutput("litre_sales_qoq", height = "200px")
                               )
                     ), # end fluidRow 2
                     fluidRow( ## fluidRow 3 src sales ----
                               tags$h2("Litre Sales by Source", 
                                       class='section',
                                       id="bsrc_sales"),
                               column(width = 6,
                                      #plotlyOutput("beer_sales_yr_cat")
                               ),
                               column(width = 6,
                                      #plotlyOutput("beer_sales_yr_cat_pc")
                               )
                     ), # end fluidRow 3
                     fluidRow( ## fluidRow 4 ----
                               column(width = 6,
                                      #plotlyOutput("beer_sales_yoy_cat_chg", height = "500px")
                               ),
                               column(width = 6,
                                      #plotlyOutput("beer_sales_yoy_cat_chg_pt", height = "500px")
                               )
                     ), # end fluidRow 4
                     fluidRow( ## fluidRow 5 bc cat ----
                               tags$h2("BC-Produced Beer by Category: $ & %", 
                                       class='section',
                                       id="bcat_sales"),
                               column(width = 6,
                                      #plotlyOutput("beer_sales_yr_bc_cat")
                               ),
                               column(width = 6,
                                      #plotlyOutput("beer_sales_yr_bc_cat_pc")
                               ),
                     ), # end fluidRow 5
                     fluidRow( ## fluidRow 6 BC beer subcat % chg ----
                               column(width = 6,
                                      #plotlyOutput("beer_sales_yoy_bc_cat_chg")
                               ),
                               column(width = 6,
                                      #plotlyOutput("beer_sales_yoy_bc_cat_chg_pt") 
                               )
                     ), # end fluidRow 6
                     fluidRow( ## fluidRow 7 import ----
                               tags$h2("Import Beer Sales by Ctry/Region", 
                                       class='section',
                                       id="bimp_sales"),
                               column(width = 6,
                                      #plotlyOutput("beer_sales_yr_import_cat")
                               ),
                               column(width = 6,
                                      #plotlyOutput("beer_sales_yr_import_cat_pc") 
                               )
                     ), # end fluidRow 7
                     fluidRow( ## fluidRow 8 import % chg ----
                               column(width = 6,
                                      #plotlyOutput("beer_sales_yoy_import_cat_chg")
                               ),
                               column(width = 6,
                                      #plotlyOutput("beer_sales_yoy_import_cat_chg_pt") 
                               )
                     ) # end fluidRow 8
            ), # end tabPanel 2
            # tabPanel 3: About ----
            tabPanel("About", value = 3,
                     fluidRow( ## fluidRow 1 ----
                       tags$h2("About the BC Liquor Market Review Dashboard", 
                               class='section marginb',
                               id = 'about'),
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
                               tags$li("provided year-over-year and quarter-over-quarter comparisons, using ",
                                       tags$strong("calendar yr")," (rather than fiscal yr) for convenience"),
                               tags$li("consolidated data into total, category-level, and category-specific breakdowns")
                               ),
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
                      tags$p(tags$strong("Note: This is an unofficial, personal project and is 
                                         not affiliated with the BC Liquor Distribution Branch.")),
                      tags$h2("Who did this?", 
                              class='section marginb',
                              id='who'),
                      tags$p("This dashboard was created by John Yuill, a data analyst and data product developer based in Vancouver, BC. 
                             "),
                      tags$p("For more information, questions, or feedback, 
                             you can reach me on", 
                             tags$a(href="https://www.linkedin.com/in/johnyuill/", "LinkedIn.", class="non-tab")
                             ),
                      tags$h2("Release Notes", 
                              class='section marginb',
                              id='release'),
                      tags$h4("2025-02-02"),
                      tags$ul(tags$li("Wine: interactive treemaps for sales by region and type, with drilldown capability.")),
                      tags$h4("2025-01-11"),
                      tags$ul(tags$li("Additional category-specific visualizations, with category/region breakdowns.")),
                      tags$h4("2024-01-05"),
                      tags$ul(tags$li("First full version of the BC Liquor Market Review Dashboard,
                                      full category coverage.")),
                      tags$h4("2024-10-27"),
                      tags$ul(tags$li("Initial version of the BC Liquor Market Review Dashboard,
                                      starting with Beer category but framed up for scaling to other categories."))
                      ) # end fluidRow 1
              ) # end tabPanel 3
            ), # end tabsetPanel ----
          # FOOTER ----
          tags$div(
            style = "margin-top: 20px; padding: 10px; border-top: 2px solid #ddd; text-align: center;",
            "© 2025 A", 
            tags$a(href="https://www.catbird-analytics.com/", 'Catbird Analytics'),
            " Production, John Yuill; see 'About' tab for details", 
            class="footer")
        ) # end mainPanel ----
    ) # end sidebarLayout ----
) # end shinyUI
