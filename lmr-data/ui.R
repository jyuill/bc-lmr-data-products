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
  theme = bslib::bs_theme(version = 4,
                          preset = 'lux',
                          `enable-shadows` = TRUE,
                          `enable-rounded` = TRUE,
                          font_scale = NULL),
    # Link to the external CSS file
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    # Application title
    titlePanel("BC Liquor Market Report (LMR) data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          class = "sidebar",
            # select one or more years, including multiple years
            # different options tried
            # selector is good but takes up space, not so intuitive/elegant
            # selectizeInput(inputId="cyr_select", "Select a year", 
            #                choices = unique(lmr_data$cyr), 
            #                selected = unique(lmr_data$cyr),
            #                multiple = TRUE
            #                ),
            # checkbox works but screen real estate
            # checkboxGroupInput(inputId = "cyr_check", "Select a year", 
            #                    choices = unique(lmr_data$cyr), 
            #                    selected = unique(lmr_data$cyr),
            #                    inline = FALSE
            #                    ),
            # picker for max flexibility/usability
            uiOutput("dynamic_cyr"),
            # filter for quarters
            uiOutput("dynamic_qtr"),
            # filter for categories
            uiOutput("dynamic_cat"),
        ), # end sidebarPanel

        # main panel with content
        mainPanel(
          class = "main",
          fluidRow(
            tags$h2("Total Sales by Year and Quarter (all categories)", class='section'),
            column(width = 6,
                   plotlyOutput("sales_yr")
            ),
            column(width = 6,
                   plotlyOutput("sales_qtr")
            )
          ), # end fluidRow 1
          fluidRow(
            column(width = 6,
                   plotlyOutput("sales_yoy", height = "200px")
            ),
            column(width = 6,
                   plotlyOutput("sales_qoq", height = "200px")
          )
          ), # end fluidRow 2
          fluidRow(
            tags$h2("Category Sales by Year and Quarter", class='section'),
            column(width = 6,
                   plotlyOutput("sales_yr_cat")
            ),
            column(width = 6,
                   plotlyOutput("sales_qtr_cat")
          )
          ), # end fluidRow 3
          fluidRow(
            column(width = 6,
                   plotlyOutput("sales_yoy_cat", height = "500px")
            ),
            column(width = 6,
                   plotlyOutput("sales_qoq_cat", height = "500px")
          )
          ), # end fluidRow 4
        ) # end mainPanel
    ) # end sidebarLayout
) # end shinyUI
