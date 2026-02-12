#
# User Interface for BC Beer Sales Dashboard

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(bslib)
library(RColorBrewer)
library(shinyjs)

# Define UI for application 
fluidPage(
  useShinyjs(), # for toggling side panel
  # apply bootstrap theme - set using 'preset'
  theme = bslib::bs_theme(version = 5,
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
    # Removed for embedding in www.bcbeer.ca
    #titlePanel("BC Beer Sales"),
    #tags$h3("An (unofficial) consolidated view of quarterly BC Beer Sales data, 
    #        compiled from", tags$a(href="https://www.bcldb.com/publications/liquor-market-review", "govt. sources", class='non-tab'),
    #        class = "sub"),
    layout_sidebar(
      # sidebar panel ----
        sidebar = sidebar(
          id = "sidebar", # needed for toggling
          open = "desktop", # open on desktop, collapsible on mobile
          width = "25%", # matches default sidebarPanel width
          class = "sidebar",
          # dynamic sidebar displays filter options depending on tab selected (courtesy of chatGPT)
          # - allows for re-use of same filter setup across multiple tabs
          # - conttrolled by dynamic_sidebar in server.R
          # abandoning in favour of static sidebar for now for more robust approach
          #uiOutput("dynamic_sidebar") 
          # ---- Sidebar content ----
          # filters - consistent across tabs 1-3
          div(id = "sidebar_filters",
              tagList(
                     tags$p(max_date_note, class="note", id="max_date_note"),
                     dynamic_grain,
                     dynamic_cyr,
                     dynamic_qtr,
                     dynamic_beer_cat,
                     dynamic_beer_bc_subcat
              )
              ),
          # specific content for each tab sidebar
          div(id="sidebar_tab1_content",
              tagList(
                     tags$h4("Contents"),
                     tags$a(href="#overview_comparison", "Net $ & Litre Sales"),tags$br(),
                     tags$a(href="#multi_year_summary", "Multi-Yr Summary"),tags$br(),
                     tags$a(href="#overview_by_source", "Sales by Source"), tags$br(),
                     tags$a(href="#overview_bc_cat", "BC Producer Categories"),
                     tags$br(),tags$br(),
                     tags$h4("Notes"),
                     tags$p(sb_note_calyr, class="sb_note"), # consistent variables set in support_vars.R
                     tags$p(sb_note_charts, class="sb_note"),
                     tags$p(sb_note_sales, class="sb_note"),
                     tags$p(sb_note_src, class="sb_note")
              )),
          div(id="sidebar_tab2_content",
              tagList(
                 tags$h4("Contents"),
                     tags$a(href="#beer_sales", "$ Sales by Yr & Qtr"),tags$br(),
                     tags$a(href="#bsrc_sales", "$ Sales by Source"), tags$br(),
                     tags$a(href="#bcat_sales", "BC Beer by Category"), tags$br(),
                     tags$a(href="#bimp_sales","Import Sales by Ctry"), tags$br(),
                     tags$br(), tags$br(),
                     tags$h4("Notes"),
                     tags$p(sb_note_calyr, class="sb_note"),
                     tags$p(sb_note_charts, class="sb_note"),
                     tags$p(sb_note_sales, class="sb_note"),
                     tags$p(sb_note_src, class="sb_note")
          )),
          div(id="sidebar_tab3_content",
              tagList(
                     tags$h4("Contents"),        
                     tags$a(href="#litre_sales", "Ttl Litres by Yr & Qtr"),tags$br(),
                     tags$a(href="#bsrc_sales_litre", "Litres by Source"), tags$br(),
                     tags$a(href="#bcat_sales_litre", "BC Litres by Category"), tags$br(),
                     tags$a(href="#bimp_sales_litre","Import Litres by Ctry"), tags$br(),
                     tags$br(), tags$br(),
                     tags$h4("Notes"),
                     tags$p(sb_note_calyr, class="sb_note"),
                     tags$p(sb_note_charts, class="sb_note"),
                     tags$p(sb_note_src, class="sb_note")
          )),
          div(id="sidebar_tab4_content",
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
                     tags$h4("Contents"),
                     tags$a(href="#chglog", "Change Log"), 
                     tags$br()
          ))
        ), # end sidebar

        # main panel with content ----
        # Note: layout_sidebar() handles the main content without mainPanel wrapper
        div(class = "main",
        tabsetPanel( # tabsetPanel ----
            id = "tabselected",
            # tabPanel 1: Overview ----
            tabPanel("Overview", value = 1,
                     fluidRow( ## fluidRow 1 overview comparison ----
                       tags$h2("Beer Sales in BC: Net $ vs Litres",
                               class='section',
                               id="overview_comparison"),
                       tags$h3("Overall trends in beer sales in BC (select 'Annual' or 'Quarterly' at left)",
                               class='subtitle'),
                       column(width = 6,
                              plotlyOutput("overview_sales_qtr")
                       ),
                       column(width = 6,
                              plotlyOutput("overview_litres_qtr")
                       )
                     ), # end fluidRow 1
                     fluidRow( ## fluidRow 2 overview YoY ----
                       tags$h3("What are % changes between comparative periods?",
                               class='subtitle'),
                       column(width = 6,
                              plotlyOutput("overview_sales_yoy", height = "200px")
                       ),
                       column(width = 6,
                              plotlyOutput("overview_litres_yoy", height = "200px")
                       )
                     ), # end fluidRow 2
                     fluidRow( ## fluidRow 3 overview summary chart ----
                       tags$h2("Multi-Year Performance Summary", 
                            class='section', 
                            id="multi_year_summary"),
                            tags$h3("What are the longer-term trends in % changes?", class='subtitle'),    
                       column(width = 12,
                              plotlyOutput("overview_summary_chart", height = "300px")
                       )
                     ), # end fluidRow 3
                     
                     fluidRow( ## fluidRow 5 source/cat net $ and litres by category line ----
                       tags$h2("Net $ Beer Sales & Litres by Source", 
                                      class='section',
                                      id="overview_by_source"),
                       tags$h3("Where is the beer coming from?",
                                   class='subtitle'),     
                       column(width = 6,  
                              plotlyOutput("beer_sales_qtr_cat_line", height = "400px")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_litre_qtr_cat_line", height = "400px")
                       )
                     ), # end fluidRow 5
                     fluidRow( ## fluidRow 6 source/cat % chg yoy ----
                       tags$h3("How are sales changing by source for comparative periods?", 
                       class='subtitle'),
                       column(width = 6,
                              plotlyOutput("sales_qtr_cat_yoy", height = "500px")
                              ),
                       column(width = 6,
                              plotlyOutput("litres_qtr_cat_yoy", height = "500px")
                       )
                     ), # end fluidRow 6
                     fluidRow( ## fluidRow 7 source/cat % share ----
                       tags$h3("Share of sales from each source", class='subtitle'),
                       column(width = 6,
                              plotlyOutput("beer_sales_qtr_cat", height = "400px")
                              ),
                       column(width = 6,
                              plotlyOutput("beer_litre_qtr_cat", height = "400px")
                       )
                     ), # end fluidRow 7
                     fluidRow( ## fluidRow 8 BC produced category ----
                       column(width = 12,
                              tags$h2("BC Producer Categories: Net $ Sales & Litres", 
                                      class='section',
                                      id="overview_bc_cat"),
                            tags$h3("How do BC producer categories compare?",
                                   class='subtitle')
                       ),     
                       column(width = 6,  
                              plotlyOutput("beer_sales_yq_subcat_line", height = "400px")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_litre_yq_subcat_line", height = "400px")
                       )
                     ), # end fluidRow 8
                     fluidRow( ## fluidRow 9 % chg by BC produced category ----
                       tags$h3("How are sales changing by category for comparative periods?", 
                               class='subtitle'),
                       column(width = 6,
                              plotlyOutput("sales_subcat_yoy", height = "500px")
                       ),
                       column(width = 6,
                              plotlyOutput("litres_subcat_yoy", height = "500px")
                       )
                     ), # end fluidRow 9  
                     fluidRow( ## fluidRow 10 % share by BC category ----
                       tags$h3("Share of sales from each category", class='subtitle'),
                       column(width = 6,
                              plotlyOutput("beer_sales_yq_subcat_stacked", height = "400px")
                              ),
                       column(width = 6,
                              plotlyOutput("beer_litre_yq_subcat_stacked", height = "400px")
                       )
                     ), # end fluidRow 10       
            ), # end tabPanel 1
            # tabPanel 2: Net $ ----
            tabPanel("Net $ Sales", value = 2,
                     fluidRow( ## fluidRow 1 ttl sales----
                       tags$h2("Beer Net $ Sales by Year and Qtr (all cat.)", 
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
                       tags$h2("Beer Net $ Sales by Source", 
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
                               tags$h2("BC-Produced Beer by Category: Net $ Sales", 
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
                              plotlyOutput("beer_sales_yoy_bc_cat_chg", height = "500px")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_sales_yoy_bc_cat_chg_pt", height = "500px") 
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
                              plotlyOutput("beer_sales_yoy_import_cat_chg", height = "700px")
                       ),
                       column(width = 6,
                              plotlyOutput("beer_sales_yoy_import_cat_chg_pt", height = "700px") 
                       )
                     ) # end fluidRow 8
            ), # end tabPanel 1
            # tabPanel 3: Litres $ ----
            tabPanel("Litre Sales", value = 3,
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
                                       id="bsrc_sales_litre"),
                               column(width = 6,
                                      plotlyOutput("litre_sales_yr_cat")
                               ),
                               column(width = 6,
                                      plotlyOutput("litre_sales_yr_cat_pc")
                               )
                     ), # end fluidRow 3
                     fluidRow( ## fluidRow 4 ----
                               column(width = 6,
                                      plotlyOutput("litre_sales_yoy_cat_chg", height = "500px")
                               ),
                               column(width = 6,
                                      plotlyOutput("litre_sales_yoy_cat_chg_pt", height = "500px")
                               )
                     ), # end fluidRow 4
                     fluidRow( ## fluidRow 5 bc cat ----
                               tags$h2("BC-Produced Litres by Category: $ & %", 
                                       class='section',
                                       id="bcat_sales_litre"),
                               column(width = 6,
                                      plotlyOutput("litre_sales_yr_bc_cat")
                               ),
                               column(width = 6,
                                      plotlyOutput("litre_sales_yr_bc_cat_pc")
                               ),
                     ), # end fluidRow 5
                     fluidRow( ## fluidRow 6 BC beer subcat % chg ----
                               column(width = 6,
                                      plotlyOutput("litre_sales_yoy_bc_cat_chg", height = "500px")
                               ),
                               column(width = 6,
                                      plotlyOutput("litre_sales_yoy_bc_cat_chg_pt", height = "500px") 
                               )
                     ), # end fluidRow 6
                     fluidRow( ## fluidRow 7 import ----
                               tags$h2("Import Litres by Ctry/Region", 
                                       class='section',
                                       id="bimp_sales_litre"),
                               column(width = 6,
                                      plotlyOutput("litre_sales_yr_import_cat")
                               ),
                               column(width = 6,
                                      plotlyOutput("litre_sales_yr_import_cat_pc") 
                               )
                     ), # end fluidRow 7
                     fluidRow( ## fluidRow 8 import % chg ----
                               column(width = 6,
                                      plotlyOutput("litre_sales_yoy_import_cat_chg", height = "700px")
                               ),
                               column(width = 6,
                                      plotlyOutput("litre_sales_yoy_import_cat_chg_pt", height = "700px") 
                               )
                     ) # end fluidRow 8
            ), # end tabPanel 2
            # tabPanel 4: About ----
            tabPanel("About", value = 4, class = "about_tab",
                     fluidRow( ## fluidRow 1 ----
                       tags$h2("About the BC Beer Sales Dashboard", 
                               class='section marginb',
                               id = 'about'),
                       tags$p("This collection of data visualizations provides a consolidated view of BC beer sales, 
                       compiled from the ", 
                              tags$a(href="https://www.bcldb.com/publications/liquor-market-review", 
                                     "BC Liquor Market Review", class="non-tab"), 
                              "published each quarter by the British Columbia Liquor Distribution Branch."),
                       tags$p("Each edition of the Liquor Market Review is published in PDF format and 
                              includes the ", tags$strong("most recent quarterly sales data for the province, 
                              plus previous 4 quarters."), "with limited visualizations. 
                              So the advantage here is that I have:"),
                       tags$ul(tags$li(tags$strong("consolidated data from all quarterly reports since 2015")),
                               tags$li("provided interactive visualizations with filtering, details when hovering over the charts"),
                               tags$li("provided year-over-year and quarter-over-quarter comparisons, using ",
                                       tags$strong("calendar yr")," (rather than fiscal yr) for convenience"),
                               tags$li("consolidated data into total, category-level, and category-specific breakdowns")
                               ),
                          tags$p("Not all of the detail available in the official version of the Liquor Market Review may be captured here. 
                             Further, I offer", tags$strong("no guarantees"), "as to the accuracy of the data presented here. 
                             If in doubt or using this information for important purposes, 
                             please refer to the official Liquor Market Review."),
                      tags$p(tags$strong("Note: This is an unofficial project  
                                         not affiliated with the BC Liquor Distribution Branch, 
                                         provided for information to the general public."))
                      #,
                      #tags$p("For more info, check out the site's ", 
                      #       tags$a(href="https://www.bcbeer.ca/about.html", "About", class="non-tab"), " page.")
                      ), # end fluidRow 1
                     fluidRow( ## fluidRow 2: change log ----
                       tags$h3("Change Log", class="subabout", id = "chglog"),
                       tags$p("Latest changes, developments, improvements"),
                       tags$h4("2026-01-21"),
                       tags$ul(
                              tags$li("Multi-year performance summary: improved calculations and display to be based on rolling 12/24/36/48 mth periods from most recent quarter, 
                              instead of just calendar yr comparisons."),
                              tags$li("Technical: moved Shiny app from shinyapps.io to Posit Connect Cloud for improved deployment and sustainability."),
                              tags$li("Technical: Streamlined backend code to improve performance, make it more modular and easier to maintain."),
                              tags$li("Technical: Developed", 
                                   tags$a(href="https://github.com/jyuill/lmrtools",
                                   "lmrtools R package", target="_blank", class="non-tab"),
                                   " to improve data structure and improve efficiency of data access."
                                   ), 
                              class = "chglog"
                       ),
                       tags$h4("2025-12-18"),
                       tags$ul(
                         tags$li(tags$strong("DATA UPDATE to: Sep 30, 2025"), "from latest",
                          tags$a(href="https://www.bcldb.com/files/Liquor_Market_Review_F25_26_Q2_September_2025.pdf", 
                         "Liquor Market Review", target="_blank", class="non-tab"), class = "about"),
                         tags$li("Added filters for BC produced beer by category (major, regional, micro)."),
                         tags$li("Corrected calculations for % of total stacked charts to maintain accuracy when filters applied."),
                         tags$li("Improved error handling with user-friendly messaging."),
                         tags$li("Various usability enhancements like better vertical spacing for faceted charts, 
                         consistent color-coding."), class = "chglog"
                       ),
                       tags$h4("2025-10-28"),
                       tags$ul(
                         tags$li("Overview: Added breakdowns for BC produced beer by category (major, regional, micro)."),
                         tags$li("Improved info available when hovering over charts, including formatting for readability."),
                            class = "chglog"),
                       tags$h4("2025-10-14"),
                       tags$p("Overview: Added 'Annual' or 'Quarterly' time grain setting. 
                       Updated all charts to respond to this filter and show data by year or quarter accordingly for 
                       broader range of insights from the same page.", class="chglog"),
                       tags$p("Overview: Built out charts for source/origin of beer 
                       (BC, Import, Other Canada).", class="chglog"),
                       tags$h4("2025-09-25"),
                       tags$p("New OVERVIEW tab added, with key summary charts comparing Net $ Sales and Litre Sales.
                              This provides a high-level snapshot of overall trends, 
                              without the need to dig through the more detailed tabs.",
                              class="chglog"),
                       tags$h4("2025-09-15"),
                       tags$p("Converted original high-level bar charts to line charts for better trend visualization, 
                              especially for YoY and QoQ comparisons.",
                              class="chglog"),
                       tags$p("Added grey lines and bars to highlight comparisons based on partial years, 
                              to help with comparisons between full years and partial years, to reduce potential confusnion.",
                              class="chglog"),
                       tags$h4("2025-09-06"),
                       tags$p("DATA UPDATE: Updated with latest ",
                        tags$a(href="https://www.bcldb.com/files/Liquor_Market_Review_F25_26_Q1_June_2025.pdf",
                        "Liquor Market Review data", target="_blank", class="non-tab")," for quarter ended ",
                        tags$strong("Jun 30, 2025"),
                        class="chglog"),
                       tags$h4("2025-06-05"),
                       tags$p("Fixed the way filters work relative to % of total charts, to ensure that the charts show 
                              % of total among the filtered data, rather than the total of all data.",
                              class="chglog"),
                       tags$h4("2025-06-02"),
                       tags$p("DATA UPDATE: Updated with latest ",
                        tags$a(href="https://www.bcldb.com/files/Liquor_Market_Review_F24_25_Q4_March_2025.pdf",
                        "Liquor Market Review data", target="_blank", class="non-tab")," for quarter ended ",
                        tags$strong("Mar 31, 2025"),
                        class="chglog"),
                       tags$h4("2025-03"),
                       tags$p("Added new tab for Litre Sales, with same visualizations as Net $ Sales tab, 
                                 but using litre as the unit of measure. 
                                 This provides  more complete view of beer sales in BC, 
                                 aligning with the reports in the Liquor Market Review.",
                              class="chglog"),
                     ), # end fluidRow 2
              ) # end tabPanel 3
            ), # end tabsetPanel ----
          # FOOTER ----
          tags$div(
            style = "margin-top: 20px; padding: 10px; border-top: 2px solid #ddd; text-align: center;",
            "© 2025 A",
            tags$a(href="https://www.fig4.com", target="_blank", 'Figure 4'),
            " Production, John Yuill; see 'About' tab for details",
            class="footer")
        ) # end div.main (contains tabsetPanel + footer) ----
    ) # end layout_sidebar ----
) # end shinyUI
