# Variables that are used for Plots or other support purposes
# - separate file to make easier to reference while working on code
# - minimizes clutter in server.R or other code files
# - sourced at top of server.R

library(RColorBrewer)

### COLORS etc ----
# set plot theme
# - under 'plots' below
# set color palette
# colors needed:
# 1. bar and line colors
# 2. beer category colors: BC, Other Prov, Import
# 3. quarters: 4
# 4. bc categories: 3
# 5. countries: 5
# base palette
#RColorBrewer::display.brewer.all()
base_pal <- "YlOrBr"
# - bar and line colors
bar_col <- brewer.pal(n=3, name=base_pal)[2] 
# for labels on facet
strp_col <- brewer.pal(n=9, name='Greys')[6]
strp_col <- brewer.pal(n=9, name=base_pal)[5]
# - palette for quarters
#qpal <- brewer.pal(n=9, name="YlGnBu")
qpal <- brewer.pal(n=9, name="YlOrRd")
#qtr_color <- c("Q1"=qpal[3], "Q2"=qpal[4], "Q3"=qpal[5], "Q4"=qpal[6])
qtr_color <- brewer.pal(n=5, name=base_pal)[2:5]
# for full vs partial year in annual charts
yr_flag_color <- c("full"=bar_col, "partial"='grey80')
# palette for beer categories
beer_pal <- brewer.pal(n=9, name="YlOrRd")
#beer_cat_color <- c("BC"=beer_pal[9], "Other Prov"=beer_pal[7], "Import"=beer_pal[5])
beer_cat_color <- brewer.pal(n=4, name=base_pal)[2:4]
beer_bc_cat_color <- c("BC Major"=beer_pal[9], "BC Regional"=beer_pal[7], "BC Micro"=beer_pal[5])
#beer_bc_cat_color <- beer_cat_color
beer_imp_color2 <- beer_pal[5:9]
beer_imp_color <- brewer.pal(n=6, name=base_pal)[2:6]
# linewidth for line charts
lwidth <- 1
# point size for line charts
lpointsize <- 2.2

# PLOT THEMES--------------------------------------------------------------------
  ## ggplot themes ----
  theme_set(theme_light()+theme(panel.grid.minor = element_blank(),
                                panel.grid.major = element_line(color = 'grey90', 
                                linewidth=0.1)))
  # x-axis text - set angle and other formats
  theme_xax <- theme(axis.ticks.x = element_blank(),
                   axis.text.x = element_text(angle = 90, hjust = 1))
  theme_xaxq <- theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
  # no legend
  theme_nleg <- theme(legend.position = "none")
  # facet chart spacing 
  theme_facet <- theme(panel.spacing.y = unit(0.1,"lines"))
  # customize tooltip format -> DO NOT APPEAR TO BE USED ANYWHERE
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


## PLOT TITLES ----
  # consistent titles to apply across same charts
  # separate metric (Net $, Litres) from title info
  # - metric added in plot function call
  # yr & qtr
  yr_sales <- "Sales by Yr (grey = partial yr)"
  qtr_sales <- "Sales by Quarter"
  pop_chg_sales <- "% Chg - by Year (grey = partial yr)"
  pop_chg_sales_qtr <- " % Chg - by Quarter"
  # source
  yr_source <- "Sales by Source"
  yr_source_pc <- "% of Total by Source"
  yr_source_pc_chg <- "Yrly % Chg by Src (grey = partial yr)"
  qtr_source_pc_chg <- "% Chg by Src - vs Same Qtr Prev Yr"
  yr_source_pcpt_chg <- "% Pt Chg in Share by Src"
  # BC category
  yr_sales_cat <-  "Sales by BC Producer Category"
  yr_sales_cat_pc <-  "% of Total BC Prod. by Category"
  yr_sales_cat_pc_chg <- "Yrly % Chg by BC Cat. (grey = partial yr)"
  qtr_sales_cat_pc_chg <- "% Chg by BC Cat. - vs Same Qtr Prev Yr"
  yr_sales_cat_pcpt_chg <-  "% Pt Chg in Share by BC Cat."
  # Import category
  yr_sales_imp <-  "Import Sales - Ctry/Region"
  yr_sales_imp_pc <-  "% of Total Imports by Ctry/Region"
  yr_sales_imp_pc_chg <- "Yrly % Chg by Ctry/Reg (grey=partial yr)"
  yr_sales_imp_pcpt_chg <-  "% Pt Chg in Share by Ctry/Region"

  # LITRE sales chart titles - not all these are used because multi-purpose versions above work with metric
  yr_litre_cat <- "Yrly Litres by Category"
  yr_litre_pc_cat <- "% of Ttl Litres by Category"
  yr_litre_pc_chg_cat <- "Yrly % Chg Litres by Category"
  yr_litre_pcpt_chg_cat <- "Yrly % Pt Chg Litres % of Ttl"

## Sidebar notes ----
  sb_note_calyr <- "Years & Quarters refer to calendar year, not LDB fiscal year."
  sb_note_src <- "Source: BC Liquor Distribution Branch - LDB Market Reports; sales through wholesale data from all distribution channels; 
                  see source for additional definitions. Details and links in 'About' tab."
  sb_note_charts <- "All charts are interactive: hover for details; zoom in using box select (click & drag); reset view with home icon, etc."
  sb_note_sales <- "Net $ Sales is based on price paid by (wholesale) customer, excluding taxes."