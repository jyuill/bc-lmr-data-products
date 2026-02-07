# BC Liquor Market Review project

## Introduction

This project is a review of the liquor market in British Columbia, Canada. The data used is from the BC Liquor Market Review produced quarterly by the BC Liquor Distribution Board, the government agency responsible for liquor control within the province. The dataset contains the sales data of BC Liquor Stores, in net Canadian dollars and litre volume, starting in 2015.

### Data source & Updates

-   source data is from the BC Liquor Distribution Branch collection of pdf reports: [BC Liquor Market Review reports](https://www.bcldb.com/publications/liquor-market-review)
-   data is extracted from pdf tables, cleaned, compiled and stored in a database on AWS RDS. Currently PostgreSQL database, originally MySQL.
-   database contains complete data going back to 2015, while quarterly reports from LDB are in pdf format with only the most recent 5 quarters reported.
-   **bc-lmr-update-explore** is a separate project (github repo) with code for data extraction from PDF report. When new report is published, this is the project used to extract data from pdf and update database.

## Publishing

As of Sep 2025, products are R shiny apps hosted on [shinyapps.io](https://www.shinyapps.io/).

For basic data updates, nothing needed as the apps connect directly to the database on AWS RDS.

As of Jan 2026, BC Beer shiny app was moved to [**Posit Connect Cloud** ](https://connect.posit.cloud/fig4)for easier management (push to Github), more robust and sustainable platform.

This should be done for any other shiny apps or quarto docs, as convenient.

Mainly involves:

1.  console: `rsconnect::writeManifest(appDir="<app folder>")`
2.  Posit Connect Cloud:
    1.  walk through Publish steps
    2.  Add any environment variables needed, such as for db credentials

### Deploying changes to code

**Posit Connect Cloud**

-   deploys **directly from Github**: push changes, they will be reflected
-   IF update to **lmrtools package**, may need to run:
    -   `rsconnect::writeManifest(appDir="BC-Beer-Market-Review")`
    -   if probs, reinstall: `devtools::install_github('jyuill/lmrtools')`

**shinyapps.io: rsconnect**

-   each app has an **/rsconnect** folder with account info for deployment
-   run the **deploy-to-shiny.R** file in each product folder to deploy the app

## Products

There are several component products in this project:

## Project 1: BC Liquor Market Review - Shiny Dashboard

This is a Shiny Dashboard that provides an interactive visualization of the BC Liquor Market Review data.

### General Setup

-   organized by overview at highest level 'category type' (beer, wine, etc), followed by tabs with deeper dives into each category, with subcategories
-   net sales \$ data and litre volume data starting at high level and drilling down to categories
-   annual and quarterly data
-   quantity and period-over-period change %
-   heavy use of bar charts for representing period data, including stacked where relevant
-   annual and quarter charts are shown side by side
-   charts built using plotly for interactivity
-   filters in sidebar allow for:
    -   selection of category type
    -   selection of category
    -   selection of period (year and/or quarter)
-   filters are managed by dynamic filter section in server.R

### Process

1.  Data is imported from the database I have established, as mentioned above.
2.  Datasets are created for each major category type (beer, wine, spirits, etc), including renaming of categories and subcategories for brevity.
3.  For each category type, a dashboard tab is created with the following:
    -   bar charts of net sales \$ for the category type by year and quarter
    -   bar chart showing period-over-period change % for the category type by year and quarter
    -   additional category breakdowns, depending on the category type
4.  Reactive objects are used to apply changes based on user input from filters, including
    -   annual data and quarter data in separate datasets
5.  Consistent formatting for charts is applied using themes and other settings set in 'PLOTS' section server.R.
    -   including facet plots for subcategory breakdowns
6.  Content of each tab is controlled by:
    -   the dataset created for each category type
    -   the reactive objects created for each category type derived from the dataset
    -   the dynamic sidebar, with text and filters, created in the server.R file
        -   any text in sidebar is set in dynamic sidebar section in server.R for each tab
        -   sidebar content is dynamic based tabset selected by the user
        -   filters are created based on the unique values in the dataset for each category type
        -   main panel content is dynamic based on the filters selected by the user - including checks for existence of ALL filters before displaying charts (check sections in server.R for set up filters, dynamic sidebar, start of each category tab)
    -   the key functions used to create the charts

### Key functions

#### BC Beer

[**lmrtools package**](https://github.com/jyuill/lmrtools) is heavily used to import and manage data.

Should also be used in future development of all categories, and continue to evolve.

#### BC LMR general

Functions are used as much as possible, since there are repetitive processes for each category type. Note that many of these functions were developed starting with the Refreshment Beverage category, based on what worked for Beer category.

-   AnnualCatData() - creates a dataset for a category type with annual data
-   QuarterCatData() - creates a dataset for a category type with quarterly data
-   TtlChart() - creates a bar chart for a category type, with variations for annual and quarter data
-   CatChart() - creates a bar chart for categories within category type, with variations for annual and quarter data
    -   can be used for raw numbers or % of total
    -   with % of total, set pos parameter to 'stack' for actual % of ttl that doesn't change with category filters OR use 'fill' if you want % of items shown to always add up to 100% (potentially misleading)
-   PoPChart() - creates a bar chart for period-over-period change % for categories within category type, with variations for annual and quarter data