# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a data analytics project for the BC Liquor Market Review, containing quarterly liquor sales data from BC Liquor Distribution Board. The project consists of multiple R Shiny applications that visualize liquor market data through interactive dashboards.

## Architecture

### Core Components

**Main Products:**
- `BC-Liquor-Market-Review/` - Full liquor market dashboard with all categories (beer, wine, spirits, refreshment beverages)
- `BC-Beer-Market-Review/` - Specialized beer market analysis dashboard
- `BC-LMR-LLM/` - AI/LLM experimental component

**Data Flow:**
- Data source: BC Liquor Distribution Board quarterly PDF reports
- Storage: AWS RDS PostgreSQL database (previously MySQL)
- Connection: Via `query_pg.R` files in each app directory
- Credentials: Stored in `config.yml` (not in git, deployed with apps)

### R Shiny App Structure

Each Shiny app follows a consistent structure:
- `server.R` - Main server logic with reactive data processing
- `ui.R` - User interface definition
- `functions.R` / `functions_data.R` / `functions_plots.R` - Modular functions
- `query_pg.R` - Database connection and queries
- `support_vars.R` - Supporting variables and configurations
- `rsconnect/` - Deployment configuration for shinyapps.io

### Key Functions Architecture

**Data Functions (in functions_data.R):**
- `AnnualCatTypeData()` - Annual category type summaries with YoY changes
- `AnnualCatData()` - Annual category breakdowns
- `QuarterCatData()` - Quarterly data processing

**Plot Functions (in functions_plots.R):**
- `TtlChart()` - Bar charts for category totals (annual/quarterly)
- `CatChart()` - Category breakdown charts with % of total options
- `PoPChart()` - Period-over-period change visualization
- Uses plotly for interactivity, consistent theming throughout

**Dynamic UI Patterns:**
- Sidebar content changes based on selected tab
- Filters are dynamically generated from data unique values
- Charts only render when all required filters are selected

## Development Commands

### R Environment
```r
# Install required packages (see individual app directories for specific dependencies)
install.packages(c("shiny", "shinyjs", "tidyverse", "lubridate", "scales", "plotly", "bslib", "RColorBrewer"))

# Run Shiny apps locally
shiny::runApp("BC-Liquor-Market-Review")
shiny::runApp("BC-Beer-Market-Review")
```

### Python Environment
```bash
# Activate virtual environment
source .venv/bin/activate

# Install dependencies
pip install -r requirements.txt

# Run Jupyter notebooks
jupyter lab
```

### Deployment
```r
# Deploy to shinyapps.io
source("BC-Liquor-Market-Review/rsconnect/shinyapps.io/fig4/deploy-to-shiny.R")
source("BC-Beer-Market-Review/rsconnect/shinyapps.io/fig4/deploy-to-shiny.R")
```

## Database Connection

- Current: PostgreSQL on AWS RDS
- Legacy: MySQL (queryz_mysql.R files still present)
- Connection handled via config.yml with separate configurations for local development and production deployment
- Database credentials are environment-specific and deployed with the apps

## Data Processing Patterns

### Filtering and Aggregation
- Data filtered to recent years (typically last 6 years) for performance
- Year-over-year calculations built into data functions
- Percentage of total calculations at category and subcategory levels
- Partial year flagging for current year data visualization

### Consistent Styling
- Uses plotly for interactive charts
- RColorBrewer palettes for consistent coloring
- Custom themes defined in support_vars.R
- Responsive design with bslib theming

## File Organization

- Root level contains project-wide configuration and documentation
- Each product in its own directory with complete Shiny app structure
- Shared data files in `/data` directory
- Virtual environment for Python components in `.venv`
- Deployment scripts co-located with each app

## Development Notes

- Apps connect directly to production database - data updates are automatic
- Code changes require redeployment via rsconnect deployment scripts
- Database structure includes calendar year (cyr) and quarter (cqtr) fields
- Category hierarchies: cat_type (beer/wine/spirits) > category > subcategory
- Sales data in Canadian dollars (netsales) and volume in litres