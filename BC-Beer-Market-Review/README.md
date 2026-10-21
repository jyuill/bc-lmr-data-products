# BC Beer Market Review

This project is a review of the beer market in British Columbia, Canada. The data used is from the BC Liquor Market Review produced quarterly by the BC Liquor Distribution Board, the government agency responsible for liquor control within the province. The dataset contains the sales data of BC Liquor Stores, in net Canadian dollars and litre volume, starting in 2015.

- Shiny dashboard focused on beer only.
- Embedded in [bcbeer.ca](https://www.bcbeer.ca)
- Data from postgres database
  - uses **beer_data.rds** as data source for speed - should be automatically updated quarterly when LMR database is updated (global.R).
  - may need to refresh locally and push to repo to ensure latest data available on public server (happened with June 30, 2026 update).
- **lmrtools** package used for various data functions.
- **global.R** file used to set up data and provide support functions.