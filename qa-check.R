
library(tidyverse)
library(lubridate)
library(scales)

# get data
lmr_check <- lmr_data

# Mexico beer
# surprising drop after 2020
mexico_beer <- lmr_check %>%
  filter(cat_type == "Beer", str_detect(subcategory, "Mexico"), fyr >= 2018) %>%
  group_by(fy_qtr, subcategory) %>%
  summarise(total_sales = sum(netsales, na.rm = TRUE),
            total_litres = sum(litres)) %>%
  ungroup()
# create a bar plot of mexico beer sales over time by fy_qtr
mexico_beer %>%
  ggplot(aes(x = fy_qtr, y = total_sales, fill = subcategory)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Mexico Beer Sales Over Time",
       x = "Fiscal Year Quarter",
       y = "Total Sales (in $)",
       color = "Subcategory") +
  theme_minimal()+
  #rotate x-axis labels 90 degrees
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# create the same bar chart but for litre sales of mexico beer
mexico_beer_litres <- lmr_check %>%
  filter(cat_type == "Beer", str_detect(subcategory, "Mexico"), fyr >= 2018) %>%
  group_by(fy_qtr, subcategory) %>%
  summarise(total_litres = sum(litres, na.rm = TRUE)) %>%
  ungroup()
# create a bar plot of mexico beer litres over time by fy_qtr
mexico_beer_litres %>%
  ggplot(aes(x = fy_qtr, y = total_litres)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Mexico Beer Litres Over Time",
       x = "Fiscal Year Quarter",
       y = "Total Litres",
       color = "Subcategory") +
  theme_minimal()+
  #rotate x-axis labels 90 degrees
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
