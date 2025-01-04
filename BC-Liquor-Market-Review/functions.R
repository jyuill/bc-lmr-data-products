# Functions for patterns that recur over the categories

# Summary data ----
# -- use for: annual data by category type (beer, refresh bev, spirits, wine)
# -- - includes year-over-year changes in sales and litres
AnnualData <- function(dataset) {
  dataset <- dataset %>% group_by(cat_type, cyr) %>% 
    summarize(netsales = sum(netsales),
              litres = sum(litres)) %>% ungroup() %>%
    mutate(yoy_sales = (netsales - lag(netsales))/lag(netsales),
           yoy_litres = (litres - lag(litres))/lag(litres))
  return(dataset)
}
# annual category data
# use for: annual data by category type and category
AnnualCatData <- function(dataset, n_cats) {
  # number of categories selected use to calculate lag for yoy calcs
  n_lag <- n_cats
  dataset <- dataset %>% group_by(cat_type, cyr, category) %>% 
    summarize(netsales = sum(netsales),
              litres = sum(litres)) %>% ungroup() %>%
    mutate(yoy_sales = (netsales - lag(netsales, n=n_lag))/lag(netsales, n=n_lag),
           yoy_litres = (litres - lag(litres, n=n_lag))/lag(litres, n=n_lag),
           cat_type = reorder(cat_type, netsales, FUN = sum)
    )
  return(dataset)
}

# Qtr smry data
QtrData <- function(dataset, n_qtr) {
  # takes n_qtr from number of quarters selected in input selector for calc yoy lag
  dataset <- dataset %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt) %>%
    summarize(netsales = sum(netsales),
              litres = sum(litres)) %>% ungroup() %>%
    mutate(qoq_sales = (netsales - lag(netsales))/lag(netsales),
           qoq_litres = (litres - lag(litres))/lag(litres),
           yoy_qoq_sales = (netsales - lag(netsales, n=n_qtr))/lag(netsales, n=n_qtr),
           # for same qtr prev yr comparisons
           yoy_qoq_litres = (litres - lag(litres, n=n_qtr))/lag(litres, n=n_qtr),
           yr_qtr = paste(cyr, cqtr, sep = "-")
    )
  return(dataset)
}
# Qtr category summary data
QtrCatData <- function(dataset, n_cats, n_qtr) {
  # takes data, number of categories from iput selector, number of quarters from input selector
  # number of quarters used to calculate yoy_qoq_sales, yoy_qoq_litres
  n_lag <- n_cats
  dataset <- dataset %>% group_by(cat_type, cyr, cqtr, cyr_qtr, end_qtr_dt, category) %>%
    summarize(netsales = sum(netsales),
              litres = sum(litres)) %>% ungroup() %>%
    mutate(qoq_sales = (netsales - lag(netsales, n=n_lag))/lag(netsales, n=n_lag),
           qoq_litres = (litres - lag(litres, n=n_lag))/lag(litres, n_lag),
           yoy_qoq_sales = (netsales - lag(netsales, n=n_lag*n_qtr))/lag(netsales, n=n_lag*n_qtr),
           yoy_qoq_litres = (litres - lag(litres, n=n_lag*n_qtr))/lag(litres, n=n_lag*n_qtr),
           yr_qtr = paste(cyr, cqtr, sep = "-")
    )
  return(dataset)
}

# Plot Sales for Category ----
TtlChart <- function(chart_title, dataset, x_var, y_var, fill_var, fill_color, theme_list, tunits) {
  x <- dataset
  x <- x %>% tooltip_fmt(dim = x_var, units = tunits, y_var = y_var)
  ch_title <- chart_title
  p <- x %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var), text = tooltip_text)) +
    geom_col() +
    scale_y_continuous(labels = label_currency(scale = 1e-6, suffix = "M", accuracy = 1),
                       expand = expansion(mult=c(0,0.05))) +
    scale_fill_manual(values=fill_color) +
    labs(title=ch_title, x="", y="") +
    theme_list
  return(ggplotly(p, tooltip = "text"))
}

# plot for period-over-period change in sales
# - accommodates fill colors based on variable; use with single overall dimension if no breakdown
PoPChart <- function(chart_title, dataset, x_var, y_var, fill_var, fill_color, theme_list, tunits) {
  x <- dataset
  x <- x %>% tooltip_fmt(dim = x_var, units = tunits, y_var = y_var)
  max_y <- max(x[[y_var]], na.rm = TRUE)
  min_y <- min(x[[y_var]], na.rm = TRUE)
  max_val <- max(abs(min_y), abs(max_y))
  ch_title <- chart_title
  p <- x %>% 
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var), text = tooltip_text)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                       expand = expansion(mult=c(0,0.05)),
                       limits = c(0 - max_val, max_val)) +
    # set fill colors based on variable colors or overall dimension
    scale_fill_manual(values=fill_color)+
    labs(title=ch_title, x="", y="")+
    theme_list
  return(ggplotly(p, tooltip = "text"))
}

# Category charts ----
# plot for category metrics
# uses 'pos' variable so that can be used for unit or % stack charts (pos = 'stack' or 'dodge')
# - includes programattic setting of label scales based on units provided
# - use going fwd; ideally, replace CatChart with this version (beer data)
CatChart <- function(chart_title, dataset, x_var, y_var, fill_var, fill_color, 
                     pos, theme_list, tunits) {
  x <- dataset
  x <- x %>% tooltip_fmt(dim = fill_var, units = tunits, y_var = y_var) %>% mutate(
    category = fct_reorder(!!sym(fill_var), !!sym(y_var), .fun = sum)
  )
  ch_title <- chart_title
  # set scales based on units - function below
  label_set <- label_fmt(tunits)
  
  p <- x %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = category, text = tooltip_text)) +
    geom_col(position = pos) +
    scale_y_continuous(labels = label_set,
                       expand = expansion(mult=c(0,0.05))) +
    scale_fill_manual(values=fill_color)+
    labs(title=ch_title, x="", y="")+
    theme(axis.ticks.x = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.margin = unit(c(1, 0, 0, 0), "cm"))+
    theme_list
  p_plotly <- ggplotly(p, tooltip = "text")
  # Customize legend in plotly
  p_plotly <- p_plotly %>% layout(
    legend = list(
      orientation = "h",     # Horizontal legend
      x = 0.5,               # Center legend horizontally
      xanchor = "center",    # Align legend center with x position
      y = 1,                 # Place legend at the top
      yanchor = "bottom",    # Align legend bottom with y position
      title = list(text = "")  # Remove legend title
    )
  )
  return(p_plotly)
}

# facet charts for change ----
CatChgChart <- function (chart_title, dataset, x_var, y_var, fill_var, fill_color, strp_color,
                         theme_list) {
  x <- dataset
  max_y <- max(x[[y_var]], na.rm = TRUE)
  min_y <- min(x[[y_var]], na.rm = TRUE)
  max_val <- max(abs(min_y), abs(max_y))
  x <- x %>% tooltip_fmt(dim = x_var, units = '%', y_var = y_var) %>% mutate(
    category = fct_reorder(category, !!sym(y_var), .fun = sum)
    )
  ch_title <- chart_title
  p <- x %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var), text = tooltip_text)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    facet_grid(category~.) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                       expand = expansion(mult=c(0,0.05)),
                       limits = c(0 - max_val, max_val)) +
    scale_fill_manual(values=fill_color) +
    theme(strip.background = element_rect(fill = strp_color)) +
    theme(strip.text=element_text(color='white'))+
    labs(title=ch_title, x="", y="")+
    theme_list
  ggplotly(p, tooltip = "text")
}

# label & tooltip formatting ----
# designed for one dimension/label and one metric
tooltip_fmt <- function(data, dim, units, y_var) {
      if (units == "B") {
        data <- data %>%
          mutate(tooltip_text = paste0(!!sym(dim), ": ", 
                                       label_currency(scale = 1e-9, 
                                                      suffix = "B", accuracy = 0.1)
                                       (!!sym(y_var))))
      } else if (units == "M") {
        data <- data %>%
          mutate(tooltip_text = paste0(!!sym(dim), ": ", 
                                       label_currency(scale = 1e-6, 
                                                      suffix = "M", accuracy = 0.1)
                                       (!!sym(y_var))))
      } else if (units == "%") {
        data <- data %>%
          mutate(tooltip_text = paste0(!!sym(dim), ": ", 
                                       scales::percent_format(accuracy = 0.1)
                                       (!!sym(y_var))))
       } else {
         data <- data %>%
           mutate(tooltip_text = paste0(!!sym(dim), ": ", !!sym(y_var)))
       }
     
    return(data)
}

#tt <- tooltip_fmt("cyr", "B", "netsales")
#print(tt)

label_fmt <- function(tunits) {
  if (tunits == "B") {
    label_set <- scales::label_currency(scale = 1e-9, suffix = "B")
  } else if (tunits == "M") {
    label_set <- scales::label_currency(scale = 1e-6, suffix = "M")
  } else if (tunits == "%") {
    label_set <- scales::label_percent(accuracy = 1)
  } else {
    label_set <- scales::label_currency(scale = 1, suffix = "")
  }
  return(label_set)
}
