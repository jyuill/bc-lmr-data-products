# Functions for patterns that recur over the categories
# SPECIFICALLY FOR PLOTTING

# Plot Sales for Category ----
TtlChart <- function(metric="", chart_title, dataset, x_var, y_var, fill_var, fill_color, 
                    theme_list, tunits, 
                    partial_yr_color = 'grey50', lwidth = 0.5, lpointsize = 1) {
  x <- dataset
  ch_title <- paste(metric, chart_title)
  x <- x %>% tooltip_fmt(dim = x_var, units = tunits, y_var = y_var)
  y_labels <- y_label_format(y_var, tunits)
  x_partial <- x %>% filter(yr_flag_line == "partial")
  p <- x %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), text = tooltip_text, group = 1)) +
        # line chart with overlay to indicate partial yr (converted from bar chart Sep 2025)
        geom_line(linewidth = lwidth, color = fill_color) +
        geom_line(data = x_partial, aes(color = yr_flag_line), linewidth = lwidth) +
        geom_point(aes(color = yr_flag), size = lpointsize) +
        # use colors from pallette for full/partial yr 
        scale_color_manual(values = partial_yr_color) +
        scale_y_continuous(labels = y_labels,
                            expand = expansion(mult=c(0,0.05)),
                            limits = c(0, max(x[[y_var]], na.rm = TRUE))) +
        labs(title=ch_title, x="", y="") +
        theme_list
  return(ggplotly(p, tooltip = "text"))
}

# quarter total chart
# chg to line chart from bar chart Sep 2025; geom_points colored to match qtr color
QtrChart <- function(metric="",chart_title, dataset, x_var, y_var, fill_var, fill_color, 
                     theme_list, tunits, lwidth = 0.5, lpointsize = 1) {
  x <- dataset
  x <- x %>% tooltip_fmt(dim = x_var, units = tunits, y_var = y_var)
  # set scale labels based on variable and units (currency v commas)
  y_labels <- y_label_format(y_var, tunits)
  ch_title <- paste(metric, chart_title)
  p <- x %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(fill_var), 
            text = tooltip_text, group = 1)) +
            geom_line(linewidth = lwidth, color = '#FEC44F') + #hard-coded to match bar_color
            geom_point(size = lpointsize, aes(color = !!sym(fill_var))) +
            scale_y_continuous(labels = y_labels,
                              expand = expansion(mult=c(0,0.05)),
                              limits = c(0, max(x[[y_var]], na.rm = TRUE))) +
            scale_color_manual(values=fill_color)+
            labs(title=ch_title, x="", y="")+
            theme_list
  return(ggplotly(p, tooltip = "text"))
}

# plot for period-over-period change in sales
# - accommodates fill colors based on variable; use with single overall dimension if no breakdown
PoPChart <- function(metric = "", chart_title, dataset, x_var, y_var, fill_var, fill_color, 
                     theme_list, tunits) {
  ch_title <- paste(metric, chart_title)
  x <- dataset
  x <- x %>% tooltip_fmt(dim = x_var, units = tunits, y_var = y_var)
  max_y <- max(x[[y_var]], na.rm = TRUE)
  min_y <- min(x[[y_var]], na.rm = TRUE)
  max_val <- max(abs(min_y), abs(max_y))
  
  p <- x %>% 
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var), 
               text = tooltip_text)) +
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
# - includes programattic setting of label scales based on units provided (tunits)
# - use going fwd; ideally, replace CatChart with this version (beer data)
CatChart <- function(metric = "",chart_title, dataset, x_var, y_var, fill_var, 
                    fill_color, pos, theme_list, tunits) {
  ch_title <- paste(metric, chart_title)
  x <- dataset
  x <- x %>% tooltip_fmt(dim = fill_var, units = tunits, y_var = y_var) %>% mutate(
    category = fct_reorder(!!sym(fill_var), !!sym(y_var), .fun = sum)
  )
  # set scale labels based on variable and units (currency v commas)
  y_labels <- y_label_format(y_var, tunits)
  
  p <- x %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = category, text = tooltip_text)) +
    geom_col(position = pos) +
    scale_y_continuous(labels = y_labels,
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
CatChgChart <- function (metric, chart_title, dataset, x_var, y_var, sort_var = "netsales", fill_var, facet_var, 
                         fill_color, strp_color, theme_list, tunits="%") {
  ch_title <- paste(metric, chart_title)
  x <- dataset
  max_y <- max(x[[y_var]], na.rm = TRUE)
  min_y <- min(x[[y_var]], na.rm = TRUE)
  max_val <- max(abs(min_y), abs(max_y))
  x <- x %>% tooltip_fmt(dim = x_var, units = tunits, y_var = y_var) %>% mutate(
    # sort categories by y_variable -> may not always want this
    !!sym(facet_var) := fct_reorder(!!sym(facet_var), !!sym(sort_var), .fun = sum)
    )
  
  # set scales based on units - function below
  # applied in scale_y_continuous
  label_set <- label_fmt(tunits)
  
  p <- x %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(fill_var), 
               text = tooltip_text)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    facet_grid(as.formula(paste(facet_var, "~ ."))) +
    scale_y_continuous(labels = label_set,
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
                                       scales::percent_format(accuracy = 1)
                                       (!!sym(y_var))))
       } else if (units == "dol") {
         data <- data %>%
           mutate(tooltip_text = paste0(!!sym(dim), ": ", 
                                       label_currency(scale = 1, 
                                                      suffix = "", accuracy = 1)
                                       (!!sym(y_var))))
        } else if (units == "num") {
          data <- data %>%
            mutate(tooltip_text = paste0(!!sym(dim), ": ", 
                                       scales::number_format(scale = 1, 
                                                             suffix = "", accuracy = 0.1)
                                       (!!sym(y_var))))
        } else {
         data <- data %>%
           mutate(tooltip_text = paste0(!!sym(dim), ": ", !!sym(y_var)))
       }
    return(data)
}

#tt <- tooltip_fmt("cyr", "B", "netsales")
#print(tt)

y_label_format <- function(yvar, tunits) {
  if(tunits == 'M') {
    scale_val <- 1e-6
  } else if (tunits == 'B') {
    scale_val <- 1e-9
  } else if (tunits == 'K') {
    scale_val <- 1e-3
  } else {
    scale_val <- 1
  }
  print(scale_val)
  y_labels <- switch(yvar,
                     'netsales' = label_currency(scale = scale_val, suffix = tunits, accuracy = 1),
                     'litres' = label_comma(scale = scale_val, suffix = tunits, accuracy = 1),
                     'pct_ttl_sales' = label_percent(accuracy = 1),
                     'pct_ttl_litres' = label_percent(accuracy = 1),
                     )
  
  return(y_labels)
}

# deprecated: less sophisticated label function used in places
label_fmt <- function(tunits) {
  if (tunits == "B") {
    label_set <- scales::label_currency(scale = 1e-9, suffix = "B")
  } else if (tunits == "M") {
    label_set <- scales::label_currency(scale = 1e-6, suffix = "M")
  } else if (tunits == "%") {
    label_set <- scales::label_percent(accuracy = 1)
  } else if (tunits == "dol") {
    label_set <- scales::label_currency(scale = 1, suffix = "")
  } else if (tunits == "num") {
    label_set <- scales::label_number(scale = 1, suffix = "")
  } else {
    label_set <- scales::label_number(scale = 1, suffix = "")
  }
  return(label_set)
}
