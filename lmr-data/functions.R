# Functions for patterns that recur over the categories

## Plot metrics by year ----
AnnualChart <- function(chart_title, dataset, x_var, y_var, fill_color, theme_list) {
  # get filtered, aggregated data -> usually reactive object
  x <- dataset
  ch_title <- chart_title
  p <- x %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_col(fill=fill_color) +
    scale_y_continuous(labels = label_currency(scale = 1e-6, suffix = "M", accuracy = 1),
                       expand = expansion(mult=c(0,0.05))) +
    labs(title=ch_title, x="", y="") +
    theme_list + tooltip_fmt(x_var, "M", y_var)
  return(ggplotly(p, tooltip = "text"))
}

# plot for year-over-year change in metrics
YoYChart <- function(chart_title, dataset, x_var, fill_color, theme_list) {
  x <- dataset
  max_y <- max(x$yoy, na.rm = TRUE)
  min_y <- min(x$yoy, na.rm = TRUE)
  max_val <- max(abs(min_y), abs(max_y))
  ch_title <- chart_title
  p <- x %>% 
    ggplot(aes(x = !!sym(x_var), y = yoy)) +
    geom_col(fill=fill_color) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult=c(0,0.05)),
                       limits = c(0 - max_val, max_val)) +
    labs(title=ch_title, x="", y="")+
    theme_list + tooltip_fmt(x_var, "%", "yoy")
  return(ggplotly(p, tooltip = "text"))
}

# Plot Sales by Quarter ----
QtrChart <- function(chart_title, dataset, x_var, y_var, fill_color, theme_list) {
  x <- dataset
  ch_title <- chart_title
  p <- x %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = cqtr)) +
    geom_col() +
    scale_y_continuous(labels = label_currency(scale = 1e-6, suffix = "M", accuracy = 1),
                       expand = expansion(mult=c(0,0.05))) +
    scale_fill_manual(values=fill_color) +
    labs(title=ch_title, x="", y="") +
    theme_list + tooltip_fmt(x_var, "M", y_var)
  return(ggplotly(p, tooltip = "text"))
}

# plot for quarter-over-quarter change in sales
QoQChart <- function(chart_title, dataset, x_var, y_var, dim, fill_color, theme_list) {
  x <- dataset
  max_y <- max(x[[y_var]], na.rm = TRUE)
  min_y <- min(x[[y_var]], na.rm = TRUE)
  max_val <- max(abs(min_y), abs(max_y))
  ch_title <- chart_title
  p <- x %>% 
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(dim))) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    scale_y_continuous(labels = scales::label_percent(accuracy = 0.1),
                       expand = expansion(mult=c(0,0.05)),
                       limits = c(0 - max_val, max_val)) +
    scale_fill_manual(values=fill_color)+
    labs(title=ch_title, x="", y="")+
    theme_list + tooltip_fmt(x_var, "%", "qoq")
  return(ggplotly(p, tooltip = "text"))
}

PoPChart <- function(chart_title, dataset, x_var, y_var, dim, fill_color, theme_list) {
  x <- dataset
  max_y <- max(x[[y_var]], na.rm = TRUE)
  min_y <- min(x[[y_var]], na.rm = TRUE)
  max_val <- max(abs(min_y), abs(max_y))
  ch_title <- chart_title
  p <- x %>% 
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(dim))) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    scale_y_continuous(labels = scales::label_percent,
                       expand = expansion(mult=c(0,0.05)),
                       limits = c(0 - max_val, max_val)) +
    scale_fill_manual(values=fill_color)+
    labs(title=ch_title, x="", y="")+
    theme_list + tooltip_fmt(x_var, "%", y_var)
  return(ggplotly(p))
}



TestF <- function(ds, x, y, dim, fill_color) {
  print(head(ds))
  max_val <- max(ds[[y]], na.rm = TRUE)
  print(max_val)
  print(fill_color)
  p <- ds %>%
    ggplot(aes(x = !!sym(x), y = !!sym(y), fill = !!sym(dim))) +
    geom_col() +
    scale_y_continuous(labels = label_currency(scale = 1e-6, suffix = "M", accuracy = 1),
                       expand = expansion(mult=c(0,0.05)),
                       limits = c(0 - max_val, max_val)) +
    scale_fill_manual(values=fill_color)
    
  return(ggplotly(p))  
}
# annual
#TestF(beer_annual_data, 'cyr', 'netsales', 'red', 'red')
# qtrly
#TestF(beer_qtr_data, 'cyr_qtr', 'netsales', 'cqtr', qtr_color)
# Category charts ----
# plot for category metrics
CatChart <- function(chart_title, dataset, x_var, y_var, dim, fill_color, theme_list) {
  x <- dataset
  x$category <- reorder(x$category, x$netsales, FUN = sum)
  ch_title <- chart_title
  p <- x %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(dim))) +
    geom_col(position = "stack") +
    scale_y_continuous(labels = label_currency(scale = 1e-6, suffix = "M"),
                       expand = expansion(mult=c(0,0.05))) +
    scale_fill_manual(values=fill_color)+
    labs(title=ch_title, x="", y="")+
    theme(axis.ticks.x = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.margin = unit(c(1, 0, 0, 0), "cm"))+
    theme_list + tooltip_fmt(x_var, "M", y_var)
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
CatChgChart <- function (chart_title, dataset, x_var, y_var, f_var, fill_color, theme_list) {
  x <- dataset
  #x$category <- reorder(x$category, x$yoy, FUN = sum)
  ch_title <- "% Chg Net $ Sales by Source"
  p <- x %>%
    ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(f_var))) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    facet_grid(category~.) +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult=c(0,0.05))) +
    scale_fill_manual(values=fill_color) +
    theme(strip.background = element_rect(fill = fill_color)) +
    theme(strip.text=element_text(color='white'))+
    labs(title=ch_title, x="", y="")+
    theme_list
  ggplotly(p)
}

# tooltip formatting ----
tooltip_fmt <- function(dim, units, y_var) {
      if (units == "B") {
        text_fmt <- aes(text = paste0(!!sym(dim), ": ", 
                                      label_currency(scale = 1e-9, suffix = "B")
                                      (!!sym(y_var))))
      } else if (units == "M") {
        text_fmt <- aes(text = paste0(!!sym(dim), ": ", 
                                    label_currency(scale = 1e-6, suffix = "M", accuracy = 0.1)
                                    (!!sym(y_var))))
      } else if (units == "%") {
         text_fmt <- aes(text=paste0(!!sym(dim), ": ", 
                                     percent_format(accuracy = 0.1)
                                     (!!sym(y_var))))
       } else {
         text_fmt <- aes(text=paste0(!!sym(dim), ": ", !!sym(y_var)))
       }
     
    return(text_fmt)
}

#tt <- tooltip_fmt("cyr", "B", "netsales")
#print(tt)
