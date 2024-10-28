# Functions for patterns that recur over the categories

## Plot Sales by year ----
AnnualChart <- function(chart_title, dataset, fill_color, theme_list) {
  # get filtered, aggregated data -> usually reactive object
  x <- dataset
  ch_title <- chart_title
  p <- x %>%
    ggplot(aes(x = cyr, y = netsales)) +
    geom_col(fill=fill_color) +
    scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                       expand = expansion(mult=c(0,0.05))) +
    labs(title=ch_title, x="", y="") +
    theme_list
  return(ggplotly(p))
}
# Plot Sales by Quarter ----
QtrChart <- function(chart_title, dataset, fill_color, theme_list) {
  x <- dataset
  ch_title <- chart_title
  p <- x %>%
    ggplot(aes(x = cyr_qtr, y = netsales, fill = cqtr)) +
    geom_col() +
    scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                       expand = expansion(mult=c(0,0.05))) +
    scale_fill_manual(values=fill_color) +
    labs(title=ch_title, x="", y="") +
    theme_list
  return(ggplotly(p))
}

# plot for year-over-year change in sales
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
    theme_list
  return(ggplotly(p))
}

# plot for quarter-over-quarter change in sales
QoQChart <- function(chart_title, dataset, fill_color, theme_list) {
  x <- dataset
  max_y <- max(x$qoq, na.rm = TRUE)
  min_y <- min(x$qoq, na.rm = TRUE)
  max_val <- max(abs(min_y), abs(max_y))
  ch_title <- chart_title
  p <- x %>% 
    ggplot(aes(x = cyr_qtr, y = qoq, fill = cqtr)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult=c(0,0.05)),
                       limits = c(0 - max_val, max_val)) +
    scale_fill_manual(values=fill_color)+
    labs(title=ch_title, x="", y="")+
    theme_list
  return(ggplotly(p))
}

# plot for category sales
CatChart <- function(chart_title, dataset, x_var, fill_color, theme_list) {
  x <- dataset
  x$category <- reorder(x$category, x$netsales, FUN = sum)
  ch_title <- chart_title
  p <- x %>%
    ggplot(aes(x = !!sym(x_var), y = netsales, fill = category)) +
    geom_col(position = "stack") +
    scale_y_continuous(labels = label_currency(scale = 1e-9, suffix = "B"),
                       expand = expansion(mult=c(0,0.05))) +
    scale_fill_manual(values=fill_color)+
    labs(title=ch_title, x="", y="")+
    theme(axis.ticks.x = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.margin = unit(c(1, 0, 0, 0), "cm"))+
    theme_list
  p_plotly <- ggplotly(p)
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