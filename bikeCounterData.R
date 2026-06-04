library(jsonlite)
library(httr)
library(httr)
library(jsonlite)
library(dplyr)

# Define the target URL
#100057501 selkirk
#100059041 JSB
#selkirk flow ID 101057501;102057501;103057501;104057501;105057501;106057501
#JSB flow ID 101059041;102059041;103059041;104059041
url <- "https://www.eco-visio.net/api/aladdin/1.0.0/pbl/publicwebpageplus/data/100057501"

params <- list(
  idOrganisme = "4828",
  idPdc = "100057501",
  fin = "01/06/2026",
  debut = "01/01/2025",
  interval = "3",
  flowIds = "105057501;106057501",
  flowmode=2 #bikes
)

# Send GET request
response <- GET(url, query = params)
status_code(response)

# Parse JSON content
data_json <- content(response, as = "text", encoding = "UTF-8")
df <- as.data.frame(fromJSON(data_json))

# Convert date column to Date
df <- df %>%
  mutate(date_only = as.Date(V1, format = "%m/%d/%Y")) %>%
  group_by(date_only) %>%
  mutate(
    hour = 0:(n() - 1),  # assumes 24 rows per group
    datetime = as.POSIXct(paste(date_only, sprintf("%02d:00:00", hour)), tz = "UTC")
  ) %>%
ungroup()

library(ggplot2)

df$count = as.numeric(df$V2)

#ggplot(df, aes(x = datetime, y = count)) + geom_point()  

p <- ggplot(df, aes(x = date_only, y = hour,fill = count)) +
  scale_fill_gradientn(colors = hcl.colors(50)) +
  geom_tile() +
  #coord_fixed() +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_continuous(limits=c(0,600), type = "viridis") +
  ggtitle("2025 Hourly Cyclists at Selkirk") +
  #ylim(ymin = 0, ymax = 24)
  labs(x = "", y = "Hour of the Day", fill = "Cyclists per Hour") +
  theme_bw()
  #theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  #theme(legend.position = "bottom",
  #      legend.text.position = "top",
  #      label.theme = element_text(angle = 90))

ggsave("hourly_cyclists_JSB_2024.png", p)

library(plotly)
ggplotly(p)

#for JSB
url <- "https://www.eco-visio.net/api/aladdin/1.0.0/pbl/publicwebpageplus/data/100059041"

params <- list(
  idOrganisme = "4828",
  idPdc = "100059041",
  fin = "02/06/2026", #day/month/year
  debut = "01/01/2025",
  interval = "3",
  flowIds = "101059041;102059041",
  flowmode=2 #bikes
)


# Send GET request
response <- GET(url, query = params)
status_code(response)

# Parse JSON content
data_json <- content(response, as = "text", encoding = "UTF-8")
df <- as.data.frame(fromJSON(data_json))

# Convert date column to Date
df <- df %>%
  mutate(date_only = as.Date(V1, format = "%m/%d/%Y")) %>%
  group_by(date_only) %>%
  mutate(
    hour = 0:(n() - 1),  # assumes 24 rows per group
    datetime = as.POSIXct(paste(date_only, sprintf("%02d:00:00", hour)), tz = "UTC")
  ) %>%
  ungroup()

df$count = as.numeric(df$V2)

#plotting function
plot_hourly_cyclists <- function(df, 
                                 title = "Hourly Cyclists", 
                                 fill_limits = c(0, 600)) {
  p <- ggplot(df, aes(x = date_only, y = hour, fill = count)) +
    geom_tile() +
    scale_x_date(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_continuous(limits = fill_limits, type = "viridis") +
    ggtitle(title) +
    labs(x = "", y = "Hour of the Day", fill = "Cyclists per Hour") +
    theme_bw()
  
  return(p)
}

#weekly bar plot
plot_weekly_avg_cyclists <- function(df,
                                     title = "Weekly Average Cyclists",
                                     bar_color = "steelblue") {
  weekly_avg <- df %>%
    mutate(week = floor_date(date_only, unit = "week")) %>%
    group_by(week) %>%
    summarise(avg_count = mean(count, na.rm = TRUE))
  
  p <- ggplot(weekly_avg, aes(x = week, y = avg_count)) +
    geom_col(fill = bar_color) +
    scale_x_date(expand = c(0, 0), date_breaks = "1 month", date_labels = "%b %Y") +
    scale_y_continuous(expand = c(0, 0)) +
    ggtitle(title) +
    labs(x = "", y = "Average Cyclists per Hour", fill = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

plot_weekly_cyclists <- function(df,
                                 title = "Weekly Total Cyclists",
                                 bar_color = "steelblue") {
  weekly_total <- df %>%
    mutate(week = floor_date(date_only, unit = "week")) %>%
    group_by(week) %>%
    summarise(total_count = sum(count, na.rm = TRUE))
  
  p <- ggplot(weekly_total, aes(x = week, y = total_count)) +
    geom_col(fill = bar_color) +
    scale_x_date(expand = c(0, 0), date_breaks = "1 month", date_labels = "%b %Y") +
    scale_y_continuous(expand = c(0, 0)) +
    ggtitle(title) +
    labs(x = "", y = "Total Cyclists", fill = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

#difference calcs
plot_yoy_comparison <- function(df,
                                start_date,
                                end_date,
                                title = NULL) {
  
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  # Shift dates back by exactly one year for the baseline
  baseline_start <- start_date - years(1)
  baseline_end   <- end_date - years(1)
  
  summarise_weekly <- function(data, start, end) {
    data %>%
      filter(date_only >= start & date_only <= end) %>%
      mutate(week = floor_date(date_only, unit = "week")) %>%
      group_by(week) %>%
      summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      mutate(week_num = row_number())
  }
  
  period1 <- summarise_weekly(df, baseline_start, baseline_end)
  period2 <- summarise_weekly(df, start_date, end_date)
  
  combined <- inner_join(period1, period2, by = "week_num", suffix = c("_p1", "_p2")) %>%
    mutate(
      difference = total_count_p2 - total_count_p1,
      direction  = if_else(difference >= 0, "Increase", "Decrease")
    )
  
  # Auto-generate title if not provided
  if (is.null(title)) {
    title <- paste0(
      format(start_date, "%b %d %Y"), " to ", format(end_date, "%b %d %Y"),
      " vs same period ", year(start_date) - 1
    )
  }
  
  p <- ggplot(combined, aes(x = week_num, y = difference, fill = direction)) +
    geom_col() +
    geom_hline(yintercept = 0, linewidth = 0.5, color = "grey30") +
    scale_fill_manual(values = c("Increase" = "#2ecc71", "Decrease" = "#e74c3c")) +
    scale_x_continuous(
      breaks = combined$week_num,
      labels = format(combined$week_p1, "%b %d")
    ) +
    ggtitle(title) +
    labs(x = "", y = "Change in Cyclists", fill = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

plot_yoy_comparison(
  df,
  start_date = "2026-02-01",
  end_date = "2026-05-31",
  title = "Spring 2026 vs Spring 2025 — Weekly Cyclists"
)

##
plot_yoy_comparison <- function(df,
                                start_date,
                                end_date,
                                title = NULL) {
  
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  baseline_start <- start_date - years(1)
  baseline_end   <- end_date - years(1)
  
  summarise_weekly <- function(data, start, end) {
    # Find the first complete week start (Monday on or after start)
    first_complete_week <- ceiling_date(start, unit = "week", week_start = 1)
    # Find the last complete week end (Sunday on or before end)
    last_complete_week  <- floor_date(end, unit = "week", week_start = 1) - days(1)
    
    data %>%
      filter(date_only >= first_complete_week & date_only <= last_complete_week) %>%
      mutate(week = floor_date(date_only, unit = "week", week_start = 1)) %>%
      group_by(week) %>%
      summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      mutate(week_num = row_number())
  }
  
  period1 <- summarise_weekly(df, baseline_start, baseline_end) %>%
    mutate(year = as.character(year(baseline_start)))
  
  period2 <- summarise_weekly(df, start_date, end_date) %>%
    mutate(year = as.character(year(start_date)))
  
  combined <- bind_rows(period1, period2)
  
  # Use period1 week dates as x-axis labels
  week_labels <- period1 %>% select(week_num, week)
  
  if (is.null(title)) {
    title <- paste0(
      format(start_date, "%b %d"), " to ", format(end_date, "%b %d"),
      " — ", year(baseline_start), " vs ", year(start_date)
    )
  }
  
  p <- ggplot(combined, aes(x = week_num, y = total_count, fill = year)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("steelblue", "#e67e22")) +
    scale_x_continuous(
      breaks = week_labels$week_num,
      labels = format(week_labels$week, "%b %d")
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    ggtitle(title) +
    labs(x = "", y = "Total Cyclists", fill = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

plot_yoy_comparison(df, start_date = "2026-03-01", end_date = "2026-05-31")
