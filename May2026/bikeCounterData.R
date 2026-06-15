library(jsonlite)
library(httr)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(cowplot)

#How many fewer people biked over the selkirk tressetle as a result of the closure from 
#January to March 2026 vs previous years

#location ids
#100057501 selkirk
#100059041 JSB

#this generates all flow ids from 1 to 9
#having extra flow ids doesn't hurt but missing them causes incomplete data
#these seem to be sensor ids and change from time to time but always count up
generate_id_list <- function(base_id) {
  base_str <- as.character(base_id)
  
  prefix <- substr(base_str, 1, 2)   # "10"
  suffix <- substr(base_str, 4, 9)   # "057501"
  
  ids <- paste0(prefix, 1:9, suffix)
  paste(ids, collapse = ";")
}

#function to get data for a location
get_bike_count_data <- function(stn_location, start_date, end_date, flow_id, interval) {
  
  #build the url
  url <- paste0("https://www.eco-visio.net/api/aladdin/1.0.0/pbl/publicwebpageplus/data/", stn_location)
  
  
  flowIdsSelect = generate_id_list(stn_location) #JSB
  
  if (stn_location == '100057501') {
    flowIdsSelect = '105057501;106057501' #SLK
  }
  
  if (stn_location == '100057507') {
    flowIdsSelect = '103057507;104057507' #E and N
  }
  
  
  #build the query parameters
  params <- list(
    idOrganisme = "4828",
    idPdc = stn_location,
    fin = format(end_date, "%d/%m/%Y"),
    debut = format(start_date, "%d/%m/%Y"),
    interval = interval, #3 is hourly data 4 is daily 5 is weekly
    flowIds = flowIdsSelect, #direction
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
  
  #clean up the resultant data frame
  df$count = as.numeric(df$V2)
  df <- df %>% select(-c("V1", "V2"))
  df$station_id <- stn_location
  
  return(df)

}

#function to make heat map
plot_hourly_cyclists_hm <- function(df, title = "Hourly Cyclists", fill_limits = c(0, 600)) {
  p <- ggplot(df, aes(x = date_only, y = hour, fill = count)) +
    geom_tile() +
    scale_x_date(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_continuous(limits = fill_limits, type = "viridis") +
    ggtitle(title) +
    labs(x = "", y = "Hour of the Day", fill = "Cyclists per Hour", caption = "@jeremy77.bsky.social") +
    theme_bw()
  
  return(p)
}

#daily plot
plot_daily_cyslists_ts <- function(df, title = "Daily Cyclists", y_limits = NULL) {
  ggplot(df, aes(x = datetime, y = count)) +
    geom_line() +
    geom_line(color = "grey70") +
    geom_area(fill = "grey70", alpha = 0.15) +
    geom_line(color = "grey70", linewidth = 0.7) +
    geom_smooth(method = "loess", span = 0.15, se = FALSE, color = "steelblue", linewidth = 1) +
    scale_x_datetime(date_breaks = "4 month", date_labels = "%b %Y", expand = c(0, 0)) +
    ggtitle(title) +
    labs(x = "", y = "Daily Cyclists") +
    scale_y_continuous(limits = y_limits) +
    theme_bw()
}

#my study period will span from June 2023 to June 2026 so that plots don't look too busy
#but there will still be enough data to make inferences I will also zoom in on the
#trail closure period Nov 15 to Mar 15

start_date = as.Date('2023-06-01')
end_date = as.Date('2026-06-01')
data_int = "4" #3 is hourly, 4 is daily

bld_jsb <- get_bike_count_data('100059041', start_date, end_date, interval = data_int)
bld_slk <- get_bike_count_data('100057501', start_date, end_date, interval = data_int)
bld_en <- get_bike_count_data('100057507', start_date, end_date, interval = data_int)

#Daily plots with a smooth line
jsb_daily <- plot_daily_cyslists_ts(bld_jsb, title = "Johnson St. Bridge", y_limits = c(0,5000))
slk_daily <- plot_daily_cyslists_ts(bld_slk, title = "Selkirk Trestle North End", y_limits = c(0,5000))
en_daily <- plot_daily_cyslists_ts(bld_en, title = "E&N at Hereward Rd.", y_limits = c(0,2000))

#JSB matches the website but SLK is a little high, like 5% too high weird. Porbably something
#to do with the flow ids

#Make a nice cowplot for both stations
combined <- plot_grid(

    slk_daily, jsb_daily, en_daily,
  ncol  = 1,       # stack vertically
  align = "v",     # align vertical axes
  rel_heights = c(1, 1)  # equal height, adjust if needed
)

title <- ggdraw() +
  draw_label("Galloping Goose and E&N Regional Trails", fontface = "bold", x = 0, hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 10)) +
  labs(caption = "@jeremy77.bsky.social")

plot_grid(title, combined, ncol = 1, rel_heights = c(0.05, 1), ggdraw() + draw_label("@jeremy77.bsky.social", x = 1, hjust = 1, size = 8))

#again for hourly data
hrly_jsb <- get_bike_count_data('100059041', start_date, end_date, interval = "3")
hrly_slk <- get_bike_count_data('100057501', start_date, end_date, interval = "3")
hrly_en <- get_bike_count_data('100057507', start_date, end_date, interval = "3")

plot_hourly_cyclists_hm(hrly_jsb, title = "Johnson Street Bridge", fill_limits = c(0,600))
plot_hourly_cyclists_hm(hrly_slk, title = "Selkirk North end of Trestle", fill_limits = c(0,600))
plot_hourly_cyclists_hm(hrly_en, title = "E&N at Hereward Rd.", fill_limits = c(0,200))

#show a short period to see the weekly pattern
plot_hourly_cyclists_hm(hrly_slk %>% filter(between(date_only, as.Date('2026-05-02'), as.Date('2026-05-31'))),
                        title = "Hourly Cyclists at Selkirk Trestle May 2026")

#focus on the closure period Nov-01 to Mar-31
summarise_winter_periods <- function(df, period_start_month = 11, period_start_day = 15,
                                     period_end_month = 3,   period_end_day = 15,
                                     title = "Total Cyclists by Winter Period",
                                     bar_color = "steelblue") {
  
  years     <- unique(year(df$date_only))
  min_year  <- min(years)
  max_year  <- max(years)
  
  periods <- do.call(rbind, lapply(min_year:(max_year - 1), function(yr) {
    tibble(
      period_label = paste0(yr, "/", substr(yr + 1, 3, 4)),
      start        = as.Date(paste(yr,     period_start_month, period_start_day, sep = "-")),
      end          = as.Date(paste(yr + 1, period_end_month,   period_end_day,   sep = "-"))
    )
  }))
  
  data_start <- min(df$date_only)
  data_end   <- max(df$date_only)
  
  periods <- periods %>%
    filter(end >= data_start & start <= data_end)
  
  results <- do.call(rbind, lapply(1:nrow(periods), function(i) {
    df %>%
      filter(date_only >= periods$start[i] & date_only <= periods$end[i]) %>%
      summarise(total_cyclists = sum(count, na.rm = TRUE)) %>%
      mutate(period = periods$period_label[i],
             start  = periods$start[i],
             end    = periods$end[i])
  }))
  
  # Flag incomplete periods where data doesn't cover the full window
  results <- results %>%
    mutate(complete = start >= data_start & end <= data_end)
  
  p <- ggplot(results, aes(x = period, y = total_cyclists, fill = complete)) +
    geom_col() +
    geom_text(aes(label = scales::comma(total_cyclists)), 
              vjust = -0.5, size = 3.5) +
    scale_fill_manual(values = c("TRUE" = bar_color, "FALSE" = "grey70"),
                      labels = c("TRUE" = "Complete", "FALSE" = "Incomplete"),
                      name   = "") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                       labels = scales::comma) +
    ggtitle(title) +
    labs(x = "", y = "Total Cyclists") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold"),
      legend.position    = "bottom"
    )
  
  print(p)
  return(invisible(results))
}

#shows the raw numbers
summarise_winter_periods(bld_jsb, title = "Total Winter Cyclists at Johnson St. Bridge")
summarise_winter_periods(bld_slk, title = "Total Winter Cyclists at Selkirk Trestle")
summarise_winter_periods(bld_en, title = "Total Winter Cyclists at E&N")

#shows relative change year over year
summarise_winter_periods_yoy <- function(df, period_start_month = 11, period_start_day = 15,
                                         period_end_month = 3,   period_end_day = 15,
                                         title = "Year-over-Year Change in Winter Cyclists",
                                         bar_color = "steelblue") {
  
  years    <- unique(year(df$date_only))
  min_year <- min(years)
  max_year <- max(years)
  
  periods <- do.call(rbind, lapply(min_year:(max_year - 1), function(yr) {
    tibble(
      period_label = paste0(yr, "/", substr(yr + 1, 3, 4)),
      start        = as.Date(paste(yr,     period_start_month, period_start_day, sep = "-")),
      end          = as.Date(paste(yr + 1, period_end_month,   period_end_day,   sep = "-"))
    )
  }))
  
  data_start <- min(df$date_only)
  data_end   <- max(df$date_only)
  
  periods <- periods %>%
    filter(end >= data_start & start <= data_end)
  
  results <- do.call(rbind, lapply(1:nrow(periods), function(i) {
    df %>%
      filter(date_only >= periods$start[i] & date_only <= periods$end[i]) %>%
      summarise(total_cyclists = sum(count, na.rm = TRUE)) %>%
      mutate(period   = periods$period_label[i],
             start    = periods$start[i],
             end      = periods$end[i],
             complete = periods$start[i] >= data_start & periods$end[i] <= data_end)
  }))
  
  # Calculate percent change vs previous period
  results <- results %>%
    mutate(
      pct_change = (total_cyclists - lag(total_cyclists)) / lag(total_cyclists) * 100,
      direction  = case_when(
        is.na(pct_change)  ~ "baseline",
        pct_change >= 0    ~ "increase",
        TRUE               ~ "decrease"
      )
    )
  
  # Drop the first period since it has no baseline to compare against
  plot_data <- results %>% filter(!is.na(pct_change))
  
  p <- ggplot(plot_data, aes(x = period, y = pct_change, fill = direction)) +
    geom_col() +
    geom_hline(yintercept = 0, linewidth = 0.5, color = "grey30") +
    geom_text(aes(label = paste0(ifelse(pct_change > 0, "+", ""), round(pct_change, 1), "%")),
              vjust = ifelse(plot_data$pct_change >= 0, -0.5, 1.5), size = 3.5) +
    scale_fill_manual(values = c("increase" = "#2ecc71", "decrease" = "#e74c3c")) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0.15, 0.15))) +
    ggtitle(title) +
    labs(x = "", y = "Year-over-Year Change",
         caption = paste0("Baseline: ", results$period[1], " (", scales::comma(results$total_cyclists[1]), " cyclists)")) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold"),
      legend.position    = "none"
    )
  
  print(p)
  return(invisible(results))
}


summarise_winter_periods_yoy(bld_jsb, title = "Year-over-Year Change in Winter Cyclists at JSB")
summarise_winter_periods_yoy(bld_slk, title = "Year-over-Year Change in Winter Cyclists at SLK")

#function to show two data frames and compare to baseline at each location
summarise_winter_periods_yoy <- function(df_list, labels,
                                         period_start_month = 11, period_start_day = 15,
                                         period_end_month = 3,   period_end_day = 15,
                                         title = "2025/26 vs Historical Average — Winter Cyclists",
                                         baseline_color = "steelblue") {
  
  if (length(df_list) != length(labels)) {
    stop("df_list and labels must be the same length")
  }
  
  get_period_totals <- function(df, location_label) {
    years    <- unique(year(df$date_only))
    min_year <- min(years)
    max_year <- max(years)
    
    periods <- do.call(rbind, lapply(min_year:(max_year - 1), function(yr) {
      tibble(
        period_label = paste0(yr, "/", substr(yr + 1, 3, 4)),
        start        = as.Date(paste(yr,     period_start_month, period_start_day, sep = "-")),
        end          = as.Date(paste(yr + 1, period_end_month,   period_end_day,   sep = "-"))
      )
    }))
    
    data_start <- min(df$date_only)
    data_end   <- max(df$date_only)
    
    periods <- periods %>%
      filter(end >= data_start & start <= data_end)
    
    results <- do.call(rbind, lapply(1:nrow(periods), function(i) {
      df %>%
        filter(date_only >= periods$start[i] & date_only <= periods$end[i]) %>%
        summarise(total_cyclists = sum(count, na.rm = TRUE)) %>%
        mutate(period   = periods$period_label[i],
               start    = periods$start[i],
               end      = periods$end[i],
               complete = periods$start[i] >= data_start & periods$end[i] <= data_end)
    }))
    
    results$location <- location_label
    return(results)
  }
  
  all_results <- do.call(bind_rows, lapply(seq_along(df_list), function(i) {
    get_period_totals(df_list[[i]], labels[i])
  }))
  
  comparison <- all_results %>%
    group_by(location) %>%
    arrange(start) %>%
    summarise(
      current_period   = last(period),
      current_total    = last(total_cyclists),
      baseline_periods = paste(period[-n()], collapse = ", "),
      baseline_avg     = mean(total_cyclists[-n()], na.rm = TRUE),
      abs_change       = current_total - baseline_avg,
      pct_change       = abs_change / baseline_avg * 100,
      .groups = "drop"
    ) %>%
    mutate(direction = if_else(pct_change >= 0, "increase", "decrease")) %>%
    mutate(location = factor(location, levels = labels))
  
  p <- ggplot(comparison, aes(x = location, y = pct_change, fill = direction)) +
    geom_col() +
    geom_hline(yintercept = 0, linewidth = 0.5, color = "grey30") +
    geom_text(aes(label = paste0(
      ifelse(pct_change > 0, "+", ""), round(pct_change, 1), "%\n",
      "(", ifelse(abs_change > 0, "+", ""), scales::comma(round(abs_change)), ")"
    )),
    vjust = ifelse(comparison$pct_change >= 0, -0.1, 1.1), size = 3.5, lineheight = 0.9) +
    scale_fill_manual(values = c("increase" = "#2ecc71", "decrease" = "#e74c3c")) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0.2, 0.2))) +
    ggtitle(title) +
    labs(x = "", y = "% Change vs Historical Average", caption = "@jeremy77.bsky.social") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold"),
      axis.text.x        = element_text(angle = 45, hjust = 1),
      legend.position    = "none"
    )
  
  print(p)
  return(invisible(list(period_totals = all_results, comparison = comparison)))
}

summarise_winter_periods_yoy(df_list = list(bld_jsb, bld_en, bld_slk), labels = c('Johnson St. Bridge', 'E&N at Hereward Rd.', 'Selkirk Trestle'))

######
plot_flow_id_availability <- function(base_id, start_date, end_date, interval = 4) {
  
  base_str <- as.character(base_id)
  prefix   <- substr(base_str, 1, 2)
  suffix   <- substr(base_str, 4, 9)
  
  results <- list()
  
  for (n in 1:9) {
    flow_id <- paste0(prefix, n, suffix)
    
    url <- paste0("https://www.eco-visio.net/api/aladdin/1.0.0/pbl/publicwebpageplus/data/", base_id)
    
    params <- list(
      idOrganisme = "4828",
      idPdc       = base_id,
      fin         = format(as.Date(end_date),   "%d/%m/%Y"),
      debut       = format(as.Date(start_date), "%d/%m/%Y"),
      interval    = interval,
      flowmode    = 1,
      flowIds     = flow_id
      
    )
    
    tryCatch({
      response <- GET(url, query = params)
      data_json <- content(response, as = "text", encoding = "UTF-8")
      df <- as.data.frame(fromJSON(data_json))
      
      if (nrow(df) > 0) {
        df <- df %>%
          mutate(
            date_only = as.Date(V1, format = "%m/%d/%Y"),
            count     = as.numeric(V2),
            flow_id   = as.factor(flow_id)
          ) %>%
          select(date_only, count, flow_id)
        
        results[[as.character(n)]] <- df
      } else {
        message("No data for flow ID: ", flow_id)
      }
    }, error = function(e) {
      message("Error fetching flow ID ", flow_id, ": ", e$message)
    })
  }
  
  if (length(results) == 0) {
    message("No data found for any flow ID")
    return(invisible(NULL))
  }
  
  combined <- bind_rows(results)
  
  # Filter to only flow IDs that have non-zero data
  active_flows <- combined %>%
    group_by(flow_id) %>%
    filter(sum(count, na.rm = TRUE) > 0) %>%
    ungroup()
  
  p <- ggplot(active_flows, aes(x = date_only, y = count, color = flow_id)) +
    geom_line() +
    facet_wrap(~ flow_id, ncol = 1, scales = "free_y") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0, 0)) +
    ggtitle(paste("Flow ID Availability —", base_id)) +
    labs(x = "", y = "Count", color = "Flow ID") +
    theme_bw() +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      legend.position  = "none",
      strip.background = element_rect(fill = "steelblue"),
      strip.text       = element_text(color = "white", face = "bold")
    )
  
  print(p)
  return(invisible(combined))
}


plo#get data for the last two full years to get a sense of the normal ranges
selkirk <- get_bike_count_data('100057501', as.Date('2024-06-01'), as.Date('2026-06-08'), c("5", "6"), "3")
jsb <- get_bike_count_data('100059041', as.Date('2024-06-01'), as.Date('2026-06-08'), c("1","2"), "4")

plot_hourly_cyclists_hm(jsb, title = "Johnson Street Bridge", fill_limits = c(0,600))
plot_hourly_cyclists_hm(selkirk, title = "Selkirk North end of Bridge", fill_limits = c(0,600))

selkirk_25 <- get_bike_count_data('100057501', as.Date('2024-09-01'), as.Date('2025-06-01'), c("5", "6"))
plot_hourly_cyclists(selkirk_25, title = "Selkirk 2025", fill_limits = c(0,600))

selkirk_26 <- get_bike_count_data('100057501', as.Date('2025-09-01'), as.Date('2026-06-01'), c("5", "6"))
plot_hourly_cyclists(selkirk_26, title = "Selkirk 2026", fill_limits = c(0,600))

#JSB
JSB_25 <- get_bike_count_data('100059041', as.Date('2024-11-01'), as.Date('2025-04-01'), c("1", "2"))
plot_hourly_cyclists(selkirk_25, title = "JSB 2025", fill_limits = c(0,400))

JSB_26 <- get_bike_count_data('100059041', as.Date('2025-11-01'), as.Date('2026-04-01'), c("1", "2"))
plot_hourly_cyclists(selkirk_26, title = "JSB 2026", fill_limits = c(0,400))

#Total Cyclists Dec 1 - Apr 1 2025 vs 2026
#JSB
JSB_net = sum(JSB_26$count) - sum(JSB_25$count) #less 959 
JSB_rel = (JSB_net/((sum(JSB_26$count) + sum(JSB_25$count))/2)) * 100 #less -0.8%

#
Selkirk_net = sum(selkirk_26$count) - sum(selkirk_25$count) #less 21,028
selkirk_rel = selkirk_rel = (Selkirk_net/((sum(selkirk_26$count) + sum(selkirk_25$count))/2)) * 100
#13.6% decline!



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


plot_weekly_cyclists <- function(df,
                                 title = "Weekly Total Cyclists",
                                 bar_color = "steelblue",
                                 y_limits = NULL,
                                 highlight = TRUE,
                                 highlight_start = NULL,
                                 highlight_end = NULL) {
  
  first_complete_week <- ceiling_date(min(df$date_only), unit = "week", week_start = 1)
  last_complete_week  <- floor_date(max(df$date_only), unit = "week", week_start = 1) - days(1)
  
  weekly_total <- df %>%
    filter(date_only >= first_complete_week & date_only <= last_complete_week) %>%
    mutate(week = floor_date(date_only, unit = "week", week_start = 1)) %>%
    group_by(week) %>%
    summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop")
  
  # Build highlight rectangles
  if (highlight && !is.null(highlight_start) && !is.null(highlight_end)) {
    # Use user-supplied date range
    highlight_rects <- tibble(
      xmin = as.Date(highlight_start),
      xmax = as.Date(highlight_end)
    )
  } else if (highlight) {
    # Default to Nov 15 - Mar 15 for each year in the data
    years_in_data <- unique(year(weekly_total$week))
    highlight_rects <- do.call(rbind, lapply(years_in_data, function(yr) tibble(
      xmin = as.Date(paste0(yr - 1, "-11-15")),
      xmax = as.Date(paste0(yr,     "-03-15"))
    ))) %>%
      filter(xmax >= first_complete_week & xmin <= last_complete_week) %>%
      mutate(
        xmin = pmax(xmin, first_complete_week),
        xmax = pmin(xmax, last_complete_week)
      )
  }
  
  p <- ggplot(weekly_total, aes(x = week, y = total_count)) +
    { if (highlight) geom_rect(data = highlight_rects,
                               aes(xmin = xmin, xmax = xmax,
                                   ymin = -Inf, ymax = Inf),
                               inherit.aes = FALSE,
                               fill = "lightblue", alpha = 0.3) } +
    geom_col(fill = bar_color) +
    scale_x_date(expand = c(0, 0), date_breaks = "1 month", date_labels = "%b %Y") +
    scale_y_continuous(expand = c(0, 0), limits = y_limits) +
    ggtitle(title) +
    labs(x = "", y = "Total Cyclists", fill = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

