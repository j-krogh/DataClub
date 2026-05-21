library(jsonlite)
library(httr)

library(httr)
library(jsonlite)

# Define the target URL
#100057501 selkirk
#100059041 JSB
#selkirk flow ID 101057501;102057501;103057501;104057501;105057501;106057501
#JSB flow ID 101059041;102059041;103059041;104059041
url <- "https://www.eco-visio.net/api/aladdin/1.0.0/pbl/publicwebpageplus/data/100057501"

params <- list(
  idOrganisme = "4828",
  idPdc = "100057501",
  fin = "04/02/2026",
  debut = "01/01/2026",
  interval = "3",
  flowIds = "105057501;106057501"
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
  scale_fill_continuous(limits=c(0,400), type = "viridis") +
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
