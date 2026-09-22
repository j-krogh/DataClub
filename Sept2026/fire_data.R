library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(gt)


#Read historical fire shape file
h_fires <- read_sf("./PROT_HISTORICAL_FIRE_POLYS_SP/H_FIRE_PLY_polygon.shp")

#What are trends like over time for total burned area? Calculate by year summary

#yearly burned area summary
h_fires_n_sp <- st_drop_geometry(h_fires)

#exploratory plot
h_fires_n_sp %>% group_by(FIRE_YEAR) %>% summarize(total_burned_area_year_ha = sum(AREA_SQM)/10000) %>%
  ggplot(aes(x=FIRE_YEAR, y=total_burned_area_year_ha)) +
  geom_col(stat = 'identity')

#Data is really noisy but clearly something changed around 2010
h_fires_n_sp %>% group_by(FIRE_YEAR) %>% summarize(total_burned_area_year_ha = sum(AREA_SQM)/10000) %>%
  ggplot(aes(x=total_burned_area_year_ha)) +
  geom_histogram(bins = 30)


#try 5 year bin time series
p1 <- h_fires_n_sp %>% mutate(grouped_year = round(FIRE_YEAR/5)*5) %>% 
  group_by(grouped_year, FIRE_CAUSE) %>% 
  summarize(total_burned_area_year_ha = (sum(AREA_SQM)/(10000 * 1e6))/length(unique(FIRE_YEAR))) %>%
  ggplot(aes(x=grouped_year, y=total_burned_area_year_ha, fill = FIRE_CAUSE)) +
  geom_col(stat = 'identity') +
  labs(y = "Burned Area (million ha)", x = "", title = "Average Annual Burned Area in BC",
       caption = "Data averaged over five year periods.\n
       Source: BC Wildfire Fire Perimeters - Historical published by the BC Wildfire Service") +
  scale_fill_manual(values = c("Lightning" = "#006BA2", "Person" = "#379A8B", "Unknown" = "#758D99")) +
  theme_economist() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.25, "cm")) +
  scale_x_continuous(expand = c(0,0))

ggsave("./Sept2026/five_year_running_mean.png",p1)

#year by year plot
p2 <- h_fires_n_sp %>%
  group_by(FIRE_YEAR, FIRE_CAUSE) %>% 
  summarize(total_burned_area_year_ha = (sum(AREA_SQM)/(10000 * 1e6))) %>%
  ggplot(aes(x=FIRE_YEAR, y=total_burned_area_year_ha, fill = FIRE_CAUSE)) +
  geom_col(stat = 'identity') +
  labs(y = "Burned Area (million ha)", x = "", 
       title = "Annual Burned Area in BC",
       caption = "Source: BC Wildfire Fire Perimeters - Historical published by the BC Wildfire Service") +
  scale_fill_manual(values = c("Lightning" = "#006BA2", "Person" = "#379A8B", "Unknown" = "#758D99")) +
  theme_economist() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.25, "cm")) +
  scale_x_continuous(expand = c(0,0))

ggsave("./Sept2026/annual_burn_area.png",p2)

#Size of fires over time
p3 <- h_fires_n_sp %>% filter(FIRE_CAUSE != "Unknown") %>% 
  group_by(FIRE_YEAR, FIRE_CAUSE) %>% 
  summarize(p90 = quantile(SIZE_HA, 0.9, na.rm = T), p50 = quantile(SIZE_HA, 0.5, na.rm = T), p10 = quantile(SIZE_HA, 0.1, na.rm = T)) %>%
  ggplot(aes(x=FIRE_YEAR, y=p50, colour = FIRE_CAUSE)) +
  geom_point() +
  geom_smooth() +
  labs(x = "",
       y = "Median Fire Size (Ha)",
       title = "Median Size of Wildfires",
       caption = "Source: BC Wildfire Fire Perimeters - Historical published by the BC Wildfire Service") +
  scale_colour_manual(values = c("Lightning" = "#006BA2", "Person" = "#379A8B", "Unknown" = "#758D99")) +
  theme_economist() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.25, "cm")) +
  scale_x_continuous(expand = c(0.01,0.01))

ggsave("median_fire_size.png",p3)

#mean fire size
p4 <- h_fires_n_sp %>% filter(FIRE_CAUSE != "Unknown") %>% 
  group_by(FIRE_YEAR, FIRE_CAUSE) %>% 
  summarize(avg_fire_size = mean(SIZE_HA, na.rm = T)) %>%
  ggplot(aes(x=FIRE_YEAR, y=avg_fire_size, colour = FIRE_CAUSE)) +
  geom_point() +
  geom_smooth() +
  labs(x = "",
       y = "Average Fire Size (Ha)",
       title = "Average Size of Wildfires",
       caption = "Source: BC Wildfire Fire Perimeters - Historical published by the BC Wildfire Service") +
  scale_colour_manual(values = c("Lightning" = "#006BA2", "Person" = "#379A8B", "Unknown" = "#758D99")) +
  theme_economist() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.25, "cm")) +
  scale_x_continuous(expand = c(0.01,0.01))

ggsave("average_fire_size.png",p4)

#Total count of fires
p5 <- h_fires_n_sp %>% 
  group_by(FIRE_YEAR, FIRE_CAUSE) %>% 
  summarize(num_fires_year = n()) %>%
  ggplot(aes(x=FIRE_YEAR, y=num_fires_year, colour = FIRE_CAUSE)) +
  geom_point() + 
  geom_smooth() +
  labs(x = "",
       y = "Number of Fires",
       title = "Total Number of Wildfires",
       caption = "Source: BC Wildfire Fire Perimeters - Historical published by the BC Wildfire Service") +
  scale_colour_manual(values = c("Lightning" = "#006BA2", "Person" = "#379A8B", "Unknown" = "#758D99")) +
  theme_economist() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.25, "cm")) +
  scale_x_continuous(expand = c(0.01,0.01))

ggsave("total_number_fire.png",p5)


#Make a leader board of years with the most burned area top 10
top_10_h_fires <- h_fires_n_sp %>% group_by(FIRE_YEAR) %>% summarize(total_burned_area_year_ha = (sum(AREA_SQM)/(10000 * 1e6))) %>%
  slice_max(order_by = total_burned_area_year_ha, n = 10)

#Add Rank
top_10_h_fires$Rank <- seq(1,10)
top_10_h_fires <- top_10_h_fires %>% relocate(Rank, .before = FIRE_YEAR)

#round
top_10_h_fires$total_burned_area_year_ha <- round(top_10_h_fires$total_burned_area_year_ha,2)
  
gt(top_10_h_fires) %>% tab_header(title = md("**BC Wildfire Burned Area**"), subtitle = "Top ten years") %>%
  cols_label(Rank = md(""), FIRE_YEAR = md("**Year**"), total_burned_area_year_ha = md("**Area burned (million ha)**")) %>%
  tab_source_note("Source: BC Wildfire Fire Perimeters - Historical published by the BC Wildfire Service") %>% 
  opt_row_striping(row_striping = TRUE) %>%
  tab_options(
    table.background.color = "#daeaf2",
    heading.background.color = "#daeaf2",
    column_labels.background.color = "#daeaf2",
    row.striping.background_color = "#bed9e6",
    table.font.color = "#3F5661",
    column_labels.border.bottom.color = "#3F5661",
    heading.border.bottom.color = "#daeaf2",
    table.border.top.color = "#daeaf2",
    table.border.bottom.color = "#daeaf2",
    source_notes.font.size = 10,
    source_notes.padding = 6,
    source_notes.padding.horizontal = 10
  )

#current year data https://pub.data.gov.bc.ca/datasets/cdfc2d7b-c046-4bf0-90ac-4897232619e1/
cur_fire <- read_sf('./PROT_CURRENT_FIRE_POLYS_SP/C_FIRE_PLY_polygon.shp')
sum(cur_fire$SIZE_HA)/1e6

#2026 305 fires in total
#0.43 million ha burned
#



x <- h_fires_n_sp %>% mutate(grouped_year = round(FIRE_YEAR/5)*5) %>% 
  group_by(grouped_year) %>% 
  summarize(total_burned_area_year_ha = (sum(AREA_SQM)/10000)/length(unique(FIRE_YEAR))) 
