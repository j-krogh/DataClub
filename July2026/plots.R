#Data Club Graphs for July 2026
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(plotly)

#Read the supply and load data from stats canada
sl <- read.csv('25100016.csv', stringsAsFactors = F)

sl$REF_DATE <- as.Date(paste0(sl$REF_DATE, "-01"), format = '%Y-%m-%d')

sl %>% filter(GEO %in% c('British Columbia', 'Alberta'), Electric.power..components == 'Total electricity available for use within specific geographic border') %>%
  ggplot(aes(x = REF_DATE, y=VALUE/1e6, colour = GEO)) +
  geom_point() +
  geom_line() +
  labs(y = "TWh", x = 'Date', title = "Electricity Used")

sl %>% filter(GEO %in% c('British Columbia', 'Alberta'), Electric.power..components == 'Total electricity available for use within specific geographic border') %>%
  mutate(years = lubridate::year(REF_DATE)) %>% group_by(GEO, years) %>% 
  summarize(mean_val = mean(VALUE, na.rm = T)) %>% filter(years != 2026) %>% #2026 isn't a full year!
  ggplot(aes(x = years, y=mean_val/1e6, colour = GEO)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = F, linetype = 'solid') +
  labs(y = "TWh", x = 'Date', title = "Electricity Used")


#Make a nice plot for just BC
#https://sa.ipaa.org.au/wp-content/uploads/2026/02/Economist-CHARTstyleguide_20170505.pdf
sl %>% filter(GEO %in% c('British Columbia'), Electric.power..components == 'Total electricity available for use within specific geographic border') %>%
  ggplot(aes(x = REF_DATE, y=VALUE/1e6)) +
  #geom_point(color = '#006BA2', size = 2) +
  geom_line(color = '#006BA2', linewidth = 0.75) +
  labs(y = "TWh", x = '', title = "Total Monthly Electricity Used in BC 2008 - 2026", caption = "Statistics Canada Table: 25100016") +
  theme_economist()

p <- sl %>% filter(GEO %in% c('British Columbia'), Electric.power..components == 'Total electricity available for use within specific geographic border') %>%
  mutate(years = lubridate::year(REF_DATE)) %>% group_by(GEO, years) %>% 
  summarize(sum_val = sum(VALUE, na.rm = T)) %>% filter(years != 2026) %>% #2026 isn't a full year!
  ggplot(aes(x = years, y=sum_val/1e6)) +
  #geom_point() +
  #geom_line(color = '#006BA2', linewidth = 0.75) +
  geom_col(fill = '#006BA2')+
  coord_cartesian(ylim = c(40, 75)) +
  #geom_smooth(method = "lm", se = F, linetype = 'solid', color = '#3F5661') +
  labs(y = "TWh", x = 'Date', title = "Total Annual Electricity Used in BC", caption = "Statistics Canada Table: 25100016 \n @jeremy77.bsky.social") +
  theme_economist()

  ggsave("./plots/BC_Electricity_Useage.png",p)
  ggsave("./plots/BC_Electricity_Useage_small.png", p, width = 6, height = 5)

#explore why 2015 was so low
sl <- sl %>% mutate(year = year(REF_DATE),
                    month = month(REF_DATE, label = T, abbr = T))


p1 <- sl %>% filter(GEO %in% c('British Columbia'), Electric.power..components == 'Total electricity available for use within specific geographic border') %>%
  ggplot(aes(x = month, y = VALUE/1e6, group = year, color = factor(year)))+
  geom_line(linewidth = 0.7)+
  geom_point(size = 1)+
  scale_color_viridis_d(name = "Year")+ # perceptually even colors for many years5  
  labs(x = "", y = "TWh", title = "Year-over-Year Comparison Total Electricity Consumption in BC")+
  theme_minimal()

ggplotly(p1)

#Was 2015 low elec use drivin by a warm spring?

#Import, exports, net
sl_net <- sl %>% select(REF_DATE,Electric.power..components, GEO, VALUE) %>%
  pivot_wider(names_from = Electric.power..components, values_from = VALUE) %>% 
  mutate(Net = `Total receipts` - `Total deliveries`, 
         sign = case_when(is.na(Net) ~ NA_character_, 
                          Net >= 0   ~ "Positive", 
                          TRUE       ~ "Negative"))


library(dplyr); library(tidyr); library(ggplot2)

bc <- sl_net %>%
  filter(GEO == "British Columbia") %>%
  arrange(REF_DATE) %>%
  mutate(
    val = Net / 1e6,
    pos = pmax(val, 0),   # positive part, 0 where negative
    neg = pmin(val, 0)    # negative part, 0 where positive
  )

ggplot(bc, aes(x = REF_DATE)) +
  geom_ribbon(aes(ymin = 0, ymax = pos, fill = "Positive")) +
  geom_ribbon(aes(ymin = neg, ymax = 0, fill = "Negative")) +
  geom_line(aes(y = val), linewidth = 0.3) +
  #geom_point(aes(y = val), size = 0.6) +
  geom_hline(yintercept = 0, colour = "grey40") +
  scale_fill_manual(values = c("Positive" = "#DB444B",
                               "Negative" = "#006BA2"), name = NULL) +
  coord_cartesian(ylim = c(-2, 2)) +
  labs(x = "", y = "TWh", title = "Monthly Total Imports and Exports to BC") +
  annotate('text', x=as.Date('2009-01-10'), y= -1.25, label = 'Exports from BC') +
  annotate('text', x=as.Date('2009-01-10'), y= +1.25, label = 'Imports to BC') +
  theme_economist() +
theme(legend.position = "none")


#As a bar chart for years
p<-bc %>% mutate(years = lubridate::year(REF_DATE)) %>% group_by(years) %>% 
  summarize(sum_val = sum(Net, na.rm = T)) %>% filter(years != 2026) %>% #2026 isn't a full year!
  ggplot(aes(x = years, y=sum_val/1e6, fill = sum_val > 0)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#DB444B",
                               "FALSE" = "#006BA2")) +
  coord_cartesian(ylim = c(-12, 12)) +
  labs(y = "TWh", x = '', title = "Annual Total Imports and Exports to BC", caption = "Statistics Canada Table: 25100016 \n @jeremy77.bsky.social") +
  annotate('text', x=2009, y= -5.75, label = 'Exports from BC') +
  annotate('text', x=2009, y= +5.75, label = 'Imports to BC') +
  theme_economist() +
  theme(legend.position = "none")

ggsave("./plots/BC_Electricity_Import_Export.png",p)
ggsave("./plots/BC_Electricity_Import_Export_small.png", p, width = 6, height = 5)
  
#Read the generation type data from stats canada
gen <- read.csv('25100015.csv', stringsAsFactors = F)
gen$REF_DATE <- as.Date(paste0(gen$REF_DATE, "-01"), format = '%Y-%m-%d')


#focus on utilites (not industry) and only BC and Alberta
gen <- gen %>% filter(GEO %in% c('British Columbia', 'Alberta'), 
                      Class.of.electricity.producer == 'Electricity producers, electric utilities') 


p2<-gen %>% filter(GEO %in% c('British Columbia'), Class.of.electricity.producer == 'Electricity producers, electric utilities') %>%
  ggplot(aes(x = REF_DATE, y = VALUE, colour = Type.of.electricity.generation)) +
  geom_point() +
  geom_line()
  
ggplotly(p2)

#plot total electricity generation
gen %>% filter(GEO %in% c('British Columbia'), Type.of.electricity.generation == 'Total all types of electricity generation') %>% ggplot(aes(x = REF_DATE, y = VALUE/1e6)) +
  geom_line(colour = "#006BA2", linewidth = 0.75) +
  geom_ribbon(aes(ymin=0, ymax = VALUE/1e6), fill = "#006BA2", alpha = 0.1)+
  labs(x = "", y = "TWh", title = "Total Monthly Utility Electricity Generation in BC") +
  theme_economist() +
  theme(legend.position = "none")


#okay now do windy and solar only as monthly totals
p<-gen %>% filter(GEO %in% c('British Columbia'), Type.of.electricity.generation %in% c('Solar','Wind power turbine')) %>% 
  ggplot(aes(x = REF_DATE, y = VALUE/1e6, colour = Type.of.electricity.generation)) +
  geom_line(linewidth = 0.75) +
  coord_cartesian(ylim = c(0, 1.7)) +
  labs(x = "", y = "TWh", title = "Monthly Solar and Wind Electricity in BC", caption = "Statistics Canada: Table 25100015 \n @jeremy77.bsky.social") +
  scale_colour_manual(name = "",
                        values = c('Solar' = '#EBB434', 
                                   'Wind power turbine'= '#379A8B'),
                        labels = c("Solar" = "Solar",
                                   "Wind power turbine" = "Wind")) +
  theme_economist() +
  theme(legend.position = "right") +
  scale_x_date(expand = c(0,0))

ggsave("./plots/BC_Electricity_Wind_Solar.png",p)
ggsave("./plots/BC_Electricity_Wind_Solar_small.png", p, width = 6, height = 5)

p<-gen %>% filter(GEO %in% c('Alberta'), Type.of.electricity.generation %in% c('Solar','Wind power turbine')) %>% 
  ggplot(aes(x = REF_DATE, y = VALUE/1e6, colour = Type.of.electricity.generation)) +
  geom_line(linewidth = 0.75) +
  coord_cartesian(ylim = c(0, 1.7)) +
  labs(x = "", y = "TWh", title = "Monthly Solar and Wind Electricity in Alberta", caption = "Statistics Canada: Table 25100015 \n @jeremy77.bsky.social") +
  scale_colour_manual(name = "",
                      values = c('Solar' = '#EBB434', 
                                 'Wind power turbine'= '#379A8B'),
                      labels = c("Solar" = "Solar",
                                 "Wind power turbine" = "Wind")) +
  theme_economist() +
  theme(legend.position = "right") +
  scale_x_date(expand = c(0,0))

ggsave("./plots/AB_Electricity_Wind_Solar.png",p)
ggsave("./plots/AB_Electricity_Wind_Solar_small.png", p, width = 6, height = 5)

#https://public.tableau.com/app/profile/icbc/viz/QuickStatistics-Policiesinforce/VehicleInsurancePoliciesinForce
#EVs 175k by 2025
#15000km/yr * 17kwh per 100km = 2500 kwh/car/year - 437 GWh/yr or 0.437 TWh

#Data Centers
#Mr. Dix says 60 megawatts max

#LNG

#Roof top solar maybe 0.06 - 0.1 TWh per year not nothing but still way below Ab

#----



# 1. Build the summarized data once
plot_df <- sl %>%
  filter(GEO %in% c('British Columbia'),
         Electric.power..components == 'Total electricity available for use within specific geographic border') %>%
  mutate(years = lubridate::year(REF_DATE)) %>%
  group_by(GEO, years) %>%
  summarize(mean_val = mean(VALUE, na.rm = TRUE), .groups = "drop") %>%
  mutate(twh = mean_val / 1e6) %>% filter(years != 2026)

# 2. Compute the slope (TWh/year) for each GEO
slopes <- plot_df %>%
  group_by(GEO) %>%
  summarize(
    slope = coef(lm(twh ~ years))[["years"]],
    # position for the label: last year, near the group's max value
    x = max(years),
    y = max(twh),
    .groups = "drop"
  ) %>%
  mutate(label = paste0("slope = ", round(slope * 1000, 1), " GWh/yr"))

# 3. Plot
ggplot(plot_df, aes(x = years, y = twh)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_text(data = slopes,
            aes(x = x, y = y, label = label),
            hjust = 1, vjust = -0.5, show.legend = FALSE) +
  labs(y = "TWh", x = "Date", title = "Electricity Used")

#https://public.tableau.com/app/profile/icbc/viz/QuickStatistics-Policiesinforce/VehicleInsurancePoliciesinForce

#EVs 175k by 2025
#15000km/yr * 17kwh per 100km = 2500 kwh/car/year - 437 GWh/yr 


