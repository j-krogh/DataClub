#Data Club Graphs for July 2026
library(dplyr)
library(tidyr)
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
  summarize(sum_val = sum(VALUE, na.rm = T)) %>% filter(years != 2026) %>% #2026 isn't a full year!
  ggplot(aes(x = years, y=sum_val/1e6, colour = GEO)) +
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
  labs(y = "TWh", x = 'Date', title = "Total Annual Electricity Used in BC", caption = "Statistics Canada Table: 25100016") +
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

#Make a nice version of this plot
sl$lineType <- "other"
sl$lineType[sl$year == 2026] = "this_year"
sl$lineType[sl$year == 2025] = "last_year"

sl$lineType <- factor(sl$lineType, levels = c("this_year", "last_year", "other"))

sl %>% filter(GEO %in% c('British Columbia'), Electric.power..components == 'Total electricity available for use within specific geographic border') %>%
  ggplot(aes(x = month, y = VALUE/1e6, group = year, color = lineType, linewidth = lineType))+
  geom_line()+
  #geom_point(size = 1)+
  scale_linewidth_manual(values = c("this_year" = 2, 
                                    "last_year" = 1.25, 
                                    "other" = 0.5),
                         labels = c("this_year" = "2026",
                                    "last_year" = "2025",
                                    "other" = "2008 - 2025")) + 
  scale_color_manual(values = c("other" = "#A4BDC9",
                                "this_year" = "#DB444B",
                                "last_year" = "#379A8B"),
                     labels = c("this_year" = "2026",
                                "last_year" = "2025",
                                "other" = "2008 - 2025"))+
  labs(x = "", y = "TWh", title = "Year-over-Year Comparison Total Electricity Consumption in BC")+
  theme_economist() +
  theme(legend.position = "bottom", legend.title = element_blank())

#Was 2015 low elec use driven by a warm spring? Yeah looks that way
install.packages(
  "weathercan",
  repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org")
)
library(weathercan)

stations_search("Vancouver", interval = "day")

#use Van Harbour 888 as record is continous from 1925-2026
van_weather<- weather_dl(station_ids = 888, start = '2008-01-01', end = '2026-04-01')

#only care about temp
van_weather_daily <- van_weather %>% group_by(date) %>% summarize(daily_temp = mean(temp, na.rm = T))

#converet to proper date object then group by month
van_weather_daily$date <- as.Date(van_weather_daily$date)
van_weather_daily$year <- year(van_weather_daily$date)
van_weather_daily$month <- month(van_weather_daily$date)

van_weather_monthly <- van_weather_daily %>% group_by(year, month) %>% 
  summarize(monthly_temp = mean(daily_temp, na.rm = T))

#do a yearly plot like for electricity use
p<-ggplot(van_weather_monthly, aes(x = month, y = monthly_temp, group = year, color = factor(year)))+
  geom_line(linewidth = 0.7)+
  geom_point(size = 1)+
  scale_color_viridis_d(name = "Year")+ # perceptually even colors for many years5  
  labs(x = "", y = "TWh", title = "Year-over-Year Temperature in Vancouver BC")+
  theme_minimal()

ggplotly(p)

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
  labs(y = "TWh", x = '', title = "Annual Total Imports and Exports to BC", caption = "Statistics Canada Table: 25100016") +
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
  labs(x = "", y = "TWh", title = "Monthly Solar and Wind Electricity in BC", caption = "Statistics Canada: Table 25100015") +
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
  labs(x = "", y = "TWh", title = "Monthly Solar and Wind Electricity in Alberta", caption = "Statistics Canada: Table 25100015") +
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
#Mr. Dix says 60 megawatts for data centers 166MW for crypto Bill up to 400MW more to come
#https://www.biv.com/news/real-estate/data-centres-are-coming-to-bc-but-is-there-enough-power-12044005
#Annual average use is ~8000MW

#LNG 138MW for Woodfiber but again units...

#Battery on the island 100MW units

#Roof top solar maybe 0.06 - 0.1 TWh per year not nothing but still way below Ab

#---- Intertie Stuff
BCH_Int <- read.csv("BCH_Intertie_Hist.csv")

#BCH has negative as import and postive as exports. Lets switch that to match Stats Canada
BCH_Int$bc_ab_MWh = -1 * BCH_Int$bc_ab_MWh
BCH_Int$bc_us_MWh = -1 * BCH_Int$bc_us_MWh

BCH_Int$datetime_pst <- ymd_hms(BCH_Int$datetime_pst, truncated = 3)
BCH_Int$Month <- lubridate::month(BCH_Int$datetime_pst)
BCH_Int$Year <- lubridate::year(BCH_Int$datetime_pst)

#Data is in MWh per hour so MW
#Trends in Power Imports/Exports as monthly MWh
BCH_Int_Monthly <- BCH_Int %>% filter(!is.na(Year)) %>% group_by(Year, Month) %>% 
  summarize(bc_us_monthly = sum(bc_us_MWh, na.rm = T), 
            bc_ab_monthly = sum(bc_ab_MWh, na.rm = T)) %>% 
  mutate(date = as.Date(paste0(Year, "-", Month, "-15")))

#Any hourly patterns?

#BC USA
ggplot(BCH_Int_Monthly, aes(x = date)) + 
  geom_ribbon(ymin = 0, ymax = ) + 
  geom_line()

#Nice looking plot
BCH_Int_Monthly_long <- pivot_longer(BCH_Int_Monthly,
                                     cols = c('bc_us_monthly', 'bc_ab_monthly'),
                                     values_to = "value",
                                     names_to = "Intertie")


BCH_Int_Monthly_long <- BCH_Int_Monthly_long  %>%
  arrange(date) %>%
  mutate(
    val = value / 1e6,
    pos = pmax(val, 0),   # positive part, 0 where negative
    neg = pmin(val, 0)    # negative part, 0 where positive
  )

BCH_Int_Monthly_long %>% filter(Intertie == "bc_us_monthly") %>%
ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = 0, ymax = pos, fill = "Positive")) +
  geom_ribbon(aes(ymin = neg, ymax = 0, fill = "Negative")) +
  geom_line(aes(y = val), linewidth = 0.3) +
  #geom_point(aes(y = val), size = 0.6) +
  geom_hline(yintercept = 0, colour = "grey40") +
  scale_fill_manual(values = c("Positive" = "#DB444B",
                               "Negative" = "#006BA2"), name = NULL) +
  coord_cartesian(ylim = c(-2, 2)) +
  labs(x = "", y = "TWh", title = "Monthly Total Imports and Exports to BC - USA") +
  annotate('text', x=as.Date('2009-01-10'), y= -1.25, label = 'Exports from BC to USA') +
  annotate('text', x=as.Date('2009-01-10'), y= +1.25, label = 'Imports to BC from USA') +
  theme_economist() +
  theme(legend.position = "none")



#BC AB
BCH_Int_Monthly_long %>% filter(Intertie == "bc_ab_monthly") %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = 0, ymax = pos, fill = "Positive")) +
  geom_ribbon(aes(ymin = neg, ymax = 0, fill = "Negative")) +
  geom_line(aes(y = val), linewidth = 0.3) +
  #geom_point(aes(y = val), size = 0.6) +
  geom_hline(yintercept = 0, colour = "grey40") +
  scale_fill_manual(values = c("Positive" = "#DB444B",
                               "Negative" = "#006BA2"), name = NULL) +
  coord_cartesian(ylim = c(-2, 2)) +
  labs(x = "", y = "TWh", title = "Monthly Total Imports and Exports to BC - AB") +
  annotate('text', x=as.Date('2009-01-10'), y= -1.25, label = 'Exports from BC to AB') +
  annotate('text', x=as.Date('2009-01-10'), y= +1.25, label = 'Imports to BC from AB') +
  theme_economist() +
  theme(legend.position = "none")

year_plot = 2025

#Look at hourly data for 2025 the last full year of data
p1<-BCH_Int %>% filter(datetime_pst > as.POSIXct(paste0(year_plot,"-01-01")), datetime_pst < as.POSIXct(paste0(year_plot + 1,"-01-01"))) %>% 
  mutate(hour_of_day = lubridate::hour(datetime_pst), Month = as.factor(Month)) %>% 
  group_by(Month, hour_of_day) %>% summarize(month_hr_mean = mean(bc_ab_MWh, na.rm = T)) %>%
  ggplot(aes(x = hour_of_day, y=month_hr_mean, colour = Month)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  coord_cartesian(ylim = c(-2000, 2000)) +
  annotate("text", x = 2, y = 1250, label = "Imports")+
  annotate("text", x = 2, y = -1250, label = "Exports")+
  scale_color_discrete(
    labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr",
               "5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug",
               "9" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")
  ) +
  labs(y = "MWh", x = "Hour of the Day", title = paste0(year_plot," BC - AB Intertie"), 
       caption = "Source: BC Hydro Histroical Actual Flow") +
  theme_economist() +
  theme(legend.position = 'right')

#USA
p2<-BCH_Int %>% filter(datetime_pst > as.POSIXct(paste0(year_plot,"-01-01")), datetime_pst < as.POSIXct(paste0(year_plot + 1,"-01-01"))) %>% 
  mutate(hour_of_day = lubridate::hour(datetime_pst), Month = as.factor(Month)) %>% 
  group_by(Month, hour_of_day) %>% summarize(month_hr_mean = mean(bc_us_MWh, na.rm = T)) %>%
  ggplot(aes(x = hour_of_day, y=month_hr_mean, colour = Month)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  coord_cartesian(ylim = c(-2000, 2000)) +
  annotate("text", x = 2, y = 1250, label = "Imports")+
  annotate("text", x = 2, y = -1250, label = "Exports")+
  scale_color_discrete(
    labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr",
               "5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug",
               "9" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")
  ) +
  labs(y = "MWh", x = "Hour of the Day", title = paste0(year_plot, " BC - USA Intertie"),
       caption = "Source: BC Hydro Histroical Actual Flow") +
  theme_economist() +
  theme(legend.position = 'right')


#https://public.tableau.com/app/profile/icbc/viz/QuickStatistics-Policiesinforce/VehicleInsurancePoliciesinForce

#EVs 175k by 2025
#15000km/yr * 17kwh per 100km = 2500 kwh/car/year - 437 GWh/yr 


