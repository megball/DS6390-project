##################
# LOAD LIBRARIES #
##################

library(readr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(gganimate)

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Or read in the data manually

energy_world <- readr::read_csv('data - input/energy_world.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')

#save locally
write.csv(energy_world, "data - input/energy_world.csv", row.names = FALSE)
write.csv(country_totals, "data - input/country_totals.csv", row.names = FALSE)

#import US data
us_energy <- read_csv('data - input/MER_T01_02.csv')

#import greenhouse data - global
world_greenhouse <- readr::read_csv('data - input/world_bank_greenhouse.csv')

#import greenhouse data - US
us_greenhouse <- readr::read_csv('data - input/US_greenhouse.csv')

#population data
population <- readr::read_csv('data - input/world_population.csv')

#SAIDI and SAIFI
saidi <- readr::read_csv('data - input/SAIDI.csv', skip=1)
saifi <- readr::read_csv('data - input/SAIFI.csv', skip=1)

#################
# DATA CLEAN UP #
#################

#Europe data is in GWh, US data in quad (10^15 BTU)
#1,000,000 Gigawatt Hours to Quads = 3.4121
#1 Quads to Gigawatt Hours = 293071.0702

#convert US data from Quads to GWh
us_energy <- us_energy %>%
  mutate(GWh = Value * 293071.072) %>%
  rename(Quad = Value)

#rename data sources to match European data
us_energy <- us_energy %>%
  mutate(type = case_when(Description == "Coal Production" ~ "Coal",
                          Description == "Natural Gas (Dry) Production" ~ "Natural Gas",
                          Description == "Crude Oil Production" ~ "Crude Oil",
                          Description == "Natural Gas Plant Liquids Production" ~ "Natural Gas",
                          Description == "Total Fossil Fuels Production" ~ "Conventional thermal",
                          Description ==  "Nuclear Electric Power Production" ~ "Nuclear",
                          Description == "Hydroelectric Power Production" ~ "Hydro",
                          Description == "Geothermal Energy Production" ~ "Geothermal",
                          Description == "Solar Energy Production" ~ "Solar",
                          Description == "Wind Energy Production" ~ "Wind",
                          Description == "Biomass Energy Production" ~ "Other",
                          Description == "Total Renewable Energy Production" ~ "Total Renewable",
                          Description == "Total Primary Energy Production" ~ "Total"))

#create month & year columns
us_energy <- us_energy %>%
  mutate(year = str_sub(YYYYMM, 1, 4)) %>%
  mutate(month = str_sub(YYYYMM, 5, 7))

#calculate annual totals
us_energy_annual <- us_energy %>%
  group_by(year, type) %>%
  summarize(GWh = mean(GWh)) %>%
  mutate(country = "US", country_name = "United States")

#save new data set
write.csv(us_energy_annual, "data - output/us_energy_annual.csv", row.names = FALSE)

#reload after updates in Excel for % of total renewables and just using years 2002 - 2018
us_energy_sub <- read.csv("data - input/us_energy_annual_sub.csv")

#pivot to match with years as rows to tidy data
energy_world <- energy_world %>%
  pivot_longer(cols = c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", 
                        "2013", "2014", "2015", "2016", "2017", "2018"), names_to = "year", values_to = "GWh", 
               values_drop_na = TRUE)

#pivot population data
population <- population %>%
  pivot_longer(cols = c(5:65), names_to = "year", values_to = "total population") %>%
  rename("country_name" = "Country Name")

#remove level 2 and level column
energy_world <- energy_world %>%
  filter(level == "Level 1") %>%
  select(-c("level"))

#concatenate energy tables
energy_all <- rbind(energy_world, us_energy_sub)

#join to population data to calculate per capita
energy_all <- merge(energy_all, population, by = c("country_name", "year"))

#calculate per capita energy use
energy_all <- energy_all %>%
  mutate(per_capita_kwh = (GWh / `total population`)*1000000)


#pivot us greenhouse
us_greenhouse <- us_greenhouse %>%
  pivot_longer(cols = c(2:30), names_to = "year", values_to = "emissions")

#pivot world greenhouse
world_greenhouse <- world_greenhouse %>%
  pivot_longer(cols = c(5:8), names_to = "year", values_to = "emissions")

########################
# Save cleaned up data #
########################
#per capita kWh energy - US & Europe
write.csv(energy_all, "data - output/energy_all.csv", row.names = FALSE)

#greenhouse emissions in millions metric tons of CO2 equiv in US
write.csv(us_greenhouse, "data - output/us_greenhouse_pivot.csv", row.names = FALSE)

#greenhouse emissions in kt of CO2 equiv in the world
write.csv(world_greenhouse, "data - output/world_greenhouse_pivot.csv", row.names = FALSE)

#open saved files
energy_all <- read.csv("data - output/energy_all.csv")
us_greenhouse <- read.csv("data - output/us_greenhouse_pivot.csv")
world_greenhouse <- read.csv("data - output/world_greenhouse_pivot.csv")

all <- read.csv("data - input/owid-co2-data.csv")

##############
## Analyze ##
#############

#plot conventional thermal use by country
ggplot(energy_all %>% filter(type == "Conventional thermal", year >= 2016), aes(x=year, y=per_capita_kwh, color = country_name)) +
  geom_point()

ggplot(all %>% filter(year >= 2000, country == c("United States", "Albania")), aes(x=year, y=energy_per_capita, color = country)) +
  geom_point()

#plot greenhouse gas emissions over the years EU vs US
world_greenhouse %>%
  filter(`Country Code` == "USA" | `Country Code` == "EUU") %>%
  ggplot(aes(x=year, y=emissions, fill = `Country Code`)) +
  geom_col(position = "dodge2") +
  scale_fill_manual(values = c("USA" = "#006699", "EUU" = "#999999")) +
  labs(title="Greenhouse gas emissions have increased in the US since 1990",
      subtitle="while emissions have decreased in the European Union",
      y = "Emissions in kiloton of CO2 equivalent") +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 13),
        legend.position = "") +
  coord_flip()

#find most improved country in terms of renewables


 #plot renewable sources vs US
world_greenhouse %>%
  filter(`Country Code` == "USA" | `Country Code` == "EUU") %>%
  ggplot(aes(x=year, y=emissions, fill = `Country Code`)) +
  geom_col(position = "dodge2")

#plot % renewable by year for US versus UK
energy_all %>%
  filter(type == "% Renewable") %>%
  filter(country == "US" | country == "UK") %>%
  ggplot(aes(x=year, y=GWh, fill = country)) +
  geom_col(position = "dodge2") +
  ylab("% Of Energy Renewable")

#visualize energy mix for US for 2016-2019
energy_all %>%
  filter(country == "US") %>%
  filter(year < 2020) %>%
  filter(type %in% c('Coal', 'Crude Oil', 'Geothermal', 'Hydro', 'Natural Gas', 'Nuclear', 'Solar', 'Wind', 'Other')) %>%
  ggplot(aes(x=year, y=GWh, fill = type)) +
  geom_col(position = "dodge2") +
  ylab("GWh")

#line graph version
energy_all %>%
  filter(country == "US") %>%
  filter(year < 2020) %>%
  filter(type %in% c('Coal', 'Crude Oil', 'Geothermal', 'Hydro', 'Natural Gas', 'Nuclear', 'Solar', 'Wind', 'Other')) %>%
  ggplot(aes(x=year, y=GWh, color = type, group = type)) +
  geom_line() +
  ylab("GWh")

#% of 2019 by renewable vs fossil fuels
energy_all %>%
  filter(country == "US") %>%
  filter(year < 2020) %>%
  filter(type %in% c('Conventional thermal', 'Total Renewable')) %>%
  ggplot(aes(x=year, y=GWh, fill = type)) +
  geom_col(position = "dodge2") +
  ylab("GWh")

energy_all %>%
  filter(country == "US") %>%
  filter(year < 2020) %>%
  filter(type %in% c('Conventional thermal', 'Total Renewable')) %>%
  ggplot(aes(x=year, y=GWh, color = type, group = type)) +
  geom_point() +
  geom_line() +
  ylab("GWh")

energy_all %>%
  filter(country %in% c("US", "UK", "DE", "SE")) %>%
  filter(year < 2020) %>%
  filter(type %in% c('Conventional thermal', 'Total Renewable')) %>%
  ggplot(aes(x=year, y=per_capita_kwh, color = type, group = type)) +
  geom_point() +
  geom_line() +
  ylab("Per Capita Kwh") +
  facet_wrap(~country)

energy_all %>%
  filter(country %in% c("US", "UK", "DE", "SE")) %>%
  filter(year < 2020) %>%
  filter(type == 'Conventional thermal') %>%
  ggplot(aes(x=year, y=per_capita_kwh, color = country, group = country)) +
  geom_point() +
  geom_line() +
  ylab("Per Capita Kwh")

#total energy use
energy_all %>%
  filter(country == "US") %>%
  filter(year < 2020) %>%
  filter(type == 'Total') %>%
  ggplot(aes(x=year, y=per_capita_kwh, fill = type)) +
  geom_col(position = "dodge2") +
  ylab("kWh per capita")

country_list <- c('United States','China', 'India','United Kingdom')

#animated co2 emissions by continent
all %>%
  filter(country %in% country_list) %>%
  filter(year >= 1980) %>%
  ggplot(aes(x = population, y = co2_per_capita, size = gdp, color = country)) +
  geom_point(alpha = 0.7) +
  labs(title = 'Year: {frame_time}', x = 'GDP', y = 'CO2 emissions') +
  transition_time(year) +
  ease_aes('linear')

anima <- all %>%
  filter(country %in% country_list) %>%
  filter(year >= 1980) %>%
  ggplot(aes(x = year, y = co2_per_capita, size = gdp, color = country)) +
  geom_point(alpha = 0.7) +
  transition_states(country,
                    transition_length = 2,
                    state_length = 1) +
  ggtitle("CO2 Emissions per Capita from 1980 - Present")

p <- anima +
  enter_fade() +
  exit_shrink()


animate(
  anima + enter_fade() + exit_shrink(),
  renderer = av_renderer())


anim_save('ball_megan',animation = p)