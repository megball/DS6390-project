##################
# LOAD LIBRARIES #
##################

library(readr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(gganimate)

# Get the Data

#Europe energy breakdown 2016-2018
#source Eurostat
energy_EU <- readr::read_csv('data - input/FINAL/energy_EU.csv')

#import US data energy breakdown
#source ?
us_energy <- read_csv('data - input/FINAL/us_energy_bkdwn.csv')

#import world CO2 emissions data
#source https://github.com/owid/co2-data 
world_greenhouse <- readr::read_csv('data - input/FINAL/co2_greenhouse.csv')

#import percent thermal & renewable energy data
#source: World 
thermal <- readr::read_csv('data - input/FINAL/thermal_pct.csv')
renewable <- readr::read_csv('data - input/FINAL/renewable_pct.csv')
hydro <- readr::read_csv('data - input/FINAL/hydro_pct.csv')

#SAIDI and SAIFI
saidi <- readr::read_csv('data - input/FINAL/SAIDI.csv', skip=1)
saifi <- readr::read_csv('data - input/FINAL/SAIFI.csv', skip=1)

#population data for per capita calculations
population <- readr::read_csv('data - input/world_population.csv')

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

#filter to 2016-2018 only
us_energy_2018 <- us_energy_annual %>%
  filter(year %in% c("2016", "2017", "2018"))

#pivot to match with years as rows to tidy data - European data
energy_EU <- energy_EU %>%
  pivot_longer(cols = c("2016", "2017", "2018"), names_to = "year", values_to = "GWh", 
               values_drop_na = TRUE)

#remove level 2 and level column
energy_EU <- energy_EU %>%
  filter(level == "Level 1") %>%
  select(-c("level"))

#concatenate energy tables
energy_df <- rbind(energy_EU, us_energy_2018)

#pivot population data
population <- population %>%
  pivot_longer(cols = c(5:65), names_to = "year", values_to = "total population") %>%
  rename("country_name" = "Country Name")

#join to population data to calculate per capita
energy_df <- merge(energy_df, population, by = c("country_name", "year"))

#calculate per capita energy use
energy_df <- energy_df %>%
  mutate(per_capita_kwh = (GWh / `total population`)*1000000)

##combine hydro, renewable, and thermal pct into one table including emissions

#pivot hydro
hydro <- hydro %>%
  pivot_longer(cols = c(5:20), names_to = "year", values_to = "% hydro")

#pivot thermal
thermal <- thermal %>%
  pivot_longer(cols = c(5:20), names_to = "year", values_to = "% thermal")

#pivot renewable
renewable <- renewable %>%
  pivot_longer(cols = c(5:20), names_to = "year", values_to = "% renewable")

#merge hydro and thermal
df2 <- merge(hydro, thermal, by = c("Country Code", "year"))

#drop unneeded columns
df2 <- df2 %>%
  select(-c(`Indicator Name.x`,`Indicator Code.x`,`Country Name.y`,`Indicator Name.y`,`Indicator Code.y`))

#merge with renewable
df2 <- merge(df2, renewable, by = c("Country Code", "year"), all=TRUE)

#drop unneeded columns
df2 <- df2 %>%
  select(-c(`Country Name.x`,`Indicator Code`,`Indicator Name`))

#rename country code column in co2 data
world_greenhouse <- rename(world_greenhouse, "Country Code" = "iso_code")

#merge with co2 data
df2 <- merge(df2, world_greenhouse, by= c("Country Code", "year"), all=TRUE)

#pivot SAIDI
saidi <- saidi %>%
  pivot_longer(cols = c(2:16), names_to = "year", values_to = "SAIDI")

#pivot SAIFI
saifi <- saifi %>%
  pivot_longer(cols = c(2:16), names_to = "year", values_to = "SAIFI")

#rename countries to prep for merge
saidi <- saidi %>%
  mutate(Country = if_else(Country == "Great Britain","United Kingdom",Country)) %>%
  rename(country = Country)

saifi <- saifi %>%
  mutate(Country = if_else(Country == "Great Britain","United Kingdom",Country))%>%
  rename(country = Country)

df2 <- df2 %>%
  mutate(country = if_else(is.na(country),`Country Name`,country)) %>%
  mutate(country = if_else(country == "Czechia","Czech Republic",country))

df2 <- merge(df2, saifi, by = c("country", "year"), all = TRUE)
df2 <- merge(df2, saidi, by = c("country","year"), all = TRUE)

df2 <- df2 %>%
  rename(renewable_pct = "% renewable") %>%
  rename(thermal_pct = "% thermal") %>%
  rename(hydro_pct = "% hydro")

#add hydro and renewable together to make new renewable value including hydro
df2 <- df2 %>%
  mutate(renewable_pct_all = renewable_pct + hydro_pct)

########
# Save #
########
#greenhouse emissions in kt of CO2 equiv in the world
write.csv(df2, "data - output/df2.csv", row.names = FALSE)
write.csv(energy_df, "data - output/energy_df.csv", row.names = FALSE)