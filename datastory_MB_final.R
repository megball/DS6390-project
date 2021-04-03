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

##############
## Analyze ##
#############
str(df2)
df2$year <- as.numeric(df2$year)

#plot renewables by emissions for US
df2 %>%
  filter(country == "United States") %>%
  filter(year < 2016) %>%
  ggplot(aes(x = year, group = 1)) +
  geom_line(aes(y=renewable_pct_all), color = "#006699", size = 1.5) +
  geom_line(aes(y=ghg_per_capita), color = "#006699", size = 1.5) +
  labs(title="Percent Energy Generated by Renewables has increased in the US since 2000",
       subtitle="while overall per capita greenhouse gas emissions have decreased slightly") +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 13),
        legend.position = "")

#plot renewables for Europe
df2 %>%
  filter(!(country %in% "United States")) %>%
  filter(year < 2016) %>%
  ggplot(aes(x = year, group = country, color = country)) +
  geom_line(aes(y=renewable_pct_all), size = 1.5) +
  labs(title="",
       subtitle="") +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 13),
        legend.position = "")

#add coding for US only
df2 <- df2  %>%
  mutate(country_1=ifelse(country=="United States", "1", "0"))

#pick only a few countries to plot
df2 %>%
  filter(country %in% c("United States", "United Kingdom", "Germany", "France", "Ireland")) %>%
  filter(year < 2016) %>%
  filter(year >= 2010) %>%
  ggplot(aes(x = year, group = country, color = country_1)) +
  geom_line(aes(y=renewable_pct_all), size = 1.5) +
  labs(title="Percent energy generated by renewables in the US has remained relatively flat",
       subtitle="from the years 2010 to 2015",
       x = "Year") +
  scale_color_manual(values = c("1"="#669933", "0"="darkgray"), guide = FALSE )+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12))

df2 %>%
  filter(country %in% c("United States", "United Kingdom", "Germany", "France", "Ireland")) %>%
  filter(year < 2016) %>%
  filter(year >= 2010) %>%
  ggplot(aes(x = year, group = country, color = country)) +
  geom_line(aes(y=renewable_pct_all), size = 1.5) +
  labs(title="Percent energy generated by renewables in the US has remained relatively flat",
       subtitle="from the years 2010 to 2015",
       x = "Year") +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 13))


#Slope chart, all countries, US highlighted
df2 %>%
  filter(year == 2000 | year == 2015) %>%
  ggplot(aes(x = year, group = country, color = country_1)) +
  geom_line(aes(y=renewable_pct_all), size = 1.5) +
  scale_color_manual(values = c("1"="#006699", "0"="darkgray"), guide = FALSE )+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 13),
        legend.position = "")

#Slope chart, subset of countries
df2 %>%
  filter(country %in% c("United States", "United Kingdom", "Germany", "France", "Ireland")) %>%
  filter(year == 2000 | year == 2015) %>%
  ggplot(aes(x = year, group = country, color = country_1)) +
  geom_line(aes(y=renewable_pct_all), size = 1.5) +
  scale_color_manual(values = c("1"="#006699", "0"="darkgray"), guide = FALSE )+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 13),
        legend.position = "")

#percent thermal
df2 %>%
  filter(country %in% c("United States", "United Kingdom", "Germany", "France", "Ireland")) %>%
  filter(year < 2016) %>%
  filter(year >= 2010) %>%
  ggplot(aes(x = year, group = country, color = country_1)) +
  geom_line(aes(y=thermal_pct), size = 1.5) +
  scale_color_manual(values = c("1"="#990000", "0"="darkgray"), guide = FALSE )+
  labs(title="As of 2015, the US had the highest percent generated by thermal sources",
       subtitle="compared to France, Germany, Ireland, and the UK",
       x = "Year") +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12))
 
##############
## Emissions #
################

#calculate world average by year
df2 %>%
  group_by(year) %>%
  filter(year == 2016)%>%
  summarise(average = mean(ghg_per_capita, na.rm = TRUE))

df2_2016 <- df2 %>%
  filter(year == 2016)

df2_2016 <- df2_2016 %>%
  mutate(ghg_z = (ghg_per_capita - mean(ghg_per_capita, na.rm = TRUE))/sd(ghg_per_capita, na.rm = TRUE))  # compute normalized mpg

df2_2016$ghg_type <- ifelse(df2_2016$ghg_z < 0, "below", "above")  # above / below avg flag
df2_2016 <- df2_2016[order(df2_2016$ghg_z), ]  # sort
df2_2016$country <- factor(df2_2016$country, levels = df2_2016$country)


df2_2016 %>%
  ggplot(aes(x=country, y=ghg_z, label=ghg_z)) + 
  geom_bar(stat='identity', aes(fill=ghg_type), width=.5)  +
  scale_fill_manual(name="Emissions", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()

#make a bar chart comparing the countries to global average

df2 %>%
  filter(country %in% c("United States", "United Kingdom", "Germany", "France", "Ireland")) %>%
  filter(year == 2000 | year == 2015) %>%
  ggplot(aes(x = year, group = country, color = country_1)) +
  geom_line(aes(y=ghg_per_capita), size = 1.5) +
  scale_color_manual(values = c("1"="#006699", "0"="darkgray"), guide = FALSE )+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 13),
        legend.position = "")

df2 %>%
  filter(country %in% c("United States", "United Kingdom", "Germany", "France", "Ireland")) %>%
  filter(year < 2016) %>%
  filter(year >= 2010) %>%
  ggplot(aes(x = year, group = country, color = country_1)) +
  geom_line(aes(y=ghg_per_capita), size = 1.5) +
  scale_color_manual(values = c("1"="#006699", "0"="darkgray"), guide = FALSE )+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 13),
        legend.position = "")

df2 %>%
  filter(country %in% c("United States", "United Kingdom", "Germany", "France", "Ireland")) %>%
  filter(year == 2015) %>%
  ggplot(aes(x = country, y=ghg_per_capita, fill = country_1)) +
  geom_col() +
  scale_fill_manual(values = c("1"="#990000", "0"="darkgray"), guide = FALSE )+
  labs(y = "Greenhouse gas emissions per capita") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(13),
        axis.text.y = element_text(13),
        panel.background = element_blank(),
        legend.position = "")

#plot SAIDI
df2 %>%
  filter(country %in% c("United States", "United Kingdom", "Germany", "France", "Ireland")) %>%
  filter(year < 2017) %>%
  filter(year >= 2012) %>%
  ggplot(aes(x = year, group = country, color = country_1)) +
  geom_line(aes(y=SAIDI), size = 1.5) +
  scale_color_manual(values = c("1"="#006699", "0"="darkgray"), guide = FALSE )+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 13),
        legend.position = "")

########
# mix #
########

energy_df %>%
  filter(country_name %in% c("United States", "United Kingdom", "Germany", "France", "Ireland")) %>%
  filter(type %in% c('Geothermal','Nuclear','Wind','Hydro','Solar')) %>%
  ggplot(aes(x = year, y = per_capita_kwh, color = type)) +
  geom_point() +
  facet_wrap(~country_name)
