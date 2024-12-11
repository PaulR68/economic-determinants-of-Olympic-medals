rm(list = ls())

library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

#----first step : manage data bases ----
#we import our different data bases 
medals = read.csv("~/FAC/olympics_dataset.csv")
gdp_per_capita = read_excel("~/gdp.xlsx", sheet = "Full data") 
democracy = read.csv("~/democracy.csv")
hdi = read_excel("~/hdr-data.xlsx")
gov_expenditures = read_excel("~/gov_expenditures.xlsx")

#we clean our data bases and create one with all data we need inside
medals = medals %>% 
  select(-player_id, -Event, -NOC) %>% 
  filter(Medal != "No medal") %>% 
  rename(country = Team) %>% 
  rename(year = Year) %>% 
  rename(city = City)

medals_by_country = medals %>% 
  group_by(country, year, city) %>% 
  summarise(
    total_bronze = sum(Medal == "bronze"),
    total_silver = sum(Medal == "Silver"),
    total_gold = sum(Medal == "Gold"),
    total_medals = n()) %>% 
  ungroup()

medals_by_country <- medals_by_country %>%
  mutate(country = str_replace(country, "-\\d+$", "")) #there were different words for a same country so we 

medals_by_country <- medals_by_country %>%
  group_by(country, year, city) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),  
            .groups = "drop")

#gross domestic product per capita
gdp_per_capita = gdp_per_capita %>% 
  select(-countrycode) %>% 
  mutate(growth = as.numeric(growth))
  
database = medals_by_country %>% 
  left_join(gdp_per_capita, by = c("country", "year"))

#human development index
hdi = hdi %>% 
  select(-countryIsoCode, -indexCode, -index, -dimension, - indicatorCode, -indicator, -note) %>% 
  mutate(year = as.numeric(year)) %>% 
  rename(hdi = value)

database = database %>% 
  left_join(hdi, by = c("country", "year"))

#general government total expenditure on recreational and sporting services in % of GDP
gov_expenditures = gov_expenditures %>%
  mutate(across(where(is.numeric), as.character))  %>% 
  rename(country = ...1) %>% 
  pivot_longer (
    col = -country,
    names_to = "year",
    values_to = "expenditures") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(expenditures = as.numeric(expenditures))

database = database %>% 
  left_join(gov_expenditures, by = c("country", "year"))

#host country (we had to create manually this database bc no one existed)

host_data <- tibble(
  year = c(1896, 1900, 1904, 1906, 1908, 1912, 1920, 1924, 1928, 1932, 1936, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020),
  host_country = c("Greece", "France", "United States", "Greece", "Great Britain", "Sweden", "Belgium", "France", "Netherlands", "Unites States", "Germany", "Great Britain", "Finland", "Australia", "Italy", "Japan", "Mexico", "Germany", "Canada", "Russia", "United States", "South Korea", "Spain", "United States", "Australia", "Greece", "China", "Great Britain", "Brazil", "Japan")
)


database = database %>% 
  left_join(host_data, by = "year") %>% 
  mutate(host = ifelse(country == host_country, 1, 0)) #we create a binary variable for the future regression

#democracy
democracy = democracy %>% 
  select(-ID_country_code, -ID_country_year, -ID_region, -ID_subregion, -ID_region_name, -ID_subregion_name, -C_SD11, -C_SD12, -C_SD13, -C_SD14, -C_SD21, -C_SD22, -C_SD23 , -C_SD31, -C_SD32, -C_SD33, -C_SD41, -C_SD42) %>% 
  rename(country = ID_country_name) %>% 
  rename(year = ID_year)

database = database %>% 
  left_join(democracy, by = c("country", "year"))

View(database)

#----regression----
reg = lm(total_medals ~ gdppc + pop + host, data = database)
summary(reg)


  