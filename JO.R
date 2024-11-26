rm(list = ls())

library(dplyr)
library(readxl)

medals = read.csv("~/FAC/olympics_dataset.csv")


#first we have to clean our data bases and create one with all data we need inside
medals = medals %>% 
  select(-player_id, -Event, -NOC) %>% 
  filter(Medal != "No medal") %>% 
  rename(country = Team) %>% 
  rename(year = Year)

medals_by_country = medals %>% 
  group_by(country, year, City) %>% 
  summarise(
    total_bronze = sum(Medal == "bronze"),
    total_silver = sum(Medal == "Silver"),
    total_gold = sum(Medal == "Gold"),
    total_medals = n()
  ) %>% 
  ungroup()


gdp_per_hab = read_excel("~/gdp.xlsx", sheet = "Full data") 
gdp_per_hab = gdp_per_hab %>% 
  select(-countrycode) %>% 
  mutate(growth = as.numeric(growth))
  
database = medals_by_country %>% 
  left_join(gdp_per_hab, by = c("country", "year"))

democracy = read.csv("~/democracy.csv")

democracy = democracy %>% 
  select(-ID_country_code, -ID_country_year, -ID_region, -ID_subregion, -ID_region_name, -ID_subregion_name, -C_SD11, -C_SD12, -C_SD13, -C_SD14, -C_SD21, -C_SD22, -C_SD23 , -C_SD31, -C_SD32, -C_SD33, -C_SD41, -C_SD42) %>% 
  rename(country = ID_country_name) %>% 
  rename(year = ID_year)


reg = lm(total_medals ~ gdppc + pop + hdi, data = database)
summary(reg)

View(database)


hdi = read_excel("~/hdr-data.xlsx")
hdi = hdi %>% 
  select(-countryIsoCode, -indexCode, -index, -dimension, - indicatorCode, -indicator, -note) %>% 
  mutate(year = as.numeric(year)) %>% 
  rename(hdi = value)

str(hdi)

database = database %>% 
  left_join(hdi, by = c("country", "year"))

View(database)




database = database %>% 
  left_join(democracy, by = c("country", "year"))
gov_expenditures = read_excel("~/gov_expenditures.xlsx")

