rm(list = ls())

#loading of libraries we'll need
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(car)
library(sandwich)
library(lmtest)

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
  mutate(country = str_replace(country, "-\\d+$", "")) #there were different words for a same country so we merge these lines

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
  year = c(1896, 1900, 1904, 1906, 1908, 1912, 
           1920, 1924, 1928, 1932, 1936, 1948, 
           1952, 1956, 1960, 1964, 1968, 1972, 
           1976, 1980, 1984, 1988, 1992, 1996, 
           2000, 2004, 2008,  2012, 2016, 2020, 2024),
  host_country = c("Greece", "France", "United States", "Greece", "Great Britain", "Sweden", 
                   "Belgium", "France", "Netherlands", "Unites States", "Germany", "Great Britain",
                   "Finland", "Australia", "Italy", "Japan", "Mexico", "Germany", 
                   "Canada", "Russia", "United States", "South Korea", "Spain", "United States", 
                   "Australia", "Greece", "China", "Great Britain", "Brazil", "Japan", "France"))


database = database %>% 
  left_join(host_data, by = "year") %>% 
  mutate(host = ifelse(country == host_country, 1, 0)) #we create a binary variable for the future regression

#democracy
democracy = democracy %>% 
  select(country = ID_country_name, 
         year = ID_year, 
         representative_gov = C_A1,
         fundamental_rights = C_A2,
         checks_on_gov = C_A3,
         impartial_adm = C_A4) 

database = database %>% 
  left_join(democracy, by = c("country", "year"))

View(database)



#transformation for some variables
database = database %>% 
  mutate(log_gdppc = log(gdppc),
         log_pop = log(pop),
         log_growth = ifelse(growth > 0, log(growth), -log(abs(growth))),
         interaction_log_gdppc_host = log_gdppc*host,
         interaction_log_pop_host = log_pop*host)



#----second step : analysis ----
#gdppc may be endogeneous ()




#----third step : regressions----
reg = lm(total_medals ~ log_gdppc + log_pop + host + interaction_log_gdppc_host + interaction_log_pop_host, data = database)
summary(reg)
  
#heteroscedasticty test
bptest(reg)

#standard robust errors to solve heteroscedasticity
robust_se = vcovHC(reg, type = "HC1")  
coeftest(reg, vcov = robust_se)

#multicolinearity test
vif(reg) #big multicolinearity so we center our variables to solve this problem

database = database %>%
  mutate(log_gdppc_centered = log_gdppc - mean(log_gdppc, na.rm = TRUE),
         log_pop_centered = log_pop - mean(log_pop, na.rm = TRUE),
         host_centered = host - mean(host, na.rm = TRUE),
         interaction_gdppc_host_centered = log_gdppc_centered * host_centered,
         interaction_pop_host_centered = log_pop_centered * host_centered)



reg_centered = lm(total_medals ~ log_gdppc_centered + log_pop_centered + host_centered + interaction_gdppc_host_centered + interaction_pop_host_centered, data = database)
summary(reg_centered)

vif(reg_centered) #no more multicolinearity issue



#regression with others variables (variables that are available for only a few observations) 

reg = lm(total_medals ~ log_gdppc_centered + log_pop + host_centered + interaction_gdppc_host_centered + interaction_pop_host_centered + expenditures + hdi, data = database)
summary(reg)


reg = lm(total_medals ~ log_gdppc_centered + log_pop_centered + host_centered + interaction_gdppc_host_centered + interaction_pop_host_centered + checks_on_gov + impartial_adm, data = database)
summary(reg)

vif(reg)


#----brouillon----

# Histogramme des résidus
hist(residuals(reg), breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

# Test de normalité des résidus
shapiro.test(residuals(reg))  # si p-value > 0.05 : résidus normalement distribués


dwtest(reg)


reg_time <- lm(total_medals ~ log_gdppc_centered + log_pop_centered + host_centered +
                 interaction_gdppc_host_centered + interaction_pop_host_centered + year, 
               data = database)

dwtest(reg_time)


reg_fixed <- lm(total_medals ~ log_gdppc_centered + log_pop_centered + host_centered +
                  interaction_gdppc_host_centered + interaction_pop_host_centered + factor(country), 
                data = database)

dwtest(reg_fixed)


library(nlme)

# Modèle GLS avec structure AR(1)
reg_ar1 <- gls(total_medals ~ log_gdppc_centered + log_pop_centered + host_centered +
                 interaction_gdppc_host_centered + interaction_pop_host_centered + factor(country),
               data = database,
               correlation = corAR1(form = ~ year | country))

summary(reg_ar1)


install.packages("plm")
library(plm)
# Conversion en données panel
panel_data <- pdata.frame(database, index = c("country", "year"))

# Régression avec effets fixes
reg_fe <- plm(total_medals ~ log_gdppc_centered + log_pop_centered + host_centered + 
                interaction_gdppc_host_centered + interaction_pop_host_centered, 
              data = panel_data, model = "within")

summary(reg_fe)


reg_re <- plm(total_medals ~ log_gdppc_centered + log_pop_centered + host_centered + 
                interaction_gdppc_host_centered + interaction_pop_host_centered, 
              data = panel_data, model = "random")

summary(reg_re)

phtest(reg_fe, reg_re)

