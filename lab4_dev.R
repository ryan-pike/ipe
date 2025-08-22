# Lab 3 Prepper 


library(tidyverse); library(fixest); library(patchwork); library(haven)


finnegan_rep <- readr::read_delim(file = "C:/Users/rmpik/Downloads/Finnegan_Changing prices in a changing climate.tab")

glimpse(finnegan_rep)

# Make data more transparent for students

lab4 <- finnegan_rep %>% 
  mutate(country = case_when(
    country_num == 1 ~ "australia",
    country_num == 2 ~ "austria",
    country_num == 3 ~ "belgium",
    country_num == 4 ~ "canada",
    country_num == 5 ~ "denmark",
    country_num == 6 ~ "finland",
    country_num == 7 ~ "france",
    country_num == 8 ~ "germany",
    country_num == 9 ~ "greece",
    country_num == 10 ~ "ireland",
    country_num == 11 ~ "italy",
    country_num == 12 ~ "japan",
    country_num == 13 ~ "netherlands",
    country_num == 14 ~ "new zealand",
    country_num == 15 ~ "norway",
    country_num == 16 ~ "portugal",
    country_num == 17 ~ "spain",
    country_num == 18 ~ "sweden",
    country_num == 20 ~ "uk",
    country_num == 21 ~ "usa"
    ),
    pr = ifelse(country_num %in% c(2,3,5,6,8,9,13,14,15,16,17,18), 1, 0),
    eu = ifelse(country_num %in% c(2,3,5,6,7,8,9,10,11,13,16,17,18,20), 1, 0)) %>% 
  select(year, country, country_num, gas_excisetax_nomUSDPPP, perchange, elect_comp_pl, pr, elect_year, gas_percap_litres_100, eu) %>% 
  rename(gas_tax = gas_excisetax_nomUSDPPP,
         gas_cons = gas_percap_litres_100,
         electoral_comp = elect_comp_pl)
write_csv(file = "lab4.csv", x = lab4)

lab4 %>% 
ggplot(data = ., aes(x = year, y = electoral_comp))+
  geom_line()+
  facet_wrap(facets = ~country)
  
# Testing out basic probes of the argument and related to institituions ####


# Institutions and Taxation -- Building on intuition from Rogowski 


lab4 %>%
  ggplot(data = ., aes(x = pr, y = gas_tax, fill = pr)) +
  stat_summary(fun = 'mean', geom = 'bar')

# Electoral Competition and Taxation -- Finnegan argument 

lab4 %>% 
  ggplot(data = ., aes(x = electoral_comp, y = gas_tax, group = pr, color = pr)) +
  geom_point() + 
  geom_smooth(method = "lm")


# Political Cycle 

lab4 %>% 
  feols(gas_tax ~ elect_year | country + year)

lab4 %>% 
  filter(country_num != 21) %>% 
  feols(gas_tax ~ electoral_comp| country + year)

lab4 %>% 
  filter(pr == 1) %>% 
  feols(gas_tax ~ electoral_comp | country + year)



ggplot(data = finnegan_rep, aes(x = year, y = gas_excisetax_nomUSDPPP)) + 
  geom_line() +
  facet_wrap(facets = ~country_num)
