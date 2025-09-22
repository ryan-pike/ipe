# Assignment 1 

library(haven); library(tidyverse); library(usmap); library(tidycensus)

a1_data = haven::read_dta(file = "C:/Users/rmpik/Downloads/114677-V1/Replicate/20150578R1Data/PublicFinancesGoods.dta") %>% 
  group_by(czone) %>% 
  mutate(spending = lag(lgf_exp_tot_pc)) %>% 
  select(czone, year, spending) %>% 
  filter(year == 2007)


president_2000_2016 <- read_dta("C:/Users/rmpik/Desktop/Yale/Prospectus/Energy Transitions/3.so.what/data/usa/president_2000_2016.dta") %>% 
  select(czone, d_imp_usch_pd_0008, cty_fips)

county_df <- tidycensus::fips_codes %>% 
  mutate(fips = as.numeric(paste0(state_code, county_code))) %>% 
  select(fips, county, state_name)

a1_df <- president_2000_2016 %>% 
  left_join(x = ., 
            y = a1_data, 
            join_by(czone == czone)) %>% 
  left_join(x = ., 
            y = county_df, 
            join_by(cty_fips == fips)) %>% 
  select(county, state_name, cty_fips, czone, d_imp_usch_pd_0008, spending) %>% 
  rename(fips = cty_fips,
         commuting_zone = czone, 
         imports = d_imp_usch_pd_0008,
         spending_change = spending) %>% 
  arrange(fips)

write.csv(a1_df, file = "a1_df.csv")

a1_df %>% 
  plot_usmap(regions = c("counties")), exclude =c("AK", "HI"), values = "imports")
 


a1_df <- a1_data %>% 
  group_by(czone) %>% 
  mutate(spending = lag(lgf_exp_tot_pc)) %>% 
  select(czone, year, spending) %>% 
  filter(year == 2007) %>% 
  left_join(x = .,
            y = president_2000_2016, 
            join_by(czone == czone), multiple = "first")
a1_df %>% 
  ungroup() %>% 
  mutate(quartile_imports = ntile(d_imp_usch_pd_0008, 4)) %>% 
  ggplot(data =., aes(x = quartile_imports, y = spending)) + 
  stat_summary(geom ="point", fun = "mean") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)
