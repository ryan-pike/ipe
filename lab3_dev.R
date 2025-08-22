#Bacinni and Weymouth Prepper ####

library(haven);library(tidyverse); library(patchwork)

county_rep <- read_dta(file = "C:/Users/rmpik/Downloads/Replication File/Replication File/county_level.dta")

library(fixest)

# replicate a model to test 
county_rep %>% 
  filter(year == 2016) %>% 
  feols(ddem_votes_pct1 ~ LAU_unemp_rate_4y + pers_m_total_share_4y + pers_coll_share_4y + 1 | id_state | msl_pc4y2 ~ bartik_leo5,
        vcov = "HC1")

# test appropriatness for basic visualizaitons 

county_rep %>% 
  filter(year == 2016) %>% 
  ungroup() %>% 
  mutate(manu_loss = ntile(msl_pc4y2, 4),
         manu_loss_w = ntile(msl_w_pc4y2, 4),
         manu_loss_nw = ntile(msl_nw_pc4y2, 4)) %>% 
  ggplot(data = ., aes(x = manu_loss, y = ddem_votes_pct1)) + 
  stat_summary(geom = "point", fun = "mean") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar")


county_rep %>% 
  filter(year == 2016) %>% 
  ungroup() %>% 
  mutate(manu_loss = ntile(msl_pc4y2, 4),
         manu_loss_w = ntile(msl_w_pc4y2, 4),
         manu_loss_nw = ntile(msl_nw_pc4y2, 4)) %>%
  feols(ddem_votes_pct1 ~ factor(manu_loss), vcov = "HC1")

# Build a basic dataframe for class 
lab3 <- county_rep %>% 
  filter(year == 2016) %>% 
  mutate(
    dvs = ddem_votes_pct1,
    china_shock = autor,
    manu_loss = msl_pc4y2,
    wh_manu_loss = msl_w_pc4y2,
    nwh_manu_loss = msl_nw_pc4y2,
    male_manu_loss = msl_pc4y2 * msl_male_prop,
    female_manu_loss = msl_pc4y2 * (1 - msl_male_prop)
  ) %>% 
  select(year, state_name, pan_id, dvs, china_shock, manu_loss, wh_manu_loss,
         nwh_manu_loss, male_manu_loss, female_manu_loss) %>% 
  ungroup() %>% 
  mutate(manu_quartile = ntile(manu_loss, 4),
         wh_quartile = ntile(wh_manu_loss, 4),
         nwh_quartile = ntile(nwh_manu_loss, 4),
         f_quartile = ntile(female_manu_loss, 4))

write.csv(lab3, file = "lab3.csv")
