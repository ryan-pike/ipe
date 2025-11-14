# Offshoring and its Electoral Consequences Development Script ####

# 12 November 2025 ####
library(haven); library(tidyverse); library(fixest); library(readxl)
off_df <- read_dta(file = "offshoring_lab/National Replication Data.dta")
off_region <- read_dta(file = "offshoring_lab/Regional Replication Data.dta")
e2000 <- read_xlsx(path = "offshoring_lab/G2000_mesas.xlsx", sheet = 8)
off_df %>% 
  group_by(codi) %>% 
  mutate(treated = any(treatment_postp==1),
         omit = any(is.na(treatment_postp))) %>%
  filter(omit == 0) %>% 
  feols(spp ~ treated * postp |, cluster = ~codi)


cat_df <- e2000 %>% 
  filter(COD.CCAA == "09") %>% 
  mutate(spp = (PP/VOTOS.CANDIDATURAS)*100,
         spp = round(spp, 4),
         spsoe = (PSOE/VOTOS.CANDIDATURAS)*100)%>% 
  select(cod.mun, ANYO, PROVINCIA, MUNICIPIO, spp, spsoe)

rob_df <- read_dta(file = "offshoring_lab/Table 3 Replication Data.dta")



fdi_df <- read_csv(file = "offshoring_lab/GMP replication data.csv")

m1 <- fdi_df %>% 
  arrange(imfcode, year) %>% 
  group_by(imfcode) %>% 
  mutate(lagged_dv = lag(lawpos, 1),
         lawtradecontext1 = lag(lawtradecontext, 1),
         fdiflows1 = lag(FDIflows, 1),
         trade1 = lag(Trade, 1), 
         income1 = lag(income, 1),
         population1 = lag(population, 1),
         democracy1 = lag(democracy, 1),
         civilwar1 = lag(civilwar, 1),
         hardpta1 = lag(hardpta, 1),
         softpta1 = lag(softpta, 1)) %>% 
  ungroup() %>% 
  feols(data = ., lawpos ~ lagged_dv + 
          lawtradecontext1 + 
          fdiflows1 + 
          trade1 +
          income1 + population1  +
          democracy1 + civilwar1 +  hardpta1 + softpta1, cluster = ~imfcode)
m2 <- fdi_df %>% 
  arrange(imfcode, year) %>% 
  group_by(imfcode) %>% 
  mutate(lagged_dv = lag(lawpos, 1),
         lawtradecontext1 = lag(lawtradecontext, 1),
         fdiflows1 = lag(FDIflows, 1),
         trade1 = lag(Trade, 1), 
         income1 = lag(income, 1),
         population1 = lag(population, 1),
         democracy1 = lag(democracy, 1),
         civilwar1 = lag(civilwar, 1),
         hardpta1 = lag(hardpta, 1),
         softpta1 = lag(softpta, 1)) %>% 
  ungroup() %>% 
  feols(data = ., lawpos ~ lagged_dv + 
          lawtradecontext1 + 
          fdiflows1 + 
          trade1 +
          democracy1 + civilwar1 +  hardpta1 + softpta1 |imfcode + year, cluster = ~imfcode)
texreg::texreg(l = list(m1, m2),
               custom.coef.map = list("lagged_dv" = "Lagged DV",
                                      "lawtradecontext1" = "Bilateral Trade Context: Law"),
               stars = c(0.1,0.05, 0.01),
               threeparttable = T)
fdi_df %>% 
  ggplot(data = ., aes(x = year, y = lawtradecontext)) + 
  stat_summary(fun = "mean", geom = "pointrange") +
  theme_minimal() + ylab("Average Export Market Labor Rights") + xlab("Year")

fdi_df %>% 
  ggplot(data = ., aes(x = year, y = lawpos)) + 
  stat_summary(fun = "mean", geom = "pointrange") + 
  theme_minimal() + ylab("Average Labor Rights") + xlab("Year")

fdi_df %>%
  ggplot(data = ., aes(x = lawtradecontext, y = lawpos)) + 
  geom_point() + geom_smooth(method = lm) + theme_minimal() +
  xlab("Average Export Market Labor Rights") + ylab("Average Labor Rights")

fdi_df %>% 
  group_by(year) %>% 
  mutate(year_average = mean(lawpos, na.rm = T),
         year_average_x = mean(lawtradecontext, na.rm = T),
         demeaned = lawpos - year_average,
         demeaned_x = lawtradecontext - mean(lawtradecontext, na.rm = T),
         decade = case_when(year %in% 1980:1989 ~ 1, year %in% 1990:1999 ~ 2, year %in% 2000:2002 ~ 3),
         decade = factor(decade),
         quartile = factor(ntile(lawtradecontext, 4))) %>% 
  ungroup() %>% 
  mutate(lagged_ev = lag(lawtradecontext, 1),
         change = lawtradecontext - lagged_ev, 
         lagged_dv = lag(lawpos, 1), 
         change_dv = lawpos - lagged_dv,
         group = paste0(year, quartile)) %>% 
  ggplot(data = ., aes(x = year, y = change_dv) + 
  stat_summary(geom = "point", fun = "mean")
  
  
# Lab 7 Prep 

off_df <- read_dta(file = "offshoring_lab/National Replication Data.dta")
off_df <- off_df %>% 
  rename(municipal_id = codi, 
         incumbent_voteshare = spp,
         post_period = postp,
         offshored_factory = treatment_postp,
         job_loss = intensity_postp) %>% 
  group_by(municipal_id) %>% 
  mutate(offshored_factory = as.numeric(any(offshored_factory==1))) %>% 
  select(municipal_id, year, post_period, offshored_factory, incumbent_voteshare) %>% filter(!is.na(offshored_factory))
write.csv(off_df, file = "offshoring_data.csv")  

fdi_df <- read_csv(file = "offshoring_lab/GMP replication data.csv")
fdi_df <- fdi_df %>% 
  arrange(imfcode, year) %>% 
  group_by(imfcode) %>% 
  mutate(lagged_dv = lag(lawpos, 1),
         lawtradecontext1 = lag(lawtradecontext, 1),
         fdiflows1 = lag(FDIflows, 1),
         trade1 = lag(Trade, 1), 
         income1 = lag(income, 1),
         population1 = lag(population, 1),
         democracy1 = lag(democracy, 1),
         civilwar1 = lag(civilwar, 1),
         hardpta1 = lag(hardpta, 1),
         softpta1 = lag(softpta, 1)) %>% 
  select(country, year, lagged_dv, lawtradecontext1, fdiflows1, trade1, income1,
         population1, democracy1, civilwar1, hardpta1, softpta1, lawpos) %>% 
  arrange(country, year)
write.csv(fdi_df, file = "fdi_df.csv")  


