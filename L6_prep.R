# Lab 6 Development Script ####
# GOVT 329 
# Original Article: Ballard-Rosa et al. (2022) Coming to Terms. IO.
library(tidyverse); library(fixest);library(ggplot2)

#----#

lab6_df <- read_tsv(file = "Coming_to_Terms_data.tab")

# First Figure

lab6_df %>% 
  filter(oecd == 0) %>% 
  group_by(year) %>% 
  summarize(outcome = mean(propDom_all, na.rm = T)) %>% 
  ggplot(data = ., aes(x = year, y = outcome)) + 
  geom_line(linewidth = 1) + xlab("Year") + ylab("Percentage of Bonds in Domestic Domination") 
  

# Add month term for transparency

lab6_df <- lab6_df %>% 
  mutate(month = rep(x = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), nrow(lab6_df)/12))

cleaned_df <- lab6_df %>% 
  filter(oecd == 0) %>% 
  mutate(lag_govt = lag(execrlc_mo, 1)) %>% 
  select(ccode, month, year,propDom_gt1yr, peg, highCBI, lngdppc, lag_govt, yield_avg_mo, maturity_avg_mo) %>% 
  mutate(govt_type = case_when(lag_govt == 1 ~ "Right",
                               lag_govt == 2 ~ "Center",
                               lag_govt == 3 ~ "Left")) %>% 
  rename(country = ccode,
         domestic_pct = propDom_gt1yr, 
         fixed_xr = peg,
         duration = maturity_avg_mo,
         bond_r = yield_avg_mo)

# Does receiving a bond in domestically denominated currency come with costs?

cleaned_df %>% 
  feols(c(duration, bond_r) ~ domestic_pct |country + year)

# Maybe these countries have gotten wealthier?
cleaned_df %>% 
  feols(domestic_pct ~ lngdppc | country + year)

# Tying Hands and Credible Commitments 

cleaned_df %>%
  feols(domestic_pct ~ highCBI | country + year)

# Political Ideology 
cleaned_df %>% 
  feols(domestic_pct ~ i(govt_type, ref = "Center") | country + year)


# Interactions 

m1 <- cleaned_df %>% 
  feols(domestic_pct ~ i(govt_type, ref = "Center") + i(govt_type, highCBI, ref ="Center") | country + year, cluster = ~country)
m1
# Visualization 

as.data.frame(tibble(
  estimate = c(m1$coefficients[1], m1$coefficients[2], m1$coefficients[3], m1$coefficients[4]),
  se = c(m1$coeftable[1,2], m1$coeftable[2,2], m1$coeftable[3,2], m1$coeftable[4,2]),
  group = rep(c("Left", "Right"), 2),
  highCBI = c(0,0,1,1)
)) %>% # What are the following two lines of code calculating? 
  mutate(lwr = estimate - 2 * se,
         upr = estimate + 2 * se) %>% 
  ggplot(data = ., aes(x = factor(highCBI), y = estimate, group = group, color = group)) +
  geom_point(position = position_dodge(width = 0.1)) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), position = position_dodge(width = 0.1), width = 0) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  coord_flip() + xlab("High Central Bank Independence?") + ylab("Average Domestically Denominated") +
  labs(color = "Party Ideology") + theme_minimal()



# What about a fixed XR? Predictions? 

m2 <- cleaned_df %>% 
  feols(domestic_pct ~ i(govt_type, ref = "Center") + i(govt_type, fixed_xr, ref = "Center") | country + year, cluster = ~country)


# How about both? Prediction?

m3 <- cleaned_df %>% 
  mutate(double_commitment = ifelse(highCBI == 1 & fixed_xr == 1, 1, 0)) %>% 
  feols(domestic_pct ~ i(govt_type, ref = "Center") + i(govt_type, highCBI, ref = "Center") + i(govt_type, double_commitment, ref = "Center") | country + year, cluster = ~ country)



