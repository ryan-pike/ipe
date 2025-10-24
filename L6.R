# Lab VI: Original Sin or Partisanship as Usual?
# GOVT 329 -- International Political Economy
# Professor Ryan Pike 
# 4 November 2025
#----#

# The Original Sin argument suggests that developing countries are unlikely to 
# be able to issue loans in their own currency as foreign investors are concerned
# about risks related to currency manipulation and more generally the exchange 
# rate. In today's lab, we will investigate the empirical validity of this 
# argument using novel data from 1990-2016 among non-OECD countries. 

# If you are unfamiliar with the OECD, see here: https://en.wikipedia.org/wiki/OECD

# The Original Sin perspective suggests that developing countries have little 
# agency in their ability to negotiate and offer bonds, we will consider this 
# along two dimensions: (1) credible commitments & (2) ideology. Credible 
# commitments refer to a broad category of political phenomena; in the present
# context we can think of them as means by which governments in developing 
# contexts can convincingly commit to not engaging in currency manipulation or 
# risking financial crises. With respect to ideology, we want to be thinking 
# about how parties on the right versus the left may vary in their preferences 
# for domestic monetary policy given the interests they represent.

# Beyond this substantive focus, we are going to be thinking about interactions
# between our explanatory variables and ways to interpret them. We already 
# touched on this in Lab IV when thinking about the differences between MAJ and 
# PR systems at differing levels of electoral competition, but today it will be 
# the main methodological focus.

# Calling in the relevant libraries####
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("fixest", quietly = TRUE)) {
  install.packages("fixest")
}
# Load the libraries
library(tidyverse); library(ggplot2); library(fixest) 

# Read in data 

getwd() # ;setwd() # If needed, set the working directory and remove the first #

bonds_df <- read.csv(file = "bonds_df.csv")

dim(bonds_df)
glimpse(bonds_df) 

# What is the unit of analysis? 


# We have a few variables of interest in the dataset.

# domestic_pct --> percentage of bonds denominated in the domestic currency for 
# a given month. Our primary outcome variable.

# govt_type --> the ideological orientation of the government, it can left, 
# right, or center.

# fixed_xr --> whether the country has a fixed exchange rate

# highCBI --> whether the country has an independent central bank

# bond_r --> the average interest rate of the bonds from that month

# duration --> the average duration (maturity) of the bonds from that month

# STEP 1: Assess the validity of the Original Sin Hypothesis ####

bonds_df %>% 
  group_by(year) %>% 
  summarize(outcome = mean(domestic_pct, na.rm = T)) %>% 
  ggplot(data = ., aes(x = year, y = outcome)) + 
  geom_line(linewidth = 1) + xlab("Year") + ylab("Percentage of Bonds in Domestic Domination") 

# What is your interpretation in terms of the Original Sin argument? 

# Maybe, these countries have gotten wealthier, making them less risky to foreign
# lenders. Let's consider this:

bonds_df %>% 
  feols(domestic_pct ~ lngdppc | country + year, cluster = ~ country)

# Is there evidence of a wealth effect? 

# STEP 2: Thinking about credible commitments #### 

# Governments of developing countries might have alternatives to signal their 
# inability to engage in currency manipulation or induce fiscal crises. In small 
# groups, consider the different sorts of exchange rate and monetary policy 
# arrangements that might let these governments make more credible claims on 
# repaying their commitments. How might they be able to tie their hands via 
# institutions? 

# Fill in the VARIABLE--HERE in the regression equation with the variables from
# the bond_df you think might be good ways of government tying their hands. 

bonds_df %>% 
  feols(domestic_pct ~ VARIABLE--HERE | country + year, cluster = ~ country)
bonds_df %>% 
  feols(domestic_pct ~ VARIABLE--HERE | country + year, cluster = ~ country)

# Is there evidence for developing countries to be able to make credible 
# commitments and alleviate the the challenges of the Original Sin? 

# STEP 3: Introducing Political Ideology 

# Governments on the left and right typically have different ideologies and 
# supporters, we'll focus on ideology today. In small groups, how might the 
# varying positions of the right and left in terms of market independence shape
# their preferences for domestically denominated bonds? Elaborate a hypothesis
# about the level of domestically denominated currency compared to a centrist 
# government for the left and the right in your group. 

# Let's assess these hypotheses in a regression equation:

bonds_df %>% 
  feols(domestic_pct ~ i(govt_type, ref = "Center") | country + year, cluster = ~ country)

# NOTE: the i() here is just letting us choose the baseline for our categorical 
# variable, rather than whatever is first in the dataframe. Given our comparison
# to centrist governments, we want to select "center" as the reference or baseline
# category.

# Does the evidence support your hypothesis? 

# STEP 3: Partisanship and Hand-Tying ####

# We've seen thus far that certain types of institutions can help governments 
# appear more likey to repay their sovereign debt. We've also seen that ideology
# shapes the which governments are more or less likely to use domestically
# denominated sovereign bonds. Now, foreign lenders might have read the political
# science literature and understand that left-leaning governments prefer to have
# domestically denominated bonds, but also the ability to engage in counter-cyclical
# monetary policy. This raises concerns over repayment. Let's consider if there
# are institutional solutions that allow left governments to trade off between 
# these two objectives when in the sovereign bond market.

# To do this, we will interact our institutional variables with partisanship. 
# This lets us assess the difference in denomination rates by left governments 
# under different institutional settings. We'll first take a look at central 
# bank independence. 

# Do we think central bank indepedence will increase or decrease left-leaning 
# governments ability to denominate sovereign bonds in domestic currency? Discuss
# in small groups.

m1 <- bonds_df %>% 
  feols(domestic_pct ~ i(govt_type, ref = "Center") +
          i(govt_type, highCBI, ref = "Center") | country + year,
        cluster = ~country)

# What is our interpretation? 

# In table format this can be a bit challenging. Let's visualize the interactions
# with point estimates and confidence intervals.

as.data.frame(tibble(
  estimate = c(m1$coefficients[1], m1$coefficients[2], m1$coefficients[3], m1$coefficients[4]), # Pull out estimates from model
  se = c(m1$se[1], m1$se[2], m1$se[3], m1$se[4]), # Pull out standard errors from model
  group = rep(c("Left", "Right"), 2),
  highCBI = c(0,0,1,1)
)) %>% # What are the following two lines of code calculating? 
  mutate(lwr = estimate - 2 * se,
         upr = estimate + 2 * se) %>% 
  ggplot(data = ., aes(x = factor(highCBI), y = estimate, group = group, color = group)) +
  geom_point(position = position_dodge(width = 0.2), size = 5) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), position = position_dodge(width = 0.2), width = 0, size = 2.5) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  coord_flip() + xlab("High Central Bank Independence?") + ylab("Average Domestically Denominated") +
  labs(color = "Party Ideology")

# Takeaway? 

# Let's think about a fixed exchange rate now on top of CBI. 

m2 <- bonds_df %>% 
  mutate(double_commitment = ifelse(highCBI == 1 & fixed_xr == 1, 1, 0)) %>% 
  feols(domestic_pct ~ i(govt_type, ref = "Center") + 
          i(govt_type, highCBI, ref = "Center") + 
          i(govt_type, double_commitment, ref = "Center") | country + year,
        cluster = ~ country)
m2

# Takeaway?

# We see something similar to above. CBI is beneficial to left governments to 
# offer domestically denominated bonds, but there isn't much more that a fixed
# XR will do on top of this (beta = 0.04, p = 0.53). CBI seems more important 
# than a fixed XR.

# Step 4: Are there costs then for domestically denominated bonds? Let's consider
# two other components of bonds: maturity and the interest rate.

bonds_df %>% 
  feols(domestic_pct ~ duration + bond_r | country)

# In general, we see that bonds are shorter in duration and the interest rate
# is higher. Why is this coefficient so small? 

#--> The interest rate takes a large set of values. We'll work on interpreting 
# continuous variables in an upcoming lab

range(bonds_df$bond_r, na.rm = T)
