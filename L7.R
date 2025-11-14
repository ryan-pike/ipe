# Lab VII: FDI Inflows and Outflows
# 18 November 2025 ####
# GOVT 329 ####
# Ryan Pike #### 
#----#

# Today's lab has three questions of interest:

# 1.) Is there a California Effect for labor standards among trade partners?

# 2.) What is the effect of offshoring on incumbent vote share? 

# 3.) How does our interpretation of regression coefficients change as we 
# include fixed effects? 

# Today's lab will be slightly different than our usual set as we've discussed
# one of the results already. Rather than have you come up with hypotheses for 
# this question, we will compare the coefficients across several regressions 
# that include different sets of fixed effects to understand methodologically
# the differences in the controls that we are using. We'll then consider
# the offshoring question.

#----#
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

# Part 1: Fixed Effects and a Labor California Effect? 

getwd() # ;setwd() # If needed, set the working directory and remove the first #

fdi_df <- read.csv("fdi_df.csv")

# Step 1: Replicate the original result:

m1 <- fdi_df %>% 
  feols(lawpos ~ lagged_dv + 
          lawtradecontext1 + 
          fdiflows1 + 
          trade1 +
          income1 + population1  +
          democracy1 + civilwar1 +  hardpta1 + softpta1, cluster = ~imfcode)
m1
# We can check with the following logical statements: 
# We won't have exact numbers but we can see if they are very close from the table in
# the slides

as.numeric(m1$coefficients[1]) - 13.192 < 0.01 # Constant
as.numeric(m1$coefficients[2]) - 0.641 < 0.01 # Lagged Outcome Variable
as.numeric(m1$coefficients[3]) - 0.2 < 0.01 # Trade Law Context

# Why might it be important to replicate before we test the robustness? 

# Step 2: Common Shocks 

# Let's add in a time fixed effect.

fdi_df %>% 
  feols(lawpos ~ lagged_dv + 
          lawtradecontext1 + 
          fdiflows1 + 
          trade1 +
          income1 + population1  +
          democracy1 + civilwar1 +  hardpta1 + softpta1 | year, cluster = ~imfcode)

# Does our result change? Discuss in groups

# Step 3: Country-Specific Characteristics

# Let's now add in unit fixed effects

fdi_df %>% 
  feols(lawpos ~ lagged_dv + 
          lawtradecontext1 + 
          fdiflows1 + 
          trade1 +
          income1 + population1  +
          democracy1 + civilwar1 +  hardpta1 + softpta1 | imfcode + year, cluster = ~imfcode)

# Does our result change? Discuss in groups

# What does this tell us about what might be driving the results in equation 
# without fixed effects? Can you think of a slightly different regression equation
# to probe this? Copy and Paste the above model and delete one variable. 


# Step 4: Understanding a benefit of fixed effects

# Fixed effects aren't perfect, but one major benefit is we don't need to collect
# a lot of additional data, we just need the unit of analysis, here country-year
# and with this we can account for two major sources of confounding. Oftentimes
# it is difficult to collect lots of time-varying confounders, indeed they might 
# missing. 

# Let's take a basic example. Consider the impact of civil war on the ability of 
# the state, NGOs, or IOs to collect data. Do we think civil war will make it 
# more or less likely that other data is missing?

# Let's check:
fdi_df %>% 
  mutate(missing = (is.na(trade1)|is.na(income1)|is.na(population1)|is.na(fdiflows1))) %>% 
  feols(missing ~ civilwar1)

# Fixed effects provide us with a means of controlling for many characteristics 
# in a "data-cheap" fashion as we are able to introduce indicators for each 
# unit and each time period, something we always have! It's not perfect though
# as we can't account for time-varying confounders, but if these are missing 
# we face different problems.


#----#

# Part 2: Political Impacts of Offshoring? #### 

# We'll consider this question in Spain, specifically
# Catalonia, the most industrialized region in the country, between 2000-2004. 
# Our incumbent party in Madrid, is the Partido Popular (People's Party or PP).
# In small groups, come up with a hypothesison the change in incumbent vote share
# in muncipalities that experienced a factory closure due to offshoring. 

# Load the data 

os_df <- read.csv("offshoring_data.csv")

# We have several variables of interest

# post_period = an indicator for the election year following the closure of a 
# factory. In practice, this is 0 for 2000 and 1 for 2004 because we only have 2
# years. This is common to all units 

# offshored_factory = a binary indicator if a municipality had an offshored factory,
# this does not vary over time

# incumbent_voteshare = the Partido Popular vote share in a given municipality-year

head(os_df)
 
# What is the unit of analysis?

# Step 1: No Fixed effects

os_df %>% 
  feols(incumbent_voteshare ~ offshored_factory*post_period, cluster = ~municipal_id)

# Our intercept here is the vote share in 2000 for PP in municipalities without 
# an offshored factory ever. offshored_factory gives us the difference in 2000
# for places that experienced off shoring. post-period is the difference for 
# those that never experienced offshoring in 2004, and finally the interaction term is 
# for those place that experienced offshoring in 2004. We can see this by 
# computing group means along the two variables:

os_df %>% 
  group_by(offshored_factory, post_period) %>%
  summarize(mean = mean(incumbent_voteshare, na.rm = T))

# Let's add a time fixed effect

os_df %>% 
  feols(incumbent_voteshare ~ offshored_factory*post_period | year, cluster = ~municipal_id)

# Why did the post_period disappear? Discuss in small groups.

# Let's add the unit fixed effect

os_df %>% 
  feols(incumbent_voteshare ~ offshored_factory*post_period |municipal_id + year, cluster = ~municipal_id)

# Where are all the variables going? 

# Key Takeaway:

# In simplified settings, such as a two-period study, our year fixed effect is 
# effectively a (0,1) variable. This partially explains why we aren't seeing 
# much change in our interaction term, we already included a time fixed effect
# just by different names. In terms of interpretation, we can interpret the 
# interaction as the average change in PP vote share in places that experienced
# an offshoring effect compared to those that didnt. It is the difference in the 
# vote change between these two groups or a difference-in-difference design. 




