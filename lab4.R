# Lab 4
# GOVT 329

# Institutions, Electoral Competition and Fossil Fuel Taxation ####

# Gasoline taxes are arguably the most common type of climate taxation, almost 
# every country in the world places an excise (sin) tax on gasoline consumption. 
# There is ample variation, however, across countries, and within them over time 
# in terms of the actual tax rate. In today's lab, we will take the ideas about 
# institutions and preference aggregation and apply them to the domain of gas
# taxes. The original paper is Finnegan (2022), which will be uploaded after 
# class today. 

# Check for necessary packages and load in the libraries. 

if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven")
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("fixest", quietly = TRUE)) {
  install.packages("fixest")
}

library(haven); library(ggplot2); library(patchwork); library(fixest); library(tidyverse)

# Set the working directory and import the data:

getwd() #setwd() # if needed!

lab4 <- read.csv(file = "lab4.csv")


# We will be considering the following variables:

# gas_tax = gasoline excise tax rate in nominal USD cents per liter.

# electoral_comp = this measures the probability of the plurality party being 
# removed at the next election. Higher values denote greater competition 

# pr = a binary indicator for whether a country has proportional representation 

# elect_year = a binary indicator if a given country has an election that year

# gas_cons = gasoline consumption per capita

#------------------------------------------------------------------------------#

# Let's start by taking a look at the data and determine the unit of analysis.

glimpse(lab4)
dim(lab4)

# Let's start by considering the impact of electoral institutions. Institutions 
# translate preferences into power. Electoral institutions shape the way the votes 
# of individuals are converted into seats and ultimately political power. We will 
# consider two broad types of electoral institutions today: majoritarian versus 
# proportional representation (PR). 

# In small groups, discuss the primary differences between the two and how this 
# shapes representation and accountability to voters. Then consider how this might impact
# willingness to impose taxes on voters. Hint: think about electoral competition

# First, lets look at the distribution of countries and institutions:

lab4 %>% 
  filter(!duplicated(country)) %>% 
  select(country, pr) %>% 
  arrange(desc(pr))

# Are there any trends we notice in the data?

# Next, lets consider whether there is a difference in the average gas tax 
# between these two sets of electoral institutions.

# First, let's just look at the trends for each country in the gasoline tax.

lab4 %>% 
  ggplot(data = ., aes(x = year, y = gas_tax, color = factor(pr))) +
  geom_line() +
  facet_wrap(facets =~country)

lab4 %>%
  ggplot(data = ., aes(x = pr, y = gas_tax, fill = factor(pr))) +
  stat_summary(fun = 'mean', geom = 'bar')

# What might be some issues with this comparison? ####

# Let's assess in a regression format:

lab4 %>% 
  feols(gas_tax ~ pr)

lab4 %>% 
  feols(gas_tax ~ pr + eu)

# How should we interpret our original estimate of PR in light of the second 
# estimate? 

#-----------------------------------------------------------------------------#

# Institutions are sticky, meaning they don't often change over time. So it's 
# challenging to assess how they can impact something that varies dynamically 
# such as the gasoline tax. Once established, other characteristics of politics
# might impact the ways that countries tax pollution. We'll consider political 
# competition. In small groups, discuss what you think the relationship between
# political competition and gasoline taxation is. First, consider which variables
# can provide us with a proxy of political competition, then generate hypotheses
# for these variables' effect on the level of gasoline taxation.

lab4 %>% 
  feols(gas_tax ~ elect_year | country + year)

# In your groups discuss, does this support your hypothesis?

# Next let's consider the other measure of competition:

lab4 %>%
feols(gas_tax ~ electoral_comp | country + year)

# In your groups discuss, does this support your hypothesis?
# What might explain the difference between the two?

#------------------------------------------------------------------------------#

# Now let's bring institutions back in. We know that PR is associated with 
# higher levels of gasoline taxation. We also know that electoral competition is
# associated with lower levels of gasoline taxation. An open question is whether
# electoral competition's impact on gasoline tax depends on the electoral system. 
# This is an exploratory analysis, beyond the scope of the paper, but worth 
# considering as we try to understand preference aggregation.

# To assess this we will consider the impact of electoral competition **within**
# each type of electoral system. We can achieve this by interacting or multiplying 
# the two variables together. 

lab4 %>% 
  feols(gas_tax ~ electoral_comp * pr | country + year)

# We can also inspect this visually.

lab4 %>% 
  ggplot(data = ., aes(x = electoral_comp, y = gas_tax, group = pr, color = factor(pr))) +
  geom_point() +
  geom_smooth(method = 'lm')

# How should we think about this? What does this tell us about preference aggregation 
# and electoral accountability more generally?

# Takeaway? Electoral competition translates differently across institution types
# in majoritarian systems in which losing a swing seat might mean the party is 
# out of office, electoral competition is more salient. In contrast, in PR systems
# the coalition may survive with some minor replacements, meaning that politicians 
# are more insulated from electoral backlash. Again to reiterate, this is an 
# exploratory analysis and further tests are needed. 
