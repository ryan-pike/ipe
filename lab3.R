#Lab 3 -- GOVT 329####
#Identity and Deindustrialization####
#Note: This lab is adapted from a similar lab in D. Queralt's Globalization and 
#Domestic Politics course (F2024).


#install necessary packages####
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
##Load necessary libraries####
library(haven); library(ggplot2); library(patchwork); library(fixest)
#------------------------------------------------------------------------------#

# How do manufacturing workers vote when they lose their jobs to import competition?
# To try and answer this question, we will consider the impact of the "China Shock"
# on the 2016 U.S. presidential election. The data we will be using today is from 
# Baccini and Weymouth (2021). I have simplified the data for ease of exposition, 
# if you are interested, the full paper will be added to the course moodle after
# the lab. 

# The data file we are working with today is called XK.


# Data Import
# Let's import the dataset.

getwd() ## Make sure that the working directory is set to where you have saved the file!!! 

lab3 <- read.csv(file = "lab3.csv")


# The variables we will be using today are:


# `china_shock` measures the localized effect of Chinese imports to the U.S. at the 
# county level in 2016: larger values indicate a greater influx of Chinese goods. This 
# variable measures how vulnerable every specific county is to the arrival of Chinese 
# imports into the U.S., regardless of where the products are sold.

# `dvs` measures the change in democratic vote share between 2012 and 2016. 

# `manu_loss` measures the manufacturing job loss in a county between 2012 and 2015

# `wh_manu_loss` is the analogous measure, but just among non-Hispanic whites.

# `nwh_manu_loss` is the analogous measure, but for all non-whites.

# `female_manu_loss` is the analogous measure, but for females. 

# What's the unit of analysis? To figure this out we need to look at the raw data

View(lab3)

# Now, lets assess the economcs. Is greater import competition associated with 
# increased manufacturing job loss? 

lab3 %>% 
  feols(manu_loss ~ china_shock)

# The interpretation of this regression table is a bit challenging, so let's 
# visualize it.

lab3 %>% 
  ggplot(data = ., aes(x = china_shock, y = manu_loss)) + # Note convention on X v. Y
  geom_smooth()

# Our conclusion? 

# We have evidence of import competition being correlated with heightened manufacturing 
# job loss. What might be the political consequences? Recall that in 2016, President 
# Trump ran on a more protectionist platform, emphasizing his support for tariffs. 
# In contrast, Democratic candidate Hillary Clinton favored the status quo of U.S. 
# economic engagement. 

# In small groups, formulate hypotheses to the following question:
# What is the effect of manufacturing job loss on Democratic vote shares?

#-----------------------------------------------------------------------------#

# To assess this question, we are going to work with quartiled version of manufacturing loss,
# instead of the continuous one that we just used. There are several versions of 
# this variable, we'll start with XK

# We'll start by plotting the relationship between manufacturing loss, our 
# independent variable, and democratic vote share, our dependent or outcome 
# variable. 

lab3 %>% 
  ggplot(data = ., aes(x = manu_quartile, y = dvs)) +
  stat_summary(fun = "mean", geom = "point")

# What can we say about the relationship between manufacturing job loss and 
# voting for the Democrats in 2016?

# So far, we only have point estimates, let's assess the precision of these 
# estimates by adding confidence intervals. 

a <- lab3 %>% 
  ggplot(data = ., aes(x = manu_quartile, y = dvs)) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar")

# How does this revise our interpretation? 

# Let's now consider whether certain this relationship between job loss and 
# voting behavior holds for different groups, namely by gender and race. Given 
# that white males dominate the manufacturing sector, 

# Let's start with gender. 

lab3 %>% 
  ggplot(data = ., aes(x = f_quartile, y = dvs)) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar")

# How does this graph compare with the previous one?

# Now let's consider race. In small groups, discuss whether you expect non-whites
# to vary from whites in terms of their political response to manufacturing 
# job loss. 

b <- lab3 %>% 
  ggplot(data = ., aes(x = nwh_quartile, y = dvs)) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar")

# To ease interpretation lets compare the two graphs side by side:


(a | b)

# How did this compare with your group hypothesis? What might be some reasons 
# behind this finding?

# Extra if time:

# Today we've focused largely on graphical interpretations, but it's important 
# be able to translate between tabular and graphical data representations. Let's 
# take the non-white version and what the regression outputs

lab3 %>% 
  feols(dvs ~ factor(nwh_quartile))

# How can we connect the two? Discuss in small groups and then we will walk 
# through it as a class. (Hint: start with the intercept.)