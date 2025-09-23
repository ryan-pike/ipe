# GOVT 329 
# 25 September 2025

# R Demo Script #####

# This script will introduce you to some key tidbits of information that we 
# will use over the course of the semester. If you've successfully downloaded
# R you should be able to execute all of these. 

# A few notes before we begin:


## R Studio and computers in general are stupid, ChatGPT does not change this!!
## What I mean by this is you need to make sure that everything is set up correctly
## or an error will be thrown immediately. If the input is not correct, R Studio
## cannot infer what you wanted, it will just throw an error. While I will be 
## providing you with the majority of the code in the labs. Things like syntax and 
## spelling are very important which means you need to proofread your input. Even
## if it's a single variable name!

## <-- The hashtag symbol will ``comment'' out a line of code. This means that 
## R Studio will interpret it as text rather than something to be executed. It 
## will only comment out text to the RIGHT of the hashtag

1 + 1 # An example


## To execute a line of code we place the cursor on that line and press CTRL + Enter
## for Windows. On Mac I believe it is the Mac Key + Enter. The output will 
## appear in the Console or if it is a visualization in the Plots Pane.

## Easy Pitfalls to avoid:

# 1.) Make sure the working directory is set up to where your file for that day's
# lab. 

getwd() # <-- If the files are not in this folder on your computer, you cannot read them in

setwd() # We can change the directory with this command and giving a file path in ""

# Note: if you use a windows computer, you need to replace the \ in the file 
# path with \, if you copy the location of your files from the explorer. 

# You can alternatively set the working directory by clicking on the clear button
# in the Files pane and setting the currently viewed file directory as the 
# working directory. 

# 2.) Make sure that the required packages are downloaded and called. We will use some
# packages, premade code that does a lot of the work for us, to faciitate the labs. 
# These packages are not a part of ``base'' R or R Studio and so we will have to 
# download them. We only need to download them once, but each time we open an R
# session, we will need to call the package with the library() function!

# Example:

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
} # This should install the package unless you've used R before!

library(tidyverse) # This will call the package and give you access to the functions inside the package!
# We need to use the library function each time we open R. 

#------------------------------------------------------------------------------#

# We will now briefly go through a practice "made-up" example of a simple exercise
# we might do in the labs.

# We need to call/download a few libraries:
if (!requireNamespace("fixest", quietly = TRUE)) {
  install.packages("fixest")
}
library(fixest)

if (!requireNamespace("usmap", quietly = TRUE)) {
  install.packages("usmap")
}
library(usmap)


# We will use some population data that comes with the usmap package

population_df <- usmap::citypop

# We can View the data like this:

View(population_df)

# What do we see? 

# Let's see whether there is a correlation between latitude and population

population_df %>% 
  feols(fml = city_pop ~ lat)

# The regression table appears in the console. Is there a relationship?

#We can also visualize this relationship at the state level with a map:

population_df %>% 
  plot_usmap(data = ., 
             regions = "states", 
             exclude = c("AK", "HI"), 
             values = "city_pop")

# If we have further questions about a function, we can ask for help:

?plot_usmap

# If we want to see all the functions in a package's library we use two colons ::
# and press TAB

#usmap::
  
  
# We can also make graphs with point estimates and confidence intervals like 
# we learned about in Lab 1
  
  
# First we'll define a new variable that takes a value of 1 if the latitutde 
# of a given city is above the mean (national average)
  
population_df$above_av <- ifelse(population_df$lat > mean(population_df$lat), 1, 0)
# The arrow, a combination of < + -, assigns a new variable to the population 
# dataframe using the conditional statement for above average latitude.

# We can then plot this variable as our x-axis or explanatory variable and the
# city population as our y-axis or outcome variable. This lets us assess any 
# correlation visually. 

population_df %>% 
  ggplot(data = ., aes(x = above_av, y = city_pop)) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0)

