# Assignment 1
# Question 4

# Install and load any necessary libraries

libraries <- c("tidyverse", "usmap", "fixest", "patchwork")
for(i in 1:length(libraries)){
  if (!requireNamespace(libraries[i], quietly = TRUE)){
    install.packages(libraries[i])
  }
}
library(tidyverse); library(patchwork); library(usmap)

# Load data, making sure the file is in the folder that is set to the working directory!

getwd() #If the file is not here, move it to this folder! 

a1_df <- read.csv(file = "a1_df.csv")[, -1]

View(a1_df)


# Subquestion 4.2

p1 <- plot_usmap(regions = c("counties"), data = a1_df, exclude =c("AK", "HI"), values = "imports")

p2 <- plot_usmap(regions = c("counties"), data = a1_df, exclude = c("AK", "HI"), values = "spending_change")

(p1 + p2)
# Subquestion 4.3 

# To ease interpretation we'll create a new variable ``quartile_imports'' which
# breaks apart the continuous measure of imports into 4 buckets of increasing 
# exposure to imports. 

a1_df <- a1_df %>% 
  ungroup() %>% 
  mutate(quartile_imports = ntile(imports, 4))

# Plot the appropriate graph with your explanatory or independent variable on the 
# x axis and the outcome or dependent variable on the y axis. This means you 
# will need to fill in variable names for the x and y in the "aes" function. 

a1_df %>% 
  ggplot(data =., aes(x = , y = )) + 
  stat_summary(geom ="point", fun = "mean") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Once you have run the visualization click on the Export button and copy and 
# paste the visualization into your Assignment. Please place this at the bottom
# of the document. It will not count against the 4 page limit. 
