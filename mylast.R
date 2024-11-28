### Loading the packages

library(tidyverse)

# data

starwars <- starwars

# EDA 

glimpse(starwars)

dim(starwars)

# filtering and selecting

starwars %>% 
  select(name, height, mass, sex, species) %>% 
  filter(sex %in% c("male", "female"), species == "Human")
