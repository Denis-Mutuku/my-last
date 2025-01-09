# Loading the packages ----

library(tidyverse)
library(Hmisc)

# data ----

starwars <- starwars

# EDA ----

glimpse(starwars)

dim(starwars)

# filtering and selecting ----

data1 <- starwars %>% 
  select(name, height, mass, sex, species) %>% 
  filter(sex %in% c("male", "female"), species == "Human")

# bmi calc ----

data2 <- data1 %>% 
  drop_na(height, mass) %>% 
  mutate(height = height/100,
         bmi = mass/height^2,
         weight = case_when(bmi < 18 ~ "underweight",
                            bmi > 18 & bmi < 25 ~ "normal",
                            bmi > 25 & bmi < 30 ~ "overweight",
                            bmi > 30 ~ "obese"))
# study recruitment ----

data2 <- data2 %>% 
  mutate(participant = ifelse(weight == "normal",
                          "yes", "no"))

# the number of recruits ----

data2 %>% 
  filter(participant == "yes")

# the average bmi ----

data2 %>% 
  summarise(mean_bmi = mean(bmi),
            mean_height = mean(height),
            mean_mass = mean(mass))

# labeling the variables

label(data2$height) = "Height in meters"
label(data2$mass) = "weight in Kgs"
label(data2$participant) = "Those to be recruited"

# filtering those to be included in the study
recruits <- data2 %>% 
  filter(participant == "yes")

# End ----


# practicing causal inference analysis using non experimental methods such as
# difference in difference analysis, regression discontinuity, etc

#These methods can be used to assess the impact of implemented programs 






