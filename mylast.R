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


# more practice on select, filter and mutate and case_when, if else
starwars <- starwars

starwars %>% 
  select(name, height, mass, sex, species) %>% 
  filter(sex %in% c("male", "female"), species == "Human") %>% 
  mutate(height = height/100,
         bmi = mass/height^2,
         body_size = case_when(bmi < 18 ~ "underweight",
                               bmi > 18 & bmi < 25 ~ "normal",
                               bmi > 25 & bmi < 30 ~ "overweight",
                               bmi > 30 ~ "obese"),
         recruitment = ifelse(body_size %in% c("normal", "underweight"),
                              "yes", "no")) %>% 
  filter(recruitment == "yes")
  

starwars %>% 
  select(name, height, mass, sex, species) %>% 
  filter(sex %in% c("male", "female"), species == "Human") %>%
  group_by(sex) %>% 
  summarise(average_height = mean(height, na.rm = TRUE),
            average_mass = mean(mass, na.rm = TRUE)) %>% 
  arrange(desc(sex))

# using the msleep dataset to practice the use of sep and unite

data_msleep <- msleep %>% 
  select(name, vore, sleep_total, sleep_rem)

data_msleep %>% 
  head(n = 10) 

data_msleep <- data_msleep %>% 
  mutate(sleep_total = as.character(sleep_total))

data_msleep$sleep_total1 <- gsub(".", ",", data_msleep$sleep_total)
data_msleep


# starwars dataset

starwars


head(starwars)

tail(starwars)

str(starwars)

dim(starwars)

names(starwars)

distinct(starwars)

duplicated(starwars)

# the next steps is the learning of the use of other functions in the 
# mean way of dealing with other ways to be okay 

library(tidyverse)

str(msleep)
