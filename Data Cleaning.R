# GRAPH CODES FOR MY VISUALS

# doownloaded our various libraries

library(sf)
library(readxl)
library(tidyverse)
library(janitor)

##############################################################

# DATA CLEANING FOR THE GENDER PLOT!

# Got the data for the official 2015 korean religions count data
x <- read_xlsx("Korea/raw-data/2015_Religions_Korea.xlsx") %>% 
  clean_names()

#fixes merged column problem from excel for the dataset

test<- x %>% fill(gender) %>% fill(korean_region)

#trying to filter data for only the total number of korean data
gender_data <- x %>% 
  filter(korean_region == "All of Korea") %>% 
  filter(gender != "Korea - Total") %>% 
  filter(age == "Total")

#needs to be a data frame in order to use gather
gender_data <- data.frame(gender_data)

#gather flips the rows and columns
gender_data_vertical <- gather(gender_data)


gender_data_vertical <- gender_data_vertical[11:30,] 


gender_data_vertical<- gender_data_vertical %>% 
  mutate(gender = ifelse(row_number()%%2 == 0,
                         "Female",
                         "Male")) %>% 
  mutate(religion = ifelse(row_number()%%2 == 0,
                           str_c(key, "F"),
                           str_c(key, "M")))

gender_data_vertical <- transform(gender_data_vertical, value = as.numeric(value))

##############################################################

# KOREAN AGE DATA CLEANING

age_data <- x %>% 
  filter(korean_region == "All of Korea") %>% 
  filter(age != "Total") %>% 
  filter(gender == "Korea - Total") %>%
  mutate(age_number = as.numeric(substr(age,1,2))) %>% 
  mutate(age2 = as.numeric(age_number)^2)


##############################################################

# WORLD RELIGION DATA CLEANING!!!

# Read in the data from the Pew Research Center's The Global Religious Landscape 
# database ; needed to specify the various columns for each variable
# cleaned names to have useful labels for the graph

world<- read_csv("Korea/raw-data/World_Religion_Data.csv", col_types = cols(
    Country = col_character(),
    Christian = col_double(),
    Muslim = col_double(),
    Unaffiliated = col_double(),
    Hindu = col_double(),
    Buddhist = col_double(),
    Folk = col_double(),
    Other = col_double(),
    Jewish = col_double())) %>% 
  clean_names()

# Need to clean up my data sources to have only the overall world totals data
# utilized summarize function in order to get totals of every single religion; 
# numbers based upon information from totlas of every nation, numbers are the 
# percentages of people who practice the religion

world_summary<- world %>% 
  summarize(christian = mean(as.numeric(christian)),
            muslim = mean(as.numeric(muslim)),
            unaffiliated = mean(as.numeric(unaffiliated)),
            hindu = mean(as.numeric(hindu)),
            buddhist = mean(as.numeric(buddhist)),
            folk = mean(as.numeric(folk)),
            other = mean(as.numeric(other)),
            jewish = mean(as.numeric(jewish))) 

# Get the separate data for korean numbers totals from the korean religion dataset
# Took out the country column as it was unncecessary

korea_totals <- world %>% 
  filter(country == "South Korea") %>% 
  select(-matches("country"))

# Merged the world data with the korea totals data in order to make a plot that could 
# compare percentages of the world religions with the korean religions

world_korea_summary <- rbind(data.frame(world_summary),data.frame(korea_totals))

# flipped the rows and columns in order to have data in a format I can put in a bar
# plot with groupings

vertical_world <- gather(world_korea_summary)%>% 
  mutate(country = ifelse(row_number()%%2 == 1, "World","South Korea"))

#Used write_rds() to write out vertical world data into a separate rds file
write_rds(vertical_world, "Korea/world.rds")
write_rds(gender_data_vertical, "Korea/gender.rds")
write_rds(age_data, "Korea/age.rds")

