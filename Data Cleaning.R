# GRAPH CODES FOR MY VISUALS

# doownloaded our various libraries

library(sf)
library(readxl)
library(tidyverse)
library(janitor)

# Read in the data from the Pew Research Center's The Global Religious Landscape 
# database ; needed to specify the various columns for each variable
# cleaned names to have useful labels for the graph

world<- read_csv("raw-data/World_Religion_Data.csv", col_types = cols(
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

