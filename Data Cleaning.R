# GRAPH CODES FOR MY VISUALS

# doownloaded our various libraries

library(sf)
library(leaflet)
library(readr)
library(readxl)
library(tidyverse)
library(nnet)
library(rgdal)
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
  clean_names() %>% 
  na.exclude()

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
  filter(country == "Korea, Republic of") %>% 
  select(-matches("country"))

# Merged the world data with the korea totals data in order to make a plot that could 
# compare percentages of the world religions with the korean religions

world_korea_summary <- rbind(data.frame(world_summary),data.frame(korea_totals))

# flipped the rows and columns in order to have data in a format I can put in a bar
# plot with groupings

vertical_world <- gather(world_korea_summary)%>% 
  mutate(country = ifelse(row_number()%%2 == 1, "World","South Korea"))

######################################################################

# KOREA MAP DATA WRANGLING
# used the korea religions dataset :D

clean <- x %>% 
  filter(gender == "Total") %>% 
  tail(17) %>% 
  select(-matches("age")) %>% 
  select(-matches("gender"))

colnames(clean)[1]<-"id"

############################################################# 
############### START OF SHAPEFILE STUFF ####################
#############################################################


# Read in Shape files of the korean administrative area boundaries
shp <- readOGR("Korea/raw-data/KOR_adm/KOR_adm1.shp", stringsAsFactors = F)

# tidyed the data for the region :D, need the region name so that the longitude data points match up the various busan seoul other area names 
shp_df<- tidy(shp, region = "NAME_1")
head(shp_df)

joined <- inner_join(shp_df,clean, by = "id")

######################################################################

#############  Line plot data   ##############

# 2015, 2005, 1995, 1985 Religion data for Korea

x2015 <- read_xlsx("Korea/raw-data/2015_Religions_Korea.xlsx") %>% 
  clean_names()
x2005 <- read_xlsx("Korea/raw-data/2005_Religions_Korea.xlsx") %>% 
  clean_names()
x1995 <- read_xlsx("Korea/raw-data/1995_Religions_Korea.xlsx") %>% 
  clean_names()
x1985 <- read_xlsx("Korea/raw-data/1985_Religions_Korea.xlsx") %>% 
  clean_names()

# Fixes merged column problem from excel for the dataset

x2015 <- x2015 %>% fill(gender) %>% fill(korean_region)
x2005 <- x2005 %>% fill(gender) %>% fill(korean_region)
x1995 <- x1995 %>% fill(gender) %>% fill(korean_region)
x1985 <- x1985 %>% fill(gender) %>% fill(korean_region)


# Data cleaning, need to only get the regions of all the total populations of Korea of
# 2015, only the gender and age totals, and we only selected the specific religions
# we wanted to view on the line plot, christianity and buddhism and religious totals
# and non religious totals

totals2015 <- x2015 %>% 
  filter(korean_region == "All of Korea") %>% 
  filter(gender == "Korea - Total") %>% 
  filter(age == "Total") %>% 
  select(total,religious_total,christianity_protestant,
         christianity_catholic,buddhism,no_religion) %>%
  mutate(year = "2015")

# Data cleaning, need to only get the regions of all the total populations of Korea of
# 2005, only the gender and age totals, and we only selected the specific religions
# we wanted to view on the line plot, christianity and buddhism and religious totals
# and non religious totals

totals2005 <- x2005 %>% 
  filter(korean_region == "All of Korea") %>% 
  filter(gender == "Korea - Total") %>% 
  filter(age == "Total") %>% 
  select(total,religious_total,christianity_protestant,
         christianity_catholic,buddhism,no_religion) %>%
  mutate(year = "2005")

# Data cleaning, need to only get the regions of all the total populations of Korea of
# 1995, only the gender and age totals, and we only selected the specific religions
# we wanted to view on the line plot, christianity and buddhism and religious totals
# and non religious totals

totals1995 <- x1995 %>% 
  filter(korean_region == "All of Korea") %>% 
  filter(gender == "Korea - Total") %>% 
  filter(age == "Total") %>% 
  select(total,religious_total,christianity_protestant,
         christianity_catholic,buddhism,no_religion) %>%
  mutate(year = "1995")

# Data cleaning, need to only get the regions of all the total populations of Korea of
# 1985, only the gender and age totals, and we only selected the specific religions
# we wanted to view on the line plot, christianity and buddhism and religious totals
# and non religious totals

totals1985 <- x1985 %>% 
  filter(korean_region == "All of Korea") %>% 
  filter(gender == "Total") %>% 
  filter(age == "Total") %>% 
  select(total,religious_total,christianity_protestant,
         christianity_catholic,buddhism,no_religion) %>%
  mutate(year = "1985")

# Combined the four different years datas so that we could have it all in one set

combined2 <- rbind(totals1985, totals1995, totals2005, totals2015) %>% 
  mutate(freq = 6)

# Converted to a data frame, duplicated each row 6 times so that we could make a column
# that contains all of the various religion data for grouped line bar plots

combined <- as.data.frame(combined2[rep(row.names(combined2),
                                        combined2$freq), 1:7])

# Made several rows that could contain numbers for the four different religous population
# numbers, and the total population numbers and non religious population numbers
# had a identifier row called religion and numbers in population
# needed a case_when function because we had so many different possibilities

combined <- combined %>% 
  mutate(religion = case_when(row_number() %% 6 == 1 ~ "Total Population",
                              row_number() %% 6 == 2 ~ "All Religions",
                              row_number() %% 6 == 3 ~ "Christianity Protestant",
                              row_number() %% 6 == 4 ~ "Christianity Catholic",
                              row_number() %% 6 == 5 ~ "Buddhism",
                              row_number() %% 6 == 0 ~ "No Religion",
                              TRUE ~ "no")) %>% 
  mutate(population = case_when(row_number() %% 6 == 1 ~ total,
                                row_number() %% 6 == 2 ~ religious_total,
                                row_number() %% 6 == 3 ~ christianity_protestant,
                                row_number() %% 6 == 4 ~ christianity_catholic,
                                row_number() %% 6 == 5 ~ buddhism,
                                row_number() %% 6 == 0 ~ no_religion,
                                TRUE ~ "no"))


#########################################################################

#Used write_rds() to write out vertical world data into a separate rds file
write_rds(vertical_world, "Korea/world.rds")
write_rds(gender_data_vertical, "Korea/gender.rds")
write_rds(age_data, "Korea/age.rds")
write_rds(world, "Korea/worldMap.rds")
write_rds(joined, "Korea/koreaMap.rds")
write_rds(combined, "Korea/lineplot.rds")
