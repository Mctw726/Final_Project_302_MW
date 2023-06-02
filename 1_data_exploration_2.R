#Load Packages -----------------------------------------------------------------
library(tidymodels)
library(readr)
library(ggplot2)
library(viridis)
library(tigris)
library(usmap)
library(leaflet)
library(leaflet.minicharts)
library(sf)
library(tidyverse)
library(maptools)
library(ggplot2)
#load in data-----------------------------------------------------------------
police_killings <- read_csv("data/processed/police_killings_clean.csv") %>% 
  janitor::clean_names()

police_killings_2 <- read_csv("data/raw/police_killings.csv") %>% 
  janitor::clean_names()


police_killings$age <- police_killings_2$age
save(police_killings, file = "wrangled_police_killings.rda")

load('data/wrangled/data_wrangling.rda')
police_killings$unarmed <- 1

# Update the 'unarmed' column with 'Yes' where the 'armed' column has anything but 'No'
police_killings$unarmed[police_killings$armed != "No"] <- 0

month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
# Convert month names to numeric values
police_killings$month <- match(police_killings$month, month_names)
#create a date column
police_killings$date <- as.Date(paste(police_killings$month, police_killings$day, police_killings$year, sep = "/"), format = "%m/%d/%Y")


load('data_wrangling.rda')

deaths_state_full <- full_join(killings_per_state, police_killings)

state_2 %>%
  group_by(state) %>% 
  ggplot()+
  geom_col(aes(x = reorder(state, total), y = total))+
  xlab("State") +
  ylab("People killed per state") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom")
  

deaths_state_full%>%
  group_by(state) %>% 
  ggplot()+
  geom_col(aes(x = reorder(state, total), y = total, fill = raceethnicity),
           alpha = 0.7, width = 0.8, position = "fill")+
  xlab("State") +
  ylab("People killed per state") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom")

#good dataset

police_killings%>%
  group_by(state) %>% 
  mutate(total = nrow(police_killings)) %>% 
  ggplot()+
  geom_col(aes(x = reorder(state, total), y = total, fill = raceethnicity),
           alpha = 0.7, width = 0.8, position = "fill")+
  xlab("State") +
  ylab("People killed per state by race") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom")

#data wrangling

police_kills_2 <- police_killings%>%
  group_by(state) %>% 
  summarize(state_count = n()) 

police_killings <- full_join(police_kills_2, police_killings)

  police_killings %>% 
    group_by(state) %>% 
  mutate(state_count = state_count/n()) %>% 
  ggplot()+
  geom_col(aes(x = state, y = state_count, fill = raceethnicity),
           alpha = 0.7, width = 0.8)+
  xlab("State") +
  ylab("People killed per state") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom")
  
  
  police_summary_stats <- police_killings %>% 
    filter(month == 1) %>% 
    summarise(deaths = n(),
              unarmed = sum(unarmed == 1)) 
  #data wrangling
  police_killings <- police_killings %>%
    mutate(month = month.name[month])

  most_recurring_month <- police_killings %>%
    group_by(month) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    head(1) 
  
  #Month 3 or March
  #end of wrangle
  
  