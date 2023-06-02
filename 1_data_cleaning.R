#Load Packages -----------------------------------------------------------------
library(tidyverse)
library(skimr)
library(readr)
library(naniar)
library(knitr)

#load in data-----------------------------------------------------------------
police_killings <- read_csv("data/raw/police_killings.csv") %>% 
  janitor::clean_names()

#skim the data -----------------------------------------------------------------
skim_without_charts(police_killings)

#For the most part the data is complete with under 10% of missiningness where there is some
#I removed the missininginess so that I could recode factors to numeric

#Missingness exploration -------------------------------------------------------
#Variables
police_killings_missing_table <- police_killings %>% 
  miss_var_summary() %>% 
  kable()

#County Bucket and has the most missiningess, but I will not be utalizing it a lot for my anaylsis 

#Observations
# Add an identifier column to preserve original row numbers
police_killings_filtered <- police_killings %>%
  mutate(row_number = row_number()) %>%
  mutate(total_missing = rowSums(is.na(.)),
         missingness_percentage = total_missing / ncol(.)) %>%
  filter(total_missing > 0) %>%
  select(observation = row_number, total_missing, missingness_percentage)

# Print the filtered data frame
print(police_killings_filtered)

#two observations have very high missinginess, #379 and #183, I will be deleting both due to too much incompletness
police_killings <- police_killings[-c(183, 379), ]

#check missingness again
police_killings_missing_table <- police_killings %>% 
  miss_var_summary() %>% 
  kable()

#now we only have missingness in county_bucket and streetadress which might not be that important

#Data Type Conversion:  -------------------------------------------------------
#During the skim I noticed that a few variables should be numeric
police_killings <- police_killings %>% 
  #turn pov numeric
  mutate(pov = as.numeric(pov)) %>% 
  #turn age numeric
  mutate(age = as.numeric(pov)) %>% 
  #turn share of populations numeric
  mutate(share_white = as.numeric(share_white)) %>% 
  mutate(share_hispanic = as.numeric(share_hispanic)) %>% 
  mutate(share_black = as.numeric(share_black)) %>% 
  #turn income into numeric
  mutate(p_income = as.numeric(p_income)) 

#Outliers ---------------------------------------------------------------------
#I looked at all numeric variabels and no observation seemed out of the ordinary 
#age, income etc was all in a sensible range

#Save processed data ------------------------------------------------------------
write.csv(police_killings, file = "data/processed/police_killings_clean.csv", row.names = FALSE)


