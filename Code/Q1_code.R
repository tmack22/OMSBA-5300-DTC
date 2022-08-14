# tidy data for Question 1. 

#libraries 

library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
library(estimatr)
library(RCurl)

# load in data 
q1_employment <- read_dta('./cps_00001.dta') 
covid_history <- read_csv('./covid_history.csv') 
state_lockdowns <- read_csv('./state_lockdowns.csv')
state_names <- read_csv('./state_names.csv')
timeline <- read_csv('./timeline.csv') # google trends data


# t_data = tidy data in progress (this is the df that is manipulated) 

# following code establishes a function to convert variable class()

class_conversion <- function(df, x, y){
  'str_prefix' = x
  'new_class' = y
  df %>% mutate(across(starts_with(str_prefix), new_class))
}

# q1_employment data wrangling ----

# Following code begins by filtering out 'whyunemp == 0' which is used to 
# catalog those in the military or government service.  
# It then selects the desired variables. It begins by filtering the data
# keeping only data for people who report a retail occupations (47xx), 
# then filters 'whyunemp == 1,2,3' which correspond to unemployment due to 
# layoffs, ending of temporary work, and general lose of work.  
# It converts the variables 'occ2010', 'covidunaw' and 'whyunemp' 
# from class(numeric) to class(factor).


t_employment <- q1_employment %>% filter(!(whyunemp == 0)) %>%  # t_employment tidy data in progress, q1_employment = base 
  filter(occ2010 %in% c(4700:5790)) %>% 
  filter(whyunemp %in% c(1,2,3)) %>% 
  class_conversion(c('month', 'durunemp', 'statefip'), as.numeric) %>% 
  class_conversion(c('occ', 'covidunaw', 'whyunemp'), as.factor) %>% 
  select(-'serial')

# Following code tidies up the data by converting the factor levels in 'whyunemp' 
# and 'covidunaw' to individual variables with the observation being the 
# total number of 'whyunemp', 'covidunaw' reported for the month.  It then 
# removes the base columns. 

t_employment <- t_employment %>% 
  group_by(year, month, statefip) %>% 
  mutate(layoff = sum(whyunemp == 1), 
         other_type_loss = sum(whyunemp == 2), 
         temp_job_end = sum(whyunemp == 3), 
         covidunaw = sum(covidunaw == 2)) %>% 
  mutate(covid_impact_no = sum(covidunaw == 1), 
         covid_impact_yes = sum(covidunaw == 2)) %>% 
  select(-'whyunemp', -'covidunaw', -'occ2010', -'empstat') %>% 
  summarise(layoff = sum(layoff), 
            other_type_loss = sum(other_type_loss), 
            temp_job_end = sum(temp_job_end), 
            covid_impact_yes = sum(covid_impact_yes), 
            covid_impact_no = sum(covid_impact_no)) %>% 
  mutate(survey_dates = ymd(paste0(year, '-', month, '-01'))) %>% 
  left_join(state_lockdowns, 'statefip') %>% 
  select(-'lockdown_start')


# state_lockdowns data wrangling ----

# converts state_lockdowns variable 'lockdown_start' from class(character) to class(date)

t_state_lockdowns <- state_lockdowns %>% # t_state_lockdowns = tidy data in progress, state_lockdowns = base
  mutate(lck_dwn_st = mdy(lck_dwn_st))


# covid_history data wrangling ----

# following code builds a key to join the state covid history data with the 
# q1_employment and state_lockdowns

state_names <- read_csv('state_names.csv')

state_names <- state_names %>% select(-'Abbrev') %>% 
  rename(state_code = Code)

t_covid_history <- covid_history %>%  # t_covid_history = tidy data, covid_history = base
  rename(state_code = state) %>% mutate(date = mdy(date)) %>% 
  left_join(state_names, 'state_code')

# following code wrangles the state covid history, beginning with selecting the 
# desired variables and removes NA's associated with what appears to be an an error
# following code tidies the covid state history data starting with grouping by 
# state and date then filtering for all data from April 1 2020. The code then 
# splits and creates a month and year columns to group by then summarizing the 
# variables to produce a single month observation for each state. It then builds
# a new date column to be used in the join.

t_covid_history <- t_covid_history %>% 
  rename(state = State) %>% 
  select('date', 'state', 'positive', 'positiveIncrease','hospitalizedIncrease') %>% 
  na.omit() %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(date, state) %>% 
  summarise(positive, positiveIncrease, hospitalizedIncrease)

# timeline wrangling ----

t_google_trends <- timeline %>% # t_google_trends = tidy data ip, timeline = base
  mutate(Week = mdy(Week)) %>% mutate(Week = ymd(Week)) %>% 
  rename(date = Week)

