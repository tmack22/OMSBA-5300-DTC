# tidy data for Question 1. 

# Broken down into: 
# 01. Raw data, libraries
# 02. Data analysis + Graphs, Data Visualization, Results

# --------------------------------------------------------------------------------
# 01. Raw data, libraries, base code.

#libraries 

library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
library(estimatr)
library(RCurl)
<<<<<<< HEAD:Code/Q1_code.R
library(patchwork)
library(hrbrthemes)
library(jtools)
library(ggplot2)
library(fixest)

=======

# load in data 
q1_employment <- read_dta('./cps_00001.dta') 
covid_history <- read_csv('./covid_history.csv') 
state_lockdowns <- read_csv('./state_lockdowns.csv')
state_names <- read_csv('./state_names.csv')
timeline <- read_csv('./timeline.csv') # google trends data
>>>>>>> main:Code/Working Files/Q1_code.R

# base data load (b_data = base data)
b_emp <- read_dta('./cps_00001.dta')
b_state_lockdowns <- read.csv('./state_lockdowns.csv')
b_covid_history <- read.csv('./covid_history.csv')


# following code establishes a function to convert variable class()

class_conversion <- function(df, x, y){
  'str_prefix' = x
  'new_class' = y
  df %>% mutate(across(starts_with(str_prefix), new_class))
}

# b_emp data wrangling ----

# First, we are going to filter out 'whyunemp == 0' which is used to catalog those in the military or government service.  
# Second, we then selects the desired variables. It begins by filtering the data keeping only data for people who report a retail occupations (47xx), 
# then filters 'whyunemp == 1,2,3' which correspond to unemployment due to 
# layoffs, ending of temporary work, and general lose of work.  
# It converts the variables 'occ2010', 'covidunaw' and 'whyunemp' 
# from class(numeric) to class(factor).

# t_data = tidy data

t_emp <- b_emp %>% filter(!(whyunemp == 0)) %>%
  filter(occ2010 %in% c(4700:5790)) %>% 
  filter(whyunemp %in% c(1,2,3)) %>% 
  class_conversion(c('month', 'durunemp', 'statefip'), as.numeric) %>% 
  class_conversion(c('occ', 'covidunaw', 'whyunemp'), as.factor) 


# Following code cleans up the data by converting the factor levels in 'whyunemp' 
# and 'covidunaw' to individual variables with the observation being the 
# total number of 'whyunemp', 'covidunaw' reported for the month.  
# It then removes the base columns. 

t_emp <- t_emp %>% 
  na.omit()%>%
  mutate(survey_dates = ymd(paste0(year, '-', month, '-01')))%>%
  group_by(year, month, statefip) %>% 
  mutate(layoff = ifelse(whyunemp == 1,1,0), # if reason is layoff = 1, if not = 0
         other_type_loss = ifelse(whyunemp == 2,1,0), 
         temp_emp = ifelse(whyunemp == 3,1,0),
         covidunaw=ifelse(covidunaw==1,1,0))%>% 
  mutate(covid_impact_no = ifelse(covidunaw == 0,1,0), 
         covid_impact_yes = ifelse(covidunaw == 1,1,0))%>% 
  left_join(b_state_lockdowns, 'statefip')


# state_lockdowns data wrangling ----

# converts state_lockdowns variable 'lockdown_start' from class(character) to class(date)
t_state_lockdowns <- b_state_lockdowns %>% 
  mutate(lockdown_start = mdy(lockdown_start))


# covid_history data wrangling ----

# following code builds a key to join the state covid_history data with the 
# b_emp and state_lockdowns

state_name_key <- read.csv('./state_names.csv')

names(b_covid_history)[2] <- 'Code' 

t_covid_history <- b_covid_history %>%
  left_join(state_name_key, 'Code')%>% 
  mutate(date = mdy(date)) 



# ASSUMTPION: As the virus began to spread across the country and local 
# authorities started implementing individual restrictions - first State 
# Emergency Declaration was 9 March - retail unemployment 
# (and thus the health of the retail sector) may have started to go downhill prior to 
# stay at home orders. 

# Since we are looking for changes in rates of unemployment
# specifically (layoff) before and after lock down orders filtering duration 
# employed to 4 weeks and less should allow us to capture initial layoffs that
# started occurring before stay-at-home orders as a result of individual actions.

# --------------------------------------------------------------------------------
# 02. Data analysis + graph, vizualization.

# How has COVID affected the health of the retail industry, as measured by employment?

# H0 = As measured by unemployment, COVID had no impact on retail industry health. (Key: COVID had no impact.)
# H1 = Retail industry health was affected by COVID related measures, specifically lockdowns. (Key: COVID related measures: lock downs.)



# Reason for Job Unemployment.
# Unemployment from our data was categorized into three "rationale": 
# Laid Off, Temporary Employment, and Other.
# In which we wanted to look at whether there was an increase of unemployment 
# in one category over the others. 

# From this graph, our results shows that there was an increase in unemployment 
# right around when COVID-19 cases came to the US - which was in March, with a peak in April. 
# We also identified that the majority of unemployment came from workers who were "Laid Off". 
# We also see minimal impact of unemployment for "Temporary workers". 
# Finally, at the end of 2020, we see that those who are unemployed for "Other" reasons had an increase 
# and becomes the majority reason over being laid off. 

# I would like to see further information on why this changed and if those were 
# initially considered "Laid Off" became re-categorized as "Other" for their unemployment status.

t_emp <- b_emp %>% filter(!(whyunemp == 0)) %>%
  filter(occ2010 %in% c(4700:5790)) %>% 
  filter(whyunemp %in% c(1,2,3)) %>% 
  class_conversion(c('month', 'durunemp', 'statefip'), as.numeric) %>% 
  class_conversion(c('occ', 'covidunaw', 'whyunemp'), as.factor) 


# Sum of unemployment 

sum_by_why <- t_emp %>%
  mutate(date = ymd(paste0(year, '-', month, '-01'))) %>% 
  dplyr::count(date, whyunemp) %>%
  filter(date >= "2019-12-01")

plot_by_why <- sum_by_why %>%
  ggplot(aes(x = date, y = n, fill = factor(whyunemp))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position="bottom") + 
  xlab("Month") +
  ylab("Number of People") +
  ggtitle('Figure 1. Types of Unemployment by Month')+
  scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"), name = "Reason for Unemployment: ",
                    labels=c("Laid Off", "Other", "Temporary Employment"))
plot_by_why


# How Did COVID affected Unemployment?

# We wanted to see if the reason for the unemployment was because of COVID reasons 
# and found a data set that determined if a person was unable to work because of COVID-19. 
# This data was collected from May 2020 to January 2021, so this only applies to some of 
# the unemployment data and does not include the peak in April of 2020. 

# From this graph, we can see that COVID was the majority reason for why there was unemployment. 
# The "Not Reason For" category does not appear to have been effected much when looking at data in 
# the last two months of 2019. 

sum_unemp <- t_emp %>%
  na_if(99) %>%
  mutate(date = ymd(paste0(year, '-', month, '-01'))) %>% 
  dplyr::count(date, covidunaw) %>%
  filter(covidunaw %in% c(NA, 1, 2)) %>%
  filter(date >= "2019-11-01") %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))

plot_total <- ggplot(data = sum_unemp, aes(x = date, y = n, fill = factor(covidunaw))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(name = "Covid Unemployment: ",
                    labels = c("Not Recorded", "Not Related", "COVID Related"),
                    values = c('gray', "steel blue", "red")) +
  theme(legend.position="bottom") +
  ggtitle('Figure 2. Covid-19 Attributed Unemployment by Month')
plot_total






































