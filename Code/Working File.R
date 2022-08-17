#Need to review data source - how to get more granular date data - survey
# data from multiple months? In COVID category?

#Load libraries

library(tidyverse)
library(vtable)
library(haven)
library(lubridate)

#Load raw data from IPUMS
employment_data <- read_dta('cps_00005.dta') %>% 
  mutate(cpsid = format(cpsid,scientific= FALSE),
         cpsidp = format(cpsidp, scientific = FALSE))

#empstat = 30+ refers to not in workforce - will need to consider these to
#  avoid those who aren't eligible?
#  20s refers to actually unemployed
#  10s are employed

#labstat
#  1 = not in labor force
#  2 = in labor force

#ind
#1190 retail bakeries
#5470 retail florists (2014 - 2019 only)
#5580 misc retail stores
#5790 not specified retail trade

#control for occ as type of work may vary in impact? such as frontline
#  retail vs corporate hq

#filter for desired industry, remove rows with missing data, select
# desired columns
t_employment <- employment_data %>% 
  filter(ind==1190|ind==5470|ind==5580|ind==5790) %>% 
  filter(as.numeric(cpsid) != 0) %>% 
  select(year, month, statefip, cpsidp, empstat, labforce, occ, ind)

#create variable 0 = unemployed 1 = employed
#All of these people are in labor force, can remove that variable
t_employment <- t_employment %>% 
  filter(empstat < 30 & empstat >= 10) %>% 
  mutate(employed = ifelse(empstat <20, 1, 0)) %>% 
  select(-labforce)

#filter for desired timeline and group
t_employment_test <- t_employment %>% 
  filter(year==2019 | year==2020)
