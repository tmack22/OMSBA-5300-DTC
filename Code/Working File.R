#Load libraries

library(tidyverse)
library(vtable)
library(haven)
library(lubridate)

#Load raw data from IPUMS
employment_data <- read_dta('cps_00007.dta') %>% 
  mutate(cpsid = format(cpsid,scientific= FALSE),
         cpsidp = format(cpsidp, scientific = FALSE))

#empstat = 30+ refers to not in workforce
#  20s refers to actually unemployed
#  10s are employed

#ind
#1190 retail bakeries
#5470 retail florists (2014 - 2019 only)
#5580 misc retail stores
#5790 not specified retail trade


#filter for desired industry, remove rows with missing data, select
# desired columns
t_employment <- employment_data %>% 
  filter(ind==1190|ind==5470|ind==5580|ind==5790) %>% 
  select(year, month, statefip, cpsidp, serial, empstat, occ, ind)

#create variable 0 = unemployed 1 = employed
#All of these people are in labor force, can remove that variable
t_employment <- t_employment %>% 
  filter(empstat < 30 & empstat >= 10) %>% 
  mutate(employed = ifelse(empstat <20, 1, 0)) %>% 
  mutate(unemployed = ifelse(empstat > 20, 1, 0))

#filter for desired timeline
t_employment <- t_employment %>% 
  filter(year > 2018 & year < 2022) %>% 
  mutate(month = as.factor(month)) %>% 
  mutate(year_month = as.factor(paste0(year, '-', month)))

#group by state, year, month
t_employment_group <- t_employment %>% 
  group_by(statefip, year_month) %>% 
  summarise(num_employed = sum(employed==1),
            num_unemployed = sum(unemployed==1),
            num_employable = num_employed + num_unemployed) %>% 
  mutate(percent_emp = num_employed/num_employable,
         percent_unemp = num_unemployed/num_employable)

#alt groups
alt_group <- t_employment %>% 
  group_by(year_month) %>% 
  summarise(num_employed = sum(employed==1),
            num_unemployed = sum(unemployed==1),
            num_employable = num_employed + num_unemployed) %>% 
  mutate(percent_emp = num_employed/num_employable,
         percent_unemp = num_unemployed/num_employable)

alt_group %>% 
  ggplot(aes(x=year_month, y=percent_emp)) + geom_point() +
  theme(axis.text.x= element_text(size=7, angle=90))
