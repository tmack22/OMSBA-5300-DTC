#Load libraries

library(tidyverse)
library(vtable)
library(haven)
library(lubridate)
library(fixest)

#Load raw data
employment_data <- read_dta('cps_00010.dta') %>% 
  mutate(cpsid = format(cpsid,scientific= FALSE),
         cpsidp = format(cpsidp, scientific = FALSE))

states <- read_csv('state_codes.csv')


#empstat = 30+ refers to not in workforce
#  20s refers to actually unemployed
#  10s are employed

#ind:  retail trade = 4670 through 5790

#filter for desired dates, industry and those in labor force
t_employment <- employment_data %>% 
  filter(ind>=4670 & ind<=5790) %>% 
  filter(empstat < 30 & empstat >= 10) %>% 
  filter(year > 2019)

#create employed variable 0 = unemployed 1 = employed
#All of these people are in labor force, can remove that variable
t_employment <- t_employment %>% 
  mutate(employed = ifelse(empstat < 20, 1, 0))

#Create post-covid variable, 0 = pre-covid, 1 = post-covid
t_employment <- t_employment %>% 
  mutate(month = as.factor(month)) %>% 
  mutate(year_month = ym(paste0(year, '-', month))) %>% 
  mutate(post_covid = ifelse(year_month > '2020-03-01', 1, 0))

#Create variable for covid-related unemployment, 1 = covid-related,
#0 = not covid-related
t_employment <- t_employment %>%
  mutate(covidunaw = replace_na(covidunaw,0)) %>% 
  mutate(covid_unemp = ifelse(covidunaw == 2, 1, 0))

#create variable for telework, 1 = teleworked, 0 = did not telework
t_employment <- t_employment %>%
  mutate(covidtelew = replace_na(covidtelew,0)) %>% 
  mutate(telework = ifelse(covidtelew == 2, 1, 0))

#select only desired variables
t_employment <- t_employment %>% 
  select(year, month, cpsidp, occ, employed, year_month, post_covid,
         covid_unemp, telework, statefip) %>% 
  mutate(statefip = as.numeric(statefip)) %>% 
  mutate(for_count = 1)

#join in state names
t_employment <- t_employment %>% 
  left_join(states, "statefip") %>% 
  select(-statefip)

#group by year_month
t_employment_group <- t_employment %>% 
  group_by(year_month) %>% 
  summarise(num_employable = sum(for_count),
            num_employed = sum(employed==1),
            num_unemployed = num_employable - num_employed,
            num_covid_unemp = sum(covid_unemp==1),
            num_telework = sum(telework==1)) %>% 
  mutate(percent_unemployed = num_unemployed/num_employable,
         percent_telework = num_telework/num_employable,
         post_covid = ifelse(year_month > '2020-03-01', 1, 0))

#group by year_month and state
t_employment_group_2 <- t_employment %>% 
  group_by(statename, year_month) %>% 
  summarise(num_employable = sum(for_count),
            num_employed = sum(employed==1),
            num_unemployed = num_employable - num_employed,
            num_covid_unemp = sum(covid_unemp==1),
            num_telework = sum(telework==1)) %>% 
  mutate(percent_unemployed = num_unemployed/num_employable,
         percent_telework = num_telework/num_employable,
         post_covid = ifelse(year_month > '2020-03-01', 1, 0))

#graph employment trends against year_month
#compare when grouped by state - looks like a lot of between-state
#  variation, so we'll need to take that into consideration
t_employment_group %>% 
  ggplot(aes(x=year_month, y=percent_unemployed)) + 
  geom_point()

t_employment_group_2 %>% 
  ggplot(aes(x=year_month, y=percent_unemployed, group=statename)) + 
  geom_point()

t_employment_group %>% 
  ggplot(aes(x=year_month, y=percent_telework)) + 
  geom_point()

t_employment_group_2 %>% 
  ggplot(aes(x=year_month, y=percent_telework, group = statename)) + 
  geom_point()

#run basic regression, control for state
m1 <- t_employment_group_2 %>% 
  feols(percent_unemployed~year_month + statename)

etable(m1)

m2 <- t_employment_group_2 %>% 
  feols(percent_unemployed~post_covid + statename)

etable(m2)

m3 <- t_employment_group_2 %>% 
  feols(percent_unemployed~post_covid + year_month + statename)

etable(m2, m3)

#trying fixed effects to close state back door
m4 <- t_employment_group_2 %>% 
  feols(percent_unemployed~post_covid | statename)
