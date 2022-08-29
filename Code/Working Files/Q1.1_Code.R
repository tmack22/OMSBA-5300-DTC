#Question:  How has COVID affected the health of the retail industry, 
# as measured by employment?

#To answer this question, we will use data from the Current Population
# Survey (CPS) to look at unemployment in the retail industry before and after
# COVID. To do this, we will use March 2020 as our cutoff date to determine
# our timeline as it relates to COVID. Anything after March 2020 will be
# considered post-COVID, and everything else will be considered pre-COVID.

#Our data will be for the time period January 1, 2019, through December 31,
# 2021.

#First, we'll load the libraries needed for our analysis.

library(tidyverse)
library(vtable)
library(haven)
library(lubridate)
library(fixest)

#Then, we'll load the raw data we pulled from the CPS. We'll also load a key
# that will help us translate numerical state codes to state names.

employment_data <- read_dta('cps_00001.dta')

states <- read_csv('state_codes.csv')

#Filter for only desired employment variables and values
# empstat:  Values between 10 and 29 relate to people in the labor market.
# ind:  Values between 4670 and 5790 indicate the retail industry

t_employment <- employment_data %>% 
  #filter(ind>=4670 & ind<=5790) %>% 
  filter(empstat < 30 & empstat >= 10) %>% 
  mutate(emp_Status = empstat) #industry = ind

#Create a binary variable to indicate employment status, 0 = unemployed,
# 1 = employed
t_employment <- t_employment %>% 
  mutate(employed = ifelse(empstat < 20, 1, 0))

#Create a variable to indicate whether data point is from before or after
# COVID, 0 = pre-covid, 1 = post-covid
#Add a year_month variable for grouping during analysis
t_employment <- t_employment %>% 
  mutate(month = as.factor(month)) %>% 
  mutate(year_month = ym(paste0(year, '-', month))) %>% 
  mutate(post_covid = ifelse(year_month > '2020-03-01', 1, 0))

#Select only desired variables.
t_employment <- t_employment %>% 
  select(year, month, employed, year_month, post_covid, statefip) %>% 
  mutate(statefip = as.numeric(statefip)) %>% 
  mutate(for_count = 1)

#Replace numeric state codes with state names.
t_employment <- t_employment %>% 
  left_join(states, "statefip") %>% 
  mutate(state = statename) %>% 
  select(-statefip, -statename)

#Group and summarise data by date only.
t_employment_group <- t_employment %>% 
  group_by(year_month) %>% 
  summarise(num_employable = sum(for_count),
            num_employed = sum(employed==1),
            num_unemployed = num_employable - num_employed) %>% 
  mutate(percent_unemployed = num_unemployed/num_employable,
         post_covid = ifelse(year_month > '2020-03-01', 1, 0))

#Group and summarise by date and state. We want to look at data
# grouped by state because we suspect that a lot back-door variation
# will come from economic and political differences between states.

t_employment_group_2 <- t_employment %>% 
  group_by(state, year_month) %>% 
  summarise(num_employable = sum(for_count),
            num_employed = sum(employed==1),
            num_unemployed = num_employable - num_employed) %>% 
  mutate(percent_unemployed = num_unemployed/num_employable,
         post_covid = ifelse(year_month > '2020-03-01', 1, 0))

#Before running a regression, we'll graph unemployment trends across time, both
# grouped by state and not grouped by state.

#The first graph shows a clear jump in unemployment in April, right
# after COVID. Unemployment levels decrease steadily after that, returning to
# near normal until at the end of 2021. 

t_employment_group %>% 
  ggplot(aes(x=year_month, y=percent_unemployed)) + 
  geom_point()

#If we run the same graph but with data points grouped by state, we
# see a similar trend overall, but a significant amount of variation
# each month based on state. This is an indicator that we should
# control for state in our regression.

t_employment_group_2 %>% 
  ggplot(aes(x=year_month, y=percent_unemployed, group=state)) + 
  geom_point()

#Given that we have panel data and our graphs indicate a lot of
# between-state variation, we chose a fixed effects model to look at
# pre- and post-covid changes while controlling for state variations.

m1 <- t_employment_group_2 %>% 
  feols(percent_unemployed~post_covid | state)

#Looking at our results, we can conclude that, when controlling for state,
# monthly average post-covid unemployment rates are .0467 percentage 
# points higher than average monthly pre-covid unemployment rates. This
# is true at a .001 level of significance.

etable(m1)

#A major assumption underlying this model is the assumption that all
# changes in unemployment, when controlling for state, were attributable
# to COVID. There is available data on unemployment related specifically to
# COVID, but we're concerned that self-reporting individuals may not
# accurately report on COVID impacts. Perhaps their employer didn't
# specifically indicate that they were laying off an individual due to
# COVID, but the overall circumstances leading up to their layoff
# were impacted by COVID. Or perhaps someone was not laid off due to
# COVID, but COVID-related downturns caused the individual to remain
# unemployed longer than they otherwise would have been unemployed.

#Another assumption is that not all companies within the retail industry
# responded to COVID on the same timeline. Some responded with layoffs
# right away, while others took longer to respond. While it may seem like
# an oversimplification to use a binary variable of pre/post-covid and
# look only at the average change in unemployment, the month-to-month
# variation may be mostly indicative of differences between management
# decisions and timelines in different companies.