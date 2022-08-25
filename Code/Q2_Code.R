library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
library(estimatr)
library(fixest)
library(dplyr)


setwd('./Data/')
industry_names <- read.csv('./indnames.csv') %>%
  mutate(industry = case_when(
    indname == 'Retail Trade' ~ 'Retail',
    indname != 'Retail Trade' ~ 'Non-Retail'))

data <- read_dta('./cps_00004.dta.gz')


df <- inner_join(data,industry_names,by= "ind") %>%
  mutate(cpsid = format(cpsid,scientific= FALSE)) %>%
  mutate(cpsidp = format(cpsidp,scientific= FALSE)) %>%
  mutate(year_month = paste(year,"-", month,sep= '')) %>%
  mutate(covid_active = case_when(year_month >= '2020-3' ~ 1,
                                  year_month < '2020-3' ~ 0))


filtered_df <- df %>% filter(cpsidp > 0) %>%
  filter(empstat <20 & empstat >= 10) %>% #20 and above indicates they are unemployed/no looking
  group_by(year_month,indname) %>%
  mutate(n = n())

industry_df <- filtered_df %>%
               ungroup() %>%
               select(c('industry','year_month','n')) %>%
               distinct() %>%
               group_by(industry,year_month) %>%
               mutate(count = sum(n))%>%
               select(-c(3))%>%
               distinct() %>%
               ungroup()%>%
               group_by(industry)%>%
               mutate(percent_change = ((count-lag(count))/lag(count)*100))%>%
               mutate(percent_change = case_when(percent_change = is.na(percent_change) ~ 0,
                                                 percent_change == is.na(percent_change) ~ 0,
                                                 percent_change != is.na(percent_change) ~ percent_change))
                                                  


summary <- filtered_df %>%
  select(c("year_month","indname","covid_active","Industry","n")) %>%
  distinct()

reg1 <- feols(n~indname + covid_active ,data= summary)
reg2 <- feols(log(n)~indname + covid_active,data= summary)
etable(reg1,reg2)

reg3 <- feols(n~Retail + covid_active,data= summary)
reg4 <- feols(log(n)~Retail + covid_active,data= summary)
reg5 <- feols(n~Retail,data= summary)
reg6 <- feols(log(n)~Retail,data= summary)
etable(reg3,reg4,reg5,reg6)
etable(reg5)

reg7 <- feols(n~ Retail*covid_active,data=summary)
etable(reg7)
wald(reg1)
wald(reg2)
wald(reg3)
wald(reg4)

