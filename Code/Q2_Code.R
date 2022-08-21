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
  mutate(Retail = case_when(
    indname == 'Retail Trade' ~ 1,
    indname != 'Retail Trade' ~ 0))

data <- read_dta('./cps_00004.dta.gz')


df <- inner_join(data,industry_names,by= "ind") %>%
  mutate(cpsid = format(cpsid,scientific= FALSE)) %>%
  mutate(cpsidp = format(cpsidp,scientific= FALSE)) %>%
  mutate(year_month = paste(year,"-", month,sep= '')) %>%
  mutate(covid_active = case_when(year_month >= '2020-3' ~ 1,
                                  year_month < '2020-3' ~ 0))


filtered_df <- df %>% filter(cpsid > 0) %>%
  filter(empstat <20) %>% #20 and above indicates they are unemployed/no looking
  group_by(year_month,indname) %>%
  mutate(n = n())


summary <- filtered_df %>%
  select(c("year_month","indname","covid_active","Retail","n")) %>%
  distinct()

reg1 <- feols(n~indname + covid_active ,data= summary)
reg2 <- feols(log(n)~indname + covid_active,data= summary)
etable(reg1,reg2)

reg3 <- feols(n~Retail + covid_active,data= summary)
reg4 <- feols(log(n)~Retail + covid_active,data= summary)
etable(reg3,reg4)

wald(reg1)
wald(reg2)
wald(reg3)
wald(reg4)

