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

data <- read_dta('./Industry_data.dta.gz')
        
  
df <- inner_join(data,industry_names,by= "ind") %>%
      mutate(cpsid = format(cpsid,scientific= FALSE)) %>%
      mutate(cpsidp = format(cpsidp,scientific= FALSE)) %>%
      mutate(year_month = paste(year,"-", month,sep= '')) %>%
      mutate(covid_active = case_when(year_month >= '2020-3' ~ 1,
                                      year_month < '2020-3' ~ 0))
      

filtered_df <- df %>% filter(cpsid > 0) %>%
                      group_by(year_month,indname) %>%
                      mutate(n = n())

filtered_out_df <- df %>% group_by(year_month, indname,cpsidp) %>%
                   mutate(n = n()) %>%
                   filter(n != 1)   

summary <- filtered_df %>%
           select(c("year_month","indname","covid_active","n")) %>%
           distinct()

reg <- feols(n~indname * covid_active ,data=summary)
etable(reg)


