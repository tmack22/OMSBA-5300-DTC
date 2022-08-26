library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
library(estimatr)
library(fixest)
library(dplyr)
library(ggplot2)


setwd('./Data/')
industry_names <- read.csv('./indnames.csv') %>%
  mutate(industry = case_when(
    indname == 'Retail Trade' ~ 'Retail',
    indname != 'Retail Trade' ~ 'Non-Retail'))

data <- read_dta('./cps_00004.dta.gz')


df <- inner_join(data,industry_names,by= "ind") %>%
  mutate(cpsid = format(cpsid,scientific= FALSE)) %>%
  mutate(cpsidp = format(cpsidp,scientific= FALSE)) %>%
  mutate(date = as_date(paste(year, month, '01', sep = '-'))) %>%
  mutate(covid_active = case_when(date >= '2020-03-01' ~ 1,
                                  date < '2020-03-01' ~ 0))

filtered_df <- df %>% filter(cpsidp > 0) %>%
  filter(empstat <20 & empstat >= 10) %>% #20 and above indicates they are unemployed/no looking
  group_by(date,indname) %>%
  mutate(n = n())

industry_cat_df <- filtered_df %>%
               ungroup() %>%
               select(c('industry','year','date','covid_active','n')) %>%
               distinct() %>%
               group_by(industry,date) %>%
               mutate(count = sum(n))%>%
               select(-c(5))%>%
               distinct() %>%
               ungroup()%>%
               group_by(industry)%>%
               mutate(percent_change = ((count-lag(count))/lag(count)))%>%
               mutate(percent_change = case_when(
                 percent_change = is.na(percent_change) ~ 0,
                 percent_change == is.na(percent_change) ~ 0,
                 percent_change != is.na(percent_change) ~ percent_change))

industry_df <- filtered_df %>%
  ungroup() %>%
  select(c('indname','year','date','covid_active','n')) %>%
  distinct() %>%
  group_by(indname,date) %>%
  mutate(count = sum(n))%>%
  select(-c(5))%>%
  distinct() %>%
  ungroup()%>%
  group_by(indname)%>%
  mutate(percent_change = ((count-lag(count))/lag(count)))%>%
  mutate(percent_change = case_when(
    percent_change = is.na(percent_change) ~ 0,
    percent_change == is.na(percent_change) ~ 0,
    percent_change != is.na(percent_change) ~ percent_change))


industry_cat_df %>%
  ggplot(aes(x=date,y=percent_change,group=industry,color=industry)) +
  scale_y_continuous(labels=scales::percent_format(accuracy=.1)) +
  geom_point() + geom_line() + theme(aspect.ratio=6/7) +
  labs(x="Date",y="Employment Percentage Change",color="Industry Category",
       title= "Employment Percentage Change by Industry Category")

industry_df %>%
  ggplot(aes(x=date,y=percent_change,group=indname,color=indname)) +
  scale_y_continuous(labels=scales::percent_format(accuracy=.1)) +
  geom_point() + geom_line() + theme(aspect.ratio=1) +
  labs(x="Date",y="Employment Percentage Change",color="Industry Name",
       title= "Employment Percentage Change by Industry")


summary <- filtered_df %>%
  ungroup()%>%
  group_by(date,industry)%>%
  mutate(avg_employment = mean(n))%>%
  select(c('industry','year','date','covid_active','avg_employment'))%>%
  distinct()%>%
  ungroup()%>%
  group_by(industry)%>%
  mutate(employment_change = ((avg_employment-lag(avg_employment))))%>%
  drop_na()

#Production level regressions
reg1 <- feols(n~indname,se= 'hetero',data= filtered_df)
reg1.1 <- feols(n~indname*covid_active,se='hetero',data= filtered_df)
etable(reg1,reg1.1)

reg2 <- feols(n~industry,se='hetero',data= filtered_df)
reg2.1 <- feols(n~industry*covid_active,se='hetero',data= filtered_df)
etable(reg2,reg2.1)

reg3 <- feols(employment_change~industry,se= 'hetero',data= summary)
reg3.1 <- feols(employment_change~industry*covid_active,se= 'hetero',data= summary)
etable(reg3,reg3.1)
#

