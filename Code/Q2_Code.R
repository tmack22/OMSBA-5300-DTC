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
  mutate(industry_cat = case_when(
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
  mutate(emp_count = n())

industry_cat_df <- filtered_df %>%
               ungroup() %>%
               select(c('industry_cat','year','date','covid_active','emp_count')) %>%
               distinct() %>%
               group_by(industry_cat,date) %>%
               mutate(count = sum(n))%>%
               select(-c(5))%>%
               distinct() %>%
               ungroup()%>%
               group_by(industry_cat)%>%
               mutate(percent_change = ((count-lag(count))/lag(count)))%>%
               mutate(percent_change = case_when(
                 percent_change = is.na(percent_change) ~ 0,
                 percent_change == is.na(percent_change) ~ 0,
                 percent_change != is.na(percent_change) ~ percent_change))

industry_df <- filtered_df %>%
  ungroup() %>%
  select(c('indname','year','date','covid_active','emp_count')) %>%
  distinct() %>%
  group_by(indname,date) %>%
  mutate(count = sum(emp_count))%>%
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
  ggplot(aes(x=date,y=percent_change,group=industry_cat,color=industry_cat)) +
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


q2_reg_df <- filtered_df %>%
  ungroup()%>%
  group_by(date,industry_cat)%>%
  mutate(avg_employment = mean(emp_count))%>%
  select(c('industry_cat','year','date','covid_active','avg_employment'))%>%
  distinct()%>%
  ungroup()%>%
  group_by(industry_cat)%>%
  mutate(employment_change = ((avg_employment-lag(avg_employment))))%>%
  drop_na()

#Production level regressions
reg1 <- feols(emp_count~indname,se= 'hetero',data= filtered_df)
reg1.1 <- feols(emp_count~indname*covid_active,se='hetero',data= filtered_df)
etable(reg1,reg1.1)

reg2 <- feols(emp_count~industry_cat,se='hetero',data= filtered_df)
reg2.1 <- feols(emp_count~industry_cat*covid_active,se='hetero',data= filtered_df)
etable(reg2,reg2.1)

reg3 <- feols(employment_change~industry_cat,se= 'hetero',data= q2_reg_df)
reg3.1 <- feols(employment_change~industry_cat*covid_active,se= 'hetero',data= q2_reg_df)
etable(reg3,reg3.1)
#

