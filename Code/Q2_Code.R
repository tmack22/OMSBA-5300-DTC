library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
library(estimatr)



setwd('./Data/')
industry_names <- read.csv('./indnames.csv') %>%
  mutate(Retail = case_when(
    indname == 'Retail Trade' ~ 1,
    indname != 'Retail Trade' ~ 0))

data <- read_dta('./Industry_data.dta.gz')

df <- inner_join(data,industry_names,by= "ind")

results <- df %>% group_by(indname, year, month) %>%
  mutate(n = n())
ggplot(mapping= aes(x= year,y= n, group= indname)) + geom_line(aes(color= indname)) + 
  geom_point() 

