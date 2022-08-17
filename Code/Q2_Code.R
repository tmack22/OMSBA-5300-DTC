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
        
  
df <- inner_join(data,industry_names,by= "ind") %>%
      mutate(cpsid = format(cpsid,scientific= FALSE)) %>%
      mutate(cpsidp = format(cpsidp,scientific= FALSE))

filtered_df <- df %>% group_by(year, month, indname,cpsidp) %>%
           mutate(n = n()) %>%
           filter(n != 1) #to filter out all duplicate entries
           




ggplot(mapping= aes(x= year,y= n, group= df$indname)) + 
           geom_line(aes(color= indname)) + geom_point() 

