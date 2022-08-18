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
      mutate(cpsidp = format(cpsidp,scientific= FALSE)) %>%
      mutate(year_month = paste(year,"-",month,sep= '')) %>%
      mutate(unique_id = paste(serial,pernum,sep= ''))

filtered_df <- df %>% group_by(year_month, indname,cpsidp) %>%
              # drop_na(asecwt) %>%
               mutate(n = n()) %>%
               filter(n == 1) #to filter out all duplicate entries
filtered_df <- filtered_df[-c(ncol(filtered_df))] #removing the n column

filtered_out_df <- df %>% group_by(year_month, indname,unique_id) %>%
                   mutate(n = n()) %>%
                   filter(n != 1)   

summary <- filtered_df %>%
           ungroup(year,cpsidp) %>% 
           select(c("year_month","indname")) %>%
           group_by(year_month,indname) %>%
           mutate(n = n()) %>%
           distinct()





ggplot(mapping= aes(x= year,y= n, group= df$indname)) + 
           geom_line(aes(color= indname)) + geom_point() 

