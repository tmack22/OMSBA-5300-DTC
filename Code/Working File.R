library(ipumsr)  
library(dplyr)
library(tidyverse)
library(jtools)
library(vtable)
library(car)
library(lubridate)
library(haven)

# read in IPUMS data from JAN 2019 through DEC 2021
ddi <- read_ipums_ddi("cps_00013.xml")   
dataQ3 <- read_ipums_micro(ddi)

