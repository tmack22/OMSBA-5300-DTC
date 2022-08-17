# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

# if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
# 
# ddi <- read_ipums_ddi("cps_00016.xml")
# data <- read_ipums_micro(ddi)

library(ipumsr)  
library(dplyr)
library(tidyverse)
library(jtools)
library(vtable)
library(car)
library(lubridate)
library(haven)

# Question 3

# Read in Industry codes and select industries of interest
Industries <- read_csv('indnames.csv')                            # merge using IND
Industries <- Industries %>%
  rename(IND = ind)      # IND = Numeric class
#  filter(Industry %in% c('Manufacturing', 'Information', 'Retail Trade', 'Arts, Entertainment, and Recreation, and Accommodation and Food Services',
#                         'Educational Services, and Health Care and Social Assistance', 'Transportation and Warehousing, and Utilities' )) %>%
#  mutate(Industry = case_when(Industry == 'Retail Trade' ~ 'retail', Industry == 'Manufacturing' ~ 'manufacturing', Industry == 'Information' ~ 'information',
#                              Industry == 'Arts, Entertainment, and Recreation, and Accommodation and Food Services' ~ 'art_leisure_hospitality',
#                              Industry == 'Educational Services, and Health Care and Social Assistance' ~ 'education_healthcare',
#                              Industry == 'Transportation and Warehousing, and Utilities' ~ 'transport_warehouse'))

# Read in State Federal Information Processing Codes.  
state_fips <- read_csv('state-geocodes-v2021.csv') 
# rename(STATEFIP = State '(FIPS)') %>%
#select(STATEFIP, Name)

# read in IPUMS data from JAN 2019 through DEC 2021 & filter for those >= 15 years olds
ddi <- read_ipums_ddi("cps_00016.xml")   
dataQ3 <- read_ipums_micro(ddi) %>% 
  zap_labels() %>%
  filter(AGE >= 15)                       # 41 variables
  #mutate(STATEFIP = as.character(STATEFIP))

dataQ3_a <- dataQ3 %>%
  mutate(Married = ifelse(MARST ==1,1,0))%>%
  mutate(RACE = case_when(
    HISPAN == '0' & RACE == '100' ~ 'White',
    HISPAN == '0' & RACE == '200' ~ 'Black',  
    HISPAN == '0' & RACE == '300' ~ 'AmericanIndian',
    HISPAN == '0' & RACE %in% c('650', '651', '652') ~ 'Asian',
    HISPAN == '0' & !(RACE %in% c('100', '200', '300', '650', '651', '652')) ~ 'MixedRace',
    HISPAN != '0' ~ 'Latino')) %>%
  mutate(AgeGroup = case_when(
    AGE < 20 ~ 'under20',
    AGE >= 20 & AGE < 30 ~ '20to29',
    AGE >= 30 & AGE < 40 ~ '30to39',
    AGE >= 40 & AGE < 50 ~ '40to49',
    AGE >= 50 & AGE < 60 ~ '50to59',
    AGE >= 60 & AGE <= 65 ~ '60to65',
    AGE >= 65 ~ 'RetirementAge')) %>%
  mutate(Regions = case_when(
    REGION == 11 | REGION == 12 ~ 'Northeast',
    REGION == 21 | REGION == 22 ~ 'Midwest',
    REGION == 31 | REGION == 32 |  REGION == 33 ~ 'South',
    REGION == 41 | REGION == 42 ~ 'West',
    REGION == 97 ~ 'Unknown')) %>%
  mutate(METRO = case_when (
    METRO %in% c(0, 4) ~  'Other',
    METRO == 1 ~  'Rural',
    METRO == 2 ~  'CityCenter',
    METRO == 3 ~  'Suburb')) %>%
  mutate(Education = case_when(
    EDUC99 == 00 ~ 'Unknown',
    EDUC99 >= 1 & EDUC99 <=9 ~ 'NoDiploma',
    EDUC99 == 10 ~ 'HighSchoolDiploma',
    EDUC99 == 11 ~ 'SomeCollege',
    EDUC99 >= 12 & EDUC99 <= 14 ~  'AssociateDegree',
    EDUC99 == 15 ~ 'Bachelors',
    EDUC99 == 16 ~ 'Masters',
    EDUC99 > 16 ~ 'PhDProfessional')) %>%
  mutate(LaborForce = case_when(
    LABFORCE == 0  ~ 'Military',
    LABFORCE == 1  ~ '0',
    LABFORCE == 2  ~ '1')) %>%
  mutate(EmploymentStatus = case_when(
    EMPSTAT <= 1 | EMPSTAT >= 30 ~  'Other',
    EMPSTAT >= 10 & EMPSTAT < 20 ~ '1',
    EMPSTAT >= 20 & EMPSTAT < 30 ~ '1')) %>%
  mutate(WorkStatus = case_when(
    WKSTAT %in% c(11, 14, 15) ~ 'FullTime',
    WKSTAT %in% c(12, 20, 21, 22, 40, 41) ~ 'PartTime',
    WKSTAT == 13 ~ 'FullTimeNoWork',
    WKSTAT == 42 ~ 'PartTimeNoWork',
    WKSTAT == 50 ~ 'NoWorkSeekingFullTime',
    WKSTAT == 60 ~ 'NoWorkSeekingPartTime',
    WKSTAT == 99 ~ 'MilitaryNoLaborForce',
  )) %>%
  mutate(FamilyIncome =case_when(                       # 2020 FPL 1 = $12,760, 2 = $17,240, 3 = $21,720. 4 = $26,200 
    FAMINC <= 600 ~ '<$25k_FPL_Familyof4',
    FAMINC > 600 & FAMINC <= 740 ~ '25k-50K',
    FAMINC > 740 & FAMINC <= 830 ~ '50k-75k',
    FAMINC > 830 & FAMINC <= 841~ '75k-100k',
    FAMINC == 842  ~ '100k - 150k',
    FAMINC == 843 ~ '150k+',
    FAMINC == 999 ~ 'NA'))

dataQ3_b <- dataQ3_a %>%
  left_join(state_fips, by = 'STATEFIP') %>% 
  
  mutate(State = Name) %>% 
  select(-REGION, -STATECENSUS, -PERNUM, -SEX, -MARST, - EMPSTAT, -LABFORCE, -DURUNEMP, -DURUNEM2, -WHYABSNT, 
         -WKSTAT, -MULTJOB, - NUMJOB, - UNION, -OTPAY, -ASECFLAG, -SERIAL, -HISPAN, - EDUC99, -Name) 


# %>%     # METRO = pain in ass with NA's, why?
#   mutate(STATEFIP = as.character(STATEFIP))              # , Year = as.Date(dataQ3_a$YEAR), Month = as.Date(dataQ3_a$MONTH))

dataQ3_c <- dataQ3_b %>%
  left_join(Industries, by = 'IND') %>%
  filter(indname %in% c('Manufacturing', 'Information', 'Retail Trade', 'Arts, Entertainment, and Recreation, and Accommodation and Food Services',
                        'Educational Services, and Health Care and Social Assistance', 'Transportation and Warehousing, and Utilities' )) %>%
  mutate(indname = case_when(indname == 'Retail Trade' ~ 'retail', indname == 'Manufacturing' ~ 'manufacturing', indname == 'Information' ~ 'information',
                             indname == 'Arts, Entertainment, and Recreation, and Accommodation and Food Services' ~ 'art_leisure_hospitality',
                             indname == 'Educational Services, and Health Care and Social Assistance' ~ 'education_healthcare',
                             indname == 'Transportation and Warehousing, and Utilities' ~ 'transport_warehouse'))
# %>%
  #drop_na(dataQ3_c$indname)
  
countNA(dataQ3_b$METRO)
vtable(dataQ3_c, lush = TRUE)
head(dataQ3_c)


