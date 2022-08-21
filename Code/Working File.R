library(ipumsr)  
library(dplyr)
library(tidyverse)
library(jtools)
library(vtable)
library(lubridate)
library(haven)
library(fixest)
library(ggplot2)
library(formattable)
library(echarts4r)

#### READ IN INDUSTRY CODES ####
Industries <- read_csv('indnames.csv') %>%
  rename(IND = ind)

###### Read in State Federal Information Processing Codes. ######  
state_fips <- read_csv('state-geocodes-v2021.csv') %>%
  filter(!is.na(Region)) %>%
  rename(State = Name) %>%
  select(STATEFIP, State)

###### read in IPUMS demographic data from JAN 2019 through DEC 2021 & filter for those >= 15 years olds ######
dem_ddi <- read_ipums_ddi("cps_00016.xml")   
dem_data <- read_ipums_micro(dem_ddi) %>% 
  zap_labels() %>%  
  mutate(
    month_digit= case_when( 
      nchar(str_trim(as.character(MONTH))) ==1 ~ (paste('0',str_trim(as.character(MONTH)), sep = '')),
      nchar(str_trim(as.character(MONTH))) ==2 ~ (str_trim(as.character(MONTH))), 
      TRUE ~ 'WRONG'
    )
    , date_ = as_date(paste(YEAR, month_digit, '01', sep = '-'))
  )
#### JOIN FILES INTO ONE ####
DF <- dem_data %>% 
  ## Join in geographic info
  left_join(state_fips, by = 'STATEFIP') %>% # 3.9 million observations
  ## Join in industry categories
  left_join(Industries, by = 'IND')

#### DEMOGRAPHICS DATAFRAME ####
demo_data <- DF %>%
  # mutate(CPSID = format(CPSID, scientific = FALSE), CPSIDP = format(CPSID, scientific = FALSE),
  #        HRHHID2 = format(HRHHID2, scientific = FALSE), HRHHID = format(HRHHID, scientific = FALSE)) %>%
  mutate(EmploymentStatus = case_when(
    EMPSTAT <= 1 | EMPSTAT >= 30 ~  'Other',
    EMPSTAT >= 10 & EMPSTAT < 20 ~ '1',
    EMPSTAT >= 20 & EMPSTAT < 30 ~ '0')) %>%
  mutate(sex = case_when(
    SEX == 1 ~  'male',
    SEX == 2 ~ 'female')) %>%
  mutate(MARST = case_when(
    MARST == 1 ~ 'Married',
    MARST != 1 ~ 'Single')) %>%
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
  mutate(REGION = case_when(
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
  mutate(WorkStatus = case_when(
    WKSTAT %in% c(11, 14, 15) ~ 'FullTime',
    WKSTAT %in% c(12, 20, 21, 22, 40, 41) ~ 'PartTime',
    WKSTAT == 13 ~ 'FullTimeNoWork',
    WKSTAT == 42 ~ 'PartTimeNoWork',
    WKSTAT == 50 ~ 'NoWorkSeekingFullTime',
    WKSTAT == 60 ~ 'NoWorkSeekingPartTime',
    WKSTAT == 99 ~ 'MilitaryNoLaborForce',)) %>%
  mutate(FamilyIncome =case_when(                        
    FAMINC <= 600 ~ '<$25k_FPL_Familyof4',
    FAMINC > 600 & FAMINC <= 740 ~ '25k-50K',
    FAMINC > 740 & FAMINC <= 830 ~ '50k-75k',
    FAMINC > 830 & FAMINC <= 841~ '75k-100k',
    FAMINC == 842  ~ '100k - 150k',
    FAMINC == 843 ~ '150k+',
    FAMINC == 999 ~ 'NA')) %>%
  mutate(WHYUNEMP = case_when(
    WHYUNEMP <= 3 ~ 'LostJob',
    WHYUNEMP == 4 ~ 'Quit',
    WHYUNEMP == 5 ~ 'ReEnterLaborForce',
    WHYUNEMP == 6 ~ 'FirstJob',
    WHYUNEMP == 0 ~ 'Military')) %>%
  mutate_if(is.character, as.factor) %>%
  unite(HHID, HRHHID, HRHHID2) %>%     #When combined with HRHHID2, HRHHID can uniquely identify households within basic monthly samples-IPUMS
  # mutate(HHID = as.numeric(HHID)) %>%
  drop_na(indname) %>% # Dropping N/A's in indname as NIU's in military.
  
  select(date_, YEAR, MONTH, HHID, PERNUM, EMPSTAT, CPSID, CPSIDP, HWTFINL, WTFINL, FAMINC, REGION, METRO, AGE, SEX, sex,RACE, MARST, IND,
         WHYUNEMP, AgeGroup, Education, LaborForce, EmploymentStatus, WorkStatus, FamilyIncome, indname, State)

##################### DESCRIPTIVE STATISTICS FOR VARIABLES OF INTEREST######################

summary(demo_data %>%
          select(-YEAR, -MONTH, -date_, -PERNUM, -PERNUM, -HWTFINL, -WTFINL, -IND, - EMPSTAT, 
                 -CPSID, -CPSIDP))

# distinct people in survey = 500,130
distinct_people <- demo_data %>%
  distinct(CPSIDP)

#### Generic Filters to determine time before (1/2019-2/2020), during COVID (MAR 2020) and after (APR2020-to-PRES) ####

df_Precovid <- demo_data %>%
  filter(date_ < '2020-03-01')

df_Covid <- demo_data %>%
  filter(date_ >= '2020-03-01', date_< '2020-04-01')

df_PostCovid <- demo_data %>%
  filter(date_ >= '2020-04-01')

#### EMPLOYMENT & UNEMPLOYMENT ########
##### INDUSTRY-PRE-COVID -ALL #####

EMP_INDUSTRY_PreCovid <- df_Precovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, indname) %>% 
  summarize(UmploymentRateIndustry = 1- (weighted.mean(employed == 1, w = WTFINL))) %>%
  filter(indname != 'Military', indname != 'Other')

##### INDUSTRY-COVID-ALL #####
EMP_INDUSTRY_Covid <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, indname) %>% 
  summarize(UmploymentRateIndustry= 1- (weighted.mean(employed == 1, w = WTFINL))) %>%
  filter(indname != 'Military', indname != 'Other')

##### INDUSTRY-POST-ALL #####
EMP_INDUSTRY_PostCovid <- df_PostCovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, indname) %>% 
  summarize(UmploymentRateIndustryy = 1- (weighted.mean(employed == 1, w = WTFINL))) %>%
  filter(indname != 'Military', indname != 'Other')

##### METRO-PRE-COVID #####

EMP_Metro_PreCovid <- df_Precovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, METRO) %>%
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### Construct chartS for Pre-covid unemployment by METRO, SUBURBAN, RURAL #####
EMPC <- EMP_Metro_PreCovid %>%
  pivot_wider(1, names_from = "METRO", values_from = "UnemploymentRate")

EMPC %>%
  ungroup() %>% # Sometime ungroup is needed after doing group_by() ?
  e_charts(date_) %>%
  e_line(CityCenter, color="#333") %>%
  e_line(Other, color="blue") %>%
  e_line(Rural, color="green") %>%
  e_line(Suburb, color="orange") %>%
  e_tooltip(trigger = "axis")

EMPC_avg <- EMPC %>%
  group_by() %>%
  summarise(
    CityCenter = mean(CityCenter),
    Other = mean(Other),
    Rural = mean(Rural),
    Suburb = mean(Suburb)) %>%
  ungroup()

#create a dataframe with values to chart
EMPC_avg_bar <- data.frame(
  names = c("CityCenter", "Other", "Rural", "Suburb"),
  values = c(0.0379, 0.0353, 0.0383, 0.0301))

##### Employment average by CityCenter, Rural, Suburb #####
EMPC_avg_bar %>%
  e_charts(names) %>%
  e_bar(values)

##### METRO-COVID #####
EMP_Metro_Covid <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(METRO) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))
summary(EMP_Metro_Covid)

##### Construct chartS for COVID unemployment by METRO, SUBURBAN, RURAL #####
EMPMC <- EMP_Metro_Covid %>%
  ungroup()

EMPMC %>%
  e_charts(METRO) %>%
  e_bar(UnemploymentRate) %>%
  e_tooltip()

##### METRO-Post-Covid #####
EMP_Metro_PostCovid <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, METRO) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))
summary(EMP_Metro_PostCovid)  

##### Construct chartS for POST-COVID unemployment by METRO, SUBURBAN, RURAL #####

EMPMPC <- EMP_Metro_PostCovid %>% 
  ungroup()

EMPMPC %>%
  e_charts(METRO) %>%
  e_bar(UnemploymentRate) %>%
  e_tooltip()

################# REGION #####
##### REGION-PRE-COVID ####

EMP_REGION_PreCovid <- df_Precovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, REGION) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

###### Construct chartS for PRE-COVID unemployment by REGION ####

EMP_REGION_PreCovid <- EMP_REGION_PreCovid %>%  # ungroup
  ungroup()

EMP_REGION_PreCovid <- EMP_REGION_PreCovid %>%  #need to pivot wider
  pivot_wider(1, names_from = "REGION", values_from = "UnemploymentRate")

# unemployment by region PRE-COVID line
EMP_REGION_PreCovid %>%
  ungroup() %>% # ungroup is needed after doing group_by()
  e_charts(date_) %>%
  e_line(Midwest) %>%
  e_line(Northeast) %>%
  e_line(South) %>%
  e_line(West, color="orange") %>%
  e_tooltip(trigger = "axis")

EMP_REGION_PreCovid %>%
  ungroup() %>% # ungroup is needed after doing group_by()
  e_charts(date_) %>%
  e_bar(Midwest) %>%
  e_bar(Northeast) %>%
  e_bar(South) %>%
  e_bar(West) %>%
  e_tooltip(trigger = "axis")

# mean unemployment pre-covid
EMPRPC <- EMP_REGION_PreCovid %>%
  group_by() %>%
  summarise(
    Midwest = mean(Midwest),
    Northeast = mean(Northeast),
    South = mean(South),
    West = mean(West))

#create dataframe for above values
EMP_REGION_PreCovid1 <- data.frame(
  REGION = c("Midwest", "Northeast", "South", "West"),
  UnemploymentRate = c(0.0339, 0.0344, 0.0324, 0.0362)
)

EMP_REGION_PreCovid1 %>%
  ungroup() %>%
  e_chart(REGION) %>%
  e_bar(UnemploymentRate)

##### REGION-COVID ####
EMP_REGION_Covid <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, REGION) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

###### construct chart of unemployment by REGION for COVID ####

EMP_REGION_Covid %>%
  ungroup() %>%
  e_chart(REGION) %>%
  e_bar(UnemploymentRate)

#### REGION-POST-COVID ####
EMP_REGION_PostCovid <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, REGION) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

#create chart of unemployment by REGION for POST-COVID
EMP_REGION_PostCovid %>%
  ungroup() %>%
  e_chart(REGION) %>%
  e_bar(UnemploymentRate)

########### RACE ################################################################################
#### RACE PRE-COVID EMPLOYMENT ####
EMP_RACE_Precovid <- df_Precovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(indname, date_, RACE) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

#### RACE COVID EMPLOYMENT #### 
EMP_RACE_Covid <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(indname, date_, RACE) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

#### RACE POST-COVID EMPLOYMENT ####
EMP_RACE_PostCovid <- df_PostCovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(indname, date_, RACE) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

#### EMP AGGREGATE (SEX/AGE/GROUP) ####
################ PRE-COVID AGG #################
##### race #####
EMP_AGG_Precovid_race <- df_Precovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, RACE) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### sex/race #####
EMP_AGG_Precovid_sex_race <- df_Precovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, sex, RACE) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### sex/AgeGroup #####
EMP_AGG_Precovid_sex_age <- df_Precovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, sex, AgeGroup) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### race/AgeGroup #####
EMP_AGG_Precovid_race_age <- df_Precovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, RACE, AgeGroup) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

############# COVID AGG #################
##### race #####
EMP_AGG_Covid_race <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, RACE) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### sex/race #####
EMP_AGG_Covid_sex_race <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, sex, RACE) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### sex/age #####
EMP_AGG_Covid_sex_age <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, sex, AgeGroup) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### race/Age #####
EMP_AGG_Covid_race_age <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, RACE, AgeGroup) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### race/education #####
EMP_AGG_Covid_race_education <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, RACE, Education) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### race/education #####
EMP_AGG_Covid_race_gender_indname <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, RACE, sex, indname) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

#### Post-COVID AGG ####

EMP_AGG_PostCovid_race <- df_PostCovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, RACE) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### sex/race #####
EMP_AGG_PostCovid_sex_race <- df_PostCovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, sex, RACE) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### gender/age #####
EMP_AGG_Covid_sex_age <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, sex, AgeGroup) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### race/Age #####
EMP_AGG_Covid_race_age <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, RACE, AgeGroup) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

##### race/education #####
EMP_AGG_Covid_race_education <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, RACE, Education) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))


#### a <- write_xlsx(EMP_Metro_Covid, "C:/Users/Erik C/Desktop/SU/.OMSBA 5300_Econometrics/DTC/OMSBA-5300-DTC/Code//ec_20220820") ####

#### SUMMARY OF EXPLORATORY FINDINGS ####


#### MODELS ####



##### model ideas  as follows ######

# Employed = B0 + b1gender +e

# employed = B0 + B1Gender*Race + Age + Education  + e

# employed = B0 + B1Gender*Age + Education +e
