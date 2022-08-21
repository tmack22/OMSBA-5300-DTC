library(ipumsr)  
library(dplyr)
library(tidyverse)
library(jtools)
library(vtable)
library(lubridate)
library(haven)
library(fixest)
library(ggplot2)


# Read in Industry codes and select industries of interest
Industries <- read_csv('indnames.csv') %>%
  rename(IND = ind)

# Read in State Federal Information Processing Codes.  
state_fips <- read_csv('state-geocodes-v2021.csv') %>%
  filter(!is.na(Region)) %>%
  rename(State = Name) %>%
  select(STATEFIP, State)
  
# read in IPUMS demographic data from JAN 2019 through DEC 2021 & filter for those >= 15 years olds
dem_ddi <- read_ipums_ddi("cps_00016.xml")   
dem_data <- read_ipums_micro(dem_ddi) %>% 
  zap_labels()   %>%  
  mutate(
    month_digit= case_when( 
      nchar(str_trim(as.character(MONTH))) ==1 ~ (paste('0',str_trim(as.character(MONTH)), sep = '')),
      nchar(str_trim(as.character(MONTH))) ==2 ~ (str_trim(as.character(MONTH))), 
      TRUE ~ 'WRONG'
    )
    , date_ = as_date(paste(YEAR, month_digit, '01', sep = '-'))
        )

DF <- dem_data %>% 
  ## Join in geographic info
  left_join(state_fips, by = 'STATEFIP') %>% # 3.9 million observations
  ## Join in industry categories
  left_join(Industries, by = 'IND')

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
  drop_na(indname) %>% # Dropping N/A's in indname as = NIU's in military.
  
  select(date_, YEAR, MONTH, HHID, PERNUM, EMPSTAT, CPSID, CPSIDP, HWTFINL, WTFINL, FAMINC, REGION, METRO, AGE, SEX, sex,RACE, MARST, IND,
         WHYUNEMP, AgeGroup, Education, LaborForce, EmploymentStatus, WorkStatus, FamilyIncome, indname, State)
# 
# test <- demo_data %>%
#   mutate(yearmo = (YEAR*100) + MONTH) %>%
#   mutate(yearmo = as_date(yearmo)) %>%
#           

####################################################### DESCRIPTIVE STATISTICS FOR VARIABLES OF INTEREST
## Unique CPSIDP

summary(demo_data %>%
          select(-YEAR, -MONTH, -date_, -PERNUM, -PERNUM, -HWTFINL, -WTFINL, -IND, - EMPSTAT, 
                 -CPSID, -CPSIDP))

# distinct people in survey = 500,130
distinct_people <- demo_data %>%
  distinct(CPSIDP)

################################## Generic Filters to determine time before (1/2019-2/2020), during COVID (MAR 2020) and after (APR2020-to-PRES)

df_Precovid <- demo_data %>%
  filter(date_ < '2020-03-01')

df_Covid <- demo_data %>%
  filter(date_ >= '2020-03-01', date_< '2020-04-01')

df_PostCovid <- demo_data %>%
  filter(date_ >= '2020-04-01')

### INC  = HOUSEHOLD LEVEL VARIABLE- FAMINC is annual

# # ## distinct houses = 285,492 households 1/2019-12/2021    GOAL:  What is the mean and median FAMINC by RACE, SEX, industry
# h_count <- demo_data %>%
#    select(HHID) %>%
#    distinct(HHID)
                  #  
                  # ind_money <- demo_data %>%    # may have to filter date out as FAMINC is annual?
                  #   filter(PERNUM == 1) %>%                     # filter(date_ < '2020-04-01' | date_ > '2020-04-01' |
                  #   #unique(H, YEAR) %>%
                  #   group_by(indname, RACE, FAMINC) %>%
                  #   summarise() 


#  distinct(HHID)
# 
# h_money <-  demo_data %>%
#   select(HHID, FAMINC, YEAR, MONTH, date_) %>%
#   group_by(HHID, date_) %>%
#   summarize_each(funs(max, min, mean, median, sd), FAMINC)
# 
# inc <- dd_precovid %>%
#   select(FAMINC, RACE, HRHHID, HRHHID2, date_, YEAR, MONTH) %>%
#   group_by(RACE) %>%
#   summarize(RACE, n = FAMINC)
# 
# summary(inc)
# 
# household_inc <- demo_data %>%
#   group_by(HHHHID, HRHHID2)

##################### EMPLOYMENT & UNEMPLOYMENT ###

# METRO-pre

EMP_Metro_PreCovid <- df_Precovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, METRO) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))
summary(EMP_Metro_PreCovid)

# Plot for Pre-Covid employment by METRO
plot1 <- ggplot(EMP_Metro_PreCovid, aes(METRO, UnemploymentRate, fill = indname)) +
  geom_bar()

# METRO-Covid
EMP_Metro_Covid <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, METRO) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))
summary(EMP_Metro_Covid)

# Plot for Covid employment by METRO
plot2 <- ggplot(EMP_Metro_Covid, aes(METRO, UnemploymentRate, fill = indname)) +
  geom_bar()

# METRO-Post
EMP_Metro_PostCovid <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, METRO) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))
summary(EMP_Metro_PostCovid)  

# Plot for Covid employment by METRO
plot3 <- ggplot(EMP_Metro_PostCovid, aes(METRO, UnemploymentRate, fill = indname)) +
  geom_bar()

# BIND???? to permit evaluation of difference in employment by METRO for 3x time periods?

################# REGION
# REGION-PRE
EMP_REGION_PreCovid <- df_Precovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, REGION) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))
summary(EMP_REGION_PreCovid)

# Plot for Pre-Covid employment by METRO
plot4 <- ggplot(EMP_REGION_PreCovid, aes(REGION, UnemploymentRate, fill = indname)) +
  geom_bar()

# REGION-COVID
EMP_REGION_Covid <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, REGION) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))
summary(EMP_REGION_Covid)

# Plot for Covid employment by METRO
plot5 <- ggplot(EMP_REGION_Covid, aes(REGION, UnemploymentRate, fill = indname)) +
  geom_bar()

# REGION-Post
EMP_REGION_PostCovid <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, REGION) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))
summary(EMP_REGION_PostCovid)  

# Plot for Covid employment by METRO
plot6 <- ggplot(EMP_REGION_PostCovid , aes(REGION, UnemploymentRate, fill = indname)) +
  geom_bar()

# BIND???? to permit evaluation of difference in employment by REGION for 3x time periods?

########### RACE ################################################################################
# RACE PRE-COVID EMPLOYMENT
EMP_RACE_Precovid <- df_Precovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(indname, date_, RACE) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

# Plot for Pre-Covid employment by RACE
plot7 <- ggplot(EMP_RACE_Precovid, aes(RACE, UnemploymentRate, fill = indname)) +
  geom_bar()

# RACE COVID EMPLOYMENT  
EMP_RACE_Covid <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(indname, date_, RACE) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

# Plot for Covid employment by race
plot8 <- ggplot(EMP_RACE_Covid, aes(RACE, UnemploymentRate, fill = indname)) +
  geom_bar()

#RACE POST-COVID EMPLOYMENT
EMP_RACE_PostCovid <- df_PostCovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(indname, date_, RACE) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

# Plot for Post-Covid employment by race
plot9 <- ggplot(EMP_RACE_PostCovid, aes(RACE, UnemploymentRate, fill = indname)) +
  geom_bar()

# BIND???? to permit evaluation of difference in employment by race for 3x time periods?

# EMP AGGREGATE (SEX/AGE/GROUP)
# PRE-COVID AGG
EMP_AGG_Precovid <- df_Precovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, sex, AgeGroup) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

# Plot for Pre-Covid employment by RACE
plot10 <- ggplot(EMP_RACE_Precovid, aes(AgeGroup, UnemploymentRate, fill = indname)) +
  geom_bar()

# COVID AGG  
EMP_AGG_Covid <- df_Covid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, sex, AgeGroup) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

# Plot for Covid employment by race
plot11 <- ggplot(EMP_RACE_Covid, aes(AgeGroup, UnemploymentRate, fill = indname)) +
  geom_bar()

# Post-COVID AGG
EMP_AGG_PostCovid <- df_PostCovid %>%
  filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
  mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
  group_by(date_, sex, AgeGroup) %>% 
  summarize(UnemploymentRate = 1- (weighted.mean(employed == 1, w = WTFINL)))

# Plot for Post-Covid employment by race
plot12 <- ggplot(EMP_RACE_PostCovid, aes(AgeGroup, UnemploymentRate, fill = indname)) +
  geom_bar()

##BIND??? Is there a way to pull unemployment for agegroup before, during and after covid?




 #                       
                        
                        ## Covid employment (SAVE JUST in CASE WEIGHT. MEAN WRONG!!!)
                        # EMP_Covid <- df_Covid %>%
                        #   filter(EMPSTAT < 30 & EMPSTAT >= 10) %>% 
                        #   mutate(employed = ifelse(EMPSTAT <20, 1, 0), unemployed = ifelse(EMPSTAT > 20, 1, 0)) %>%
                        #   group_by(indname, date_, RACE, sex) %>% 
                        #   summarise(num_employed = sum(employed==1),
                        #             num_unemployed = sum(unemployed==1),
                        #             num_employable = num_employed + num_unemployed) %>% 
                        #   mutate(percent_emp = num_employed/num_employable,
                        #          percent_unemp = num_unemployed/num_employable) %>%
                        #   select(indname, date_, RACE, sex, percent_unemp)



                        
                        # # Where are races working pre-covid
                        # RacePreCovid <- df_Precovid  %>%
                        #   group_by(race = as_factor(RACE), Industry = as_factor(indname)) %>%
                        #   summarize(n = sum(WTFINL)) %>%
                        #   mutate(Percent = n/sum(n))
                        # 
                        # ggplot(data = DF_precovid, x = race, y = Percent)

        
        # model ideas  as follows
        
        # Employed = Bet0 + gender +e
        
        # employed = B0 + B1Gender*Race + Age + Education  + e
        
        # employed = B0 + B1Gender*Age + Education +e