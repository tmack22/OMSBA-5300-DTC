library(ipumsr)  
library(dplyr)
library(tidyverse)
library(jtools)
library(vtable)
library(car)
library(lubridate)
library(haven)


# Read in Industry codes and select industries of interest
Industries <- read_csv('indnames.csv') %>%
  rename(IND = ind)

# Read in State Federal Information Processing Codes.  
state_fips <- read_csv('state-geocodes-v2021.csv') %>% 
  mutate(State = Name) ## Rename 'state' Name field for clarity

# read in IPUMS demographic data from JAN 2019 through DEC 2021 & filter for those >= 15 years olds
dem_ddi <- read_ipums_ddi("cps_00016.xml")   
dem_data <- read_ipums_micro(dem_ddi) %>% 
  zap_labels()   %>% 
  mutate(
    month_digit= case_when( 
      nchar(str_trim(as.character(MONTH))) ==1 ~ (paste('0',str_trim(as.character(MONTH)), sep = '')),
      nchar(str_trim(as.character(MONTH))) ==2 ~ (str_trim(as.character(MONTH))), 
      TRUE ~ 'WRONG' ## sanity check to make sure we aren't accidentally dropping anything
    )
    , date_ = as_date(paste(YEAR, month_digit, '01', sep = '-'))
  )
## Filter for legal adults only

# Read in IPUMS Covid and Unemployment Data
cov_ddi <- read_ipums_ddi("cps_00004.xml")
cov_data <- read_ipums_micro(cov_ddi) %>%
  zap_labels() %>%
  mutate(month_digit= (paste('0',str_trim(as.character(MONTH)), sep = ''))) %>%
  mutate(date_ = paste(YEAR, month_digit, '01', sep = '-'))

# Clean and Categorize Raw Data

## Create Demographics df

demographics_data <- dem_data %>%
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
  mutate(EmploymentStatus = case_when(
    EMPSTAT <= 1 | EMPSTAT >= 30 ~  'Other',
    EMPSTAT >= 10 & EMPSTAT < 20 ~ '1',
    EMPSTAT >= 20 & EMPSTAT < 30 ~ '0')) %>%
  mutate(WorkStatus = case_when(
    WKSTAT %in% c(11, 14, 15) ~ 'FullTime',
    WKSTAT %in% c(12, 20, 21, 22, 40, 41) ~ 'PartTime',
    WKSTAT == 13 ~ 'FullTimeNoWork',
    WKSTAT == 42 ~ 'PartTimeNoWork',
    WKSTAT == 50 ~ 'NoWorkSeekingFullTime',
    WKSTAT == 60 ~ 'NoWorkSeekingPartTime',
    WKSTAT == 99 ~ 'MilitaryNoLaborForce',)) %>%
  mutate(FamilyIncome =case_when(                       # 2020 FPL 1 = $12,760, 2 = $17,240, 3 = $21,720. 4 = $26,200 
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
    WHYUNEMP == 0 ~ 'Military'))%>%
  
  select(-STATECENSUS, -PERNUM, -SEX, - EMPSTAT, -LABFORCE, -DURUNEMP
         , -DURUNEM2, -WHYABSNT, 
         -WKSTAT, -MULTJOB, - NUMJOB, - UNION, -OTPAY, -ASECFLAG, -SERIAL
         , -HISPAN, - EDUC99)


##################################### KRISTEN COVID VARIABLES ######################################

covid_df <- cov_data %>%
  
  ## mutate and categorize Covid variables (May 2020 on ~)
  mutate(
    covid_work_remote = (case_when(
      COVIDTELEW == 1 ~ FALSE,
      COVIDTELEW == 2 ~ TRUE
    ))
    , covid_unable_to_work = (case_when(
      COVIDUNAW == 1 ~ FALSE,
      COVIDUNAW == 2 ~ TRUE
    ))
    , covid_pto = (case_when(
      COVIDPAID == 1 ~ FALSE,
      COVIDPAID == 2 ~ TRUE
    ))
    , covid_did_not_look = (case_when(
      COVIDLOOK == 1 ~ FALSE,
      COVIDLOOK == 2 ~ TRUE
    ))
  ) %>%
  
  ## Mutate and categorize disabilities
  mutate(
    has_disability = case_when(
      DIFFANY == 1 ~ FALSE,
      DIFFANY == 2 ~ TRUE)
    
    , num_disability =
      ifelse(DIFFHEAR == 2, 0, 1) +
      ifelse(DIFFMOB == 2, 0, 1) +
      ifelse(DIFFEYE == 2, 0, 1) +
      ifelse(DIFFREM == 2, 0, 1) +
      ifelse(DIFFPHYS == 2, 0, 1)+
      ifelse(DIFFCARE == 2, 0, 1)
    
  ) %>%
  
  ## mutate and categorize unemployment and unemployment reason codes
  mutate(
    unemployment_reason = (case_when(
      WHYUNEMP == 1 ~	"Layoff_Lost_Job",
      WHYUNEMP == 3	~ "Temp_job_ended",
      WHYUNEMP == 4	~ "Left_Job",
      WHYUNEMP == 5	~ "Re_entrant",
      WHYUNEMP == 6	~ "New_entrant"
    ))
    
    , absence_reason = (case_when(
      WHYABSNT %in% c( 01, 02)	~ "Layoff",
      WHYABSNT %in% c(03, 10)	~ "Business_Conditions_Disputes",
      WHYABSNT == 04	~ "Starting_New_Job",
      
      WHYABSNT == 05	~ "Vacation_Personal",
      WHYABSNT == 06	~ "Health",
      
      WHYABSNT %in% c(07, 08, 09)	~ "Family",
      WHYABSNT == 11	~ "Weather",
      
      WHYABSNT == 12	~ "School",
      WHYABSNT == 13	~ "Military",
      
      WHYABSNT == 14	~ "Does_Not_Work",
      WHYABSNT == 15	~ "Other"
    ))
    
    , reason_left_job = (case_when(
      UH_WHYLFT_B2 == 01 ~	"Family",
      UH_WHYLFT_B2 == 02 ~	"School",
      UH_WHYLFT_B2 == 03 ~	"Health",
      UH_WHYLFT_B2 == 04 ~	"Retirement",
      UH_WHYLFT_B2 == 05 ~	"Temporary_Job_Complete",
      UH_WHYLFT_B2 %in% c(06, 07) ~	"Business_Conditions_Disputes",
      UH_WHYLFT_B2 == 08 ~	"Other"
    ))
    
    ## Additional Earnings/Wage (Person Level)
    , worked_last_year = (case_when(
      WORKLY == 1 ~FALSE,
      WORKLY == 2 ~TRUE
    ))
    , worked_last_year = (case_when(
      WORKLY == 1 ~FALSE,
      WORKLY == 2 ~TRUE
    ))
    
    , needs_cert_for_job = (case_when(
      UH_JCERT_B1 == 1 ~TRUE,
      UH_JCERT_B1 == 2 ~FALSE
    ))
    , is_discouraged_worker = (case_when(
      UH_DSCWK_B2 == 1 ~ "YES",
      UH_DSCWK_B2 == 2 ~"Conditionally_Yes"
    ))
    
    , discouraged_wants_job = (case_when(
      UH_DWRSN_B1 == 01 ~"Yes_Maybe",
      UH_DWRSN_B1 == 02 ~"No",
      UH_DWRSN_B1 == 03 ~"Retired",
      UH_DWRSN_B1 == 04 ~"Disabled",
      UH_DWRSN_B1 == 05	~"Unable_to_Work"
    ))
    
    , last_worked_cat = (case_when(
      WNLWNILF == 10	~"Last_12_months",
      WNLWNILF == 20	~"More_than_12Months",
      WNLWNILF == 21	~"1to2YrsAgo",
      WNLWNILF == 22	~"2to3YrsAgo",
      WNLWNILF == 23	~"3to4YrsAgo",
      WNLWNILF == 24	~"4to5YrsAgo",
      WNLWNILF == 25	~"over5YrsAgo",
      WNLWNILF == 30	~"Never_Worked"
    ))
  ) %>%
  select (
    date_, CPSIDP,
    starts_with("cov"),has_disability, unemployment_reason, absence_reason, worked_last_year)


DF <- demographics_data %>% 
  ## Join in geographic info
  left_join(state_fips, by = 'STATEFIP') %>%
  ## Join in industry categories
  left_join(Industries, by = 'IND') %>% 
  ## Join in Covid/Unemployment Data
  left_join(covid_df, by = "CPSIDP") %>%   # 25 million rows
  mutate(industry_cats = case_when(
    indname == 'Retail Trade' ~ 'retail'
    , indname == 'Manufacturing' ~ 'manufacturing'
    , indname == 'Information' ~ 'information'
    , indname == 'Arts, Entertainment, and Recreation, and Accommodation and Food Services' ~ 'art_leisure_hospitality'
    , indname == 'Educational Services, and Health Care and Social Assistance' ~ 'education_healthcare'
    , indname == 'Transportation and Warehousing, and Utilities' ~ 'transport_warehouse'
    , indname == 'Construction' ~ 'Construction',
    TRUE ~ 'OTHER') 
  ) 

## mutate and select & convert into factors
DF_convert_class <- DF %>%
  mutate(Metro = as_factor(METRO), Race = as_factor(RACE), MaritalStatus = as_factor(MARST),
         Occupation = as_factor(OCC), WhyUnemployed = as_factor(WHYUNEMP),
         AgeGroup = as_factor(AgeGroup), Regions = as_factor(REGION),
         Education = as_factor(Education), LaborForce = as_factor(LaborForce),
         EmploymentStatus = as_factor(EmploymentStatus), FamilyIncome = as_factor(FamilyIncome),
         WorkStatus = as_factor(WorkStatus), IndustryName = as_factor(indname), 
         IndustryCategories = as_factor(indname), State = as_factor(Name),
         CovidTelework = as_factor(COVIDTELEW), CovidNoWork = as_factor(COVIDUNAW), CovidPaid = as_factor(COVIDPAID),
         CovidLook = as_factor(COVIDLOOK), CovidMed = as_factor(COVIDMED),
         unemployment_reason = as_factor(unemployment_reason),
         absence_reason = as_factor(absence_reason),
  )



# Filters
DF_precovid <- DF %>%
  filter(date_.x <= '2020-03-01', AGE >= 18)

DF_Covid <- DF %>%
  filter(date_.x >= '2020-03-01', date_.x< '2020-04-01', AGE >= 18)

DF_PostCovid <- DF %>%
  filter(date_.x > '2020-04-01', AGE >= 18)


GEOGRAPHY_MONEY <- DF  %>%   # this is broken
  select(FAMINC, REGION, State, HRHHID, HRHHID2, date_.x, YEAR, MONTH) %>%
  group_by(REGION, State, FAMINC) %>%
  summarize(REGION)

# Where are races working pre-covid
racePreCovid <- DF_precovid  %>%
  group_by(RACE = as_factor(RACE), Industry = as_factor(indname), YEAR, MONTH, AgeGroup) %>%
  summarize(n = sum(WTFINL)) %>%
  mutate(Percent = n/sum(n))


# Where are races working during covid
raceCovid <-DF_Covid %>%
  group_by(RACE = as_factor(RACE), Industry = as_factor(indname), YEAR, MONTH) %>%
  summarize(n = sum(WTFINL)) %>%
  mutate(Percent = n/sum(n))

# Difference between 

# Where are races working during post_covid
racePostCovid <-DF_PostCovid %>%
  group_by(RACE = as_factor(RACE), Industry = as_factor(indname)) %>%
  summarize(n = sum(WTFINL)) %>%
  mutate(Percent = n/sum(n))


# unemployment by race
race_unemployment <- DF_Covid %>%
  filter(EmploymentStatus != C('Other',1)) %>%
  group_by(AvgUnempRate = 1-weighted.mean(EmploymentStatus)
           
           
           # ################################ PRE-COVID COHORT ###############################################################
           # 
           # 
           # pre_covid_demo <- DF_convert_class %>%
           #   select(YEAR, MONTH, HWTFINL, CPSID, REGION, Regions, METRO, Metro, FAMINC, FamilyIncome, WTFINL, AGE, RACE, Race, MARST, MaritalStatus, WHYUNEMP, WhyUnemployed, PAIDHOUR, AgeGroup,
           #          Education, LaborForce, EmploymentStatus, WorkStatus, FamilyIncome, State, indname, IndustryName, date_.x, IndustryCategories) %>%
           #   filter(date_.x <= '2020-03-01', AGE >= 18)
           #   #drop_na(Industry)
           # 
           # 
           # #descriptive statistics
           # pre_covid_demo <- summary(pre_covid_demo %>%
           #                               select(Regions, Metro, FamilyIncome, FAMINC, AGE, Race, MaritalStatus, AgeGroup, Education, WorkStatus, State, IndustryName)) 
           # 
           # ######################################## COVID ################################################
           # 
           # covid_demo <- DF_convert_class %>%
           #   select(YEAR, MONTH, HWTFINL, CPSID, REGION, Regions, METRO, Metro, FAMINC, FamilyIncome, WTFINL, AGE, RACE, Race, MARST, MaritalStatus, WHYUNEMP, WhyUnemployed, PAIDHOUR, AgeGroup,
           #          Education, LaborForce, EmploymentStatus, WorkStatus, FamilyIncome, State, indname, IndustryName, date_.x, IndustryCategories) %>%
           #   filter(date_.x >= '2020-03-01', date_.x< '2020-04-01', AGE >= 18)
           # 
           # #descriptive statistics
           # covid_demo_descriptive <- summary(covid_demo %>%
           #                                     select(Regions, Metro, FamilyIncome, FAMINC, AGE, Race, MaritalStatus, AgeGroup, Education, WorkStatus, State, IndustryName)) 
           # 
           # 
           # ############################### POST-COVID #########################################################
           # 
           # post_covid_demo <- DF_convert_class %>%
           #   select(YEAR, MONTH, HWTFINL, CPSID, REGION, Regions, METRO, Metro, FAMINC, FamilyIncome, WTFINL, AGE, RACE, Race, MARST, MaritalStatus, WHYUNEMP, WhyUnemployed, PAIDHOUR, AgeGroup,
           #          Education, LaborForce, EmploymentStatus, WorkStatus, FamilyIncome, State, indname, IndustryName, date_.x, IndustryCategories) %>%
           #   filter(date_.x > '2020-04-01', AGE >= 18)
           # 
           # #descriptive statistics
           # desc_post_covid_ <- summary(post_covid_demo %>%
           #                               select(Regions, Metro, FamilyIncome, FAMINC, AGE, Race, MaritalStatus, AgeGroup, Education, WorkStatus, State, IndustryName)) 
           # 
           # 
           
           
           
           ############# GENDER  ####
           # womens' ageGroup and Head of Household?
           
           # industry gender?
           
           # which industry has highest female unemployment?
           
           
           
           ####EDUCATION ######
           
           # Education by Gender?
           
           
           # unemployment by education level?
           
           
           
           
           
           
           
           
           
           
           
           # model ideas  as follows
           
           # Employed = Bet0 + gender +e
           
           # employed = B0 + B1Gender*Race + Age + Education  + e
           
           # employed = B0 + B1Gender*Age + Education +e
