# Jomaica - Q1. How has COVID affected the health of the retail industry, as measured by employment?

The following analysis attempts to determine how COVID-19 affected the retail industry's health and retail unemployment is used as a measure of industry health in this analysis. The assumption is that a healthy retail industry has a consistent flow of revenue, allowing it to keep a consistent sized employee pool. An unhealthy industry would be characterized by a high rate of layoffs as retail revenues decline, forcing retailers to minimize losses by reducing employee-related operational costs.

### How Did COVID Impact Unemployment? 

[IPMUS employment data](https://cps.ipums.org/cps/index.shtml) was used in this analysis, with sample dates ranging from January 2019 to January 2021. The following variables were used:

| Variable |  Definition
|:------    |:-------------------------------------------------     | 
| `statefip` | identifies the household's state of residence alphabetically |
| `durunemp` | indicates for how many consecutive weeks each currently unemployed respondent had been without a job and looking for work |
| `empstat` | employment status |
| `whyunemp` | specifies why respondents were unemployed |            |
| `occ2010`  | respondent's occupation based on the Census Bureau's scheme |
| `covidunaw` | could respondent's unemployment be attributed to COVID-19 |
| `lockdown_start` | 


**Raw Data**:
```
q1_employment <- read_dta('cps_00001.dta') 
covid_history <- read_csv('covid_history.csv') 
state_lockdowns <- read_csv('state_lockdown_data.csv')
state_names <- read_csv('state_names.csv')
timeline <- read_csv('timeline.csv')
```
