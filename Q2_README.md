# Q2. How has retail fared relative to other industries?

The below analysis is focused on how the retail industry fared against other industries between 2019-01 to 2022-12. The key assumption is that employment level is the determining factor on whether a industry is successful or not. 

[IPMUS employment data](https://cps.ipums.org/cps/index.shtml) was used in this analysis, with sample dates ranging from January 2019 to December of 2021. The following variables were used:

| Variable |  Definition
|:------    |:-------------------------------------------------     | 
| `year` | survey year |
| `serial` | household serial number |
| `month` | month |
| `hwtfinal` | Household weight, Basic Monthly |            |
| `cpisid`  | CPSID, household record |
| `asecflag` | Flag for ASEC |
| `pernum` | Person number in sample unit  |
| `wtfinl` | Final Basic Weight |
| `cpsidp` | CPSID, person record  |
| `empstat` | Employment status |
| `ind` | Industry Identifier |
| `indname` | Industry name |
| `industry_cat` | Whether the industry is considered to be Retail or Non-Retail |
| `date` | The date of the survey |
| `covid_active` | A binary flag indicating covid was active
                   0 : date < '2020-03' 
                   1 : date >= '2020-03'|
| `emp_count` | The total count of employment |
| `avg_employment` | The average employment amount per month |
| `employment_change` | The amount of employment change from the previous period |
| `percent_change` | The percent change in employment from the previous period  |








