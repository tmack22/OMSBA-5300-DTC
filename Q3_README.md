# Q3. Retail needs to worry about who has money to spend - what has changed about who is working and earning money?

[IPMUS employment data](https://cps.ipums.org/cps/index.shtml) was used in this analysis, with sample dates ranging from January 2019 to January 2021. The following variables were used:

| Variable |  Definition
|:------    |:-------------------------------------------------     | 
| `YEAR` | Year survey was taken |
| `MONTH` | Month survey was taken |
| `EMPSTAT` | Employment status |
| `WTFINL` | Survey person weight (as calculated by CPS) |
| `IND`  | Respondent's work industry code |
| `REGION` | Respondents location (regional) |
| `AGE`  | Respondent's age |
| `SEX` |  Marker for respondent sex assigned at birth |
| `RACE` | Marker for respondent's race |

2.  Provided industry csv (*indnames.csv*)
  Converts IPUMS industry codes to industry names 
 
| Variable |  Definition
|:------    |:-------------------------------------------------     | 
| `IND` | Work industry code (joins to IPUMS IND field) |
| `indname` | Work industry English name |


3.  Provided geographic csv (*state-geocodes-v2021.csv*)
  Converts IPUMS geographies (STATEFIP) to readable geo names.

| Variable |  Definition
|:------    |:-------------------------------------------------     | 
| `STATEFIP` | State FIP code (joins to IPUMS STATEFIP field) |
| `State` | State English name |
