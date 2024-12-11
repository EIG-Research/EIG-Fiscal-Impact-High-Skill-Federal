# LAST UPDATED 12/06/2024 BY SARAH ECKHARDT

# Description:

  # this file estimates the 95th and 99th percentile of H-1B incomes using the
  # LCA disclosure files, for fiscal year 2023.

  # note that the median and mean incomes come directly from the USCIS's 
  # fiscal year report for 2023; as the LCA disclosure data does not accurately
  # represent the H-1B population. The 95th and 99th percentile estimates should
  # be interpreted with skepticism.


# remove dependencies
rm(list = ls())

# load packages
library(readxl)
library(tidycensus)
library(tidyr)
library(dplyr)
library(haven)
library(stringr)
library(readr)



# set directories
user_path = "/Users/sarah/Documents/GitHub"
project_path = file.path(user_path, "EIG-Fiscal-Impact-High-Skill-Federal")
data_path = file.path(project_path, "Data/DOL Prevailing Wage Test")
output_path = file.path(project_path, "Output")

setwd(data_path)

# read in data and combine
quarters = list()

for (i in (1:4)) {
  print(i)
  file_name = paste0("LCA_Disclosure_Data_FY2023_Q",i,".csv")
  
  # load and clean
    file = read_csv(file_name) %>%
    filter(CASE_STATUS =="Certified") %>%
    filter(VISA_CLASS == "H-1B") %>%
    select(CASE_NUMBER,
           CASE_STATUS,
           VISA_CLASS,
           DECISION_DATE,
           NEW_EMPLOYMENT,
           WAGE_RATE_OF_PAY_FROM, 
           WAGE_RATE_OF_PAY_TO, 
           PREVAILING_WAGE,
           WAGE_UNIT_OF_PAY) %>%
    mutate(WAGE_RATE_OF_PAY_FROM = 
             as.numeric(str_replace_all(WAGE_RATE_OF_PAY_FROM,
                                        c("\\$"="", ","=""))),
           WAGE_RATE_OF_PAY_TO = 
             as.numeric(str_replace_all(WAGE_RATE_OF_PAY_TO,
                                        c("\\$"="", ","=""))),
           PREVAILING_WAGE =  
             as.numeric(str_replace_all(PREVAILING_WAGE,
                                        c("\\$"="", ","=""))))
    
  quarters[[i]] = file
}

LCA2023_all = bind_rows(quarters)

    # clean up
    rm(quarters, file)
    
    # duplicate obs, with case # as unique id.
    sum(duplicated(LCA2023_all$CASE_NUMBER)==TRUE)
    sum(duplicated(LCA2023_all$CASE_NUMBER)==TRUE) == sum(duplicated(LCA2023_all)==TRUE)
    
    # drop duplicates
    LCA2023_all = unique(LCA2023_all)


# get average pay
LCA2023_all = LCA2023_all %>%
  
  # some of these ranges have NA values; formula dependent on NA vals
  
mutate(AVG_PAY = case_when(
          is.na(WAGE_RATE_OF_PAY_FROM) & is.na(WAGE_RATE_OF_PAY_TO) ~ 
                NA,
          is.na(WAGE_RATE_OF_PAY_FROM) & !is.na(WAGE_RATE_OF_PAY_TO) ~ 
                WAGE_RATE_OF_PAY_TO,
          !is.na(WAGE_RATE_OF_PAY_FROM) & is.na(WAGE_RATE_OF_PAY_TO) ~ 
               WAGE_RATE_OF_PAY_FROM,
          !is.na(WAGE_RATE_OF_PAY_FROM) & !is.na(WAGE_RATE_OF_PAY_TO) ~ 
               (WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO)/2)) %>%
  
  # make time period adjustments
  mutate(ADJUSTED_PAY = case_when(
          WAGE_UNIT_OF_PAY== "Year" ~ AVG_PAY,
          WAGE_UNIT_OF_PAY== "Week" ~ AVG_PAY*52,
          WAGE_UNIT_OF_PAY == "Month" ~ AVG_PAY*12,
          WAGE_UNIT_OF_PAY == "Hour" ~ AVG_PAY*2080,
          WAGE_UNIT_OF_PAY == "Bi-Weekly" ~ AVG_PAY*26),
         
  # prevailing wages, for comparison 
    ADJUSTED_PREVAILING = case_when(
          WAGE_UNIT_OF_PAY== "Year" ~ PREVAILING_WAGE,
          WAGE_UNIT_OF_PAY== "Week" ~ PREVAILING_WAGE*52,
          WAGE_UNIT_OF_PAY == "Month" ~ PREVAILING_WAGE*12,
          WAGE_UNIT_OF_PAY == "Hour" ~ PREVAILING_WAGE*2080,
          WAGE_UNIT_OF_PAY == "Bi-Weekly" ~ PREVAILING_WAGE*26)) %>%
  
  mutate(average_higher_than_prevailing = ADJUSTED_PAY>ADJUSTED_PREVAILING) %>%
  
  # set pay as prevailing for when the adjusted pay is less than the prevailing wage
  mutate(ADJ_PAY_PREVAIL = case_when(
    ADJUSTED_PAY >= ADJUSTED_PREVAILING ~ ADJUSTED_PAY,
    ADJUSTED_PAY < ADJUSTED_PREVAILING ~ ADJUSTED_PREVAILING # illegal
  ))



###################### ISSUE 1 ######################
# is the proposed wage higher or lower than prevailing wage?
table(LCA2023_all$average_higher_than_prevailing)

# FALSE   TRUE 
# 74035 414352 

# in 15% of cases, the average wage IS NOT higher than prevailing. this is an issue;
# options: (1) go with prevailing
#          (2) go with average unless it's lower than prevailing


###################### ISSUE 2 ######################
count(LCA2023_all)

# we have more approved LCA petitions than approved H-1B cases for 2023
# 386,318 is what is reported as approved from USCIS
# (https://www.uscis.gov/sites/default/files/document/reports/OLA_Signed_H-1B_Characteristics_Congressional_Report_FY2023.pdf)

print("LCA petitions versus approved H1B visas for 2023 discrepency")
count(LCA2023_all)  - 386318
# = 102,070 more LCA petitions than h1b visas applied

# this is because LCAs are filed before the H1B petition, so this 2023 data reflects potential future h1b workers
# LCAs are linked to Job Positions & can be filed for more than one position.
  # could have lower-wage h1bs for 1 LCA application  


###################### SOLUTION ######################

# use USCIS estimates for the federal model mean and median
# use LCA for 95th percentile and 99th percentile

income_95th_h1b = quantile(LCA2023_all$ADJUSTED_PAY, 
                           c(0.95), na.rm=TRUE)

setwd(output_path)
save(income_95th_h1b, file = "h1b_income_95th.RData") 

