# LAST UPDATED 12/06/2024 BY SARAH ECKHARDT

# DESCRIPTION:
  # 1. Take in constructed scenarios
  # 2. generate annual incomes (2023)
  # 3. read in CBO EIC for income growth adjustment
  # 4. apply to scenarios for 10 year horizon; construct panel


# remove dependencies
rm(list = ls())

# load in packages
library(dplyr)
library(readxl)
library(openxlsx)

# set project directories
user_path = "/Users/sarah/Documents/GitHub"
project_path = file.path(user_path, "EIG-Fiscal-Impact-High-Skill-Federal")
data_path = file.path(project_path, "Data")
output_path = file.path(project_path, "Output")



#############################
# read in scenario framework
scenarios = read_excel(
  paste(data_path,
        "model_inputs.xlsx",
        sep="/"),
  sheet = "scenarios"
)


##########################
# generate annual incomes

  # median and mean h-1b incomes from USCIS
  # https://www.uscis.gov/sites/default/files/document/reports/OLA_Signed_H-1B_Characteristics_Congressional_Report_FY2023.pdf
income_median_h1b = 118000
income_mean_h1b = 130000

  # see LCA 95th percentile H1B incomes.R

load(file.path(output_path, "h1b_income_95th.RData"))


  # spouse incomes
    # 2019 estimate from Brannon, Ike, and M. Kevin McGee. "Repealing H-4 Visa Work Authorization: A Cost-Benefit Analysis." Available at SSRN 3349786 (2019)
    # https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3349786

income_mean_h4_2019 = 77000

  # adjusted to 2023 applying growth in mean h-1b incomes from 2019-2023
  # source: USCIS
    # 2019: https://www.uscis.gov/sites/default/files/document/reports/Characteristics_of_Specialty_Occupation_Workers_H-1B_Fiscal_Year_2019.pdf
    # 2023: https://www.uscis.gov/sites/default/files/document/reports/OLA_Signed_H-1B_Characteristics_Congressional_Report_FY2023.pdf
   
    income_mean_h1b_2019 = 107000
    growth_factor = income_mean_h1b/income_mean_h1b_2019
    
    income_mean_h4 = income_mean_h4_2019*growth_factor
   
scenarios = scenarios %>%
  mutate(income_h1b = 
           case_when(
              income_type =="median" ~ income_median_h1b,
              income_type == "mean" ~ income_mean_h1b,
              income_type =="95th percentile" ~ income_95th_h1b),
         
         income_spouse = case_when(
           spouse_works == 1 ~ income_mean_h4,
           spouse_works == 0 ~ 0
         ))
    

#####################################
# 10-year income growth projections
    # using the CBO's ECI
    income_growth = read_excel(
      paste(data_path,
            "CBO",
            "51135-2024-06-Economic-Projections.xlsx",
            sep="/"),
      sheet = "2. Calendar Year",
      skip = 6
    ) %>%
      filter(...2 == "Employment Cost Index (ECI), Private Wages and Salaries") %>%
      select(-c(...1, ...2, ...3, Units))

    income_growth = income_growth %>%
      pivot_longer(col = names(income_growth),
                   names_to = "Year",
                   values_to = "EIC") %>%
      mutate(EIC = as.numeric(EIC),
             Year = as.numeric(Year)) %>% filter(Year >=2023)
    
EIC_base = income_growth[income_growth$Year==2023,]$EIC

income_growth = income_growth %>%
  mutate(EIC_adj = EIC/EIC_base)



#############################
# construct annual estimates

scenarios_panel = scenarios %>%
  
  # expand 10x
  slice(rep(1:n(), 10)) %>%
  mutate(Replication = rep(1:10, each = nrow(scenarios))) %>%
  mutate(Year = Replication -1 + 2023) %>% select(-c(Replication)) %>%
  
  left_join(income_growth, by = "Year") %>%
  
  mutate(income_h1b = income_h1b*EIC_adj,
         income_spouse = income_spouse*EIC_adj) %>%
  
  select(-c(contains("EIC")))


##############
# save. output
save(scenarios_panel, file = "h1b_scenarios_panel.RData")
