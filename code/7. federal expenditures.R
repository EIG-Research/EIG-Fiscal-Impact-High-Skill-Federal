# LAST UPDATED 12/06/2024 BY SARAH ECKHARDT

# DESCRIPTION:
  # 1. read in expenditure data from OBM
  # 2. read in computed growth rates in expenditures
  # 3. apply to expenditures by category; then apply to h1bs depending on specified scenario.

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
methods_path = file.path(project_path, "Methodology")
output_path = file.path(project_path, "Output")


# read in expenditures methodology

expenditures_methods= read_excel(
  paste(methods_path, 
        "expenditures methodology.xlsx",
        sep="/")) %>% na.omit() %>%
  mutate(`1` = case_when(
    `Scenario 1` == "fixed" ~ 0,
    `Scenario 1` == "excluded" ~ 0,
    `Scenario 1` == "average" ~ 1),
    
    `2` = case_when(
      `Scenario 2` == "fixed" ~ 0,
      `Scenario 2` == "excluded" ~ 0,
      `Scenario 2` == "average" ~ 1,
      `Scenario 2` == "average (X2)" ~ 2),
    `3` = case_when(
      `Scenario 3` == "fixed" ~ 0,
      `Scenario 3` == "excluded" ~ 0,
      `Scenario 3` == "average" ~ 1,
      `Scenario 3` == "average (X2)" ~ 2),
    
    `4` = case_when(
      `Scenario 4` == "fixed" ~ 0,
      `Scenario 4` == "excluded" ~ 0,
      `Scenario 4` == "average" ~ 1,
      `Scenario 4` == "average (X2)" ~ 2),
    
    `5` = case_when(
      `Scenario 5` == "fixed" ~ 0,
      `Scenario 5` == "excluded" ~ 0,
      `Scenario 5` == "average" ~ 1,
      `Scenario 5` == "average (X2)" ~ 2)) %>%
  select(-c(contains("Scenario"))) %>%
  
  # adjust elementary education spending to the # of children in the household
  # 1.48 is the number of estimated children per household, conditional on being married.
  # see 8. h1b demography.do
  
  mutate(`2` = ifelse(`Function and Subfunction`=="501 Elementary, secondary, and vocational education",
                      `2`*1.48, `2`),
         `3` = ifelse(`Function and Subfunction`=="501 Elementary, secondary, and vocational education",
                      `3`*1.48, `3`))


# read in expenditures data
fed_expenditures = read_excel(
  paste(data_path, "OBM",
        "hist03z2_fy2025.xlsx", sep="/"),
  skip = 2) %>%
  select(c(`Function and Subfunction`, `2023`)) %>%
  mutate(`2023` = as.integer(`2023`),
         `2023` = ifelse(`2023` <0, 0, `2023`))

# get US population in 2023
# https://www.census.gov/data/datasets/time-series/demo/popest/2020s-national-total.html
US_population_2023 = read_excel(
  paste(
    data_path, 
    "Census", 
    "NST-EST2023-POP.xlsx", 
    sep="/"),
  skip = 2) %>%
  rename(`pop2023`=`...6`) %>%
  filter(`Geographic Area`=="United States")
US_population_2023=US_population_2023$pop2023[1]

print("US population as of 2023:")
US_population_2023

# per capita expenditure by category, according to scenario assumptions.
expenditures = merge(fed_expenditures, expenditures_methods, 
                     by="Function and Subfunction", keep.all=TRUE) %>%
  
  # in millions
  mutate(expenditures_per_capita = 
           `2023`*1000000/US_population_2023) %>% select(-c(`2023`))


# apply growth rates. non applicables are left blank.
growth_rates = read_excel(paste(
  data_path,
  "OBM", 
  "expenditure projections.xlsx", sep="/"
), skip=2) %>%
  select(`Function and Subfunction`, growth_rate_applied) %>% na.omit() 

# merge in
expenditures_annual = right_join(expenditures, growth_rates, by = "Function and Subfunction" ) %>%
  rename(`2023` = expenditures_per_capita) %>%
  mutate(`2024` = `2023` + `2023`*growth_rate_applied,
         `2025` = `2024` + `2024`*growth_rate_applied,
         `2026` = `2025` + `2025`*growth_rate_applied,
         `2027` = `2026` + `2026`*growth_rate_applied,
         `2028` = `2027` + `2027`*growth_rate_applied,
         `2029` = `2028` + `2028`*growth_rate_applied,
         `2030` = `2029` + `2029`*growth_rate_applied,
         `2031` = `2030` + `2030`*growth_rate_applied,
         `2032` = `2031` + `2031`*growth_rate_applied,
         `2033` = `2032` + `2032`*growth_rate_applied) %>%
  
  # collapse by year
  select(-c(growth_rate_applied)) %>%
  pivot_longer(cols = c(`2023`, `2024`, `2025`, `2026`, `2027`, `2028`, `2029`, `2030`, `2031`, `2032`, `2033`),
               names_to = "Year") %>%
  mutate(`1` = `1`*value,
         `2` = `2`*value,
         `3` = `3`*value,
         `4` = `4`*value,
         `5` = `5`*value) %>% na.omit() %>%
  ungroup() %>%
  group_by(Year) %>%
  summarise(`1` = sum(`1`),
            `2` = sum(`2`),
            `3` = sum(`3`),
            `4` = sum(`4`),
            `5` = sum(`5`)) %>%
  
  # reshape again
  pivot_longer(cols = c(`1`, `2`,`3`, `4`,`5`),
               names_to = "scenario",
               values_to = "federal_expenditures") %>%
  mutate(Year = as.numeric(Year),
         scenario = as.numeric(scenario))


# merge into full dataset, then export
setwd(output_path)

load("h1b_scenarios_panel_inc_payroll_excise_customs_tax.RData")

scenarios_panel = scenarios_panel %>%
  left_join(expenditures_annual, by = c("Year", "scenario"))

# save
save(scenarios_panel, file = "scenarios_taxes_expenditures_combined.RData")
