# LAST UPDATED 12/05/2024 BY SARAH ECKHARDT

# DESCRIPTION:
# 1. read in customs duties from the OBM
# 2. read in population projections from the CBO
# 3. construct customs duties, growing at a per-capita rate. 
      # (underestimate; true value is somewhere between population and personal income growth)
# 4. apply to h1bs based on # of adults in household


# remove dependencies
rm(list = ls())

# load in packages
library(dplyr)
library(readxl)
library(openxlsx)

# set project directories
user_path = "/Users/sarah/Library/CloudStorage/GoogleDrive-sarah@eig.org/My Drive"
project_path = file.path(user_path, "FISCAL IMPACTS FEDERAL")
data_path = file.path(project_path, "Data")
output_path = file.path(project_path, "Output")


###########################
# customs duties from OBM
customs_duties = read_excel(paste(
  data_path,
  "OBM",
  "hist02z5_fy2025.xlsx",
  sep="/"
), skip=2) %>% select(`Fiscal Year`, `Customs Duties and Fees`) %>%
  
  # using population based projections.
  # customs duties are in millions.
  
  filter(`Fiscal Year` == 2023)

customs_duties = customs_duties$`Customs Duties and Fees`[1]

# read in population projections from the CBO
# using Jan 2023 estimates for the 2023 baseline.
population_projection = read_excel(paste(
  data_path,
  "CBO",
  "57059-2023-01-Demographic-Projections.xlsx",
  sep="/"
), sheet = "Population and Growth", skip=6) 

population_projection = population_projection[5,] %>%
  select(-c(...1))
  
customs_duties_combined = population_projection %>% 
  
  # reshaping
  pivot_longer(cols = names(population_projection),
               names_to = "Year",
               values_to = "Population") %>% ungroup() %>%
  
  mutate(Year = as.numeric(Year),
         pop_2023 = case_when(
    Year == 2023 ~ Population, 
    TRUE ~ 0), pop_2023 = max(pop_2023),
    
    # population growth realtive to baseline year
    population_growth = (Population/pop_2023)) %>%
  
  # calculate customs duties per capita for each year.
  # this is per capita customs duties in 2023 (customs_duties/pop_2023)
  # multiplied by the population growth factor
  
  mutate(customs_duties_per_capita = customs_duties/pop_2023*population_growth) %>%
  select(Year, customs_duties_per_capita)


# combine, and adjust to # of adults in the household.
# using total adults rather than total wage earners as consumption = f(hh members)
# for discussion.

#####################
# read in panel data

load(file.path(output_path, "h1b_scenarios_panel_inc_payroll_excise_tax.RData"))

scenarios_panel = scenarios_panel %>%
  left_join(customs_duties_combined, by = "Year") %>%
  mutate(customs_duties = customs_duties_per_capita *(1 + spouse)) %>%
         select(-c(customs_duties_per_capita))


# export dataset
setwd(output_path)
save(scenarios_panel, file = "h1b_scenarios_panel_inc_payroll_excise_customs_tax.RData")
