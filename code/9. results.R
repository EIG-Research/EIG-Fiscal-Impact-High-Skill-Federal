# LAST UPDATED 12/06/2024 BY SARAH ECKHARDT

# DESCRIPTION:
  # 1. reads in panel data with federal expenditures and federal tax revenues
  # 2. sets scenario probabilities based on year
  # 3. applies scenario probabilities and outputs results (short-term (2023) and long-term (2023-2032))

# remove dependencies
rm(list = ls())

# load in packages
library(dplyr)
library(readxl)
library(openxlsx)

# set project directories
user_path = "/Users/sarah/Library/CloudStorage/GoogleDrive-sarah@eig.org/My Drive"
project_path = file.path(user_path, "FISCAL IMPACTS FEDERAL")
methods_path = file.path(project_path, "Methodology")
output_path = file.path(project_path, "Output")


# read in full panel
scenarios_panel = read_excel(paste(
  output_path, 
  "scenarios_taxes_expenditures_combined.xlsx",
  sep="/")) %>%
  
  mutate(taxes_individual = income_tax + total_payroll_taxes_employee + excise_taxes + customs_duties,
         taxes_employer = total_payroll_taxes_employer,
         taxes_total = taxes_individual + taxes_employer,
         
         fiscal_impact = taxes_total - federal_expenditures
         ) %>%
  
  select(Year, scenario, 
         income_type, income_h1b, income_spouse,
         taxes_individual, taxes_employer, taxes_total, 
         federal_expenditures,
         fiscal_impact)


scenarios_panel_2023 = scenarios_panel %>%
  filter(Year==2023) %>% select(-c(Year))


# set scenario probabilities
probabilities = read_excel(
  paste(methods_path,
        "scenario probabilities.xlsx",
        sep="/"),
  sheet = "Scenario probabilities",
  skip = 1
) %>% na.omit() %>%
  mutate(scenario = as.numeric(...1)) %>% 
  select(scenario, contains("Probability"))

scenarios_panel = scenarios_panel %>%
  left_join(probabilities, by = "scenario") %>%
  mutate(probability  = case_when(
    Year < 2023+6 ~ `Probability (years 1-6)`,
    Year >= 2023 + 6 ~ `Probability (years 7-10)`
  ))


scenarios_panel_all_years_collapsed = scenarios_panel %>%

    ungroup() %>% group_by(Year, income_type) %>%
  
  summarise(fiscal_impact = sum(fiscal_impact*probability),
            fed_expenditures = sum(federal_expenditures*probability),
            taxes_individual = sum(taxes_individual*probability),
            taxes_employer = sum(taxes_employer*probability),
            taxes_total = sum(taxes_total*probability))


scenarios_panel_2023_collapsed = scenarios_panel_all_years_collapsed %>%
  filter(Year == 2023)


#############
# save output
write.xlsx(scenarios_panel_2023, 
           paste(output_path, "RESULTS_2023_all_scen.xlsx", sep="/"))

write.xlsx(scenarios_panel_all_years_collapsed, 
           paste(output_path, "RESULTS_all_years_probabilistic.xlsx", sep="/"))

write.xlsx(scenarios_panel_2023_collapsed, 
           paste(output_path, "RESULTS_2023_probabilistic.xlsx", sep="/"))
