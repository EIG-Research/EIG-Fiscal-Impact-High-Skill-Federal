# LAST UPDATED 12/09/2024 BY SARAH ECKHARDT

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
library(tidyr)

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



########################################
# estimate annual mean net tax contribution each year
#translate to total revenues

# 2 sec
# assuming 100,000 and 500,000 ne

annual_100k = 100000
annual_500k = 500000

retention_rate_green_card = 0.45

cumulative_estimate = scenarios_panel_all_years_collapsed %>%
  ungroup() %>%
  filter(income_type == "mean") %>%
  mutate(
    
    # gained
    new_h1bs_100k = annual_100k,
    
    new_h1bs_500k = annual_500k,
    
    # lost
    lost_h1bs_100k = case_when(
      Year < 2029 ~ 0,
      Year >=2029 ~ annual_100k - (annual_100k*retention_rate_green_card)
    ),
    
    lost_h1bs_500k = case_when(
      Year < 2029 ~ 0,
      Year >=2029 ~ annual_500k - (annual_500k*retention_rate_green_card)
    ),
    
    # cumulative
      adjusted_h1bs_annnual_100k =  new_h1bs_100k - lost_h1bs_100k,
      
      adjusted_h1bs_annnual_500k =  new_h1bs_500k - lost_h1bs_500k,
    
      adjusted_h1bs_100k = cumsum(adjusted_h1bs_annnual_100k),
    
      adjusted_h1bs_500k = cumsum(adjusted_h1bs_annnual_500k),
    
    # adjust revenues, expenditures, total impact to be a cumulative estimate
    total_revs_100k = taxes_total*adjusted_h1bs_100k,
    total_expend_100k = fed_expenditures*adjusted_h1bs_100k,
    total_impact_100k = fiscal_impact*adjusted_h1bs_100k,
    
    total_revs_500k = taxes_total*adjusted_h1bs_500k,
    total_expend_500k = fed_expenditures*adjusted_h1bs_500k,
    total_impact_500k = fiscal_impact*adjusted_h1bs_500k)


collapsed_estimate = 
cumulative_estimate %>%
  summarise(`total revenues_100k` = sum(total_revs_100k),
            `total expenditures_100k` = sum(total_expend_100k),
            `total impact_100k`= sum(total_impact_100k),
            `total impact_100k`= sum(total_impact_100k),
            `total revenues_500k`= sum(total_revs_500k),
            `total expenditures_500k`= sum(total_expend_500k),
            `total impact_500k`= sum(total_impact_500k)) %>%
  pivot_longer(cols = c(contains("total"))) %>%
  mutate(`annual h1b inflow` = substr(name, nchar(name) - 3, nchar(name)),
         variable = substr(name, 1, nchar(name) - 5)) %>% select(variable, annual_h1b_inflow, value) %>%
  pivot_wider(names_from = variable,
              values_from = value)

write.xlsx(collapsed_estimate, paste(output_path, "cumulative impact.xlsx", sep="/"))
