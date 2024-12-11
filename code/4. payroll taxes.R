# LAST UPDATED 12/05/2024 BY SARAH ECKHARDT

# DESCRIPTION:
  # 1. take in constructed panel data from 3. income taxes.R
  # 2. read in payroll taxes
  # 3. adjust payroll tax cutoff for each year
  # 4. generate employee and employer taxes for each scenario - year

# remove dependencies
rm(list = ls())

# load in packages
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)

# set project directories
user_path = "/Users/sarah/Documents/GitHub"
project_path = file.path(user_path, "EIG-Fiscal-Impact-High-Skill-Federal")
data_path = file.path(project_path, "Data")
output_path = file.path(project_path, "Output")


###########################################
# read in scenarios panel (+ income taxes)

load(file.path(output_path, "h1b_scenarios_panel_inc_tax.RData"))

########################
# read in payroll taxes
payroll_tax_rates = read_excel(
  paste(data_path,
        "model_inputs.xlsx",
        sep="/"),
  sheet = "payroll_tax_rate"
)
  
    # expand into a panel
  payroll_tax_rates_panel = payroll_tax_rates %>%
    
    # expand 10x
    slice(rep(1:n(), 10)) %>%
    mutate(Replication = rep(1:10, each = nrow(payroll_tax_rates))) %>%
    mutate(Year = Replication -1 + 2023) %>% select(-c(Replication))
    
  rm(payroll_tax_rates)

  
#######################################################################
# read in payroll tax adjuster for social security taxes/OASID
# tax only applies to the first XX of earnings for employer & employee

oasid_cutoff = read_excel(
  paste(data_path,
        "CBO",
        "53724-2024-06-Tax-Parameters.xlsx",
        sep="/"),
  sheet = "1. Tax Parameters", skip=7) %>%
  filter(`Tax Year` == "Maximum taxable earnings for the Social Security payroll tax") %>% 
  select(-c(`Tax Year`))

  oasid_cutoff = oasid_cutoff%>%
    mutate(across(names(oasid_cutoff),
           ~as.numeric(.))) %>%
    
  pivot_longer(
    cols = names(oasid_cutoff),
    names_to = "Year",
    values_to = "oasid_inc_cutoff"
  ) %>% mutate(Year = as.numeric(Year),
               oasid_inc_cutoff = as.numeric(oasid_inc_cutoff))


########################################
# construct payroll taxes for each year.
payroll_taxes = left_join(scenarios_panel, oasid_cutoff, by = "Year") %>%
  left_join(payroll_tax_rates_panel, by = "Year")

payroll_taxes = payroll_taxes %>%
  mutate(OASDI_employee_tax_h1b = case_when(
                income_h1b > oasid_inc_cutoff ~ oasid_inc_cutoff*OASDI_employee,
                income_h1b <=oasid_inc_cutoff ~ income_h1b*OASDI_employee),
    
         OASDI_employee_tax_spouse = case_when(
                 income_spouse > oasid_inc_cutoff ~ oasid_inc_cutoff*OASDI_employee,
                 income_spouse <= oasid_inc_cutoff ~ income_spouse*OASDI_employee), # handles the 0 earning here
               
         OASDI_employer_tax_h1b = case_when(
           income_h1b > oasid_inc_cutoff ~ oasid_inc_cutoff*OASDI_employer,
           income_h1b <=oasid_inc_cutoff ~ income_h1b*OASDI_employer),
         
         OASDI_employer_tax_spouse = case_when(
           income_spouse > oasid_inc_cutoff ~ oasid_inc_cutoff*OASDI_employer,
           income_spouse <= oasid_inc_cutoff ~ income_spouse*OASDI_employer), # handles the 0 earning here
         
         HI_employee_tax_h1b = income_h1b*HI_employee,
         HI_employee_tax_spouse = income_spouse*HI_employee,
         
         HI_employer_tax_h1b =  income_h1b*HI_employer,
         HI_employer_tax_spouse = income_spouse*HI_employer,
         
         FUTA_employer_tax_h1b = 7000*FUTA_employer,
         FUTA_employer_tax_spouse = 7000*FUTA_employer,
  
  # construct summary variables
         total_payroll_taxes_employee = OASDI_employee_tax_h1b + OASDI_employee_tax_spouse + HI_employee_tax_h1b + HI_employee_tax_spouse,
         total_payroll_taxes_employer = OASDI_employer_tax_h1b + OASDI_employer_tax_spouse + HI_employer_tax_h1b + HI_employer_tax_spouse + FUTA_employer_tax_h1b + FUTA_employer_tax_spouse,
         total_payroll_taxes = total_payroll_taxes_employee + total_payroll_taxes_employer) %>%
  
  select(-c(oasid_inc_cutoff, OASDI_employee, OASDI_employer, HI_employee, HI_employer, FUTA_employee, FUTA_employer))



#################
# export dataset
setwd(output_path)

save(payroll_taxes, file = "h1b_scenarios_panel_inc_payroll_tax.RData")
