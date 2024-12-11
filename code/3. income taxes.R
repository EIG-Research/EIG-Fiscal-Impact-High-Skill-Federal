# LAST UPDATED 12/06/2024 BY SARAH ECKHARDT

# DESCRIPTION:
  # 1. take in constructed panel data from 2. construct panel.R
  # 2. read in income tax info: brackets, deductions, credits.
  # 3. adjust income tax brackets to the full time horizon using chained CPI
  # 4. apply income tax rates for each scenario - year



# Note on tax filing: In general, non-resident aliens cannot file as a 
# household head, cannot file jointly, and cannot take the standard deductions. 
# However, under the substantial presence test, H-1Bvisa holders are 
# resident aliens according to the IRS.

# Substantial Presence Test: https://www.irs.gov/individuals/international-taxpayers/substantial-presence-test
# Non-residency Rules: https://www.irs.gov/individuals/international-taxpayers/nonresident-figuring-your-tax


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


#########################
# read in scenarios panel
load(file.path(output_path, "h1b_scenarios_panel.RData"))

###############################################################
# read in chained CPI-U to grow our tax bracket cutoffs from CBO
chained_cpiu = read_excel(
  paste(data_path,
        "CBO",
        "51135-2024-06-Economic-Projections.xlsx",
        sep="/"),
  sheet = "2. Calendar Year",
  skip = 6
) %>%
  filter(...2 == "Chained CPI-U") %>%
  select(-c(...1, ...2, ...3, Units))

chained_cpiu = chained_cpiu %>%
  pivot_longer(col = names(chained_cpiu),
               names_to = "Year",
               values_to = "CPI") %>%
  mutate(CPI = as.numeric(CPI),
         Year = as.numeric(Year)) %>% filter(Year >=2023)

CPI_base = chained_cpiu[chained_cpiu$Year==2023,]$CPI

chained_cpiu = chained_cpiu %>%
  mutate(CPI_adj = CPI/CPI_base)



#######################
# read in tax brackets
brackets = read_excel(
  paste(data_path,
        "model_inputs.xlsx",
        sep="/"),
  sheet = "income_tax_rate"
)

      # apply CPI rates to construct future tax bracket cutoffs
      brackets_panel = brackets %>%
        
        # expand 10x
        slice(rep(1:n(), 10)) %>%
        mutate(Replication = rep(1:10, each = nrow(brackets))) %>%
        mutate(Year = Replication -1 + 2023) %>% select(-c(Replication)) %>%
        
        left_join(chained_cpiu, by = "Year") %>%
        
        mutate(`minimum income` = `minimum income`*CPI_adj) %>%
        select(-c(contains("CPI")))

      
      
######################################################
# read in standard deductions and adjust for inflation 
# these are inflation adjusted     
# https://www.irs.gov/newsroom/irs-provides-tax-inflation-adjustments-for-tax-year-2024
      
deductions = read_excel(
  paste(data_path,
        "model_inputs.xlsx",
        sep="/"),
  sheet = "income_tax_deductions"
)
      
      # construct panel
      deductions_panel = deductions %>%
        
        # expand 10x
        slice(rep(1:n(), 10)) %>%
        mutate(Replication = rep(1:10, each = nrow(deductions))) %>%
        mutate(Year = Replication -1 + 2023) %>% select(-c(Replication)) %>%
        
        left_join(chained_cpiu, by = "Year") %>%
        
        mutate(deduction = deduction*CPI_adj) %>%
        select(-c(contains("CPI")))

      
# clean up
rm(brackets, deductions)



#########################
# construct income taxes

# Pr(child | married); estimated in 8. h1b demography.do
nchild = 1.48

  # 1. apply standard deduction
    income_taxes = left_join(scenarios_panel, deductions_panel,
                             by = c("Year", "income_tax_filer_type")) %>%
      mutate(taxable_income = (income_h1b + income_spouse - deduction))

  # 2. apply tax brackets
  
        # merge with brackets
      income_taxes = left_join(brackets_panel, income_taxes, by = c("Year", "income_tax_filer_type"))
  
        # identify taxable income for each bracket

      income_taxes = income_taxes %>%
        ungroup() %>% group_by(Year, scenario, income_type) %>%
        
        mutate(bracket_income_taxed = case_when(
          (taxable_income > `minimum income`) & (taxable_income > lead(`minimum income`)) ~  (lead(`minimum income`) - `minimum income`)*rate, # pays full bracket
          (taxable_income < lead(`minimum income`)) & (taxable_income > `minimum income`) ~ (taxable_income - `minimum income`)*rate,          # maximum applicable bracket
          (taxable_income < lead(`minimum income`)) & (taxable_income < `minimum income`) ~ 0,                                          # inapplicable bracket
          TRUE ~ 0)) %>%                                                                                                                # last bracket
          
          # then sum across brackets to get the income tax
        ungroup() %>% group_by(Year, scenario, income_type, child) %>%
        summarise(income_tax = sum(bracket_income_taxed)) %>%
        
    # 3. apply child tax credit to scenarios in which there is a child.
        mutate(income_tax = case_when(
          child == 0 ~ income_tax,
          child == 1 ~ income_tax - 2000*nchild
        )) %>% select(-c(child))

    
    # merge in with the scenarios panel and export
scenarios_panel = left_join(scenarios_panel, income_taxes, by = c("Year", "scenario", "income_type"))    

setwd(output_path)
save(scenarios_panel, file = "h1b_scenarios_panel_inc_tax.RData")


