# LAST UPDATED 12/05/2024 BY SARAH ECKHARDT

# DESCRIPTION:
# 1. estimate excise taxes
# 2. expand out -- assuming constant share of income


# this file estimates individual expenditures on excise-tax eligible expenditures
# by income bracket and educational group
# using data from the Consumer Expenditure Survey
# using calendar year estimate for 2023; combining quarterly estimates and applying weights
# following the provided methodology by the BFS, with an example .R file available here:
# https://www.bls.gov/cex/pumd-getting-started-guide.htm

# these estimates are then converted to expected excise taxes collected per
# individual through the following steps:

# 1. For each salary quintile and education group 
#     (<HS, HS, Some College (including associates), BA+)
# compute the per individual average amount spent on excise tax
# eligible expenditure categories.
#     these are: alcohol, tobacco, telephone, gas, and airline tickets
# see "Federal Model/Supporting Docs/excise tax handling.xlsx" for the supporting
# methodology for these eligible expenditure categories.

# 2. Then the group’s share of total excise-tax eligible spending for the US
# is estimated by taking the group’s total spending / total spending across 
# education X quintiles.

# 3. The per person excise revenue per category is then taken by taking the
# BEA’s report of total excise tax revenues collected * the group’s share 
# of the contribution / the group’s population.


######################
# remove dependencies
rm(list = ls())

###########
# packages
library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(magrittr)
library(readr)
library(ggplot2)

##################
# set directories

user_path = "/Users/sarah/Library/CloudStorage/GoogleDrive-sarah@eig.org/My Drive"
project_path = file.path(user_path, "FISCAL IMPACTS FEDERAL")
data_path = file.path(project_path, "Data/CES/intrvw23")
output_path = file.path(project_path, "Output")
path_bea = file.path(project_path, "Data/BEA")

####################################
# Read in and stack the MTBI files #
####################################

# (see codebook for the relevant UCC codes; Data/CES/ce-pumd-interview-diary-dictionary.xlsx)
ucc_list = c("790330",      # alcohol ...
             "200900", 
             "630110",      # tobacco ...
             "630210",
             "270101",      # telephone ...
             "270102",
             "270104",
             "270105",
             "470111",      # gas ...
             "470112",      
             "470113",     
             "470212",     
             "530110")      # airline tickets


# select only 2023 quarters.
year = 2023

# combine CES quarters; construct annual weights.
mtbi <- lapply(
  dir(data_path, pattern = "^mtbi.*[.]csv$",full.names = TRUE),
  fread,
  select = c("NEWID", "COST", "UCC", "REF_YR"),
  na.strings = c("", ".", "NA")
) %>% bind_rows() %>%
  
  # Change the column names to lower case
  setnames(old = names(.), new = tolower(names(.))) %>%
  
  # Filter for expenditures made in the given year and UCC's used for
  # publication
  filter(ref_yr %in% year, ucc %in% ucc_list) %>%
  
  # Change "newid" to a character variable
  mutate(newid = as.character(newid)) %>%
  
  # Remove unnecessary columns
  select(-ref_yr) %>%
  
  # Group the data by newid and UCC
  group_by(newid) %>%
  
  # Get the sum of expenditures on each UCC for each newid
  summarise(total_expend = sum(cost),
            alcohol_expend = sum(cost*(ucc %in%
                                         c("790330", "200900"))),
            
            tobacco_expend = sum(cost*(ucc %in% 
                                         c("630110", "630210"))),
            
            telephone_expend = sum(cost*(ucc %in% 
                                           c("270101", "270102", "270104", "270105"))),
            
            gas_expend = sum(cost*(ucc %in% 
                                     c("470111", "470112", "470113", "470212"))),
            airline_expend = sum(cost*(ucc == "530110")))


#############################################
# Read in and stack the fmli and memi files #
#############################################
# memi files cover data for the reference person
# fmli files cover data for the reference person + household.

# we are estimating excise taxes based on individual level income; based on HH head.

# mosto relevant vars --
#FSALARYX (fmli) - family wage and salary income,before taxes
#SALARYX (memi) - individual wage and salary income, before taxes


# read in family-evel info
fmli <- lapply(
  dir(data_path, pattern = "^fmli.*[.]csv$", full.names = TRUE),
  fread,
  select = c("NEWID",
             "FINLWT21", 
             "QINTRVMO", 
             "QINTRVYR",
             "EDUC_REF",
             "FSALARYX",
             "AGE_REF"),
  
  na.strings = c("", ".", "NA")
) %>% bind_rows() %>%
  setnames(old = names(.), new = tolower(names(.)))%>%
  mutate(
    newid = as.character(newid),
    qintrvmo = as.numeric(qintrvmo))


# read in the memi files for individual income
memi <- lapply(
  dir(data_path, pattern = "^memi.*[.]csv$", full.names = TRUE),
  fread,
  select = c("NEWID",
             "SALARYX",
             "CU_CODE"),
  
  na.strings = c("", ".", "NA")
) %>% bind_rows() %>%
  setnames(old = names(.), new = tolower(names(.))) %>%
  mutate(newid = as.character(newid),
         salaryx = ifelse(is.na(salaryx), 0, salaryx))


fmli_memi = merge(memi, fmli, by = "newid") %>%
  mutate(
    
    # Generate a calendar-year population weight variable
    # based on CES's example code.
    
    popwt = ifelse(
      qintrvmo %in% 1:3 & qintrvyr %in% year,
      (qintrvmo - 1) / 3 * finlwt21 / 4,
      ifelse(
        qintrvyr %in% (year + 1),
        (4 - qintrvmo) / 3 *finlwt21 / 4,
        finlwt21 / 4
      )
    )
  ) %>%
  select(-c(qintrvyr, qintrvmo))


# combine
annual_excise_expend <- left_join(fmli_memi, mtbi, by = "newid") %>%
  mutate(across(c(contains("expend")),
                ~ replace(., is.na(.), 0)))

# clean up
rm(fmli, mtbi, fmli_memi, memi)



############
# checking #
############

# annual averages, to compare to published summary tables

annual_excise_expend %>%
  summarise(mean_excise_expend = sum(total_expend * finlwt21) / sum(popwt),
            mean_alcohol = sum(alcohol_expend * finlwt21) / sum(popwt),
            mean_tobacco = sum(tobacco_expend * finlwt21) / sum(popwt),
            mean_telephone = sum(telephone_expend * finlwt21) / sum(popwt),
            mean_gas = sum(gas_expend * finlwt21) / sum(popwt),
            mean_airline = sum(airline_expend * finlwt21) / sum(popwt),
            mean_salary_household = sum(fsalaryx * popwt) / sum(popwt),
            mean_salary_hh_head = sum(salaryx * popwt) / sum(popwt)) %>%
  pivot_longer(cols = c(contains("mean")))



###############################
# add in desired categoricals #
###############################

annual_excise_expend = annual_excise_expend %>%
  
  mutate(across(c(contains("expend")),
                ~ (.*finlwt21)/popwt,
                .names = paste0("{.col}", "_wt"))) %>%
  
  
  # education categories
  mutate(education = case_when(
    educ_ref == 0 ~ 1,                             # no edu
    (educ_ref == 10 | educ_ref == 11) ~ 1,           # less than HS grad
    (educ_ref == 12) ~ 2,                            # HS grad
    (educ_ref == 13 | educ_ref == 14) ~ 3,           # some college / < 4 year
    educ_ref >= 15 ~ 4                              # BA, and >BA. we collapse these as H-1Bs & H-4s are BA or >BA.
  ),
  
  
  # construct age groups to compare to Daniel's age/education breakouts
  age_groups = case_when(
    age_ref >=18 & age_ref <25 ~ "18-24",
    age_ref >=25 & age_ref <30 ~ "25-29",
    age_ref >=30 & age_ref <35 ~ "30-34",
    age_ref >=35 & age_ref <40 ~ "35-39",
    age_ref >=40 & age_ref <45 ~ "40-44",
    age_ref >=45 & age_ref <50 ~ "45-49",
    age_ref >=50 & age_ref <55 ~ "50-54",
    age_ref >=55 & age_ref <60 ~ "55-59",
    age_ref >=60 & age_ref <65 ~ "60-64",
    age_ref >=65 & age_ref <70 ~ "65-59",
    age_ref >=70 & age_ref <80 ~ "70-79",
    age_ref >=80  ~ "80+",
    TRUE ~ "out of age range")) %>%
  
  # distribute expenditures equally across household members to get an individual estimate.
  # this is an underestimate for the reference person
  
  ungroup() %>% group_by(newid) %>%
  mutate(members = n()) %>% ungroup() %>%
  mutate(per_member_expenditure = total_expend/members) %>%
  
  # construct indicators for household composition
  mutate(n_unmarried_partners = 1*(cu_code == 0),
         n_spouse = 1*(cu_code==2),
         n_child = 1*(cu_code==3),
         n_grandchild = 1*(cu_code==4),
         n_inlaws = 1*(cu_code ==5),
         n_sibling = 1*(cu_code==6),
         n_parents = 1*(cu_code==7),
         n_other_related = 1*(cu_code==8),
         n_unrelated = 1*(cu_code==9)) %>%
  group_by(newid) %>%
  mutate(across(contains("n_"),
                ~sum(.)))


###########################
# weight the excise taxes #
###########################

# 1. read in the BEA's report of total excise taxes collected in 2023 (in 2023 USD)
# note that this assigns ALL excise taxes to households, even the inapplicable ones.

# one model improvement would be to remove these, which we can do with Table 3.5U.

BEA_2023_excise_totals = readxl::read_excel(paste(path_bea, "Table.xlsx", sep="/"),
                                            skip=5)  %>%
  filter(`...2` =="Excise taxes") %>%
  select(`...2`, `2023`) %>% mutate(`2023` = as.numeric(`2023`))

BEA_2023_excise_totals = BEA_2023_excise_totals$`2023`[1]
BEA_2023_excise_totals


#####################################
# BASED ON INCOME OF REFERENCE PERSON

individual_excise_wts = annual_excise_expend %>%   filter(cu_code == 1) %>%
  
  # exclude individuals not reporting any wage/salary income
  filter(salaryx > 0) %>% ungroup() %>%
  
  
  # construct salary quntiles
  mutate(
    salary_quintile = hutils::weighted_ntile(salaryx, 5, weights = popwt)) %>%
  
  ungroup() %>%
  
  # generate maximum income for proper quintile assignment 
  # (do we want quintiles, or more granular categories?)
  
  group_by(salary_quintile) %>%
  mutate(max_quintile_salary = max(salaryx, na.rm = TRUE),
         mean_quintile_salary = mean(salaryx, na.rm = TRUE)) %>%
  
  # 1. mean expenditure, and population by group
  # based on household expenditure distributed equally to individuals.
  
  group_by(salary_quintile, education, max_quintile_salary, mean_quintile_salary) %>%
  summarise(mean_excise_expend = sum(per_member_expenditure * finlwt21)/ sum(popwt),
            pop = sum(popwt)) %>%
  
  # 2. compute approximate total spending by group
  ungroup() %>%
  
  mutate(group_total = mean_excise_expend * pop) %>%
  
  # 3. compute weights
  
  mutate(group_wt = group_total/sum(group_total)) %>%
  
  # 4. apply weights to BEA totals, and perform per capita transformations
  # this gives us excise tax revenues apportioned by quintile - education groups,
  # based on distribution of expenditure acrosss groups,
  # with equal expenditure across HH members (an issue with larger families with one income earner)
  
  mutate(excise_rev = BEA_2023_excise_totals * 1000000000 * group_wt / pop)



##################
# PREPARE TABLES

results_individual = individual_excise_wts %>%
  mutate(education_labs = case_when(
    education == 1 ~ "<HS",
    education == 2 ~ "HS",
    education == 3 ~ "SomeCol",
    education == 4 ~ "BA+"
  ),
  Quintile = paste("Quintile", salary_quintile)) %>%
  select(Quintile, max_quintile_salary, mean_quintile_salary, education_labs, excise_rev) %>%
  mutate(excise_rate = excise_rev/mean_quintile_salary) %>%
  select(-c(excise_rev)) %>%
  pivot_wider(names_from = education_labs,
              values_from = excise_rate,
              id_cols = c(Quintile, max_quintile_salary, mean_quintile_salary))

results_individual

##########################################################################################
# approach for now -- applying this rate to income to get nominal increase in excise taxes

# based on bracket in 2023
  # h1bs are in quintile 5
  # spouses are in quintile 4


spouse_rate = results_individual %>%
  filter(`Quintile` == "Quintile 4")
spouse_rate = spouse_rate$`BA+`[1]

h1b_rate = results_individual %>%
  filter(`Quintile` == "Quintile 5")
h1b_rate = h1b_rate$`BA+`[1]


scenarios_panel = read_excel(
  paste(output_path, "h1b_scenarios_panel_inc_payroll_tax.xlsx", sep="/")) %>%
  mutate(excise_tax_rate_h1b = h1b_rate,
         excise_tax_rate_spouse = spouse_rate,
         excise_tax_h1b = excise_tax_rate_h1b*income_h1b,
         excise_tax_spouse = excise_tax_rate_spouse*income_spouse,
         total_excise_tax_contribution = excise_tax_h1b+excise_tax_spouse) %>%
  
  select(-c(contains("excise_tax_rate")))


# export dataset
write.xlsx(scenarios_panel, paste(output_path, "h1b_scenarios_panel_inc_payroll_excise_tax.xlsx",sep="/"))
