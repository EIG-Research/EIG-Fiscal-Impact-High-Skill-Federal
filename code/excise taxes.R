
# last updated October 30th, 2024 by Sarah Eckhardt

# Description:

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

user_path = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner"
path_project = file.path(user_path, "/GitHub/EIG-Fiscal-Impact-High-Skill-Federal")
path_data = file.path(path_project, "data/CES/intrvw23")
path_bea = file.path(path_project, "data/BEA")
path_output = file.path(path_project, "output")

####################################
# Read in and stack the MTBI files #
####################################


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


year = 2023

mtbi <- lapply(
  dir(path_data, pattern = "^mtbi.*[.]csv$",full.names = TRUE),
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

#FSALARYX (fmli) - family wage and salary income,before taxes
#SALARYX (memi) - individual wage and salary income, before taxes

fmli <- lapply(
  dir(path_data, pattern = "^fmli.*[.]csv$", full.names = TRUE),
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
  dir(path_data, pattern = "^memi.*[.]csv$", full.names = TRUE),
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
 # rm(fmli, mtbi, fmli_memi, memi)

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
# add in missing categoricals #
###############################

annual_excise_expend = annual_excise_expend %>%
    
  mutate(across(c(contains("expend")),
                ~ (.*finlwt21)/popwt,
                .names = paste0("{.col}", "_wt"))) %>%
  
  mutate(education = case_when(
      educ_ref == 0 ~ 1,                             # no edu
      (educ_ref == 10 | educ_ref == 11) ~ 1,           # less than HS grad
      (educ_ref == 12) ~ 2,                            # HS grad
      (educ_ref == 13 | educ_ref == 14) ~ 3,           # some college / < 4 year
      educ_ref >= 15 ~ 4                              # BA+
    ),
    
    high_edu = case_when( # for reference person only; ignoring spouse 
      educ_ref  >= 15 ~ 1,                           # BA+
      educ_ref<15 ~ 0
    ),
    
    # to compare to Daniel's categories
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
    
    # distribute expenditures across household members to get an individual estimate.
    
    ungroup() %>% group_by(newid) %>%
    mutate(members = n()) %>% ungroup() %>%
    mutate(per_member_expenditure = total_expend/members) %>%
    
    # identify household composition
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

BEA_2023_excise_totals = readxl::read_excel(paste(path_bea, "Table.xlsx", sep="/"),
                                    skip=5)  %>%
  filter(`...2` =="Excise taxes") %>%
  select(`...2`, `2023`) %>% mutate(`2023` = as.numeric(`2023`))
  
BEA_2023_excise_totals = BEA_2023_excise_totals$`2023`[1]
BEA_2023_excise_totals

###########################
# BASED ON HOUSEHOLD/FAMILY

family_excise_wts = annual_excise_expend %>%
  
  # exclude families not reporting any wage/salary income
  filter(fsalaryx > 0) %>%
  
  # keep only household heads; duplicates. 
  filter(cu_code ==1) %>%
  
  # salary quntiles
  mutate(
    salary_quintile = hutils::weighted_ntile(fsalaryx, 5, weights = popwt)) %>%
  
  ungroup() %>%
  
  group_by(salary_quintile) %>%
  mutate(max_quintile_salary = max(fsalaryx, na.rm = TRUE)) %>%
  
  # 1. mean expenditure, and population by group
  
  group_by(salary_quintile, education, max_quintile_salary) %>%
  summarise(mean_excise_expend = sum(tot_expend * finlwt21)/ sum(popwt),
            pop = sum(popwt)) %>%
  
  # 2. compute approximate total spending by group
  ungroup() %>%
  
  mutate(group_total = mean_excise_expend * pop) %>%
  
  # 3. compute weights
  
  mutate(group_wt = group_total/sum(group_total)) %>%
  
  # 4. apply weights to BEA totals, and perform per capita transformations
  mutate(excise_rev = BEA_2023_excise_totals * 1000000000 * group_wt / pop)



######################################################
# BASED ON REFERENCE PERSON (ASSUMED TO BE INDIVIDUAL)

individual_excise_wts = annual_excise_expend %>%   filter(cu_code == 1) %>%
  
  # exclude individuals not reporting any wage/salary income
  filter(salaryx > 0) %>% ungroup() %>%
  
  # salary quntiles
  mutate(
    salary_quintile = hutils::weighted_ntile(salaryx, 5, weights = popwt)) %>%
  
  ungroup() %>%
  
  group_by(salary_quintile) %>%
  mutate(max_quintile_salary = max(salaryx, na.rm = TRUE)) %>%
  
  # 1. mean expenditure, and population by group
  
  group_by(salary_quintile, education, max_quintile_salary) %>%
  summarise(mean_excise_expend = sum(per_member_expenditure * finlwt21)/ sum(popwt),
            pop = sum(popwt)) %>%
  
  # 2. compute approximate total spending by group
  ungroup() %>%
  
  mutate(group_total = mean_excise_expend * pop) %>%
  
  # 3. compute weights
  
  mutate(group_wt = group_total/sum(group_total)) %>%
  
  # 4. apply weights to BEA totals, and perform per capita transformations
  mutate(excise_rev = BEA_2023_excise_totals * 1000000000 * group_wt / pop)


##################
# PREPARE TABLES

results_family = family_excise_wts %>%
  mutate(education_labs = case_when(
    education == 1 ~ "<HS",
    education == 2 ~ "HS",
    education == 3 ~ "SomeCol",
    education == 4 ~ "BA+"
  ),
  Quintile = paste("Quintile", salary_quintile)) %>%
  rename(`Max Quintile Salary` = max_quintile_salary) %>%
  select(Quintile, `Max Quintile Salary`, education_labs, excise_rev) %>%
  pivot_wider(names_from = education_labs,
              values_from = excise_rev,
              id_cols = c(Quintile, `Max Quintile Salary`))

results_individual = individual_excise_wts %>%
  mutate(education_labs = case_when(
    education == 1 ~ "<HS",
    education == 2 ~ "HS",
    education == 3 ~ "SomeCol",
    education == 4 ~ "BA+"
  ),
  Quintile = paste("Quintile", salary_quintile)) %>%
  rename(`Max Quintile Salary` = max_quintile_salary) %>%
  select(Quintile, `Max Quintile Salary`, education_labs, excise_rev) %>%
  pivot_wider(names_from = education_labs,
              values_from = excise_rev,
              id_cols = c(Quintile, `Max Quintile Salary`))

openxlsx::write.xlsx(results_family, 
           paste(path_output,
                 "excise_taxes_table_family.xlsx", sep="/"))

openxlsx::write.xlsx(results_individual, 
                     paste(path_output,
                           "excise_taxes_table_individual.xlsx", sep="/"))

#####################
# additional tables #
#####################

# by age group and individual salary, for estimate comparison

individual_excise_edu_wts = annual_excise_expend %>%
  
  # exclude individuals not reporting any wage/salary income
  filter(salaryx > 0) %>% filter(cu_code ==1) %>%
  
  # expand the BA and >BA education groups for comparison (collapsed in our estiamte
  # as we do not observe which H1bs  or spouses are BA and which >BA.

  mutate(education = ifelse(educ_ref == 16, 5, education)) %>%
  
  # 1. mean expenditure, and population by group
  
  group_by(education, age_groups) %>%
  summarise(mean_excise_expend = sum(per_member_expenditure * finlwt21)/ sum(popwt),
            pop = sum(popwt)) %>%
  
  # 2. compute approximate total spending by group
  ungroup() %>%
  
  mutate(group_total = mean_excise_expend * pop) %>%
  
  # 3. compute weights
  
  mutate(group_wt = group_total/sum(group_total)) %>%
  
  # 4. apply weights to BEA totals, and perform per capita transformations
  mutate(excise_rev = BEA_2023_excise_totals * 1000000000 * group_wt / pop)

results_individual_edu = individual_excise_edu_wts %>%
  mutate(education_labs = case_when(
    education == 1 ~ "<HS",
    education == 2 ~ "HS",
    education == 3 ~ "SomeCol",
    education == 4 ~ "BA",
    education ==5 ~ ">BA"
  )) %>%
  select(education_labs, age_groups, excise_rev) %>%
  pivot_wider(names_from = education_labs,
              values_from = excise_rev)
  
openxlsx::write.xlsx(results_individual_edu, 
                     paste(path_output,
                           "excise_taxes_table_individual_edu_by_age_comparison.xlsx", sep="/"))

#####################
# by salary quintile 


annual_excise_expend %>%
  filter(fsalaryx > 0) %>% filter(cu_code == 1) %>% ungroup() %>%
  mutate(
    salary_quintile = hutils::weighted_ntile(fsalaryx, 5, weights = popwt)) %>%
  ungroup() %>%   
  group_by(salary_quintile) %>%
  summarise(mean_excise_expend = sum(per_member_expenditure * finlwt21)/ sum(popwt),
            pop = sum(popwt)) %>%
  
  # 2. compute approximate total spending by group
  ungroup() %>%
  
  mutate(group_total = mean_excise_expend * pop) %>%
  
  # 3. compute weights
  
  mutate(group_wt = group_total/sum(group_total)) %>%
  
  # 4. apply weights to BEA totals, and perform per capita transformations
  mutate(excise_rev = BEA_2023_excise_totals * 1000000000 * group_wt / pop) %>%
  select(salary_quintile, mean_excise_expend)

  
  
  


#####################
# by salary quintile 

annual_excise_expend %>%
  filter(fsalaryx > 0) %>% filter(cu_code == 1) %>% ungroup() %>%
  mutate(
    salary_quintile = hutils::weighted_ntile(fsalaryx, 5, weights = popwt)) %>%
  ungroup() %>%
  group_by(salary_quintile) %>%
  summarise(mean_excise_expend = sum(total_expend * finlwt21) / sum(popwt),
            mean_alcohol = sum(alcohol_expend * finlwt21) / sum(popwt),
            mean_tobacco = sum(tobacco_expend * finlwt21) / sum(popwt),
            mean_telephone = sum(telephone_expend * finlwt21) / sum(popwt),
            mean_gas = sum(gas_expend * finlwt21) / sum(popwt),
            mean_airline = sum(airline_expend * finlwt21) / sum(popwt),
            mean_salary = sum(fsalaryx * popwt) / sum(popwt),
            max_salary = max(fsalaryx))


###############
# regressions #
###############

################################################################################
# why don't we increase excise taxes when there are children in the household??
# what is the impact of having multiple household members?


# tables 1: impact of increasing household members
model_household1 = lm(total_expend ~ members, 
                      weights = popwt,
                      data = annual_excise_expend)


model_household2 = lm(total_expend ~ members*factor(education),
                      weights = popwt,
                      data = annual_excise_expend)


model_household3 = lm(total_expend ~ members +
                        fsalaryx + factor(education), 
                      weights = popwt,
                      data = annual_excise_expend)

model_household4 = lm(total_expend ~ members +
                        fsalaryx*factor(education), 
                      weights = popwt,
                      data = annual_excise_expend)


# need an interaction

library(stargazer)
stargazer(model_household1, model_household2, model_household3, model_household4,
          type="text",
          covariate.labels = c("household members", 
                               "household wage/salary income", 
                               "HS grads", "SomeCol", "BA+", 
                               "members X HS grads",
                               "members X SomeCol",
                               "members X BA+",
                               "income X HS grads", 
                               "income X SomeCol", "income X BA+", 
                               "constant"),
          dep.var.labels = c("excise tax eligible expenditures"))


# table 2: explaining where the spending increases occur
# by type of spending
e1 = lm(alcohol_expend ~ members, 
                  weights = popwt,
                  data = annual_excise_expend)

e2 = lm(tobacco_expend ~ members, 
                  weights = popwt,
                  data = annual_excise_expend)

e3 = lm(telephone_expend ~ members, 
                  weights = popwt,
                  data = annual_excise_expend)

e4 = lm(gas_expend ~ members, 
                  weights = popwt,
                  data = annual_excise_expend)

e5 = lm(airline_expend ~ members, 
                  weights = popwt,
                  data = annual_excise_expend)

stargazer(e1, e2, e3, e4, e5,
          type="text",
          covariate.labels = c("household members", "constant"),
          dep.var.labels = c("alcohol","tobacco","telephone","gas","airline tickets"))



# table 3: explaining which household members increase spending.

h1 = lm(total_expend ~ 
                      n_unmarried_partners +
                      n_spouse +
                      n_child +
                      n_grandchild +
                      n_inlaws +
                      n_sibling +
                      n_parents +
                      n_other_related +
                      n_unrelated,
                    weights = popwt,
                    data = annual_excise_expend)


h2 = lm(alcohol_expend ~ n_unmarried_partners +
                       n_spouse +
                       n_child +
                       n_grandchild +
                       n_inlaws +
                       n_sibling +
                       n_parents +
                       n_other_related +
                       n_unrelated,
                     weights = popwt,
                     data = annual_excise_expend)

h3 = lm(tobacco_expend ~ n_unmarried_partners +
                       n_spouse +
                       n_child +
                       n_grandchild +
                       n_inlaws +
                       n_sibling +
                       n_parents +
                       n_other_related +
                       n_unrelated,
                     weights = popwt,
                     data = annual_excise_expend)

h4 = lm(telephone_expend ~ n_unmarried_partners +
                       n_spouse +
                       n_child +
                       n_grandchild +
                       n_inlaws +
                       n_sibling +
                       n_parents +
                       n_other_related +
                       n_unrelated,
                     weights = popwt,
                     data = annual_excise_expend)

h5 = lm(gas_expend ~ n_unmarried_partners +
                       n_spouse +
                       n_child +
                       n_grandchild +
                       n_inlaws +
                       n_sibling +
                       n_parents +
                       n_other_related +
                       n_unrelated,
                     weights = popwt,
                     data = annual_excise_expend)

h6 = lm(airline_expend ~ n_unmarried_partners +
                       n_spouse +
                       n_child +
                       n_grandchild +
                       n_inlaws +
                       n_sibling +
                       n_parents +
                       n_other_related +
                       n_unrelated,
                     weights = popwt,
                     data = annual_excise_expend)

stargazer(h1, h2, h3, h4, h5, h6, type = "text",
          covariate.labels = c("unmarried partner", "spouse",
                               "child",
                               "grandchild",
                               "inlaw",
                               "sibling",
                               "parent",
                               "other related persons",
                               "unrelated persons",
                               "constant"),
          dep.var.labels = c("total expenditures", "alcohol",
                             "tobacco",
                             "telephone",
                             "gas",
                             "airline tickets"))



########### additional models ########### 

model1 = lm(total_expend_wt ~ fsalaryx + factor(education),
            data = annual_excise_expend)

  
    # by category
    model1a = lm(alcohol_expend_wt ~ fsalaryx + factor(education),
                 data = annual_excise_expend)
    
    model1b = lm(tobacco_expend_wt ~ fsalaryx + factor(education),,
                 data = annual_excise_expend)
    
    model1c = lm(telephone_expend_wt ~ fsalaryx + factor(education),,
                 data = annual_excise_expend)
    
    model1d = lm(gas_expend_wt ~ fsalaryx + factor(education),,
                 data = annual_excise_expend)
    
    model1e = lm(airline_expend_wt ~ fsalaryx + factor(education),,
                 data = annual_excise_expend)


library(stargazer)

stargazer(model1, model1a, model1b, model1c, model1d, model1e,  type="text")