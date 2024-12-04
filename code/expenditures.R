# Last updated on July 25th, 2024 by Sarah Eckhardt

# Description:

  # this file estimates the per capita federal expenditures according to the model
  # assumptions.
  # see "Federal Model/Supporting Docs/h1b cost method federal estimates"
  # for the methodology.

  # and also including all costs for a over-estimate; including public goods and
  # fixed costs.


rm(list = ls())

library(readxl)
library(dplyr)


user_path = "/Users/sarah/Library/CloudStorage/GoogleDrive-sarah@eig.org/.shortcut-targets-by-id/0B8XLtQT1s8ceT2tKZWZmTkJmM1k"

path_project = file.path(user_path, "EIG/RESEARCH TEAM/1 ACTIVE/2024 H-1B Fiscal Impacts/Federal Model")
path_data = file.path(path_project, "Data")
path_methodology = file.path(path_project, "Supporting Docs")
path_output = file.path(path_project, "Output")

# cost assumptions by scenario.

scenarios= read_excel(
  paste(path_methodology, 
        "expenditures methodology.xlsx",
        sep="/")) %>%
  mutate(
    unmarried_no_kids = ifelse(`allocation (H-1B worker, no spouse, no children)` 
                                    %in% c("marginal", "average"), 1, 0),
         married_no_kids = case_when(
           `allocation (H-1B worker, spouse, no children)` == "marginal (X2)" ~ 2,
           `allocation (H-1B worker, spouse, no children)` == "average (X2)" ~ 2,
           TRUE ~ 0
         ),
         married_kids = case_when(
           `allocation (H-1B worker, spouse, children)` == "marginal (X2)" ~ 2,
           `allocation (H-1B worker, spouse, children)` == "average (X2)" ~ 2,
           `allocation (H-1B worker, spouse, children)` == "average" ~ 1, # edu spending only 1x here, for child.
           TRUE ~ 0
         )) %>%
  mutate( # for all scenarios.
    unmarried_no_kids_ALL = case_when(
      !is.na(`allocation (H-1B worker, no spouse, no children)`) ~ 1,
      TRUE ~ 0
    ),
    married_no_kids_ALL = case_when(
      !is.na(`allocation (H-1B worker, spouse, no children)`) ~ 2,
      
      TRUE ~ 0
    ),
    married_kids_ALL = case_when(
      `allocation (H-1B worker, spouse, children)` == "average" ~ 1,
      !is.na(`allocation (H-1B worker, spouse, children)`) ~ 2,
      TRUE ~ 0
    ))
  
#####################
# all federal expenditures, from White House Historical Tables, Table 3.2
expenditures = read_excel(
  paste(path_data, "White House Historical tables",
  "hist03z2_fy2025.xlsx", sep="/"),
                          skip = 2) %>%
  select(c(`Function and Subfunction`, `2023`)) %>%
  mutate(`2023` = as.integer(`2023`)) %>%
  filter(`2023`>0) # remove the negative expenditure values.

# get US population in 2023
# https://www.census.gov/data/datasets/time-series/demo/popest/2020s-national-total.html
US_population_2023 = read_excel(
  paste(
      path_data, 
      "US Population", 
      "NST-EST2023-POP.xlsx", 
      sep="/"),
  skip = 2) %>%
  rename(`pop2023`=`...6`) %>%
  filter(`Geographic Area`=="United States")
US_population_2023=US_population_2023$pop2023[1]

print("US population as of 2023:")
US_population_2023

# per capita expenditure by category, according to scenario assumptions.
expenditures = merge(expenditures, scenarios, 
                     by="Function and Subfunction", keep.all=TRUE) %>%
  
  # in millions
  mutate(expenditures_per_capita = 
           `2023`*1000000/US_population_2023) %>%
  
  # inclusion and multiple adjustments
  mutate(expenditures_unmarried_no_kids = round(expenditures_per_capita*unmarried_no_kids,2),
         expenditures_married_no_kids = round(expenditures_per_capita*married_no_kids,2),
         expenditures_married_kids = round(expenditures_per_capita*married_kids,2),
         expenditures_unmarried_no_kids_ALL = round(expenditures_per_capita*unmarried_no_kids_ALL,2),
         expenditures_married_no_kids_ALL = round(expenditures_per_capita*married_no_kids_ALL,2),
         expenditures_married_kids_ALL = round(expenditures_per_capita*married_kids_ALL,2)
         ) %>%
  
  select(`Function and Subfunction`,
         expenditures_per_capita,
         expenditures_unmarried_no_kids,
         expenditures_married_no_kids,
         expenditures_married_kids,
         expenditures_unmarried_no_kids_ALL,
         expenditures_married_no_kids_ALL,
         expenditures_married_kids_ALL)
    
table = expenditures %>%
  summarise(expenditures_unmarried_no_kids = sum(expenditures_unmarried_no_kids),
            expenditures_married_no_kids = sum(expenditures_married_no_kids),
            expenditures_married_kids = sum(expenditures_married_kids),
            expenditures_unmarried_no_kids_ALL = sum(expenditures_unmarried_no_kids_ALL),
            expenditures_married_no_kids_ALL = sum(expenditures_married_no_kids_ALL),
            expenditures_married_kids_ALL = sum(expenditures_married_kids_ALL))

openxlsx::write.xlsx(expenditures, paste(path_output,"expenditures_by_category_2023.xlsx", sep="/"))
openxlsx::write.xlsx(table, paste(path_output, "expenditures_total_2023.xlsx",sep="/"))

# display table
table
