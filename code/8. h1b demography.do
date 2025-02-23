
cd "user path/data/ACS"


*TWO DIGIT OCCSOC CODES 
import delimited "occsoc2cats.csv", clear 
sort occsoc2 
save "occsoc2cats.dta", replace 

*ALL COUNTRIES
use "usa_00039.dta", clear
keep if empstat== 1 
keep if citizen == 3
keep if yrsusa1 < 7
keep if educd>=101
g count = 1
g occsoc2 = substr(occsoc,1,2)
destring occsoc2, replace 
g male = sex == 1
g china = (bpl == 500)
g india = (bpl == 521)
replace perwt = 4.66* perwt if india
replace perwt = 3.05*perwt if china
g married = marst == 1

g has_child = nchild>0


collapse(median) age incwage (mean) married male nchild  has_child india china (sum) count [iweight = perwt], by(occsoc2)
sort occsoc2
merge 1:1 occsoc2 using "occsoc2cats.dta",
drop _merge

sort occsoc2

save "ACS_demog_all.dta", replace 


*JUST INDIA AND CHINA
use "usa_00039.dta", clear
keep if empstat== 1 
keep if citizen == 3
keep if yrsusa1 < 7
keep if inlist(bpl,500,521)
keep if educd>=101
g count = 1
g occsoc2 = substr(occsoc,1,2)
destring occsoc2, replace 
g male = sex == 1
g married = marst == 1

collapse(median)age incwage (mean) married male nchild (sum) count [fweight = perwt], by(occsoc2)
sort occsoc2
merge 1:1 occsoc2 using "occsoc2cats.dta",
drop _merge
sort occsoc2
save "ACS_demog_v2.dta", replace 


cd "user path/data/DOL Prevailing Wage Test"

forvalues x = 1(1)4 {
import delimited "LCA_Disclosure_Data_FY2023_Q`x'.csv", clear 
drop if case_status == ""
keep if case_status == "Certified"
keep if visa_class == "H-1B"
save "LCA2023_q`x'.dta", replace
}


use LCA2023_q1.dta, clear
append using LCA2023_q2.dta
append using LCA2023_q3.dta
append using LCA2023_q4.dta
save LCA2023_all.dta, replace


*GETTING STATE LEVEL AVERAGE PAY
use LCA2023_all.dta, clear


destring wage_rate_of_pay_from wage_rate_of_pay_to prevailing_wage, ignore("$" ",") replace

g avg_pay = (wage_rate_of_pay_from + wage_rate_of_pay_to)/2

g new_pay = avg_pay if wage_unit_of_pay == "Year"

replace new_pay = 52* avg_pay if wage_unit_of_pay == "Week"
replace new_pay = 12* avg_pay if wage_unit_of_pay == "Month"
replace new_pay = 2080 * avg_pay if wage_unit_of_pay == "Hour"
replace new_pay = 26 * avg_pay if wage_unit_of_pay == "Bi-Weekly"

*APPEARS TO BE DUPLICATES 
duplicates drop *, force
g count = 1

collapse (sum) count (mean) new_pay, by(worksite_state)
sort worksite_state
rename worksite_state state_abbr
/*
statastates, abbrev(state_abbr)
drop if _merge ==1
drop _merge 
sort state_name */
save "state_median_h1b_pay.dta", replace 


*MERGING LCA DATA AND CENSUS DATA ON NUMBER OF CHILDREN 
use LCA2023_all.dta, clear


destring wage_rate_of_pay_from wage_rate_of_pay_to prevailing_wage, ignore("$" ",") replace
g avg_pay = (wage_rate_of_pay_from + wage_rate_of_pay_to)/2
g new_pay = avg_pay if wage_unit_of_pay == "Year"
replace new_pay = 52* avg_pay if wage_unit_of_pay == "Week"
replace new_pay = 12* avg_pay if wage_unit_of_pay == "Month"
replace new_pay = 2080 * avg_pay if wage_unit_of_pay == "Hour"
replace new_pay = 26 * avg_pay if wage_unit_of_pay == "Bi-Weekly"



g count = 1
replace soc_code = subinstr(soc_code ,"-","",.)
g occsoc2 = substr(soc_code,1,2)
destring occsoc2, replace 
collapse(sum) count (median) new_pay, by(occsoc2)
sort occsoc2
save "LCA_data_by_occ.dta", replace


merge 1:1 occsoc2 using "ACS_demog_all.dta"
*ALL MERGED EXCEPT FOR 5 H1b HOLDERS IN WEIRD SOC CODES 
keep if _merge==3
drop _merge

su nchild [fweight = count], detail 
su age [fweight = count], detail 
su male [fweight = count], detail 
su india [fweight = count], detail
su china [fweight = count], detail
su married [fweight = count], detail

*MERGING LCA DATA AND CENSUS DATA ON NUMBER OF CHILDREN 
*ROBUSTNESS TEST: ONLY INDIAN AND CHINESE
use LCA2023_all.dta, clear

destring wage_rate_of_pay_from wage_rate_of_pay_to prevailing_wage, ignore("$" ",") replace
g avg_pay = (wage_rate_of_pay_from + wage_rate_of_pay_to)/2
g new_pay = avg_pay if wage_unit_of_pay == "Year"
replace new_pay = 52* avg_pay if wage_unit_of_pay == "Week"
replace new_pay = 12* avg_pay if wage_unit_of_pay == "Month"
replace new_pay = 2080 * avg_pay if wage_unit_of_pay == "Hour"
replace new_pay = 26 * avg_pay if wage_unit_of_pay == "Bi-Weekly"


g count = 1
replace soc_code = subinstr(soc_code ,"-","",.)
g occsoc2 = substr(soc_code,1,2)
destring occsoc2, replace 
collapse(sum) count (median) new_pay, by(occsoc2)
sort occsoc2

cd "USER PATH/data/ACS"

merge 1:1 occsoc2 using "ACS_demog_v2.dta"
*ALL MERGED EXCEPT FOR 5 H1b HOLDERS IN WEIRD SOC CODES 
keep if _merge==3
drop _merge

su nchild [fweight = count], detail 
su age [fweight = count], detail 
su male [fweight = count], detail 
su married [fweight = count], detail


/*
 60% of H-1B holders are married and 40% are unmarried. 
39% of H-1B holders have children. 
So we have 39% married with kids, and 21% (60% - 39% = 21%) married without kids, and 40% unmarried. 

The average number of kids per H-1B worker is 0.58,
 so that means E(Kids | having kids ) = E(kids) / p(having kids) = 1.49 kids. 
*/
