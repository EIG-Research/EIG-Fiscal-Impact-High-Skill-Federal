# EIG H-1B Federal Fiscal Impact

This document outlines the data and methodology used for EIG’s H-1B federal fiscal impact model. Current as of 12/06/2024

## Data
1. Office of Budget Management [Link](https://www.whitehouse.gov/omb/budget/historical-tables/)
   
    a.  For federal expenditures: Table 3.2 - OUTLAYS BY FUNCTION AND SUBFUNCTION:  1962 - 2029
   
    b.  For customs duties: Table 2.5 - COMPOSITION OF “OTHER RECEIPTS”: 1940-2029
   
 3. American Consumer Survey 2021 5-year file [Link](https://usa.ipums.org/usa/index.shtml) for pr(married) and E(children | married). Variable list provided in “Data/ACS/IPUMS ACS download instructions .png”
 4. USCIS fiscal years 2019 and 2023 Annual Report to Congress [2019](https://www.uscis.gov/sites/default/files/document/reports/Characteristics_of_Specialty_Occupation_Workers_H-1B_Fiscal_Year_2019.pdf) and [2023](https://www.uscis.gov/sites/default/files/document/reports/OLA_Signed_H-1B_Characteristics_Congressional_Report_FY2023.pdf) for mean and median income of H-1B workers.
 5. Department of Labor Prevailing Wage Test data for Q1-4 2023, for the 95th percentile H-1B income estimate [Link](https://www.dol.gov/agencies/eta/foreign-labor/wages)
 6. Brandon, Ike, and M. Kevin McGee "Repealing H-4 Visa Work Authorization: A Cost-Benefit Analysis." Available at SSRN 3349786 (2019) [Link](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3349786) for H-4 incomes in 2019
 7. Tax Foundation 2023 Tax Brackets [Link](https://taxfoundation.org/data/all/federal/2023-tax-brackets/) for income tax estimates
 8. Consumer Expenditure Survey from the BLS Public Use Microdata (PUMD) [Link](https://www.bls.gov/cex/pumd.htm) for excise tax distributions
 9. USCIS I-765 Work Authorization Reports [Link](https://www.uscis.gov/tools/reports-and-studies/immigration-and-citizenship-data) for probability that H-4 spouses work
 10. Stock of H-1Bs [Link](https://www.uscis.gov/sites/default/files/document/reports/USCIS%20H-1B%20Authorized%20to%20Work%20Report.pdf)
 11. CBO Economic Projections, June 2024 [Link](https://www.cbo.gov/data/budget-economic-data)
 12. CBO Tax Parameters, June 2024 [Link](https://www.cbo.gov/data/budget-economic-data)
 13. CBO Demographic Projections, January 2023 [Link](https://www.cbo.gov/data/budget-economic-data)
 14. Census population estimates 2023 [Link](https://www.census.gov/data/datasets/time-series/demo/popest/2020s-national-total.html)


## Scenario Probabilities

Assuming that there are no single-parent households for simplicity, and that children are distributed equally among working and non-working H-1B spouses, we construct five scenarios to estimate household-level impacts of issuing a single H-1B visa. We estimate each scenario separately for the mean, median, and 95th percentile H-1B earner.

  &nbsp;&nbsp;&nbsp;&nbsp;<b>Scenario 1:</b> unmarried H-1B worker, no children
  
  &nbsp;&nbsp;&nbsp;&nbsp;<b>Scenario 2:</b> married H-1B worker, working spouse, with children
  
  &nbsp;&nbsp;&nbsp;&nbsp;<b>Scenario 3:</b> married H-1B worker, nonworking spouse, with children
  
  &nbsp;&nbsp;&nbsp;&nbsp;<b>Scenario 4:</b> married H-1B worker, working spouse, without children
  
  &nbsp;&nbsp;&nbsp;&nbsp;<b>Scenario 5:</b> married H-1B worker, nonworking spouse, without children


The probability that an H-1B worker is married, and the number of children conditional on being married, is estimated using an identified sample of probable H-1B workers from the ACS.

The probability that a spouse is working is estimated using USCIS annual reports of I-765 Work Authorization applications. We take the midpoint of total authorizations in 2022, and authorizations issued between 2019-2022 (assuming that an EAD is held for 3 years) to approximate the number of EADs issued. The ratio between this estimate and the estimated number of H-1Bs in the U.S. as of 2019, is taken to be the share of H-1Bs with an EAD authorized spouse. This is multiplied by the share of H-4 visa holders with an H-1B spouse that are employed, provided by Brandon et al. (2019). This is multiplied by the probability that an H-1B is married to obtain the final spousal working probability.

Additional information is available in <i>Code/h1b demography.do</i>, and <i>Methodology/scenario probabilities.xlsx</i>


## Income

H-1B mean and median income estimates come from the USCIS’s 2023 Fiscal Year Report to Congress (linked above). The 95th and 99th percentile income comes from LCA prevailing wage data, taking the average of the minimum and maximum rate of pay provided by employer sponsors. The 95th and 99th percentiles are not published by the USCIS.

The reason for not using the LCA files for the mean and median incomes are:
<ol>
  <li>H-1B visas don't line up 1:1 with LCA filings</li>
  <li>Multiple H-1Bs can be approved on one LCA</li>
  <li>LCAs are filed ~6 months in advance of the H-1B visa</li>
  <li>An LCA may not reflect any real H-1B workers if the company does not get H-1B approval or file a H-1B application (which may occur for logistical reasons also, some proposed start dates on the LCA application are months to a year before the LCA is actually approved).</li>
</ol>

The gaps between LCAs filed and H-1B visas actually held are significant. Apple filed 2,781 LCA applications for start dates in 2023, and had only 707 new H-1B visa employees in 2023, 3,115 continuing. Amazon.com (excluding AWS, Amazon Fresh, Amazon Payments, etc.) had 8,628 LCA applications and 11,313 total H-1B employees; 2,826 new employees. But without more detail, we cannot determine which of these LCA applications are underpinning several H1B workers and which LCAs are underpinning no H1B workers. The direction of the bias is unknown.

The H-4 income comes from Brandon et al. (2019), and is adjusted to the 2023 value taking the growth in the mean H-1B wage from 2019-2023 provided by the USCIS. We apply the mean H-4 income to all H-1B income scenarios.


## Revenues 

### Income Taxes

Income tax brackets, standard deductions, and tax credits come from the Tax Foundation’s 2023 tax brackets. Income taxes are applied for each scenario using to the following formulas:

<table>
<tr>
  <th>Scenario</th>
  <th>Standard Deduction</th>
  <th>Bracket Rate</th>
  <th>Tax Credit</th>
</tr>
<tr>
  <td>Scenario 1</td>
  <td>Single filer</td>
  <td>Single filer</td>
  <td>none</td>
</tr>
<tr>
  <td>Scenario 2</td>
  <td>Joint filer</td>
  <td>Joint filer</td>
  <td>CTC*num child</td>
</tr>
<tr>
  <td>Scenario 3</td>
  <td>Household head</td>
  <td>Household head</td>
  <td>CTC*num child</td>
</tr>
<tr>
  <td>Scenario 4</td>
  <td>Joint filer</td>
  <td>Joint filer</td>
  <td>none</td>
</tr>
<tr>
  <td>Scenario 5</td>
  <td>Household head</td>
  <td>Household head</td>
  <td>none</td>
</tr>
</table>

In general, non-resident aliens are not allowed to take the standard deduction, file jointly, or file as a household head ([Link](https://www.irs.gov/individuals/international-taxpayers/nonresident-figuring-your-tax)). However, according to the substantial presence test, H-1B workers and their spouses are considered to be resident aliens by the IRS ([Link](https://www.irs.gov/individuals/international-taxpayers/substantial-presence-test)). There are edge cases for non-US based H-4 visa holders but we ignore these. Our interpretation is corroborated by individuals in our network: Dip, head of [Improve the Dream](https://www.improvethedream.org/), and Supriya, an software developer who is married to an H-1B, and has previously been on H-4 and H-1B visas.

#### Long-term adjustment

We assume no significant change to the tax code between 2023-2032. Bracket cutoffs are adjusted using the CBO’s CPI-U projection.

### Payroll Taxes

Both the employee and employer payroll tax rates are apportioned to H-1B workers and their working spouses. Payroll taxes include:

<ol>
  <li>OASDI/Social Security: 6.2% employer rate and 6.2% for employee rate, for a total of 12.4% rate.</li>
  <li>HI/Healthcare: 1.45% employer rate and 1.45% employee rate, for a total of 2.90% rate.</li>
  <li>FUTA: 6% employer rate applied to first $7,000 earned, applied individually, for a total of $420 for single filers and $840 for joint filers.
</li>
</ol>

Other payroll taxes, including the Railroad Retirement Act Taxes, Disability Insurance taxes, and FERS are not applicable to H-1B workers, and are not applied.

#### Long-term adjustment

The OASDI/Social Security payroll tax has a cap on taxable income for which the 6.2% rate is applied. We grow this cap using the CBO’s Tax Parameter projections.

## Excise Taxes

#### Long-term adjustment

## Customs Duties

Aggregate customs duties come from the Office of Budget Management’s (OBM) Historical Tables. As the tax incidence falls largely on consumers, these are applied at a per-capita rate to the number of adults per household (either one or two). Per-capita transformations are done using population estimates from the CBO.

#### Long-term adjustment

The growth in customs duties is likely bounded by population growth and real income growth. We assume that customs duties grow at the rate of the total U.S. population as a conservative estimate, using population projections from the CBO.

## Expenditures

Only non-fixed cost federal outlays, and those that are incurred by H-1B workers, are applied. These can be found in <i>Methodology/expenditures methodology.xlsx.</i> The methodology differs depending on household composition. Per-capita transformations are made using the Census’s 2023 U.S. population estimate. The methodology differs depending on household composition; costs are assumed to double when a H-1B worker is married. Federal elementary and secondary education expenditures are applied only to the scenarios in which an H-1B worker has children; and in those cases are adjusted by the total number of children in the household.

#### Long-term adjustment

The OBM provides outlay projections through 2029. We take the mean growth rate of the last three years (2027-2029), assuming negative growth rates to be zero, and apply this to the 2023 expenditures for years 2024-2032.


