/******************************************************************************
Authors: Mahdi Shams
Date:          Jun 25, 2024
Last modified: Oct 06, 2024
Description: 

Based on Alvaro clean_data_02_16.do. This is Alvaro's description:

"This code gets the data from the replication package in  Cengiz et al (2019),
cleans it and gets the 2007, 2010, 2015 years...""

Many data files (probably later created for replication) are made here, included:

- CPS_cleaned_merged_07_15.csv
- CPS_cleaned_merged_yearly.csv
- CPS_cleaned_merged_yearly_10_to_15.dta

*******************************************************************************/

********************************************
* SET UP USERS AND DIRECTORIES
********************************************
clear all

* Extract current username
local user = "mahdi"

* Current working folder 
local folder = "CS_Bounds"

* Directory to Dropbox folder, which depends on machine type:
if "`c(os)'" == "Windows" global path = "C:/Users/`user'/Dropbox/`folder'"
if "`c(os)'" == "Unix"    global path = "/home/`user'/Dropbox/`folder'"
else global path = "/Users/`user'/Dropbox/Research/`folder'"

if "`user'" == "dghanem" global path = "E:/Dropbox/`folder'"

global alvaro = "${path}/input/23_02_13_download_data"

*****************************************************************************
*** XXX -- I skip this section
*****************************************************************************

/* 
Create temp files for cpi deflators: 
this is taken from clean_adiminstrativedata_QJE.do
*/

use "${alvaro}/data/data/cpiursai1977-2016.dta", clear

*Dropping an empty column and column that calculates yearly average (simple)
drop v14 avg
reshape long month, i(year) j(m)
rename month cpi
rename m month
keep if year >= 1979
*If base year needs to be changed, use here.
local baseyear=2016
sum cpi if year == `baseyear', meanonly
local cpibase = r(mean)
replace cpi = 100 * cpi / `cpibase'
gen monthdate = ym(year,month)
gen quarterdate = qofd(dofm(monthdate))
collapse cpi, by(quarterdate)
tempfile cpiquarter
save `cpiquarter'
use "${alvaro}/data/data/qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
tempfile qcew
save `qcew'


*****************************************************************************
*** XXX
*****************************************************************************

use "${alvaro}/data/data/totransfer.dta", clear 

* Alvaro: As far as I understand, this is where the CPS data are

// merge 1:1 hhid age sex marital race using "data/data/totransfer_ind.dta"

/*
Alvaro: Code copy-pasted from data/dofiles/clean_admin_cps_workingdata_allind_5cents.do. This code performs some of their data cleaning of the CPS to remove imputed observations and so on, I also restrict to only keep 2007, 2010, 2015.
*/

keep month state age marital race sex esr ethnic uhours earnhr uearnwk earnwt ///
uhourse paidhre earnhre earnwke I25a I25b I25c I25d year lfsr89 lfsr94 statenum ///
orgwt monthdate quarterdate quarter division region censusnum stfips gradeat gradecp ihigrdc ///
grade92 unioncov unionmme hourslw class class94 smsa80 smsa93 smsa04 smsastat

* Drop self-employed

drop if (class==5 | class==6) & year<=1993
drop if (class94==6 | class94==7) & year>=1994

drop class class94
keep if year>=1983
compress

*** Getting rid of imputed ones and those whose incomes are not positive.

gen wage = earnhre / 100 if paidhre == 1
replace wage = earnwke/uhourse if paidhre == 2

* use allocation variables to determine hours, earnings, and hourly wage imputations
gen hoursimputed = I25a > 0 & I25a ~= .
gen earningsimputed = I25d > 0 & I25d ~= .
gen wageimputed = I25c > 0 & I25c ~= .

foreach var of varlist hoursimputed earningsimputed wageimputed {
	* use different method to identify imputations in 1989-1993 data
	replace `var' = 0 if year >= 1989 & year <= 1993
	* no reliable imputation data from Jan 1994 through Aug 1995; see Hirsch & Schumacher
	replace `var' = 0 if year == 1994 | (year == 1995 & month <= 8)
}

*** use method analogous to that employed by Hirsch & Schumacher to identify imputations in 1989-1993 data

replace wageimputed = 1 if year >= 1989 & year <= 1993 & (earnhr == . | earnhr == 0) & (earnhre > 0 & earnhre ~= .)
replace hoursimputed = 1 if year >= 1989 & year <= 1993 & (uhours == . | uhours == 0) & (uhourse > 0 & uhourse ~= .)
replace earningsimputed = 1 if year >= 1989 & year <= 1993 & (uearnwk == . | uearnwk == 0) & (earnwke > 0 & earnwke ~= .)

gen imputed = 0
replace imputed = 1 if paidhre == 2 & (hoursimputed == 1 | earningsimputed == 1)
replace imputed = 1 if paidhre == 1 & (wageimputed == 1)

replace wage = . if imputed == 1
replace wage=wage*100	
tempfile CPS_cleaned_yearly
save `CPS_cleaned_yearly'
preserve
//keep if year == 2007 | year == 2010 | year == 2015
keep if inrange(year, 2010, 2015)
tempfile CPS_cleaned_07_15
save `CPS_cleaned_07_15'
restore //back to CPS_cleaned_yearly
 
*** now, we deflate the wages!

merge m:1 statenum quarterdate using `qcew', keep(3) nogenerate
merge m:1 quarterdate using `cpiquarter', keep(3) nogenerate
replace wage = wage/(cpi/100)
tempfile CPS_cleaned_deflated_yearly
save `CPS_cleaned_deflated_yearly'
keep if year == 2007 | year == 2010 | year == 2015 
//keep if inrange(year, 2010, 2015)
tempfile CPS_cleaned_deflated_07_15
save `CPS_cleaned_deflated_07_15'

/*
Now, we create the dummy variables for wage increases in 2007-10 and 2010-15. 
We use VZ_mw_state_quarterly_new.dta which contains state-level minimum wages at
  quarterly frequency
*/
use "${alvaro}/data/data/VZ_mw_state_quarterly_new.dta", clear 
gen year = yofd(dofq(quarterly_date)) //Get yearly date
gen q_minwage_increase = 0
replace q_minwage_increase = 1 if max_mw > max_mw[_n-1] //Did minimum wage increase this quarter?
collapse (max) max_mw q_minwage_increase, by(statefips year) //Collapse yearly
rename q_minwage_increase minwage_increase
label variable minwage_increase "Increase in minwage from previous year"
rename max_mw state_mw
label variable state_mw "Max of federal and state minimum wage"
rename statefips statenum
tempfile yearly_minwage_increase
save `yearly_minwage_increase' //Save yearly minimum wage increase

/* 
We have saved yearly minimum wage increase, now do the same with 
2007-10 and 2010-15
*/

preserve
keep if year >= 2010 & year <= 2015 //Keep only 2010-15
collapse (max) minwage_increase, by(statenum) //Categorical of wage increase in 2010-15
rename minwage_increase minwage_increase_10_15
label variable minwage_increase_10_15 "Minimum wage increased between 2010 and 2015"
tempfile minwage_increase1
save `minwage_increase1'
restore
keep if year >= 2007 & year <= 2010 //Keep only 2007-10
collapse (max) minwage_increase, by(statenum) //Categorical of wage increase in 2007-10
rename minwage_increase minwage_increase_07_10
label variable minwage_increase_07_10 "Minimum wage increased between 2007 and 2010"
merge 1:1 statenum using `minwage_increase1', nogenerate
tempfile minwage_increase_10_15 
save `minwage_increase_10_15' //Save ever minimum wage increase


use "${alvaro}/data/data/state_level_quarterly_unemp_rate.dta" 
//open quarterly unemployment rate to create yearly unemp rate
gen year = yofd(dofq(quarterdate))
collapse unemp_rate , by(statenum year)
tempfile year_unemployment
save `year_unemployment' //save temp file

/*
Create the data for the last presidential election (i.e 2007  refers to 2004 
election, 2010 to 2008 and so on). The data on this is contained  in 
presidential_elections_from 1976_2016.dta. I also make a yearly presidential 
election file for the yearly cleaned and merged data.


Mahdi: Don't get this!
*/

use "${alvaro}/data/data/presidential_elections_from 1976_2016.dta", clear
preserve
sort statenum year
recode year (2004 = 2007)
recode year (2008 = 2010)
recode year (2012 = 2015)
keep if year == 2007 | year == 2010 | year == 2015
label var dem_vote "Democrat vote in last election"
label var rep_vote "Republican vote in last election"
label var political_orientation "Party that won last presidential election"
tempfile last_presidential_election
save `last_presidential_election'
restore
sort statenum year
tsset statenum year //Set as a panel by state and year
tsfill //Fill years where there were no elections with empty values
//Fill each year with the result of the last presidential election:
foreach i in numlist 1:4{
	foreach myvar in statefull year dem_vote rep_vote political_orientation statenum {
		replace `myvar' = `myvar'[_n-1] if missing(`myvar') 
	}
}
tempfile yearly_presidential_election
save `yearly_presidential_election'
 

/*
Now, we go back to the cleaned CPS data, and merge these minimum wage increases and other  variables into them 
*/

*********************************
*** Outputs: Bulding Datasets ***
*********************************

*** Create CPS_cleaned_merged_07_15

use `CPS_cleaned_07_15', clear 
//Back to the CPS data to merge the binary variables
merge m:1 statenum using `minwage_increase_10_15', nogenerate 
//Merge minimum wage increase
merge m:1 statenum using "${alvaro}/data/data/state_ideologyshares.dta", nogenerate 
//Merge state ideology share
merge m:1 statenum year using "${alvaro}/data/data/state_demog.dta", nogenerate 
//merge state demographics by year
merge m:1 statenum year using `year_unemployment', nogenerate 
//merge with yearly unemployment by state
merge m:1 statenum year using `last_presidential_election', ///
nogenerate keepusing(dem_vote rep_vote political_orientation) 
//merge with yearly unemployment by state
merge m:1 statenum year using `yearly_minwage_increase', ///
nogenerate keepusing(state_mw)

keep if year == 2007 | year == 2010 | year == 2015
save             "${path}/data/CPS_cleaned_merged_07_15.dta", replace
export delimited "${path}/data/CPS_cleaned_merged_07_15.csv", replace

preserve
describe, replace
export excel using "variable_descriptions_cps_cleaned_merged.xlsx", replace first(var)
restore 

*** Create the CPS_cleaned_merged_yearly

use `CPS_cleaned_yearly', clear 
//Do the same to merge with the yearly CPS data/CPS_cleaned_07_15
merge m:1 statenum year using `yearly_minwage_increase', nogenerate 
//Merge yearly minimum wage increase
merge m:1 statenum using "${alvaro}/data/data/state_ideologyshares.dta", nogenerate 
//Merge state ideology share
merge m:1 statenum year using "${alvaro}/data/data/state_demog.dta", nogenerate 
//merge state demographics by year
merge m:1 statenum year using `year_unemployment', nogenerate 
//merge with yearly unemployment by state
merge m:1 statenum year using `yearly_presidential_election', ///
nogenerate keepusing(dem_vote rep_vote political_orientation) 
//merge with yearly unemployment by state
merge m:1 statenum year using `yearly_minwage_increase', ///
nogenerate keepusing(state_mw) //minimum wage for each year
keep if year > 2005 | year < 2018 //Remove all the years from the 90s
save             "${path}/data/CPS_cleaned_merged_yearly.dta", replace
export delimited "${path}/data/CPS_cleaned_merged_yearly.csv", replace

keep if year == 2010 | year == 2015 

save             "${path}/data/CPS_cleaned_merged_yearly_10_to_15.dta", replace
export delimited "${path}/data/CPS_cleaned_merged_yearly_10_to_15.csv", replace



/*
*** Create the CPS_cleaned_merged_deflated_07_15

use `CPS_cleaned_deflated_07_15', clear 
//Back to the CPS data to merge the binary variables
merge m:1 statenum using `minwage_increase_10_15', nogenerate 
//Merge minimum wage increase
merge m:1 statenum using "${alvaro}/data/data/state_ideologyshares.dta", nogenerate 
//Merge state ideology share
merge m:1 statenum year using "${alvaro}/data/data/state_demog.dta", nogenerate 
//merge state demographics by year
merge m:1 statenum year using `year_unemployment', nogenerate 
//merge with yearly unemployment by state
merge m:1 statenum year using `last_presidential_election', ///
nogenerate keepusing(dem_vote rep_vote political_orientation) 
//merge with yearly unemployment by state
merge m:1 statenum year using `yearly_minwage_increase', ///
nogenerate keepusing(state_mw)

keep if year == 2007 | year == 2010 | year == 2015
save             "${path}/data/CPS_cleaned_merged_deflated_07_15.dta", replace
export delimited "${path}/data/CPS_cleaned_merged_deflated_07_15.csv", replace

*** Create the CPS_cleaned_deflated_merged_yearly

preserve
describe, replace
export excel using ///
"${output}/variable_descriptions_cps_cleaned_merged_yearly.xlsx", replace first(var)

use `CPS_cleaned_deflated_yearly', clear 
//Do the same to merge with the yearly CPS data/CPS_cleaned_07_15
merge m:1 statenum year using `yearly_minwage_increase', nogenerate 
//Merge yearly minimum wage increase
merge m:1 statenum using "${alvaro}/data/data/state_ideologyshares.dta", nogenerate 
//Merge state ideology share
merge m:1 statenum year using "${alvaro}/data/data/state_demog.dta", nogenerate 
//merge state demographics by year
merge m:1 statenum year using `year_unemployment', nogenerate 
//merge with yearly unemployment by state
merge m:1 statenum year using `yearly_presidential_election', ///
nogenerate keepusing(dem_vote rep_vote political_orientation) 
//merge with yearly unemployment by state
merge m:1 statenum year using `yearly_minwage_increase', ///
nogenerate keepusing(state_mw) 
//minimum wage for each year
keep if year > 2005 | year < 2018 //Remove all the years from the 90s
save             "${path}/data/CPS_cleaned_deflated_merged_yearly.dta", replace
export delimited "${path}/data/CPS_cleaned_deflated_merged_yearly.csv", replace
*/
