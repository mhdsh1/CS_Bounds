/********************************************
Authors:       Mahdi Shams
Date:          Oct 5, 2024
Last modified: Oct 10, 2024
Description:   We call functions csbounds, csatt, and csgini 
and replicate the results in the paper. 

********************************************/

********************************************
* SET UP USERS AND DIRECTORIES
********************************************

clear all

set more off

* Extract current username
local user = "`c(username)'"

* Current working folder 
local folder = "CS_Bounds"

else {
* Directory to Dropbox folder, which depends on machine type:
if "`c(os)'" == "Windows" global path = "C:/Users/`user'/Dropbox/`folder'"
if "`c(os)'" == "Unix"    global path = "/home/`user'/Dropbox/`folder'"
else                      global path = "/Users/`user'/Dropbox/`folder'"
 }

if "`user'" == "dghanem" global path = "E:/Dropbox/`folder'"

********************************************

use "${path}/data/CPS_cleaned_merged_yearly_10_to_15.dta", clear

* cleaning the data, the goal is have a data with y treatment and period

gen y = wage / 100

preserve
collapse (mean) state_mw2010 = state_mw ///
if year == 2010, by (statenum year month)
tempfile mw2010
save `mw2010'
restore

qui merge m:1 statenum month using  `mw2010', keepusing(state_mw2010) nogen

preserve
collapse (mean) state_mw2015 = state_mw ///
if year == 2015, by (statenum year month)
tempfile mw2015
save `mw2015'
restore

qui merge m:1 statenum month using  `mw2015', keepusing(state_mw2015) nogen

order statenum year month y state_mw*  
sort year month statenum 

gen smw_increase1015 = (state_mw2015 - state_mw2010 > 0.25)

local premw     = 8
local upremw    = .
local preperiod = 2010
local Tperiod   = 2015

g period = . 

replace period = 0 if year == 2010 
replace period = 1 if year == 2015 

g treatment = .

replace treatment = 0 if smw_increase1015 == 0   & ///
			 state_mw2010 >= `premw' & ///
			 state_mw2010 <  `upremw'

replace treatment = 1 if smw_increase1015 == 1   & ///
			 state_mw2010 >= `premw' & ///
			 state_mw2010 <  `upremw'

keep y treatment period

save "${path}/output/Stata/CPS_cleaned_merged_yearly_10_to_15_cleaned.dta", replace

*** calling the csbounds 

which csbounds

csbounds y treatment period 

save "${path}/output/Stata/CPS_cleaned_merged_yearly_10_to_15_with_csbounds.dta", replace

*** making the plots of the observed + loweu- and upper-bounds (outcomes of csbounds)

// Distributions

twoway (line FY10TDDID y if inrange(y,0,50), lcolor(blue))   || ///
       (line FY11T     y if inrange(y,0,50), lcolor(black) ), ///
ytitle("cdf") xtitle("y") graphregion(color(white)) plotregion(style(none)) ///
legend(order(1 "DistDiD" 2 "Obs") pos(11) ring(0) col(1))
graph export "${path}/output/Stata/DistDiDY10T_wages_Cengizetal2019.pdf", replace

twoway (line FY00C y if inrange(y,0,50)), ///
ytitle("cdf") xtitle("y") graphregion(color(white)) plotregion(style(none)) 
graph export "${path}/output/Stata/FY00C_wages_Cengizetal2019.pdf", replace

twoway (line FY10C y if inrange(y,0,50)), ///
ytitle("cdf") xtitle("y") graphregion(color(white)) plotregion(style(none)) 
graph export "${path}/output/Stata/FY10C_wages_Cengizetal2019.pdf", replace

twoway (line FY00T y if inrange(y,0,50)), ///
ytitle("cdf") xtitle("y") graphregion(color(white)) plotregion(style(none)) 
graph export "${path}/output/Stata/FY00T_wages_Cengizetal2019.pdf", replace

twoway (line FY11T y if inrange(y,0,50)), ///
ytitle("cdf") xtitle("y") graphregion(color(white)) plotregion(style(none)) 
graph export "${path}/output/Stata/FY11T_wages_Cengizetal2019.pdf", replace

// CS 

twoway (line FY10TUBCS y if inrange(y,0,50), lcolor(blue)) || ///
       (line FY10TLBCS y if inrange(y,0,50), lcolor(red))  || ///
       (line FY11T     y if inrange(y,0,50), lcolor(black) ), ///
ytitle("cdf") xtitle("y") graphregion(color(white)) plotregion(style(none)) ///
legend(order(1 "CS-LB" 2 "CS-UB" 3 "Obs") pos(11) ring(0) col(1))
graph export "${path}/output/Stata/FTCSbounds_wages_Cengizetal2019.pdf", replace

twoway (line FY10TUBCS y if inrange(y,0,50), lcolor(blue)) || ///
       (line FY10TLBCS y if inrange(y,0,50), lcolor(red))  , ///
ytitle("cdf") xtitle("y") graphregion(color(white)) plotregion(style(none)) ///
legend(order(1 "CS-LB" 2 "CS-UB") pos(11) ring(0) col(1))
graph export "${path}/output/Stata/FTCSbounds_noobs_wages_Cengizetal2019.pdf", replace


twoway (line FY10TLBCS y if inrange(y,0,50), lcolor(red))   || ///
       (line FY10TUBCS y if inrange(y,0,50), lcolor(blue))  || ///
       (line FY10TDDID y if inrange(y,0,50), lcolor(black) ),  ///
ytitle("cdf") xtitle("y") graphregion(color(white)) plotregion(style(none)) ///
legend(order(1 "CS-LB" 2 "CS-UB" 3 "DistDiD") pos(11) ring(0) col(1))
graph export "${path}/output/Stata/CSboundsDistDiDY10T_wages_Cengizetal2019.pdf", replace


*** calling csatt

which csatt // make sure csatt is installed 

local values 1 0.01 0.025 0.05 0.1 0.25 0.50

matrix results = J(1, 5, .)

foreach val in `values' {
    
qui    csatt `val'    
qui    matrix temp    = result 
qui    matrix results = results \ temp
}

matrix results = results[2..`=rowsof(results)', .]

local values 1 0.01 0.025 0.05 0.1 0.25 0.50
matrix rownames results = `values'
matrix colnames results = "FY11" "CSLB" "CSUB" "DiD" "DistDiD"
matrix list results

*** calling csgini

which csgini // make sure csgini is installed   

local values 1 0.01 0.025 0.05 0.1 0.25 0.50

matrix results = J(1, 4, .)

foreach val in `values' {
qui{    
    csgini `val'
    matrix temp    = result 
    matrix results = results \ temp
    }
}

matrix results = results[2..`=rowsof(results)', .]

local values 1 0.01 0.025 0.05 0.1 0.25 0.50
matrix rownames results = `values'
matrix colnames results = "FY11" "CSLB" "CSUB" "DistDiD"
matrix list results
