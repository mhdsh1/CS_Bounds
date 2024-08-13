/********************************************
Authors: Mahdi
Date:
Last modified:
Description: 
********************************************/

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
else                      global path = "/Users/`user'/Dropbox/`folder'"
 
global input_path  = "${path}/data"
global output_path = "${path}/output"

********************************************

****************
* Functions 
****************

program define qminus
    args q FY y
    tempvar result
    gen double `result' = .
    
    quietly {
        forvalues i = 1/`=_N' {
            if `FY'[`i'] >= `q' {
                if missing(`result') | `y'[`i'] < `result' {
                    replace `result' = `y'[`i']
                }
            }
        }
    }
    
    display `result'
end


program define qplus
    args q FY y
    tempvar result
    gen double `result' = .
    
    local maxobs = _N - 1
    
    quietly {
        forvalues i = 1/`maxobs' {
            if `FY'[`i'] <= `q' {
                if missing(`result') | `y'[`i'] > `result' {
                    replace `result' = `y'[`i']
                }
            }
        }
    }
    
    display `result'
end


// there shoud be a better way to do this ... should i use mata?

****************
* Reading Data 
****************

import delimited "${input_path}/CPS_cleaned_merged_07_15.csv", clear


gen wage0 = wage / 100

preserve
collapse (mean) state_mw2007 = state_mw ///
if year == 2007, by (statenum year month)
tempfile mw2007
save `mw2007'
restore

merge m:1 statenum month using `mw2007', keepusing(state_mw2007) nogen

preserve
collapse (mean) state_mw2010 = state_mw ///
if year == 2010, by (statenum year month)
tempfile mw2010
save `mw2010'
restore

merge m:1 statenum month using  `mw2010', keepusing(state_mw2010) nogen

preserve
collapse (mean) state_mw2015 = state_mw ///
if year == 2015, by (statenum year month)
tempfile mw2015
save `mw2015'
restore

merge m:1 statenum month using  `mw2015', keepusing(state_mw2015) nogen

order statenum year month wage0 state_mw*  
sort year month statenum 

gen smw_increase1015 = (state_mw2015 - state_mw2010 > 0.25)

*********************************
* making the outcomes Tn and Cn
*********************************

// Define parameters
local premw     = 8
local upremw    = .
local preperiod = 2010
local Tperiod   = 2015

g group = .

replace group = 1 if year == 2010            & ///
	             smw_increase1015 == 1   & ///
	             state_mw2010 >= `premw' & ///
		     state_mw2010 <  `upremw'

replace group = 2 if year == 2010            & ///
			 smw_increase1015 == 0   & ///
			 state_mw2010 >= `premw' & ///
			 state_mw2010 <  `upremw'
			 
replace group = 3 if year == 2015            & ///
			 smw_increase1015 == 1   & ///
			 state_mw2010 >= `premw' & ///
			 state_mw2010 <  `upremw'

replace group = 4 if year == 2015            & ///
			 smw_increase1015 == 0   & ///
			 state_mw2010 >= `premw' & ///
			 state_mw2010 <  `upremw'

label define grp_lab 1 "00T" 2 "00C" 3 "11T" 4 "10C"
label values group grp_lab

// tabstat wage0*, by(group) statistics(min p25 p50 mean p75 max var n) format(%9.2f)


sort  wage0
cumul wage0 if group == 1, gen(FY00T) equal

sort  wage0
cumul wage0 if group == 2, gen(FY00C) equal

sort  wage0
cumul wage0 if group == 3, gen(FY11T) equal

sort  wage0
cumul wage0 if group == 4, gen(FY10C) equal

keep wage0 mFY*
sort wage0 


preserve
keep if group != . 
keep wage0 FY* 
sort wage0
collapse (firstnm) FY*, by(wage0)
tempfile ecdf
save "${output_path}/ecdf", replace
restore



preserve 
replace wage0 = - wage0

sort  wage0
cumul wage0 if group == 1, gen(mFY00T) equal

sort  wage0
cumul wage0 if group == 2, gen(mFY00C) equal

keep if group != . 

keep wage0 mFY*
sort wage0 

collapse (firstnm) mFY*, by(wage0)

tempfile mecdf
save "${output_path}/mecdf", replace

keep wage0 
sort wage0 

tempfile mwage0
save "${output_path}/mwage0", replace

restore

**************************************
*  plotting cdfs for Y00C, Y10C, Y00T
**************************************

* generating FY00TR + FY10CR - FY00CR

* Distributional DiD

* FY10TDDID = FY00TR + FY10CR - FY00CR # Dist DiD assumption Slide 24/29



//Ysupp0 = c(-Inf, seq(0  ,         max(Y00Cn,Y00Tn),0.01), Inf)
//Ysupp1 = c(-Inf, seq(0  ,               max(Y10Cn),0.01), Inf)


preserve
qui summ wage0 if inlist(group,1,2)
local start        = 0
local end          = r(max)
local step         = 0.01
local steps_number = round((`end' - `start')/`step') + 1
range   y `start' `end' `steps_numbnr'
replace y = round(y, 0.01)
keep y
sort y
rename y wage0
duplicates drop
tempfile Ysupp0 
save "${output_path}/Ysupp0", replace
restore 


preserve
qui summ wage0 if inlist(group,4)
local start        = 0
local end          = r(max)
local step         = 0.01
local steps_number = round((`end' - `start')/`step') + 1
range   y `start' `end' `steps_numbnr'
replace y = round(y, 0.01)
keep y
sort y
rename y wage0
duplicates drop
tempfile Ysupp0 
save "${output_path}/Ysupp1", replace
restore 



preserve
qui summ wage0 if inlist(group,1,2,4)
local start        = -1
local end          = 1+r(max)
local step         = 0.01
local steps_number = round((`end' - `start')/`step') + 1
range   R `start' `end' `steps_numbnr'
replace R = round(R, 0.01)
keep R
sort R
rename R wage0
duplicates drop
tempfile R
save "${output_path}/R", replace
restore 

preserve 
qui summ wage0 if inlist(group,1,2,4)
local start        = -1 - r(max)
local end          = 1
local step         = 0.01
local steps_number = round((`end' - `start')/`step') + 1
range   Rm `start' `end' `steps_numbnr'
replace Rm = round(Rm, 0.01)
keep Rm
sort Rm
rename Rm wage0
duplicates drop
tempfile Rm
save "${output_path}/Rm", replace
restore 

preserve
append using "${output_path}/Ysupp0", generate(Ysupp0)
append using "${output_path}/Ysupp1", generate(Ysupp1)
append using "${output_path}/R",      generate(R)
append using "${output_path}/Rm",     generate(Rm)
append using "${output_path}/mwage0",     generate(mwage0)
keep wage0 Ysupp0 Ysupp1 R Rm mwage0
sort wage0
collapse (sum) Ysupp0 Ysupp1 R Rm mwage0, by(wage0)
tempfile y
save "${output_path}/y", replace
restore

preserve
use "${output_path}/y", clear

merge 1:m wage0 using "${output_path}/ecdf",  keepusing(FY*) nogen

merge 1:m wage0 using "${output_path}/mecdf", keepusing(mFY*) nogen
restore


/*

//preserve
keep if group != .
keep wage0 FY*
append using "${output_path}/R", generate(R)
gsort wage0 -R
collapse (firstnm) FY00C FY10C FY00T FY11T R, by(wage0)

replace FY00C = FY00C[_n-1] if FY00C == .
replace FY10C = FY10C[_n-1] if FY10C == .
replace FY00T = FY00T[_n-1] if FY00T == .
replace FY11T = FY11T[_n-1] if FY11T == .

g FY10TDDID = FY00T + FY10C - FY00C


keep if R == 1 

rename FY00C FY00CR 
rename FY10C FY10CR 
rename FY00T FY00TR 
rename FY11T FY11TR 
*/

//g FY10TDDID = FY00TR + FY10CR - FY00CR

//summ FY10TDDID


//save "${output_path}/wage0", replace
//restore



 */



*********************************
* computing CS bounds on support
*********************************

/*

this is the tricky part where you need to
use the function 

and limsup transformation ... 

*/



