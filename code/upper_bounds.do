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


g  double Y00Tn = .
g  double Y00Cn = .
g  double Y11Tn = .
g  double Y10Cn = .


// Define parameters
local premw     = 8
local upremw    = .
local preperiod = 2010
local Tperiod   = 2015

replace Y00Tn = wage0 if year == 2010            & ///
			 smw_increase1015 == 1   & ///
			 state_mw2010 >= `premw' & ///
			 state_mw2010 <  `upremw'

replace Y00Cn = wage0 if year == 2010            & ///
			 smw_increase1015 == 0   & ///
			 state_mw2010 >= `premw' & ///
			 state_mw2010 <  `upremw'
			 
replace Y11Tn = wage0 if year == 2015            & ///
			 smw_increase1015 == 1   & ///
			 state_mw2010 >= `premw' & ///
			 state_mw2010 <  `upremw'

replace Y10Cn = wage0 if year == 2015            & ///
			 smw_increase1015 == 0   & ///
			 state_mw2010 >= `premw' & ///
			 state_mw2010 <  `upremw'

/*
. tabstat Y*, statistics(min p25 p50 mean p75 max var n) format(%9.2f)

   Stats |     Y00Tn     Y00Cn     Y11Tn     Y10Cn
---------+----------------------------------------
     Min |      0.00      0.00      0.00      0.00
     p25 |     11.62     10.21     12.75     11.10
     p50 |     18.46     15.87     20.45     17.50
    Mean |     23.13     20.12     25.83     22.30
     p75 |     29.92     25.00     34.23     28.85
     Max |    857.14    225.00    961.54    307.69
Variance |    303.61    194.78    351.57    239.73
       N |  19877.00   4737.00  18039.00   4454.00
--------------------------------------------------

> summary(Y00Tn)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   11.62   18.46   23.13   29.92  857.14 
> summary(Y00Cn)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   10.21   15.87   20.12   25.00  225.00 
summary(Y10Cn)
> summary(Y11Tn)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   12.75   20.45   25.83   34.23  961.54 
> summary(Y10Cn)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   11.10   17.50   22.30   28.85  307.69 
*/


g group = .

local premw     = 8
local upremw    = .

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



sort  wage0
cumul wage0 if group == 1, gen(FY00T) equal

sort  wage0
cumul wage0 if group == 2, gen(FY00C) equal

sort wage0
cumul wage0 if group == 3, gen(FY11T) equal

sort wage0
cumul wage0 if group == 4, gen(FY10C) equal


preserve
qui summ wage0 if inlist(group,1,2,4)
local start        = -1
local end          = 1+r(max)
local step         = 0.01
local steps_number = round((`end' - `start')/`step') + 1
range   R `start' `end' `steps_numbnr'
replace R = round(R, 0.01)
keep R 
duplicates drop
rename R wage0
save "${output_path}/R", replace
restore


preserve 
keep if group != .
keep wage0 FY*
append using "${output_path}/R", generate(source)
sort wage0 source
collapse (firstnm) FY00C FY10C FY00T FY11T source, by(wage0)

replace FY00C = FY00C[_n-1] if FY00C == .
replace FY10C = FY10C[_n-1] if FY10C == .
replace FY00T = FY00T[_n-1] if FY00T == .
replace FY11T = FY11T[_n-1] if FY11T == .

save "${output_path}/wage0", replace
restore


/********************

sort  Y00Cn
cumul Y00Cn, gen(FY00C) equal

sort  Y10Cn
cumul Y10Cn, gen(FY10C) equal

sort  Y00Tn
cumul Y00Tn, gen(FY00T) equal

sort  Y11Tn
cumul Y11Tn, gen(FY11T) equal

preserve
sort Y00Cn
sort Y10Cn
sort Y00Tn
sort Y11Tn
keep Y* year 
gen id = _n  
reshape long Y, i(id) j(year)
keep Y
save "${output_path}/y2.dta", clear
restore




egen Rmax = rowmax(Y00Cn Y00Tn Y10Cn)
qui summ Rmax
local start        = -1
local end          = 1+r(max)
local step         = 0.01
local steps_number = round((`end' - `start')/`step') + 1
range   R `start' `end' `steps_numbnr'
replace R = round(R, 0.01)



preserve
egen Rmax = rowmax(Y00Cn Y00Tn Y10Cn)
qui summ Rmax
local start        = -1
local end          = 1+r(max)
local step         = 0.01
local steps_number = round((`end' - `start')/`step') + 1
range   R `start' `end' `steps_numbnr'
replace R = round(R, 0.01)
rename R y
keep y
tempfile R_data
save `R_data'
restore

preserve
rename Y00Cn y
keep y FY00C
tempfile y1_data
keep if y != . 
sort y 
duplicates drop
tempfile Y00C
save "${output_path}/Y00C", replace
restore 

preserve
rename Y10Cn y
keep y FY10C
keep if y != . 
sort y 
duplicates drop
tempfile Y10C
save "${output_path}/Y10C", replace
restore 

preserve
rename Y00Tn y
keep   y FY00T
keep if y != . 
sort y 
duplicates drop
tempfile Y00T
save "${output_path}/Y00T", replace
restore 

preserve
rename Y11Tn y
keep y FY11T
keep if y != . 
sort y 
duplicates drop
tempfile Y11T
save "${output_path}/Y11T", replace 
restore

preserve
use `R_data', clear
append using `y1_data', keep(y FY00C)
append using `y2_data', keep(y FY10C)
append using `y3_data', keep(y FY00T)
append using `y4_data', keep(y FY11T)
save "${output_path}/test2", replace 
restore



preserve 

use y, clear 




/*
preserve
summarize Y00Cn, meanonly
local min = r(min)
local max = r(max)

local step = 0.01
range R `min' `max' `=round((`max' - `min')/`step')'

tempfile seq_data
save `seq_data'

restore

preserve 
sort  Y00Cn
keep if Y00Cn != .
keep Y00Cn FY00C
tempfile original_data
save `original_data'
restore

use `seq_data', clear
joinby R using `original_data'

bysort R (Y00Cn): gen FY00C_new = FY00C[_n-1] if Y00Cn <= R
bysort R: replace FY00C_new = FY00C_new[_n-1] if missing(FY00C_new)






*******************
* Define the empirical cdfs
****************************

sort  Y00Cn
summarize Y00Cn, meanonly
local min = r(min)
local max = r(max)
local step = 0.01
range RY00Cn `min' `max' `=round((`max' - `min')/`step')'

tempfile seq_data
save `seq_data'

restore

sort Y00Cn
cumul Y00Cn, gen(FY00C) equal

// Step 5: Merge the sequence with the original data
merge m:1 R using `seq_data', keepusing(FY00C) nogen

// Step 6: Carry forward the last observed CDF value for the gaps (step function behavior)
bysort R: replace FY00C = FY00C[_n-1] if missing(FY00C)


cumul Y00Cn, gen(FY00C) equal




sort  Y10Cn
cumul Y10Cn, gen(FY10C) equal

sort  Y00Tn
cumul Y00Tn, gen(FY00T) equal

sort  Y11Tn
cumul Y11Tn, gen(FY11T) equal


local suffixes 00C 10C 00T 11T

foreach suf in `suffixes' {
    preserve
    sort Y`suf'n
    keep Y`suf'n 
    keep if Y`suf'n != .
    tempfile `suf'
    save ``suf''
    restore
}

preserve
use `00C', clear
append using `10C'
append using `00T'
append using `11T'

save "${output_path}/test", replace
restore

egen Rmax = rowmax(Y00Cn Y00Tn Y10Cn)
qui summ Rmax

local start        = -1
local end          = 1+r(max)
local step         = 0.01
local steps_number = round((`end' - `start')/`step') + 1


//range   R `start' `end' `steps_numbnr'
//replace R = round(z, 0.01)


local suffixes 00C 10C 00T 11T

foreach suf in `suffixes' {
    preserve
    sort Y`suf'n
    keep Y`suf'n FY`suf'
    keep if Y`suf'n != .
    range   R `start' `end' `steps_numbnr'
    replace R = round(R, 0.01)
    tempfile `suf'
    save "${output_path}/`suf'", replace
    restore
}


local suffixes 00C 10C 00T 11T

foreach suf in `suffixes' {

    merge m:1 R using "`temp_`suf''", keepusing(FY`suf') nogen
}
