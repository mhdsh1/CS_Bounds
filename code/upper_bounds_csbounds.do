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
* Reading Data 
****************

/*
import delimited "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15.csv", clear

gen wage0 = wage / 100

preserve
collapse (mean) state_mw2010 = state_mw ///
if year == 2010, by (statenum year month)
tempfile mw2010
save `mw2010'
restore

qui merge m:1 statenum month using  `mw2010', keepusing(state_mw2010) nogen

preserve
collapse (mean) state_mw2011 = state_mw ///
if year == 2011, by (statenum year month)
tempfile mw2011
save `mw2011'
restore

qui merge m:1 statenum month using  `mw2011', keepusing(state_mw2011) nogen


preserve
collapse (mean) state_mw2015 = state_mw ///
if year == 2015, by (statenum year month)
tempfile mw2015
save `mw2015'
restore

qui merge m:1 statenum month using  `mw2015', keepusing(state_mw2015) nogen

order statenum year month wage0 state_mw*  
sort year month statenum 

gen smw_increase1115 = (state_mw2015 - state_mw2011 > 0.25)

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

keep wage0 treatment period		
			 
save "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input.dta", replace
*/

timer clear 

timer on 1

use "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input.dta", clear

which csbounds

csbounds wage0, treatment(treatment) period(period)

timer off 1

timer list 

/*
mata: mata clear

mata:
real scalar qplus(real scalar q, real vector FY, real vector y, real vector supp) {
    cond = (FY :<= q) :& (supp :== 1)
    if (sum(cond) > 0) {
        return(max(select(y, cond)))
    }
    return(.)
}
end

mata:
	real vector calculate_FY10TLBY(string y) {

        y_data = st_data(., y)

	FY00CR = st_data(., "FY00CR")
        FY10CR = st_data(., "FY10CR")
	R      = st_data(., "R")
        Ysupp1 = st_data(., "Ysupp1")
        FY00TR = st_data(., "FY00TR")

        FY10TLBY = J(rows(FY10CR), 1, .)
        
        for (i = 1; i <= rows(FY10CR); i++) {  
            max_y = qplus(FY10CR[i], FY00CR, y_data, R)
            FY10TLBY[i] = select(FY00TR, (y_data :== max_y))
		}

        return(FY10TLBY)
    }
end

// FY10TLBY[1,i]  = FY00T(qplus(FY10C(Ysupp1noInf[i]),FY00CR,R))  


gen FY10TLBY = .

local y = "wage0"
mata: FY10TLBY = calculate_FY10TLBY("`y'")

mata: st_store(., "FY10TLBY", FY10TLBY)
*/
