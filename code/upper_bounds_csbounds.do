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

*** MATA Functions

mata:
real scalar qminus(real scalar q, real vector FY, real vector y, real vector supp) {
    cond = (FY :>= q) :& (supp :== 1)
    if (sum(cond) > 0) {
        return(min(select(y, cond)))
    }
    return(.)
}
end

****************
* Reading Data 
****************

//import delimited "${input_path}/CPS_cleaned_merged_07_15.csv", clear
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

rename wage0 y 



which csbounds

csbounds y, treatment(treatment) period(period)






/*
**********************
* Upper Bound Plots 
**********************

* Obs denotes FY11|D=1. 
* CF-LB/CF-UB denote the CS LB/UB on FY10|D=1

twoway (line FY10TUBCS y if inrange(y,0,50), lcolor(blue)) || ///
       (line FY11TR    y if inrange(y,0,50), lcolor(black) )
//graph export "${output_path}/FTCSbounds_wages_Cengizetal2019", ///
//as(pdf) replace
       

twoway (line FY10TUBCS y if (inrange(y,0,15) & inrange(FY10TUBCS,0,0.25)), lcolor(blue)) || ///
       (line FY11TR    y if (inrange(y,0,15) & inrange(FY11TR,0,0.25)),    lcolor(black) )
//graph export "${output_path}/FTCSbounds_zoombottom_wages_Cengizetal2019", ///
//as(pdf) replace

*/



