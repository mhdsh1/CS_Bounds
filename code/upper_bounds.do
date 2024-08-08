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
else global path = "/Users/`user'/Dropbox/Research/`folder'"
 
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

import delimited "path/to/your/CPS_cleaned_merged_07_15.csv", clear

gen wage0 = wage / 100

gen state_mw2007 = state_mw
gen state_mw2010 = state_mw
gen state_mw2015 = state_mw

levelsof statenum, local(states)

foreach i of local states {
    forvalues j = 1/12 {
        replace state_mw2007 = state_mw if statenum == `i' & month == `j' & year == 2007
        replace state_mw2010 = state_mw if statenum == `i' & month == `j' & year == 2010
        egen mean_mw2015 = mean(state_mw) if statenum == `i' & month == `j' & year == 2015
        replace state_mw2015 = mean_mw2015 if statenum == `i' & month == `j'
        drop mean_mw2015
    }
}


gen smw_increase0710 = (state_mw2010 - state_mw2007 > 0.25)
gen smw_increase1015 = (state_mw2015 - state_mw2010 > 0.25)
gen smw_increase0715 = (state_mw2015 - state_mw2007 > 0.5)


preserve
keep if year == 2007
tempfile data2007
save `data2007', replace

restore
keep if year == 2010
tempfile data2010
save `data2010', replace

restore
keep if year == 2015
tempfile data2015
save `data2015', replace

*********************************
* making the outcomes Tn and Cn
*********************************


// Define parameters
local premw = 8
local upremw = .
local preperiod = 2010
local Tperiod = 2015

use "data2010.dta", clear  // Adjust file path as necessary
gen byte in_range_2010 = state_mw2010 >= `premw' & state_mw2010 < `upremw'
keep if in_range_2010
bysort smw_increase1015: summarize wage0, detail
tempfile data2010subsample
save `data2010subsample', replace

use "data2015.dta", clear  // Adjust file path as necessary
gen byte in_range_2015 = state_mw2010 >= `premw' & state_mw2010 < `upremw'
keep if in_range_2015
bysort smw_increase1015: summarize wage0, detail
tempfile data2015subsample
save `data2015subsample', replace

use "data2010.dta", clear
gen byte Tn_00 = smw_increase1015 == 1 & state_mw2010 >= `premw' & state_mw2010 < `upremw'
gen byte Cn_00 = smw_increase1015 == 0 & state_mw2010 >= `premw' & state_mw2010 < `upremw'
tempfile Y00Tn
save `Y00Tn', replace if Tn_00
tempfile Y00Cn
save `Y00Cn', replace if Cn_00

use "data2015.dta", clear
gen byte Tn_11 = smw_increase1015 == 1 & state_mw2010 >= `premw' & state_mw2010 < `upremw'
gen byte Cn_10 = smw_increase1015 == 0 & state_mw2010 >= `premw' & state_mw2010 < `upremw'
tempfile Y11Tn
save `Y11Tn', replace if Tn_11
tempfile Y10Cn
save `Y10Cn', replace if Cn_10


use `data2010subsample', clear
bysort smw_increase1015: summarize wage0, detail
use `data2015subsample', clear
bysort smw_increase1015: summarize wage0, detail


use `Y00Tn', clear
list wage0

use `Y00Cn', clear
list wage0

use `Y11Tn', clear
list wage0

use `Y10Cn', clear
list wage0



// here you want to make a dataframe and save these guys there 



############################
# Define the empirical cdfs
############################

FY00C<-ecdf(Y00Cn)
FY10C<-ecdf(Y10Cn)
FY00T<-ecdf(Y00Tn)
FY11T<-ecdf(Y11Tn)

Ysupp0=c(-Inf,seq(0,max(Y00Cn,Y00Tn),0.01),Inf)
Ysupp1=c(-Inf,seq(0,max(Y10Cn),0.01),Inf)
R=c(-Inf, seq(-1,1+max(Y00Cn,Y00Tn,Y10Cn),0.01),Inf)

FY00Cs=matrix(,nrow=1,ncol=length(Ysupp0))

FY10Cs=matrix(,nrow=1,ncol=length(Ysupp1))
FY00Ts=matrix(,nrow=,ncol=length(Ysupp0))
for (i in 1:length(Ysupp0)){
  
  FY00Cs[1,i]=FY00C(Ysupp0[i])
  
  FY00Ts[1,i]=FY00T(Ysupp0[i])
}
for (i in 1:length(Ysupp1)){
  FY10Cs[1,i]=FY10C(Ysupp1[i])
}


