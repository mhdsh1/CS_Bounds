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
local user = "`c(username)'"

* Current working folder 
local folder = "CS_Bounds"

* Directory to Dropbox folder, which depends on machine type:
if "`c(os)'" == "Windows" global path = "C:/Users/`user'/Dropbox/`folder'"
if "`c(os)'" == "Unix"    global path = "/home/`user'/Dropbox/`folder'"
else                      global path = "/Users/`user'/Dropbox/`folder'"
 
global input_path  = "${path}/data"
global output_path = "${path}/output"

********************************************

use "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input.dta", clear

rename wage0 y

gen group = .
replace group = 1 if period == 0 & treatment == 1
replace group = 2 if period == 0 & treatment == 0
replace group = 3 if period == 1 & treatment == 1
replace group = 4 if period == 1 & treatment == 0

drop if group == .

sort  y
cumul y if group == 1, gen(FY00T) equal

sort  y
cumul y if group == 2, gen(FY00C) equal

sort  y
cumul y if group == 3, gen(FY11T) equal

sort  y
cumul y if group == 4, gen(FY10C) equal

keep if group != . 

keep y FY* group

sort y

collapse (firstnm) FY* group, by(y)

tempfile ecdf
save `ecdf'

*****************************************
*** Creating Supports R, Ysupp0, Ysupp1 
*****************************************

// 1 "00T" 2 "00C" 3 "11T" 4 "10C"
//Ysupp0 = c(-Inf, seq(0  ,         max(Y00Cn,Y00Tn),0.01), Inf)
//Ysupp1 = c(-Inf, seq(0  ,               max(Y10Cn),0.01), Inf)
//R      = c(-Inf, seq(-1 , 1+max(Y00Cn,Y00Tn,Y10Cn),0.01), Inf)


qui summ y if inlist(group,1,2,4)
local startR = -1
local endR   = 1 + r(max)

qui summ y if inlist(group,1,2)
local start0 = 0
local end0   = r(max)

qui summ y if inlist(group,4)
local start1 = 0
local end1   = r(max)

global Inf = 9.99e9

preserve
local step      = 0.01
local R_size    = round((`endR' - `startR') / `step') + 1
local total_obs = `R_size' + 2

clear
set obs `total_obs'

range seq `startR' `endR' `R_size'
replace seq = round(seq, 0.01)
gen y     = seq[_n-1]
replace y = -${Inf} in 1
replace y =  ${Inf} in `total_obs'
sort y
drop seq

gen R      = 1
gen Ysupp0 = ((y >= `start0' & y <= `end0') | inlist(_n, 1, _N))
gen Ysupp1 = ((y >= `start1' & y <= `end1') | inlist(_n, 1, _N))

tempfile supports
save `supports'
restore

merge m:1 y using `supports', keepusing(R Ysupp0 Ysupp1) nogen

sort y 

replace FY00C = FY00C[_n-1] if FY00C == .
replace FY10C = FY10C[_n-1] if FY10C == .
replace FY00T = FY00T[_n-1] if FY00T == .
replace FY11T = FY11T[_n-1] if FY11T == .

g FY00CR = FY00C if R == 1
g FY10CR = FY10C if R == 1
g FY00TR = FY00T if R == 1
g FY11TR = FY11T if R == 1

keep if R == 1
keep y R Ysupp0 Ysupp1 FY00CR FY10CR FY00TR FY11TR

replace FY00CR = 0 if FY00CR == .
replace FY10CR = 0 if FY10CR == .
replace FY00TR = 0 if FY00TR == .
replace FY11TR = 0 if FY11TR == .

*******************
* ecdf plots 
*******************

g FY10TDDID = FY00TR + FY10CR - FY00CR // Dist DiD assumption Slide 24/29
