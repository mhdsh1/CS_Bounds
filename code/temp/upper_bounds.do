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


***********************************
*** just getting the summ stats liKW Table 1 in GKM
***********************************

preserve 
g group = .

local premw     = 8
local upremw    = .

local preperiod = 11
local Tperiod   = 15

replace group = 1 	if year == 20`preperiod'                & ///
			smw_increase`preperiod'`Tperiod' == 1   & ///
			state_mw20`preperiod' >= `premw'        & ///
			state_mw20`preperiod' <  `upremw'

replace group = 2	if year == 20`preperiod'                & ///
			smw_increase`preperiod'`Tperiod' == 0   & ///
			state_mw20`preperiod' >= `premw'        & ///
			state_mw20`preperiod' <  `upremw'
			 
replace group = 3 	if year == 20`Tperiod'                  & ///
			smw_increase`preperiod'`Tperiod' == 1   & ///
			state_mw20`preperiod' >= `premw'        & ///
			state_mw20`preperiod' <  `upremw'

replace group = 4 	if year == 20`Tperiod'                  & ///
			smw_increase`preperiod'`Tperiod' == 0   & ///
			state_mw20`preperiod' >= `premw'        & ///
			state_mw20`preperiod' <  `upremw'

label define grp_lab 1 "00T" 2 "00C" 3 "11T" 4 "10C"
label values group grp_lab

			
tabstat wage0*, by(group) statistics(mean sd n) format(%9.2f)
restore


*********************************
*** making the outcomes Tn and Cn
*********************************

// Define parameters
local premw     = 8
local upremw    = .
local preperiod = 2010
local Tperiod   = 2015

g group = .

replace group = 1 	if year == 2010         & ///
			smw_increase1015 == 1   & ///
			state_mw2010 >= `premw' & ///
			state_mw2010 <  `upremw'

replace group = 2	if year == 2010         & ///
			smw_increase1015 == 0   & ///
			state_mw2010 >= `premw' & ///
			state_mw2010 <  `upremw'
			 
replace group = 3 	if year == 2015         & ///
			smw_increase1015 == 1   & ///
			state_mw2010 >= `premw' & ///
			state_mw2010 <  `upremw'

replace group = 4 	if year == 2015         & ///
			smw_increase1015 == 0   & ///
			state_mw2010 >= `premw' & ///
			state_mw2010 <  `upremw'

label define grp_lab 1 "00T" 2 "00C" 3 "11T" 4 "10C"
label values group grp_lab

// tabstat wage0*, by(group) statistics(min p25 p50 mean p75 max var n) format(%9.2f)

rename wage0 y 

***************************
*** creating FY* functions
***************************

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


twoway (line FY10TDDID y if inrange(y,0,50))
//graph export "${output_path}/DistDiDY10T_wages_Cengizetal2019", ///
//as(pdf) replace

twoway (line FY00CR y if inrange(y,0,50))
//graph export "${output_path}/FY00C_wages_Cengizetal2019", ///
//as(pdf) replace

twoway (line FY10CR y if inrange(y,0,50)) 
//graph export "${output_path}/FY10C_wages_Cengizetal2019", ///
//as(pdf) replace

twoway (line FY00TR y if inrange(y,0,50)) 
//graph export "${output_path}/FY00T_wages_Cengizetal2019", ///
//as(pdf) replace

twoway (line FY11TR y if inrange(y,0,50)) 
//graph export "${output_path}/FY11T_wages_Cengizetal2019", ///
//as(pdf) replace


**********************
* creating  FY10TUBY
**********************
/*
R code: 
    
 FY10TUBY[1,i]= FY00T(qminus(FY10C(Ysupp1noInf[i]),FY00CR,R))
 
*/

// FY10TUBY support is Ysupp1
// this function gives the values for all R but i dont know what does the 
// FY10TUBY for values except those in R mean ...
g FY10TUBY = .

mata:

FY00CR   = st_data(., "FY00CR")
FY10CR   = st_data(., "FY10CR")
y        = st_data(., "y")
Ysupp1   = st_data(., "Ysupp1")
FY00TR   = st_data(., "FY00TR")
FY10TUBY = J(rows(FY00CR), 1, .) 

for (i = 1; i <= rows(FY10CR); i++) {
    min_y = qminus(FY10CR[i], FY00CR, y, Ysupp1)
    FY10TUBY[i] = select(FY00TR, (y :== min_y))
}
st_store(., "FY10TUBY", FY10TUBY)
end



*************************************************
* Next step: FY10TUBCS ... limsup transformation  
*************************************************

// FY10TUBY support is R

/*
Finds the maximum value in FY10TUBY where Ysupp1noInf is less than or equal to 
the corresponding value in R. Stores this maximum value in the FY10TUBCS matrix 
or vector at the appropriate position.
*/

g FY10TUBCS = .

mata:
   
    R = st_data(., "R")  

    FY10TUBY = st_data(., "FY10TUBY")
    y        = st_data(., "y")       
    Ysupp1   = st_data(., "Ysupp1")  

    FY10TUBCS = J(rows(y), 1, .)  // Initialize FY10TUBCS vector
    
    for (n = 1; n <= rows(y); n++) {
        cond = (Ysupp1 :== 1) :& (y :<= y[n])
        
        if (sum(cond) > 0) {
            maximum = max(select(FY10TUBY, cond))
            FY10TUBCS[n] = maximum
        }
    }

    st_store(., "FY10TUBCS", FY10TUBCS)
end


g FY10TCiC = FY10TUBCS

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


****************** making the quantiles ******************

mata: mata clear

mata:
FY00CR = st_data(., "FY00CR")
R      = st_data(., "R")
y      = st_data(., "y")

p          = range(0, 1, 0.001)
qminusY00R = J(rows(p), 1, .) 

for (i = 1; i <= rows(p); i++) {
    cond = (FY00CR :>= p[i]) :& (R :== 1)
    if (sum(cond) > 0) {
        qminusY00R[i] = min(select(y, cond))
    }
}

// round the thing later 
qminusY00R

end




mata: mata clear

mata:

real scalar qminus(real scalar q, real vector FY, real vector y, real vector supp) {
    cond = (FY :>= q) :& (supp :== 1)
    if (sum(cond) > 0) {
        return(min(select(y, cond)))
    }
    return(.)
}

FY00CR = st_data(., "FY00CR")
R      = st_data(., "R")
y      = st_data(., "y")

p          = range(0, 1, 0.001)
qminusY00R = J(rows(p), 1, .)  // Initialize with missing values

for (i = 1; i <= rows(p); i++) {
    qminusY00R[i] = qminus(p[i], FY00CR, y, R)
}

qminusY00R

end


