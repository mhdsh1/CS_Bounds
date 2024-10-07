/********************************************
Authors: Mahdi
Date:
Last modified:
Description: 


// one of the atts is different 

// the problem with values in R = 4.8 

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
global temp_path   = "${path}/data/temp"

********************************************

*** MATA Functions 

mata:
real scalar qminus(real scalar q, real vector FY, real vector R) {
    cond = (FY :>= q) 
    if (sum(cond) > 0) {
        return(min(select(R, cond)))
    }
    return(.)
}
end

***

use "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15.dta", clear

gen y = wage / 100

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

order statenum year month y state_mw*  
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

keep y treatment period	
	 
//save "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input.dta", replace


*** begining of the program

global cond00C = "(period == 0 & treatment == 0)"
global cond00T = "(period == 0 & treatment == 1)"
global cond10C = "(period == 1 & treatment == 0)"
global cond11T = "(period == 1 & treatment == 1)"

qui summ y if  $cond00C
global EY00C = r(mean)
qui summ y if  $cond00T
global EY00T = r(mean)
qui summ y if  $cond10C
global EY10C = r(mean)
qui summ y if  $cond11T
global EY11T = r(mean)

qui summ y if ($cond00C | $cond00T | $cond10C) 
global max = 1 + r(max)
//global max = 858.14

/*
*** making the supports 

use "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input.dta", clear

	summ y if ($cond00C | $cond00T | $cond10C) 
    local startR = -1 - r(min)
    local endR   = 1  + r(max)

	local startRm = -1 - r(max)
    local endRm   = 1  + r(min)
    
	summ y if $cond10C
    local start1 = 0
    local end1   = r(max)
    
	global Inf = 1e30    
    
    local Rgrid = 0.01
    local R_size  = round((`endR' - `startR')   / `Rgrid') + 1
    local Rm_size = round((`endRm' - `startRm') / `Rgrid') + 1

    local total_obs = `R_size' + 2
    di `total_obs'
	
    clear
    set obs `total_obs'
    
    range   seq `startR' `endR' `R_size'
    replace seq = round(seq, `Rgrid')
    gen     y = seq[_n-1]
    replace y = -${Inf} in 1
    replace y = ${Inf} in `total_obs'
    sort y
    drop seq
    
    range   seq `startRm' `endRm' `Rm_size'
    replace seq = round(seq, `Rgrid')
    gen     ym = seq[_n-1]
    replace ym = -${Inf} in 1
    replace ym = ${Inf} in `total_obs'
    sort ym
    drop seq
   
    gen R  = 1
    gen Rm = 1
    gen Ysupp1 = ((y >= `start1' & y <= `end1') | inlist(_n, 1, _N))
    
    tempfile supports
    save `supports'
    save "${path}/data/temp/supports", replace

*** making the R 

use "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input.dta", clear

    sort  y
    cumul y if $cond00C, gen(FY00C) equal
    cumul y if $cond00T, gen(FY00T) equal
    cumul y if $cond10C, gen(FY10C) equal    
    cumul y if $cond11T, gen(FY11T) equal
	
    keep if period    != .
    keep if treatment != .
    keep if y         != .

    sort y
    collapse (firstnm) FY00C FY00T FY11T FY10C, by(y)

    qui merge m:1 y using "${path}/data/temp/supports", ///
	keepusing(R Ysupp1) nogen

    replace  R = .    if R == 0
    replace  R = y    if R == 1

    replace  Ysupp1 = . if Ysupp1 == 0
    replace  Ysupp1 = y if Ysupp1 == 1
    
	sort y 
    replace FY00C = FY00C[_n-1] if FY00C == .
    replace FY10C = FY10C[_n-1] if FY10C == .
    replace FY00T = FY00T[_n-1] if FY00T == .
    replace FY11T = FY11T[_n-1] if FY11T == .

    keep if R != .

    keep y R Ysupp1 FY00C FY00T FY10C FY11T
    
	tempfile R
    save "${path}/data/temp/R", replace

*** making the Rm

use "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input.dta", clear

   g ym = -y

    sort  ym
    cumul ym if $cond00C, gen(FmY00C) equal
    cumul ym if $cond00T, gen(FmY00T) equal

    keep if period != .
    keep if treatment != .
    keep if ym != .

    sort ym
    collapse (firstnm) FmY00C FmY00T, by(ym)

    qui merge m:1 ym using "${path}/data/temp/supports", ///
    keepusing(Rm) nogen

    sort ym

    replace  Rm = .    if Rm == 0
    replace  Rm = ym   if Rm == 1

    sort ym 
    replace FmY00C = FmY00C[_n-1] if FmY00C == .
    replace FmY00T = FmY00T[_n-1] if FmY00T == .

    keep if Rm != . 
    g y = -ym
    keep y ym Rm FmY00C FmY00T 

    tempfile Rm
    save "${path}/data/temp/Rm", replace


merge 1:1 y using "${path}/data/temp/R", keepusing (R Ysupp1 FY00C FY00T FY10C FY11T) nogen

order y R Ysupp1 FY00C FY00T FY10C FY11T ym Rm FmY00C FmY00T

replace FY00C = 0 if FY00C == .
replace FY10C = 0 if FY10C == .
replace FY00T = 0 if FY00T == .
replace FY11T = 0 if FY11T == .

replace FmY00C = 0 if FmY00C == .  
replace FmY00T = 0 if FmY00T == . 

g FY10TDDID = FY00T + FY10C - FY00C

 
*** all the bounds 

g FY10TUBY  = . 
g FY10TUBCS = . 

g FY10TLBmY  = . 
g FY10TLBCSm = . 

sort R

mata:

    Rgrid = 0.01 
    R      = round(st_data(., "R"),      Rgrid)
	Rm     = round(st_data(., "Rm"),     Rgrid)
    Ysupp1 = round(st_data(., "Ysupp1"), Rgrid)

    FY00CR  = st_data(., "FY00C")
    FY10CR  = st_data(., "FY10C")
    FY00TR  = st_data(., "FY00T")
	FY11TR  = st_data(., "FY11T")
    FmY00CR = st_data(., "FmY00C")
    FmY00TR = st_data(., "FmY00T")

    FY10TUBY  = J(rows(R), 1, .) 
    FY10TUBCS = J(rows(R), 1, .) 

    FY10TLBmY  = J(rows(Rm), 1, .) 
    FY10TLBCSm = J(rows(Rm), 1, .) 
	
    for (i = 1; i <= rows(Ysupp1); i++) {
        cond = (Ysupp1[i] != .)
        if (sum(cond) > 0) {
            min = round(qminus(FY10CR[i], FY00CR, R), Rgrid)
            FY10TUBY[i] = select(FY00TR, (R :== min))
            }
        }
	st_store(., "FY10TUBY", FY10TUBY)
	
    for (i = 1; i <= rows(R); i++) {
        cond = (Ysupp1 != .) :& (R :<= R[i])        
        if (sum(cond) > 0) {
            FY10TUBCS[i] = max(select(FY10TUBY, cond))
            }
        }
    st_store(., "FY10TUBCS", FY10TUBCS)

    for (i = 1; i <= rows(Ysupp1); i++) {
        cond = (Ysupp1[i] != .)
        if (sum(cond) > 0) {
            min = round(qminus(1-FY10CR[i], FmY00CR, Rm), Rgrid)
            FY10TLBmY[i] = 1 - select(FmY00TR, (Rm :== min))
            }
			
        }

    st_store(., "FY10TLBmY", FY10TLBmY)
		
    for (i = 1; i <= rows(R); i++) {
        cond = (Ysupp1 != .) :& (R :<= R[i])        
        if (sum(cond) > 0) {
            FY10TLBCSm[i] = max(select(FY10TLBmY, cond))
            }
        }

    st_store(., "FY10TLBCSm", FY10TLBCSm)
		
end

*/

// save "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input_with_dists.dta", replace 
 
use "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input_with_dists.dta", clear 
 
**** Table 2 for subgroup 2 gini

local u = 0.5

mata:

    Rgrid = 0.01
    u     = `u'
    maxR = 2*$max
    
    EY11T = $EY11T
    EY00C = $EY00C
    EY10C = $EY10C
    EY00T = $EY00T
	
    R      = round(st_data(., "R"),      Rgrid)
	Rm     = round(st_data(., "Rm"),     Rgrid)
    Ysupp1 = round(st_data(., "Ysupp1"), Rgrid)
    
    FY00CR  = st_data(., "FY00C")
    FY10CR  = st_data(., "FY10C")
    FY00TR  = st_data(., "FY00T")
	FY11TR  = st_data(., "FY11T")
    FY10TDDID = st_data(., "FY10TDDID")
    
	FmY00CR = st_data(., "FmY00C")
    FmY00TR = st_data(., "FmY00T")

    FY10TUBCS = st_data(., "FY10TUBCS") 
    FY10TLBCS = st_data(., "FY10TLBCSm")

    fY10TDDID = J(rows(R), 1, .)

    for (i=2; i<=rows(R); i++) {
        fY10TDDID[i] = FY10TDDID[i] - FY10TDDID[i-1]
        }

    pgrid  = 0.001
    startp = 0
    endp   = 1
    p_size = (endp - startp) / pgrid + 1
    p = startp :+ (0::(p_size - 1)) * pgrid
    p = round(p, pgrid)
    
	QY11T     = J(rows(p), 1, .) 
    QY10TCSLB = J(rows(p), 1, .)
	QY10TCSUB = J(rows(p), 1, .)
	QY10TDDID = J(rows(p), 1, .)

    for (i = 1; i <= rows(p); i++) {  
        QY11T[i]     = round(qminus(p[i], FY11TR,    R), Rgrid)
	    QY10TCSLB[i] = round(qminus(p[i], FY10TUBCS, R), Rgrid)
		QY10TCSUB[i] = round(qminus(p[i], FY10TLBCS, R), Rgrid)
		QY10TDDID[i] = round(qminus(p[i], FY10TDDID, R), Rgrid)
        }
		
    EQY11Tu     = 0
    for (i = 1; i <= rows(p); i++) {
        cond = (p[i] :<= u) :& (p[i] :> 0) 
        if (sum(cond) > 0) {
		    EQY11Tu     = EQY11Tu     + QY11T[i]  
			}
        }
	EQY11Tu     = EQY11Tu*pgrid/u

    // Table 2 Results //

    G11T     = 0
	G10TCSLB = 0
	G10TCSUB = 0
	G10TDDID = 0
	
	for (i = 1; i <= rows(p); i++) {
        cond = (QY11T[i] :<= maxR) :& 
		       (QY11T[i] :>= min(Ysupp1[2..rows(Ysupp1)])) 
        if (sum(cond) > 0) {
		    G11T = G11T + 2*(1-p[i])*QY11T[i]*pgrid
            }
        }

	for (i = 1; i <= rows(p); i++) {
        cond = (QY10TCSLB[i] :<= maxR) :& 
		       (QY10TCSLB[i] :>= min(Ysupp1[2..rows(Ysupp1)])) 
        if (sum(cond) > 0) {
		    G10TCSLB = G10TCSLB + 2*(1-p[i])*QY10TCSLB[i]*pgrid
            }
        }
	
	for (i = 1; i <= rows(p); i++) {
        cond = (QY10TCSUB[i] :<= maxR) :& 
		       (QY10TCSUB[i] :>= min(Ysupp1[2..rows(Ysupp1)])) 
        if (sum(cond) > 0) {
		    G10TCSUB = G10TCSUB + 2*(1-p[i])*QY10TCSUB[i]*pgrid
            }
        }
	
	for (i = 1; i <= rows(p); i++) {
        cond = (QY10TDDID[i] :<= maxR) :& 
		       (QY10TDDID[i] :>= min(Ysupp1[2..rows(Ysupp1)])) 
        if (sum(cond) > 0) {
		    G10TDDID = G10TDDID + 2*(1-p[i])*QY10TDDID[i]*pgrid
            }
        }
			
    // Table 3 Results //
	
    GQY11Tu     = 0
	GQY10TCSLBu = 0
	GQY10TCSUBu = 0
	GQY10TDDIDu = 0
	
	for (i = 1; i <= rows(p); i++) {
        cond = (p[i] :<= u) :& 
		       (p[i] :> 0) 
        if (sum(cond) > 0) {
		    GQY11Tu     = GQY11Tu     + 2*(u-p[i])*QY11T[i]    *pgrid/(u^2)
		    GQY10TCSLBu = GQY10TCSLBu + 2*(u-p[i])*QY10TCSLB[i]*pgrid/(u^2)
		    GQY10TCSUBu = GQY10TCSUBu + 2*(u-p[i])*QY10TCSUB[i]*pgrid/(u^2)
		    GQY10TDDIDu = GQY10TDDIDu + 2*(u-p[i])*QY10TDDID[i]*pgrid/(u^2)
            }
        }
		
	// output
		
	G11T    = round(G11T,  0.01)
    GTTCSLB = round(G11T-G10TCSUB, 0.01)
    GTTCSUB = round(G11T-G10TCSLB, 0.01)
    GTTDDID = round(G11T-G10TDDID, 0.01)

	results = (G11T, GTTCSLB, GTTCSUB, GTTDDID)
    st_matrix("results", results)
	
    GQY11Tu = round(GQY11Tu, 0.01)
    GQCSLBu = round(GQY11Tu-GQY10TCSLBu, 0.01)
    GQCSUBu = round(GQY11Tu-GQY10TCSUBu, 0.01)
    GQDDIDu = round(GQY11Tu-GQY10TDDIDu, 0.01)

    resultsu = (GQY11Tu, GQCSLBu, GQCSUBu, GQDDIDu)
    st_matrix("resultsu", resultsu)

    end

matrix colnames results = G11T GTTCSLB GTTCSUB GTTDDID
matrix rownames results = "Gini SWF"
matrix list results
	
matrix colnames resultsu = GQY11Tu GQCSLBu GQCSUBu GQDDIDu
matrix rownames resultsu = "LT Gini SWF for u= `u'"
matrix list resultsu

// mata end 
