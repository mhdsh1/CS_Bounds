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

use "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input.dta", clear

sort y

global cond00C = "(period == 0 & treatment == 0)"
global cond00T = "(period == 0 & treatment == 1)"
global cond10C = "(period == 1 & treatment == 0)"
global cond11T = "(period == 1 & treatment == 1)"

cumul y if $cond00C, gen(FY00C) equal
cumul y if $cond00T, gen(FY00T) equal
cumul y if $cond10C, gen(FY10C) equal    
cumul y if $cond11T, gen(FY11T) equal

keep if period != .
keep if treatment != .
keep if y != .

sort y
collapse (firstnm) period treatment FY00C FY00T FY11T FY10C, by(y)

summ y if ($cond00C | $cond00T | $cond10C) 
local startR = -1 - r(min)
local endR   = 1  + r(max)

summ y if $cond10C
local start1 = 0
local end1   = r(max)
    
global Inf = 1e30    
preserve
local step = 0.01
local R_size = round((`endR' - `startR') / `step') + 1
local total_obs = `R_size' + 2
    
clear
set obs `total_obs'
    
range seq `startR' `endR' `R_size'
replace seq = round(seq, 0.01)
gen y = seq[_n-1]
replace y = -${Inf} in 1
replace y = ${Inf} in `total_obs'
sort y
drop seq
    
gen R = 1
gen Ysupp1 = ((y >= `start1' & y <= `end1') | inlist(_n, 1, _N))
    
tempfile supports
save `supports'
restore
    
qui merge m:1 y using `supports', keepusing(R Ysupp1) nogen
sort y

replace  R = .      if R == 0
replace  R = y      if R == 1

replace  Ysupp1 = . if Ysupp1 == 0
replace  Ysupp1 = y if Ysupp1 == 1


replace FY00C = FY00C[_n-1] if FY00C == .
replace FY10C = FY10C[_n-1] if FY10C == .
replace FY00T = FY00T[_n-1] if FY00T == .
replace FY11T = FY11T[_n-1] if FY11T == .

keep R Ysupp1 FY00C FY00T FY11T FY10C
order R Ysupp1 FY00C FY00T FY11T FY10C

keep if R != . 

replace FY00C = 0 if FY00C == .
replace FY10C = 0 if FY10C == .
replace FY00T = 0 if FY00T == .
replace FY11T = 0 if FY11T == .

/////////////////
/// Functions ///
/////////////////

clear mata 

mata:
real scalar qminus(real scalar q, real vector FY, real vector R) {
    cond = (FY :>= q) 
    if (sum(cond) > 0) {
        return(min(select(R, cond)))
    }
    return(.)
}
end

mata:

    real vector bounds(string y) {

	FY00CR = st_data(., "FY00C")
        FY10CR = st_data(., "FY10C")
        FY00TR = st_data(., "FY00T")
	R      = st_data(., "R")
        Ysupp1 = st_data(., "Ysupp1")

        FY10TUBY = J(rows(R), 1, .)
        
        FY10TUBCS = J(rows(R), 1, .)  // Initialize FY10TUBCS vector
    
        for (i = 1; i <= rows(Ysupp1); i++) { 
	cond = (Ysupp1[i] != .)
	if (sum(cond)>0){
            min = qminus(FY10CR[i], FY00CR, R)
            FY10TUBY[i] = select(FY00TR, (R :== min))
		}
	}

	// limsup transformation
        for (i = 1; i <= rows(R); i++) {
        cond = (Ysupp1 != .) :& (R :<= R[i])        
        if (sum(cond) > 0) {
            FY10TUBCS[i] = max(select(FY10TUBY, cond))
		}
	}
    
    return(FY10TUBCS)

    }
end


// testing upper bound 
g FY10TUBCS = .
mata: FY10TUBCS = bounds("`y'")
mata: st_store(., "FY10TUBCS", FY10TUBCS)


*** ATT Function with quantile 

mata:

    R      = st_data(., "R") 
    FY10TUBCS = st_data(., "FY10TUBCS")
    FY10CR = st_data(., "FY10C")
    FY11TR = st_data(., "FY11T")
    FY00TR = st_data(., "FY00T")
    FY00CR = st_data(., "FY00C")

// defining the fY10TDDID

    FY10TDDID = J(rows(R), 1, .)
    FY10TDDID = FY00TR + FY10CR - FY00CR 
    fY10TDDID = J(rows(R), 1, .)

    for (i=2; i<=rows(R); i++) {
        fY10TDDID[i] = FY10TDDID[i] - FY10TDDID[i-1]
        }

	
    // making the p 
    step   = 0.001
    startp = 0
    endp   = 1
    p_size = (endp - startp) / step + 1
    p = startp :+ (0::(p_size - 1)) * step
    p = round(p, 0.001)

    QY10TCSLB = J(rows(p), 1, .)
    QY10TCSUB = J(rows(p), 1, .)    
    QY11T     = J(rows(p), 1, .) 
    

/* filling the QY10TCSLB
    for (i = 1; i <= rows(p); i++) {  
       QY10TCSLB[i] = qminus(p[i], FY10TUBCS, y_data, R)
       QY10TCSLB[i] = round(QY10TCSLB[i], 0.01) // we had some wiered decimals 
        }
*/

// filling the QY11T + calculating ATT for quantiles

    for (i = 1; i <= rows(p); i++) {  
       QY11T[i] = qminus(p[i], FY11TR, R)
       QY11T[i] = round(QY11T[i], 0.01) // we had some wiered decimals 
        }
	
/* finding the QY10TCSLB sum

    min_val = 0        // i should later change it to the min value of ysupp1 
    maxR    = 1716.28  // i should later define this as the 2*maxR

    vec = J(rows(p), 1, .)
    for (i = 1; i <= rows(QY10TCSLB); i++) {
    cond = (QY10TCSLB[i] :<= maxR) :& (QY10TCSLB[i] :>= min_val)
    if(sum(cond)>0){
        vec[i] = QY10TCSLB[i]
        }
    }
    sum(vec)
*/

// EQY11T01 =sum(QY11T[p<=0.01&p>0])*pgrid/0.01
/*
    vec = J(rows(p), 1, .)
    for (i = 1; i <= rows(QY11T); i++) {
    cond = (p[i] :<= 0.01) :& (p[i] :> 0) // 0.01 should be input in the function 
    if(sum(cond)>0){
        vec[i] = QY11T[i]
        }
    }
    sum(vec) // 36.29 and this is what it should be 
*/    
    
//     EQY10TDDID01d =sum(
//        R[R<Inf&R>-Inf&FY10TDDID<= 0.01]*
//fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.01]
//)

    vec = J(rows(R), 1, .)
    for (i = 1; i <= rows(R); i++) {
    cond = (R[i] :<  -R[1]) :& (R[i] :> R[1]) :& (FY10TDDID[i] :<= 0.01)
    if(sum(cond)>0){
        vec[i] = round(R[i],0.01)* fY10TDDID[i] // later you should define it as R 
        }
    }
    sum(vec) //   -.0058794004

	vec = J(rows(R), 1, .)
    for (i = 1; i <= rows(R); i++) {
    cond = (R[i] :<  -R[1]) :& (R[i] :> R[1]) :& (FY10TDDID[i] :<= 0.01)
    if(sum(cond)>0){
        vec[i] = fY10TDDID[i] // later you should define it as R 
        }
    }
    sum(vec) 
	
end

