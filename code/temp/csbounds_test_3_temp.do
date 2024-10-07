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

********************************************

use "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input.dta", clear

mata:
real scalar qminus(real scalar q, real vector FY, real vector R) {
    cond = (FY :>= q) 
    if (sum(cond) > 0) {
        return(min(select(R, cond)))
    }
    return(.)
}
end

//    local u = 0.01 

    global cond00C = "(period == 0 & treatment == 0)"
    global cond00T = "(period == 0 & treatment == 1)"
    global cond10C = "(period == 1 & treatment == 0)"
    global cond11T = "(period == 1 & treatment == 1)"

    sort  y
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

    replace  R = .    if R == 0
    replace  R = y    if R == 1

    replace  Ysupp1 = . if Ysupp1 == 0
    replace  Ysupp1 = y if Ysupp1 == 1
    
	sort y 
    replace FY00C = FY00C[_n-1] if FY00C == .
    replace FY10C = FY10C[_n-1] if FY10C == .
    replace FY00T = FY00T[_n-1] if FY00T == .
    replace FY11T = FY11T[_n-1] if FY11T == .
    	
    keep  R Ysupp1 FY00C FY00T FY11T FY10C
    order R Ysupp1 FY00C FY00T FY11T FY10C

    keep if R != . 

    replace FY00C = 0 if FY00C == .
    replace FY10C = 0 if FY10C == .
    replace FY00T = 0 if FY00T == .
    replace FY11T = 0 if FY11T == .


//g	FY10TUBCS = .

mata:

    FY00CR = st_data(., "FY00C")
    FY10CR = st_data(., "FY10C")
    FY00TR = st_data(., "FY00T")
	FY11TR = st_data(., "FY11T")
    R      = st_data(., "R")
    Ysupp1 = round(st_data(., "Ysupp1"), 0.01)

    FY10TUBY  = J(rows(R), 1, .) 
    FY10TUBCS = J(rows(R), 1, .) 

    for (i = 200; i <= 210; i++) {
	    i 
		Ysupp1[i]
        cond = (Ysupp1[i] != .)
        if (sum(cond) > 0) {
            min = round(qminus(FY10CR[i], FY00CR, R), 0.01)
			min
            //FY10TUBY[i] = select(FY00TR, (R :== min))
            }
        }

end
		/*
    // limsup transformation
    for (i = 1; i <= rows(R); i++) {
        cond = (Ysupp1 != .) :& (R :<= R[i])        
        if (sum(cond) > 0) {
            FY10TUBCS[i] = max(select(FY10TUBY, cond))
            }
        }
		
	st_store(., "FY10TUBCS", FY10TUBCS)
end	

save "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input_with_dists.dta", replace

    FY10TDDID = FY00TR + FY10CR - FY00CR 
    fY10TDDID = J(rows(R), 1, .)

    for (i=2; i<=rows(R); i++) {
        fY10TDDID[i] = FY10TDDID[i] - FY10TDDID[i-1]
        }

    step   = 0.001
    startp = 0
    endp   = 1
    p_size = (endp - startp) / step + 1
    p = startp :+ (0::(p_size - 1)) * step
    p = round(p, 0.001)

    QY11T = J(rows(p), 1, .) 
    
    for (i = 1; i <= rows(p); i++) {  
        QY11T[i] = qminus(p[i], FY11TR, R)
        QY11T[i] = round(QY11T[i], 0.01)  
        }

    vec = J(rows(R), 1, .)

    for (i = 1; i <= rows(R); i++) {
        cond = (R[i] :<  -R[1]) :& (R[i] :> R[1]) :& ///
		       (FY10TDDID[i] :<= u)
        if (sum(cond) > 0) {
            vec[i] = round(R[i], 0.01) * fY10TDDID[i]
            }
        }
    
    sum(vec)
*/
end

