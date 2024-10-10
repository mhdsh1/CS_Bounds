program csbounds, rclass
    version 14.0
	
    syntax varlist(min=3 max=3) [if]
	
    local y         : word 1 of `varlist'
    local treatment : word 2 of `varlist'
    local period    : word 3 of `varlist'
    
    sort `y'
    
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
    global maxR = 2*(1 + r(max))

    preserve
	
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
    restore

    preserve
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

    keep if R != .

    keep y R Ysupp1 FY00C FY00T FY10C FY11T
    
	tempfile R
    save `R'
    restore

// making the Rm
	
	g ym = -y

    sort  ym
    cumul ym if $cond00C, gen(FmY00C) equal
    cumul ym if $cond00T, gen(FmY00T) equal

    keep if period != .
    keep if treatment != .
    keep if ym != .

    sort ym
    collapse (firstnm) FmY00C FmY00T, by(ym)

    qui merge m:1 ym using `supports', keepusing(Rm) nogen

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
    save `Rm'

// merging all the supports and distributuins 
merge 1:1 y using `R', ///
keepusing (R Ysupp1 FY00C FY00T FY10C FY11T) nogen

order y R Ysupp1 FY00C FY00T FY10C FY11T ym Rm FmY00C FmY00T

replace FY00C = 0 if FY00C == .
replace FY10C = 0 if FY10C == .
replace FY00T = 0 if FY00T == .
replace FY11T = 0 if FY11T == .

replace FmY00C = 0 if FmY00C == .  
replace FmY00T = 0 if FmY00T == . 

g FY10TDDID = FY00T + FY10C - FY00C

sort R

g FY10TUBCS = .
mata: FY10TUBCS = ubound("`y'")
mata: st_store(., "FY10TUBCS", FY10TUBCS)

g FY10TLBCS = .
mata: FY10TLBCS = lbound("`y'")
mata: st_store(., "FY10TLBCS", FY10TLBCS)

// saving these to use them later 
g EY00C = $EY00C
g EY00T = $EY00T
g EY10C = $EY10C
g EY11T = $EY11T

end

mata:
real vector ubound(string y) {

    FY00CR = st_data(., "FY00C")
    FY10CR = st_data(., "FY10C")
    FY00TR = st_data(., "FY00T")
    R      = st_data(., "R")
    Ysupp1 = st_data(., "Ysupp1")

    FY10TUBY  = J(rows(R), 1, .)
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

mata:
real vector lbound(string y) {

    FY00CR = st_data(., "FY00C")
    FY10CR = st_data(., "FY10C")
    FY00TR = st_data(., "FY00T")
    R      = st_data(., "R")
    Rm     = st_data(., "Rm")
    Ysupp1 = st_data(., "Ysupp1")
	
    FmY00CR = st_data(., "FmY00C")
    FmY00TR = st_data(., "FmY00T")

    FY10TLBmY = J(rows(R), 1, .) 
    FY10TLBCS = J(rows(R), 1, .) 
    
    for (i = 1; i <= rows(Ysupp1); i++) {
        cond = (Ysupp1[i] != .)
        if (sum(cond) > 0) {
            min = qminus(1-FY10CR[i], FmY00CR, Rm)
            FY10TLBmY[i] = 1 - select(FmY00TR, (Rm :== min))
            }
        }
		
    for (i = 1; i <= rows(R); i++) {
        cond = (Ysupp1 != .) :& (R :<= R[i])        
        if (sum(cond) > 0) {
            FY10TLBCS[i] = max(select(FY10TLBmY, cond))
            }
        }
		
    return(FY10TLBCS)
    }
end

mata:
real scalar qminus(real scalar q, real vector FY, real vector R) {
    cond = (FY :>= q) 
    if (sum(cond) > 0) {
        return(min(select(R, cond)))
    }
    return(.)
}
end
