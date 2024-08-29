program define csbounds
    version 18.0
    syntax varlist(min=1) [if], treatment(varname) period(varname)

    * Extract variables
    local y : word 1 of `varlist'
    local treatment = "`treatment'"
    local period    = "`period'"

    di "Variable y: `y'"
    di "Treatment variable: `treatment'"
    di "Period variable: `period'"

    di as txt "Summary statistics for variable `y':"
    summarize `y'
    
    di as txt "Summary statistics for treatment variable `treatment':"
    summarize `treatment'
    
    di as txt "Summary statistics for period variable `period':"
    summarize `period'
    
        * Generate group variable
    gen group = .
    replace group = 1 if `period' == 0 & `treatment' == 1
    replace group = 2 if `period' == 0 & `treatment' == 0
    replace group = 3 if `period' == 1 & `treatment' == 1
    replace group = 4 if `period' == 1 & `treatment' == 0

    drop if group == .
    
    * Generate FY* functions
    sort `y'
    cumul `y' if group == 1, gen(FY00T) equal
    cumul `y' if group == 2, gen(FY00C) equal
    cumul `y' if group == 3, gen(FY11T) equal
    cumul `y' if group == 4, gen(FY10C) equal
    
    keep if group != .
    keep `y' FY* group
    sort `y'
    collapse (firstnm) FY* group, by(`y')
    
    summ `y' if inlist(group, 1, 2, 4)
    local startR = -1
    local endR = 1 + r(max)
    
    summ `y' if inlist(group, 1, 2)
    local start0 = 0
    local end0 = r(max)
    
    summ `y' if inlist(group, 4)
    local start1 = 0
    local end1 = r(max)
    
    global Inf = 9.99e9
    
    preserve
    local step = 0.01
    local R_size = round((`endR' - `startR') / `step') + 1
    local total_obs = `R_size' + 2
    
    clear
    set obs `total_obs'
    
    range seq `startR' `endR' `R_size'
    replace seq = round(seq, 0.01)
    gen `y' = seq[_n-1]
    replace `y' = -${Inf} in 1
    replace `y' = ${Inf} in `total_obs'
    sort `y'
    drop seq
    
    gen R = 1
    gen Ysupp0 = ((`y' >= `start0' & `y' <= `end0') | inlist(_n, 1, _N))
    gen Ysupp1 = ((`y' >= `start1' & `y' <= `end1') | inlist(_n, 1, _N))
    
    tempfile supports
    save `supports'
    restore
    
    merge m:1 `y' using `supports', keepusing(R Ysupp0 Ysupp1) nogen
    sort `y'
    
    
   * Replace missing values in FY* functions
    replace FY00C = FY00C[_n-1] if FY00C == .
    replace FY10C = FY10C[_n-1] if FY10C == .
    replace FY00T = FY00T[_n-1] if FY00T == .
    replace FY11T = FY11T[_n-1] if FY11T == .
    
    gen FY00CR = FY00C if R == 1
    gen FY10CR = FY10C if R == 1
    gen FY00TR = FY00T if R == 1
    gen FY11TR = FY11T if R == 1
    
    keep if R == 1
    keep `y' R Ysupp0 Ysupp1 FY00CR FY10CR FY00TR FY11TR

    replace FY00CR = 0 if FY00CR == .
    replace FY10CR = 0 if FY10CR == .
    replace FY00TR = 0 if FY00TR == .
    replace FY11TR = 0 if FY11TR == .
    
    gen FY10TUBY = .

    mata: FY10TUBY = calculate_FY10TUBY("`y'")
    mata: st_store(., "FY10TUBY", FY10TUBY)
    
    g FY10TUBCS = .
    
    mata: FY10TUBCS = calculate_FY10TUBCS("`y'")
    mata: st_store(., "FY10TUBCS", FY10TUBCS)
        
    g FY10TCiC = FY10TUBCS

end

*** Mata Functions 

mata:
real scalar qminus(real scalar q, real vector FY, real vector y, real vector supp) {
    cond = (FY :>= q) :& (supp :== 1)
    if (sum(cond) > 0) {
        return(min(select(y, cond)))
    }
    return(.)
}
end

mata:

    real vector calculate_FY10TUBY(string y) {

        y_data = st_data(., y)

	FY00CR = st_data(., "FY00CR")
        FY10CR = st_data(., "FY10CR")
	R      = st_data(., "R")
        Ysupp1 = st_data(., "Ysupp1")
        FY00TR = st_data(., "FY00TR")

        FY10TUBY = J(rows(FY10CR), 1, .)
        
        for (i = 1; i <= rows(FY10CR); i++) {  
            min_y = qminus(FY10CR[i], FY00CR, y_data, R)
            
            FY10TUBY[i] = select(FY00TR, (y_data :== min_y))
        }

        return(FY10TUBY)
    }
end

/*
One solution might be to put  everything in the loop 
in the if (Ysupp1[n] == 1) {}
*/

mata:

    real vector calculate_FY10TUBCS(string y) {
   
    R        = st_data(., "R")  
    FY10TUBY = st_data(., "FY10TUBY")
    y_data   = st_data(.,y)       
    Ysupp1   = st_data(., "Ysupp1")  

    FY10TUBCS = J(rows(y_data), 1, .)  // Initialize FY10TUBCS vector
    
    for (i = 1; i <= rows(y_data); i++) {
        cond = (Ysupp1 :== 1) :& (y_data :<= y_data[i])
        
        if (sum(cond) > 0) {
            maximum = max(select(FY10TUBY, cond))
            FY10TUBCS[i] = maximum
        }
    }
    
    return(FY10TUBCS)
    }
end

