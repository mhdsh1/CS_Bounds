preserve
sort  Y00Cn
//cumul Y00Tn, gen(FY00T) equal
//bys Y00Tn: gen n = _n
//keep if n == 1 & Y00Tn != .
keep Y00Cn FY00C
rename Y00Cn R
tempfile Y00C
save "${output_path}/Y00C"
restore



merge m:1 R using `Y00C', keepusing(FY00C) nogen


preserve
sort  Y00Cn
//cumul Y00Tn, gen(FY00T) equal
//bys Y00Tn: gen n = _n
//keep if n == 1 & Y00Tn != .
keep Y00Cn FY00C
rename Y00Cn R
tempfile Y00C
save "${output_path}/Y00C"
restore




preserve
sort  Y00Tn
//cumul Y00Tn, gen(FY00T) equal
//bys Y00Tn: gen n = _n
//keep if n == 1 & Y00Tn != .
keep Y00Tn FY00T
rename Y00Tn R
tempfile Y00T
save "${output_path}/Y00T"
restore


preserve
sort  Y10Cn
//cumul Y00Tn, gen(FY00T) equal
//bys Y00Tn: gen n = _n
//keep if n == 1 & Y00Tn != .
keep Y10Cn FY10C
rename Y10Cn R
tempfile Y10C
save "${output_path}/Y10C"
restore


preserve
sort  Y11Tn
//cumul Y00Tn, gen(FY00T) equal
//bys Y00Tn: gen n = _n
//keep if n == 1 & Y00Tn != .
keep Y11Tn FY11T
rename Y11Tn R
tempfile Y11T
save "${output_path}/Y11T"
restore

merge m:1 R using `Y00C', keepusing(FY00C) nogen




restore
generate R = .

egen Rmax = rowmax(Y00Cn Y00Tn Y10Cn)
qui summ Rmax

local step  = 0.01
local start = -1
local end   = 1+r(max)
local n     = round((`end' - `start')/`step') + 1
di `n'

range z `start' `end' `n'
replace z = round(z, 0.01)

local i = 1
forval x = `=round(`start'/`step', 0)'/1000(1)`=round(`end'/`step', 0)'/1000 {
    quietly replace R = `x'*`step' in `= `i' + 1'
    local i = `i' + 1
}

replace R = . in `=_N'
generate R_inf = Inf
replace R = R_inf in `=_N'
drop R_inf

mata: st_matrix("x", range(0,10,1))


/*

egen Rmax = rowmax(Y00Cn Y00Tn Y10Cn)
qui summ Rmax
local Rmax = 1+r(max)

gen seq = -1

local step_size = 0.01

forvalues i = 2/10000 {
    local new_value = -1 + `i' * `step_size'
    if `new_value' > `Rmax' {
        break
    }
    replace seq = `new_value' in `i'
    expand 1 if _n == _N
}


scalar max_value = max(Y00Cn, Y00Tn, Y10Cn)

egen Rmax = rowmax(Y00Cn Y00Tn Y10Cn)
qui summ Rmax
local Rmax = 1+r(max)


local step = 0.01
local start = -1
local end = 1 + max_value

clear
set obs 1
generate R = .

forval i = `=round(`start'/`step', 0)'/1000(1)`=round(`end'/`step', 0)'/1000 {
    quietly replace R = `i'*`step' in `=_N+1'
    addobs 1
}

replace R = . in `=_N'
generate R_inf = Inf
replace R = R_inf in `=_N'
drop R_inf



/*
bys Y00Cn: gen n00Cn = _n
bys Y10Cn: gen n10Cn = _n
bys Y00Tn: gen n00Tn = _n
bys Y11Tn: gen n11Tn = _n

keep if n00Cn == 1 | n10Cn == 1 | n00Tn == 1 | n11Tn == 1
*/

/*
preserve
sort  Y00Cn
cumul Y00Cn, gen(FY00C) equal
bys Y00Cn: gen n = _n
keep if n == 1 & Y00Cn != .
keep Y00Cn FY00C
tempfile Y00C
save `Y00C'
restore

preserve
sort  Y10Cn
cumul Y10Cn, gen(FY10C) equal
bys Y10Cn: gen n = _n
keep if n == 1 & Y10Cn != .
keep Y10Cn FY10C
tempfile Y10C
save `Y10C'
restore

preserve
sort  Y00Tn
cumul Y00Tn, gen(FY00T) equal
bys Y00Tn: gen n = _n
keep if n == 1 & Y00Tn != .
keep Y00Tn FY00T
tempfile Y00T
save `Y00T'
restore

preserve
sort  Y11Tn
cumul Y11Tn, gen(FY11T) equal
bys Y11Tn: gen n = _n
keep if n == 1 & Y11Tn != .
keep Y11Tn FY11T
tempfile Y11T
save `Y11T'
restore

use `Y00C', clear
append using `Y10C'
append using `Y00T'
append using `Y11T'




/********************

sort  Y00Cn
cumul Y00Cn, gen(FY00C) equal

sort  Y10Cn
cumul Y10Cn, gen(FY10C) equal

sort  Y00Tn
cumul Y00Tn, gen(FY00T) equal

sort  Y11Tn
cumul Y11Tn, gen(FY11T) equal

preserve
sort Y00Cn
sort Y10Cn
sort Y00Tn
sort Y11Tn
keep Y* year 
gen id = _n  
reshape long Y, i(id) j(year)
keep Y
save "${output_path}/y2.dta", clear
restore




egen Rmax = rowmax(Y00Cn Y00Tn Y10Cn)
qui summ Rmax
local start        = -1
local end          = 1+r(max)
local step         = 0.01
local steps_number = round((`end' - `start')/`step') + 1
range   R `start' `end' `steps_numbnr'
replace R = round(R, 0.01)



preserve
egen Rmax = rowmax(Y00Cn Y00Tn Y10Cn)
qui summ Rmax
local start        = -1
local end          = 1+r(max)
local step         = 0.01
local steps_number = round((`end' - `start')/`step') + 1
range   R `start' `end' `steps_numbnr'
replace R = round(R, 0.01)
rename R y
keep y
tempfile R_data
save `R_data'
restore

preserve
rename Y00Cn y
keep y FY00C
tempfile y1_data
keep if y != . 
sort y 
duplicates drop
tempfile Y00C
save "${output_path}/Y00C", replace
restore 

preserve
rename Y10Cn y
keep y FY10C
keep if y != . 
sort y 
duplicates drop
tempfile Y10C
save "${output_path}/Y10C", replace
restore 

preserve
rename Y00Tn y
keep   y FY00T
keep if y != . 
sort y 
duplicates drop
tempfile Y00T
save "${output_path}/Y00T", replace
restore 

preserve
rename Y11Tn y
keep y FY11T
keep if y != . 
sort y 
duplicates drop
tempfile Y11T
save "${output_path}/Y11T", replace 
restore

preserve
use `R_data', clear
append using `y1_data', keep(y FY00C)
append using `y2_data', keep(y FY10C)
append using `y3_data', keep(y FY00T)
append using `y4_data', keep(y FY11T)
save "${output_path}/test2", replace 
restore



preserve 

use y, clear 




/*
preserve
summarize Y00Cn, meanonly
local min = r(min)
local max = r(max)

local step = 0.01
range R `min' `max' `=round((`max' - `min')/`step')'

tempfile seq_data
save `seq_data'

restore

preserve 
sort  Y00Cn
keep if Y00Cn != .
keep Y00Cn FY00C
tempfile original_data
save `original_data'
restore

use `seq_data', clear
joinby R using `original_data'

bysort R (Y00Cn): gen FY00C_new = FY00C[_n-1] if Y00Cn <= R
bysort R: replace FY00C_new = FY00C_new[_n-1] if missing(FY00C_new)






*******************
* Define the empirical cdfs
****************************

sort  Y00Cn
summarize Y00Cn, meanonly
local min = r(min)
local max = r(max)
local step = 0.01
range RY00Cn `min' `max' `=round((`max' - `min')/`step')'

tempfile seq_data
save `seq_data'

restore

sort Y00Cn
cumul Y00Cn, gen(FY00C) equal

// Step 5: Merge the sequence with the original data
merge m:1 R using `seq_data', keepusing(FY00C) nogen

// Step 6: Carry forward the last observed CDF value for the gaps (step function behavior)
bysort R: replace FY00C = FY00C[_n-1] if missing(FY00C)


cumul Y00Cn, gen(FY00C) equal




sort  Y10Cn
cumul Y10Cn, gen(FY10C) equal

sort  Y00Tn
cumul Y00Tn, gen(FY00T) equal

sort  Y11Tn
cumul Y11Tn, gen(FY11T) equal


local suffixes 00C 10C 00T 11T

foreach suf in `suffixes' {
    preserve
    sort Y`suf'n
    keep Y`suf'n 
    keep if Y`suf'n != .
    tempfile `suf'
    save ``suf''
    restore
}

preserve
use `00C', clear
append using `10C'
append using `00T'
append using `11T'

save "${output_path}/test", replace
restore

egen Rmax = rowmax(Y00Cn Y00Tn Y10Cn)
qui summ Rmax

local start        = -1
local end          = 1+r(max)
local step         = 0.01
local steps_number = round((`end' - `start')/`step') + 1


//range   R `start' `end' `steps_numbnr'
//replace R = round(z, 0.01)


local suffixes 00C 10C 00T 11T

foreach suf in `suffixes' {
    preserve
    sort Y`suf'n
    keep Y`suf'n FY`suf'
    keep if Y`suf'n != .
    range   R `start' `end' `steps_numbnr'
    replace R = round(R, 0.01)
    tempfile `suf'
    save "${output_path}/`suf'", replace
    restore
}


local suffixes 00C 10C 00T 11T

foreach suf in `suffixes' {

    merge m:1 R using "`temp_`suf''", keepusing(FY`suf') nogen
}



