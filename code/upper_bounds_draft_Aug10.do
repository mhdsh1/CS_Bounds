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






