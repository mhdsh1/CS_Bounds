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



use "${output_path}/y", clear


merge 1:m wage0 using "${output_path}/ecdf",  keepusing(FY*) nogen

merge 1:m wage0 using "${output_path}/mecdf", keepusing(mFY*) nogen

rename wage0 y

replace FY00C = FY00C[_n-1] if FY00C == .
replace FY10C = FY10C[_n-1] if FY10C == .
replace FY00T = FY00T[_n-1] if FY00T == .
replace FY11T = FY11T[_n-1] if FY11T == .

replace mFY00T = mFY00T[_n-1] if mFY00T == .
replace mFY00C = mFY00C[_n-1] if mFY00C == .


replace FY00C = 0 if FY00C == .
replace FY10C = 0 if FY10C == .
replace FY00T = 0 if FY00T == .
replace FY11T = 0 if FY11T == .

replace mFY00T = 0 if mFY00T == .
replace mFY00C = 0 if mFY00C == .

rename FY00T FY00TR 
rename FY00C FY00CR 
rename FY11T FY11TR 
rename FY10C FY10CR

/*
input y       Ysupp0 Ysupp1 R FY00T FY00C FY11T FY10C ymin      FY10TUBY FY10TUBCS
     -9.99e30 1      1      1 0     0     0     0     -9.99e30  0        0
end
*/


/*
qminus<-function(q,FY,y){
  #y and FY are vectors of the same length where FY is the value of the cdf at
  # the corresponding y value 
  # no longer removing -Inf since it is computed on R
  qminus<-min(y[FY>=q])
}

qplus<-function(q,FY,y){
  #y and FY are vectors of the same length where FY is the value of the cdf at
  # the corresponding y value
 y  = y[-c(length(y))]
 FY = FY[-c(length(y))]
 qplus<-max(y[FY<=q])
}
*/

//g cond = FY00C >= FY10C[88807]

//egen ymin = min(cond(FY00C >= FY10C[88807], y, .)) if FY00C >= FY10C[88807] 
//replace ymin = minimum if FY00C >= FY10C[`i']

/*
keep if Ysupp1 == 1

gen ymin = .

forval n = 1/`=_N' {
egen minimum = min(cond(FY00C >= FY10C[`n'], y, .))  
replace ymin = minimum if FY00C >= FY10C[`n']
drop minimum
}
*/

gen ymin = .

/* 
qui su n if Ysupp1 == 1

forval n = `r(min)'/`r(max)' {
di `n'
egen minimum = min(cond(FY00C >= FY10C[`n'], y, .))  
replace ymin = minimum in `n'
drop minimum
}
*/

// this loops run very slow i use the mata program instead 


mata:
    FY00C  = st_data(., "FY00C")
    FY10C  = st_data(., "FY10C")
    y      = st_data(., "y")
    Ysupp1 = st_data(., "Ysupp1")
    ymin   = J(rows(FY00C), 1, .)  
    
    for (n = 1; n <= rows(FY10C); n++) {
        if (Ysupp1[n] == 1) {
            cond = (FY00C :>= FY10C[n]) :& (Ysupp1 :== 1)
            
            if (sum(cond) > 0) {
                min_y = min(select(y, cond))
                ymin[n] = min_y
            }
        }
    }

    st_store(., "ymin", ymin)
end

keep if R == 1


* ymin generated 

preserve
keep ymin
duplicates drop
drop if ymin == .
sort ymin
rename ymin y
tempfile ymin_data
save "${output_path}/ymin", replace
restore

preserve
collapse (first) FY00T, by(y)
tempfile FY00T
save `FY00T'
restore

preserve
use "${output_path}/ymin", clear
merge m:1 y using `FY00T', keepusing(FY00T)
keep if _merge == 3
drop _merge
rename y ymin
rename FY00T FY10TUBY
tempfile ymin_FY00T
save "${output_path}/ymin_FY00T", replace
restore

 
merge m:m ymin using "${output_path}/ymin_FY00T", nogen keepusing(FY10TUBY) 

* FY10TUBY generated

* Next step: FY10TUBCS 

/*
Finds the maximum value in FY10TUBY where Ysupp1noInf is less than or equal to the corresponding value in R.
Stores this maximum value in the FY10TUBCS matrix or vector at the appropriate position.
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


* Plotting 

*  Obs denotes FY11|D=1. 
* CF-LB/CF-UB denote the CS LB/UB on FY10|D=1


twoway (line FY10TUBCS y if y < 50)

twoway (line FY10TUBCS y if y < 50, lcolor(blue)) || ///
       (line FY11TR    y if y < 50, lcolor(black) )

/*
g eye = _n + 101

browse

forval n = 1/`=_N' {
    egen maximum = max(cond(FY00C >= FY10C[`n'] , FY10TUBY, .))
   
    replace ymin = minimum in `n'
     drop minimum
}


browse


FY10TUBY[1,i]= FY00T(qminus(FY10C(Ysupp1noInf[i]),FY00CR,R))
	
*** min(y[ FY00CR >= FY10C(Ysupp1noInf[i]) ])

browse

gen ymin = FY10C[_n] if Ysupp1 == 1

egen ymin = min(y) if FY00C >= FY10C[_n] & Ysupp1 == 1


egen ymin = min(cond(FY00C >= FY10C[_n], y, .))


g a = FY10C[_n-1]

sort y
g n = _n if Ysupp1 == 1

forvalues i = 1/`=_N' {

local a = FY10C[i]

egen ymin = min(y) if FY00CR > `a'

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
*/
