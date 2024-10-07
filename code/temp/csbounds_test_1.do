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

*** functions 

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

/*
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

keep wage0 treatment period		
			 
save "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input.dta", replace
*/

timer clear 
timer on 1

use "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input.dta", clear

rename wage0 y

which csbounds

csbounds wage0, treatment(treatment) period(period)

timer off 1
timer list 

save "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input_with_dists.dta", replace

*****************************
*** calculating lower bounds 
*****************************

/*
mata: mata clear

mata:
real scalar qplus(real scalar q, real vector FY, real vector y, real vector supp) {
    cond = (FY :<= q) :& (supp :== 1)
    if (sum(cond) > 0) {
        return(max(select(y, cond)))
    }
    return(.)
}
end

mata:
	real vector calculate_FY10TLBY(string y) {

        y_data = st_data(., y)

	FY00CR = st_data(., "FY00CR")
        FY10CR = st_data(., "FY10CR")
	R      = st_data(., "R")
        Ysupp1 = st_data(., "Ysupp1")
        FY00TR = st_data(., "FY00TR")

        FY10TLBY = J(rows(FY10CR), 1, .)
        
        for (i = 1; i <= rows(FY10CR); i++) {  
            max_y = qplus(FY10CR[i], FY00CR, y_data, R)
            FY10TLBY[i] = select(FY00TR, (y_data :== max_y))
		}

        return(FY10TLBY)
    }
end

// FY10TLBY[1,i]  = FY00T(qplus(FY10C(Ysupp1noInf[i]),FY00CR,R))  


gen FY10TLBY = .

local y = "wage0"
mata: FY10TLBY = calculate_FY10TLBY("`y'")

mata: st_store(., "FY10TLBY", FY10TLBY)
*/

*****************************
*** calculating Q 
*****************************

use "${input_path}/build/CPS_cleaned_merged_yearly_10_to_15_input_with_dists.dta", clear


mata:
real scalar qminus(real scalar q, real vector FY, real vector y, real vector supp) {
    cond = (FY :>= q) :& (supp :== 1)
    if (sum(cond) > 0) {
        return(min(select(y, cond)))
    }
    return(.)
}
end



//QY10TCSLB[1,i]=qminus(p[i],FY10TUBCS,R)
mata:

    y_data = st_data(., "wage0")

    FY10TUBCS = st_data(., "FY10TUBCS")
    FY10CR = st_data(., "FY10CR")
    R      = st_data(., "R")
    Ysupp1 = st_data(., "Ysupp1")
    FY00TR = st_data(., "FY00TR")

    step   = 0.001
    startp = 0
    endp   = 1

    p_size = (endp - startp) / step + 1
    p = startp :+ (0::(p_size - 1)) * step

    p = round(p, 0.001)

    QY10TCSLB = J(rows(p), 1, .)
  	
    for (i = 1; i <= 100; i++) {  
        QY10TCSLB[i] = qminus(p[i], FY10TUBCS, y_data, R)
        QY10TCSLB[i] = round(QY10TCSLB[i], 0.01) 
	// we had some wiered decimals 
        }

}
// is there a way to save this as a vector and keep working on that?
end


// finding the att with quantiles

mata:

    y_data = st_data(., "wage0")
    R      = st_data(., "R") 
    FY10TUBCS = st_data(., "FY10TUBCS")
    FY10CR = st_data(., "FY10CR")
    FY11TR = st_data(., "FY11TR")
    FY00TR = st_data(., "FY00TR")
    FY00CR = st_data(., "FY00CR")

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
       QY11T[i] = qminus(p[i], FY11TR, y_data, R)
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

   -y_data[1]
    vec = J(rows(y_data), 1, .)
    for (i = 1; i <= rows(y_data); i++) {
    cond = (y_data[i] :<  -y_data[1]) :& (y_data[i] :> y_data[1]) :& (FY10TDDID[i] :<= 0.01)
    if(sum(cond)>0){
        vec[i] = round(y_data[i],0.01) // later you should define it as R 
        }
    }
    sum(vec) //  1640.07 in R .. the difference is 4.8! 

end




