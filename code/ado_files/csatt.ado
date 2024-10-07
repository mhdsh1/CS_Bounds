program csatt, rclass
    version 14.0

    syntax [anything]
    
    local u = real("`anything'")
	
	if `u' == . local u = 1
	
mata:calculate_att(`u')

matrix colnames result = "FY11" "CSLB" "CSUB" "DiD" "DistDiD" 
matrix list result

end

mata:
real matrix calculate_att(real scalar u) {
    
    Rgrid = 0.01
    
    EY11T = mean(st_data(., "EY11T")) 
    EY00T = mean(st_data(., "EY00T"))
	EY10C = mean(st_data(., "EY10C"))
	EY00C = mean(st_data(., "EY00C"))

    R      = round(st_data(., "R"),      Rgrid) 
    Rm     = round(st_data(., "Rm"),     Rgrid)
    Ysupp1 = round(st_data(., "Ysupp1"), Rgrid)
    
    maxR = 2*round(max(R[1..rows(R)-1]), Rgrid)

    FY11TR  = st_data(., "FY11T")
    FY10TDDID = st_data(., "FY10TDDID")
    
    FY10TUBCS = st_data(., "FY10TUBCS")	
    FY10TLBCS = st_data(., "FY10TLBCS") 

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
    QY10TDDiD = J(rows(p), 1, .)

    for (i = 1; i <= rows(p); i++) {  
    QY11T[i]     = round(qminus(p[i], FY11TR,    R), Rgrid)
    QY10TCSLB[i] = round(qminus(p[i], FY10TUBCS, R), Rgrid)
    QY10TCSUB[i] = round(qminus(p[i], FY10TLBCS, R), Rgrid)
    QY10TDDiD[i] = round(qminus(p[i], FY10TDDID, R), Rgrid)
        }

    // Table 2 Results //
	
    EY10TCSLB = 0 
    EY10TCSUB = 0 
	EY10TDiD  = 0 
    EY10TDDiD = 0 
   
    for (i = 1; i <= rows(p); i++) {
        cond = (QY10TCSLB[i] :<= maxR) :& 
	       (QY10TCSLB[i] :>= min(Ysupp1[2..rows(Ysupp1)])) 
        if (sum(cond) > 0) {
	        EY10TCSLB = EY10TCSLB + QY10TCSLB[i]*pgrid	 
            }
        }
	
    for (i = 1; i <= rows(p); i++) {
        cond = (QY10TCSUB[i] :<= maxR) :& 
	       (QY10TCSUB[i] :>= min(Ysupp1[2..rows(Ysupp1)])) 
        if (sum(cond) > 0) {  
		    EY10TCSUB = EY10TCSUB + QY10TCSUB[i]*pgrid	 
            }
        }		

     EY10TDiD  = EY00T+EY10C-EY00C
	
     for (i = 1; i <= rows(p); i++) {
        cond = (QY10TDDiD[i] :<= maxR) :& 
	       (QY10TDDiD[i] :>= min(Ysupp1[2..rows(Ysupp1)])) 
        if (sum(cond) > 0) {
            EY10TDDiD = EY10TDDiD + QY10TDDiD[i]*pgrid	 
            }
        }

    // Table 3 Results //

    EQY11Tu     = 0
    EQY10TCSLBu = 0    
    EQY10TCSUBu = 0
    EQY10TDDiDu = 0

    for (i = 1; i <= rows(p); i++) {
        cond = (p[i] :<= u) :& (p[i] :> 0) 
        if (sum(cond) > 0) {
            EQY11Tu     = EQY11Tu     + QY11T[i]    *pgrid/u 
            EQY10TCSLBu = EQY10TCSLBu + QY10TCSLB[i]*pgrid/u 
            EQY10TCSUBu = EQY10TCSUBu + QY10TCSUB[i]*pgrid/u
            EQY10TDDiDu = EQY10TDDiDu + QY10TDDiD[i]*pgrid/u
		}
    }

   // Outputs 
   if (u == 1) {
    FY11     = round(EY11T, 0.01) 
    CSLB    = round(EY11T - EY10TCSUB, 0.01)
    CSUB    = round(EY11T - EY10TCSLB, 0.01)
    DiD     = round(EY11T - EY10TDiD,  0.01)
    DistDiD = round(EY11T - EY10TDDiD, 0.01)
	}
	else{
    FY11     = round(EQY11Tu, 0.01)
    CSLB    = round(EQY11Tu - EQY10TCSUBu, 0.01)
    CSUB    = round(EQY11Tu - EQY10TCSLBu, 0.01)
	DiD     = .
    DistDiD = round(EQY11Tu - EQY10TDDiDu, 0.01)
}

    real matrix result
    result = (FY11, CSLB, CSUB, DiD, DistDiD)
    st_matrix("result", result)

    return(.)
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
