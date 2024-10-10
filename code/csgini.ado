program csgini, rclass
    version 14.0

    syntax [anything]
    
    local u = real("`anything'")
	
	if `u' == . local u = 1
	
mata:calculate_gini(`u')

matrix colnames result = "FY11" "CSLB" "CSUB" "DistDiD"
matrix list result

end

mata:
real matrix calculate_gini(real scalar u) {
    
    Rgrid = 0.01

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

    G11T     = 0
    G10TCSLB = 0
    G10TCSUB = 0  
    G10TDDiD = 0
	
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
        cond = (QY10TDDiD[i] :<= maxR) :& 
		       (QY10TDDiD[i] :>= min(Ysupp1[2..rows(Ysupp1)])) 
        if (sum(cond) > 0) {
		    G10TDDiD = G10TDDiD + 2*(1-p[i])*QY10TDDiD[i]*pgrid
            }
        }

    // Table 3 Results //
	
    GQY11Tu     = 0
    GQY10TCSLBu = 0   
    GQY10TCSUBu = 0
    GQY10TDDiDu = 0
	
	for (i = 1; i <= rows(p); i++) {
        cond = (p[i] :<= u) :& (p[i] :>  0) 
        if (sum(cond) > 0) {
            GQY11Tu     = GQY11Tu     + 2*(u-p[i])*QY11T[i]    *pgrid/(u^2)
		    GQY10TCSLBu = GQY10TCSLBu + 2*(u-p[i])*QY10TCSLB[i]*pgrid/(u^2)
		    GQY10TCSUBu = GQY10TCSUBu + 2*(u-p[i])*QY10TCSUB[i]*pgrid/(u^2)
		    GQY10TDDiDu = GQY10TDDiDu + 2*(u-p[i])*QY10TDDiD[i]*pgrid/(u^2)
            }
        }

    // Outputs 
if (u == 1) {
    FY11    = round(G11T, 0.01)
    CSLB    = round(G11T - G10TCSUB, 0.01)
    CSUB    = round(G11T - G10TCSLB, 0.01) 
    DistDiD = round(G11T - G10TDDiD, 0.01)
}
else{

    FY11    = round(GQY11Tu, 0.01)
    CSLB    = round(GQY11Tu-GQY10TCSUBu, 0.01)
    CSUB    = round(GQY11Tu-GQY10TCSLBu, 0.01)
    DistDiD = round(GQY11Tu-GQY10TDDiDu, 0.01)
}

    real matrix result
    result = (FY11, CSLB, CSUB, DistDiD)
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
