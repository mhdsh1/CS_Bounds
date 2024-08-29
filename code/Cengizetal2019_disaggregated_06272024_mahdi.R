############################################################################
# Author:      
# Start:     2024-08-07
# Last Edit: 2024-08-20
# Des: 
# this version uses the quantile of -Y and the left-limit definition 
# in terms of the cdf of -Y 
############################################################################

##########
#  Setup
##########

rm(list=ls())
#install.packages("dplyr")
library(dplyr)
#install.packages("dineq")
#library(dineq)
#library(stargazer)

# Set seed
set.seed(123456)

user <- "mahdi"

# Current working folder 
folder <- "CS_Bounds"

# Directory to Dropbox folder, which depends on machine type:
os <- .Platform$OS.type
if (os == "windows") {
  path <- paste0("C:/Users/", user, "/Dropbox/", folder, "/")
} else if (os == "unix") {
  path <- paste0("/home/", user, "/Dropbox/", folder, "/")
} else {
  path <- paste0("/Users/", user, "/Dropbox/", folder, "/")
}

input_path  <- paste0(path, "data/")
output_path <- paste0(path, "output/")

############
# Functions 
############

qminus<-function(q,FY,y){
  #y and FY are vectors of the same length where FY is the value of the cdf at
  # the corresponding y value 
  # no longer removing -Inf since it is computed on R
  qminus<-min(y[FY>=q])
}

qplus<-function(q,FY,y){
  #y and FY are vectors of the same length where FY is the value of the cdf at
  # the corresponding y value
  # MahdiL why we remve the +Inf at the end? 
  y  = y[-c(length(y))]
  FY = FY[-c(length(y))]
  qplus<-max(y[FY<=q])
}

###############
# Reading Data
###############

data<-read.csv(paste0(input_path,"build/CPS_cleaned_merged_yearly_10_to_15.csv"))

data$wage0<-data$wage/100

#[!is.na(data$statenum),]
#set wage to zero if NA
#data$wage0[is.na(data$wage0)]=0

data$state_mw2010 = data$state_mw
data$state_mw2015 = data$state_mw

states=na.omit(unique(data$statenum))
for (i in states){
  for (j in 1:12){
    data$state_mw2010[data$statenum==i&data$month==j] = 
      mean(data$state_mw[which(data$statenum==i&data$month==j&data$year==2010)])
    # mean     median   sd         min   max
    # 7.47525   7.25    0.3848248  7.25  8.55
    
    data$state_mw2015[data$statenum==i&data$month==j] = 
      mean(data$state_mw[which(data$statenum==i&data$month==j&data$year==2015)])
    # mean       median  sd         min   max
    # 8.046051   8.05    0.7854732  7.25  10.5
  }
}

data$smw_increase1015 = (data$state_mw2015-data$state_mw2010 > 0.25)
# mean       median  sd         min max
# 0.5403855  1       0.4983666  0   1

data2010 = data[data$year==2010,]
data2015 = data[data$year==2015,]

##################################
# Defining the outcomes Tn and Cn
##################################

# this makes the second tow in Table 1 in the paper
#upremw does not rule out any states on the top in the sample

premw     = 8 # state minimum wage filtering.
upremw    = Inf

preperiod = 2010
Tperiod   = 2015

data2010subsample=data2010[data2010$state_mw2010>=premw&
                             data2010$state_mw2010<upremw,]
summary_stats_pre<-data2010subsample%>%group_by(smw_increase1015)%>%
  summarise(mean.pre = mean(wage0, na.rm=TRUE),
            var.pre  = var(wage0, na.rm=TRUE),
            n.pre    = sum(!is.na(wage0)))

data2015subsample=data2015[data2015$state_mw2010>=premw&
                             data2015$state_mw2010<upremw,]
summary_stats_post<-data2015subsample%>%group_by(smw_increase1015)%>%
  summarise(mean.post = mean(wage0, na.rm=TRUE),
            var.post  = var(wage0, na.rm=TRUE),
            n.post    = sum(!is.na(wage0)))

Y00Tn<-na.omit(data2010$wage0[data2010$smw_increase1015==1&
                                data2010$state_mw2010>=premw&
                                data2010$state_mw2010<upremw])

Y00Cn<-na.omit(data2010$wage0[data2010$smw_increase1015==0&
                                data2010$state_mw2010>=premw&
                                data2010$state_mw2010<upremw])

Y11Tn<-na.omit(data2015$wage0[data2015$smw_increase1015==1&
                                data2015$state_mw2010>=premw&
                                data2015$state_mw2010<upremw])

Y10Cn<-na.omit(data2015$wage0[data2015$smw_increase1015==0&
                                data2015$state_mw2010>=premw&
                                data2015$state_mw2010<upremw])

### Summary Statistics (Table 1 in the paper row 2)

# summary_stats<-cbind(summary_stats_pre,summary_stats_post)

# write.csv(summary_stats, 
#          paste0(output_path, "summarystats_wages_disaggregated06272024_06272024_",
#                 preperiod,"-",Tperiod,"_",premw,".csv"))

###################
### Empirical CDFs
###################

FY00C<-ecdf(Y00Cn)
FY10C<-ecdf(Y10Cn)
FY00T<-ecdf(Y00Tn)
FY11T<-ecdf(Y11Tn)

### Defining Supports 
Ysupp0 = c(-Inf, seq(0  ,         max(Y00Cn,Y00Tn),0.01), Inf)
Ysupp1 = c(-Inf, seq(0  ,               max(Y10Cn),0.01), Inf)
R      = c(-Inf, seq(-1 , 1+max(Y00Cn,Y00Tn,Y10Cn),0.01), Inf)


FY00Cs = matrix(,nrow=1,ncol=length(Ysupp0))
FY10Cs = matrix(,nrow=1,ncol=length(Ysupp1))
FY00Ts = matrix(,nrow=,ncol=length(Ysupp0))

for (i in 1:length(Ysupp0)){
  
  FY00Cs[1,i]=FY00C(Ysupp0[i])
  
  FY00Ts[1,i]=FY00T(Ysupp0[i])
}

for (i in 1:length(Ysupp1)){
  FY10Cs[1,i]=FY10C(Ysupp1[i])
}

FY00CR=matrix(,nrow=1,ncol=length(R))
FY10CR=matrix(,nrow=1,ncol=length(R))
FY00TR=matrix(,nrow=1,ncol=length(R))
FY11TR=matrix(,nrow=1,ncol=length(R))
for (i in 1:length(R)){
  FY00CR[1,i]=FY00C(R[i])
  FY10CR[1,i]=FY10C(R[i])
  FY00TR[1,i]=FY00T(R[i])
  FY11TR[1,i]=FY11T(R[i])
}

# Distributional DiD

FY10TDDID = FY00TR + FY10CR - FY00CR # Dist DiD assumption Slide 24/29

#########################################
#### plotting cdfs for Y00C, Y10C, Y00T
#########################################

mY00Tn = -Y00Tn 
mY00Cn = -Y00Cn

FmY00T<-ecdf(mY00Tn)
FmY00C<-ecdf(mY00Cn)

FmY00CR = matrix(,nrow=1,ncol=length(R))

Rm=c(-Inf, seq(-1-max(Y00Cn,Y00Tn,Y10Cn),1,0.01),Inf)

for (i in seq(1,length(Rm),1)){
  FmY00CR[1,i]=FmY00C(Rm[i])
}

##################################
# computing CS bounds on supports
##################################

Ysupp1noInf = Ysupp1[-c(length(Ysupp1))]
FY10TLBY    = matrix(,nrow=1,ncol=length(Ysupp1noInf))
FY10TLBmY   = matrix(,nrow=1,ncol=length(Ysupp1noInf))
FY10TUBY    = matrix(,nrow=1,ncol=length(Ysupp1noInf))
FY11TY      = matrix(,nrow=1,ncol=length(Ysupp1noInf))


for (i in 1:length(Ysupp1noInf)){
  FY10TLBY[1,i]  = FY00T(qplus(FY10C(Ysupp1noInf[i]),FY00CR,R))  
  FY10TLBmY[1,i] = 1-FmY00T(qminus(1-FY10C(Ysupp1noInf[i]),FmY00CR,Rm))
  if(Ysupp1noInf[i]==-Inf){
    FY10TUBY[1,i]=0
  }else{
    FY10TUBY[1,i]= FY00T(qminus(FY10C(Ysupp1noInf[i]),FY00CR,R))}
  FY11TY[1,i]  = FY11T(Ysupp1noInf[i])
}

############################################################
# limsup transformation for right-continuity -- Slide 17/29
############################################################

FY10TLBCS  = matrix(,nrow=1,ncol=length(R))
FY10TLBCSm = matrix(,nrow=1,ncol=length(R))
FY10TUBCS  = matrix(,nrow=1,ncol=length(R))

for (i in 1:length(R)){
  FY10TLBCS [1,i] = max(FY10TLBY [,Ysupp1noInf<=R[i]])
  FY10TLBCSm[1,i] = max(FY10TLBmY[,Ysupp1noInf<=R[i]])
  FY10TUBCS [1,i] = max(FY10TUBY [,Ysupp1noInf<=R[i]])
}

#####################################
# Bounds Plots Figure 2 in the paper
#####################################

lowesty = min(min(Y10Cn),min(Y11Tn),min(Y00Cn),min(Y00Tn))

# Obs denotes FY11|D=1. 
# CF-LB/CF-UB denote the CS LB/UB on FY10|D=1


########################
# obtaining quantiles 
########################

pgrid = 0.001
p     = seq(0,1,pgrid)

qplusY00CFY10C  = matrix(,nrow=1,ncol=length(R))
qminusY00CFY10C = matrix(,nrow=1,ncol=length(R))

for (i in 1:length(R)){
  qplusY00CFY10C[1,i]  = qplus (FY10CR[i],FY00CR,R)
  qminusY00CFY10C[1,i] = qminus(FY10CR[i],FY00CR,R)
}

boundsSBS = rbind(R, FY10CR, 
                  qplusY00CFY10C, qminusY00CFY10C, 
                  FY10TLBCS, FY10TUBCS)

#write.csv(boundsSBS, paste0(
#          output_path, "bounds_computation_wages_disaggregated06272024_",Tperiod,".csv")
#          )

##########################################################
# obtain quantile of Distributional DiD, LB and UB of CS
##########################################################

QY10TDDID  = matrix(,nrow=1,ncol=length(p))
QY10TCSLB  = matrix(,nrow=1,ncol=length(p))
QY10TCSUB  = matrix(,nrow=1,ncol=length(p))
QY10T      = matrix(,nrow=1,ncol=length(p))
QY11T      = matrix(,nrow=1,ncol=length(p))

for (i in 1:length(p)){
  QY10TDDID[1,i]=qminus(p[i],FY10TDDID,R)
  #this is the LB on the quantile function which is the quantile of the UB on cdf
  QY10TCSLB[1,i]=qminus(p[i],FY10TUBCS,R)
  #this is the UB on the quantile function which is the quantile of the LB on cdf
  QY10TCSUB[1,i]=qminus(p[i],FY10TLBCS,R)

  QY11T[1,i]=qminus(p[i],FY11TR,R)
}

##########################################################
# Table 2 -- finding expectation of the quantile function
##########################################################

maxR=2*max(R[-c(length(R))])

# Mahdi: Why maxR is 2 times the maximum value in R? 
# Mahdi:  twice the maximum value in R (except the last one which is Inf)

#DiD

# Mahdi: this pdf function is crucial for the computation of the expectation of the 
# quantile function
# we make it by taking the difference of the cdf function at each point
# and later use it to compute the expectation of the quantile function

fY10TDDID = matrix(0,nrow=1,ncol=length(R))

for(i in seq(2,length(R),1)){
  fY10TDDID[1,i]= FY10TDDID[1,i] - FY10TDDID[1,i-1]
}

EY10TDDIDd = sum(        R[  R<Inf & R>-Inf ]*
                 fY10TDDID[  R<Inf & R>-Inf ]
                 )

EY11T=mean(Y11Tn)
EY00C=mean(Y00Cn)
EY10C=mean(Y10Cn)
EY00T=mean(Y00Tn)

EY10TCSLB = sum(
  QY10TCSLB[QY10TCSLB<=maxR&QY10TCSLB>=min(Ysupp1[-c(1)])])
  *pgrid

EY10TCSUB = sum(
  QY10TCSUB[QY10TCSUB<=maxR & QY10TCSUB>= min(Ysupp1[-c(1)])])
  *pgrid

EY10TDID=EY00T+EY10C-EY00C

EY10TDDID = sum(
  QY10TDDID[QY10TDDID<=maxR&QY10TDDID>=min(Ysupp1[-c(1)])])
  *pgrid

####################################
### Table 2 continued ... gini swtt 
####################################

# SW_lambda  p is tau .. you can change it tau 

# page 16 -- set u = 1 and p is tau 

# MAhdi: here we calculate the termsi n integral 
G11T     =sum(2*(1-p[QY11T<=maxR&QY11T>=min(Ysupp1[-c(1)])])*
              (QY11T[QY11T<=maxR & QY11T>=min(Ysupp1[-c(1)])])
                )*pgrid
G10TDDID =sum(2*(1-p[QY10TDDID<=maxR&QY10TDDID>=min(Ysupp1[-c(1)])])*
           QY10TDDID[QY10TDDID<=maxR&QY10TDDID>=min(Ysupp1[-c(1)])]
           )*pgrid
G10TCSLB =sum(2*(1-p[QY10TCSLB<=maxR&QY10TCSLB>=min(Ysupp1[-c(1)])])*
           QY10TCSLB[QY10TCSLB<=maxR&QY10TCSLB>=min(Ysupp1[-c(1)])]
           )*pgrid
G10TCSUB =sum(2*(1-p[QY10TCSUB<=maxR&QY10TCSUB>=min(Ysupp1[-c(1)])])*
           QY10TCSUB[QY10TCSUB<=maxR&QY10TCSUB>=min(Ysupp1[-c(1)])]
           )*pgrid

GiniY11T=1-G11T/EY11T

# Mahdi: here we subtract the terms and calculate the SWTT
GTTCSLB = G11T-G10TCSUB
MCTCSLB = (EY11T-EY10TCSUB)*(1-GiniY11T)
ICTCSLB = MCTCSLB-GTTCSUB

GTTCSUB = G11T-G10TCSLB
MCTCSUB = (EY11T-EY10TCSLB)*(1-GiniY11T)
ICTCSUB = MCTCSUB-GTTCSLB

GTTDDID = G11T-G10TDDID
MCTDDID = (EY11T-EY10TDDID)*(1-GiniY11T)   # Mean component 
ICTDDID = -(GTTDDID - MCTDDID)             # Ineq component

###############
### Table 3
###############

EQY11T01=sum(QY11T[p<=0.01&p>0])*pgrid/0.01
EQY11T025=sum(QY11T[p<=0.025&p>0])*pgrid/0.025
EQY11T05=sum(QY11T[p<=0.05&p>0])*pgrid/0.05
EQY11T10=sum(QY11T[p<=0.1&p>0])*pgrid/0.1
EQY11T25=sum(QY11T[p<=0.25&p>0])*pgrid/0.25
EQY11T50=sum(QY11T[p<=0.5&p>0])*pgrid/0.5


EQY10TDDID01d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.01]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.01])
EQY10TDDID025d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.025]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.025])
EQY10TDDID05d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.05]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.05])
EQY10TDDID10d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.1]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.1])
EQY10TDDID25d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.25]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.25])
EQY10TDDID50d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.5]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.5])


# Mahdi: the same integral as before we just change the u accodingly 

GQY11T01=sum(2*(0.01-p[p<=0.01&p>0])*QY11T[p<=0.01&p>0])*pgrid/(0.01^2)
GQY11T025=sum(2*(0.025-p[p<=0.025&p>0])*QY11T[p<=0.025&p>0])*pgrid/(0.025^2)
GQY11T05=sum(2*(0.05-p[p<=0.05&p>0])*QY11T[p<=0.05&p>0])*pgrid/(0.05^2)
GQY11T10=sum(2*(0.1-p[p<=0.1&p>0])*QY11T[p<=0.1&p>0])*pgrid/(0.1^2)
GQY11T25=sum(2*(0.25-p[p<=0.25&p>0])*QY11T[p<=0.25&p>0])*pgrid/(0.25^2)
GQY11T50=sum(2*(0.5-p[p<=0.5&p>0])*QY11T[p<=0.5&p>0])*pgrid/(0.5^2)

GiniY11T01=1-GQY11T01/EQY11T01
GiniY11T025=1-GQY11T025/EQY11T025
GiniY11T05=1-GQY11T05/EQY11T05
GiniY11T10=1-GQY11T10/EQY11T10
GiniY11T25=1-GQY11T25/EQY11T25
GiniY11T50=1-GQY11T50/EQY11T50


EQY10TDDID01=sum(QY10TDDID[p<=0.01&p>0])*pgrid/0.01
EQY10TDDID025=sum(QY10TDDID[p<=0.025&p>0])*pgrid/0.025
EQY10TDDID05=sum(QY10TDDID[p<=0.05&p>0])*pgrid/0.05
EQY10TDDID10=sum(QY10TDDID[p<=0.1&p>0])*pgrid/0.1
EQY10TDDID25=sum(QY10TDDID[p<=0.25&p>0])*pgrid/0.25
EQY10TDDID50=sum(QY10TDDID[p<=0.5&p>0])*pgrid/0.5

GQY10TDDID01=sum(2*(0.01-p[p<=0.01&p>0])*QY10TDDID[p<=0.01&p>0])*pgrid/(0.01^2)
GQY10TDDID025=sum(2*(0.025-p[p<=0.025&p>0])*QY10TDDID[p<=0.025&p>0])*pgrid/(0.025^2)
GQY10TDDID05=sum(2*(0.05-p[p<=0.05&p>0])*QY10TDDID[p<=0.05&p>0])*pgrid/(0.05^2)
GQY10TDDID10=sum(2*(0.1-p[p<=0.1&p>0])*QY10TDDID[p<=0.1&p>0])*pgrid/(0.1^2)
GQY10TDDID25=sum(2*(0.25-p[p<=0.25&p>0])*QY10TDDID[p<=0.25&p>0])*pgrid/(0.25^2)
GQY10TDDID50=sum(2*(0.5-p[p<=0.5&p>0])*QY10TDDID[p<=0.5&p>0])*pgrid/(0.5^2)

EQDDID01=EQY11T01-EQY10TDDID01
EQDDID025=EQY11T025-EQY10TDDID025
EQDDID05=EQY11T05-EQY10TDDID05
EQDDID10=EQY11T10-EQY10TDDID10
EQDDID25=EQY11T25-EQY10TDDID25
EQDDID50=EQY11T50-EQY10TDDID50

EQDDID01d=EQY11T01-EQY10TDDID01d
EQDDID025d=EQY11T025-EQY10TDDID025d
EQDDID05d=EQY11T05-EQY10TDDID05d
EQDDID10d=EQY11T10-EQY10TDDID10d
EQDDID25d=EQY11T25-EQY10TDDID25d
EQDDID50d=EQY11T50-EQY10TDDID50d

GQDDID01=GQY11T01-GQY10TDDID01
GQDDID025=GQY11T025-GQY10TDDID025
GQDDID05=GQY11T05-GQY10TDDID05
GQDDID10=GQY11T10-GQY10TDDID10
GQDDID25=GQY11T25-GQY10TDDID25
GQDDID50=GQY11T50-GQY10TDDID50


MCTDDID01=EQDDID01*(1-GiniY11T01)
ICTDDID01=-(GQDDID01-MCTDDID01)

MCTDDID025=EQDDID025*(1-GiniY11T025)
ICTDDID025=-(GQDDID025-MCTDDID025)

MCTDDID05=EQDDID05*(1-GiniY11T05)
ICTDDID05=-(GQDDID05-MCTDDID05)

MCTDDID10=EQDDID10*(1-GiniY11T10)
ICTDDID10=-(GQDDID10-MCTDDID10)

MCTDDID25=EQDDID25*(1-GiniY11T25)
ICTDDID25=-(GQDDID25-MCTDDID25)

MCTDDID50=EQDDID50*(1-GiniY11T50)
ICTDDID50=-(GQDDID50-MCTDDID50)

EQY10TCSLB01=sum(QY10TCSLB[p<=0.01&p>0])*pgrid/0.01
EQY10TCSLB025=sum(QY10TCSLB[p<=0.025&p>0])*pgrid/0.025
EQY10TCSLB05=sum(QY10TCSLB[p<=0.05&p>0])*pgrid/0.05
EQY10TCSLB10=sum(QY10TCSLB[p<=0.1&p>0])*pgrid/0.1
EQY10TCSLB25=sum(QY10TCSLB[p<=0.25&p>0])*pgrid/0.25
EQY10TCSLB50=sum(QY10TCSLB[p<=0.5&p>0])*pgrid/0.5

EQY10TCSUB01=sum(QY10TCSUB[p<=0.01&p>0])*pgrid/0.01
EQY10TCSUB025=sum(QY10TCSUB[p<=0.025&p>0])*pgrid/0.025
EQY10TCSUB05=sum(QY10TCSUB[p<=0.05&p>0])*pgrid/0.05
EQY10TCSUB10=sum(QY10TCSUB[p<=0.1&p>0])*pgrid/0.1
EQY10TCSUB25=sum(QY10TCSUB[p<=0.25&p>0])*pgrid/0.25
EQY10TCSUB50=sum(QY10TCSUB[p<=0.5&p>0])*pgrid/0.5

GQY10TCSLB01=sum(2*(0.01-p[p<=0.01&p>0])*QY10TCSLB[p<=0.01&p>0])*pgrid/(0.01)^2
GQY10TCSLB025=sum(2*(0.025-p[p<=0.025&p>0])*QY10TCSLB[p<=0.025&p>0])*pgrid/(0.025)^2
GQY10TCSLB05=sum(2*(0.05-p[p<=0.05&p>0])*QY10TCSLB[p<=0.05&p>0])*pgrid/(0.05)^2
GQY10TCSLB10=sum(2*(0.10-p[p<=0.1&p>0])*QY10TCSLB[p<=0.1&p>0])*pgrid/(0.1)^2
GQY10TCSLB25=sum(2*(0.25-p[p<=0.25&p>0])*QY10TCSLB[p<=0.25&p>0])*pgrid/(0.25)^2
GQY10TCSLB50=sum(2*(0.5-p[p<=0.5&p>0])*QY10TCSLB[p<=0.5&p>0])*pgrid/(0.5)^2


GQY10TCSUB01=sum(2*(0.01-p[p<=0.01&p>0])*QY10TCSUB[p<=0.01&p>0])*pgrid/(0.01)^2
GQY10TCSUB025=sum(2*(0.025-p[p<=0.025&p>0])*QY10TCSUB[p<=0.025&p>0])*pgrid/(0.025)^2
GQY10TCSUB05=sum(2*(0.05-p[p<=0.05&p>0])*QY10TCSUB[p<=0.05&p>0])*pgrid/(0.05)^2
GQY10TCSUB10=sum(2*(0.10-p[p<=0.1&p>0])*QY10TCSUB[p<=0.1&p>0])*pgrid/(0.1)^2
GQY10TCSUB25=sum(2*(0.25-p[p<=0.25&p>0])*QY10TCSUB[p<=0.25&p>0])*pgrid/(0.25)^2
GQY10TCSUB50=sum(2*(0.5-p[p<=0.5&p>0])*QY10TCSUB[p<=0.5&p>0])*pgrid/(0.5)^2


EQCSLB01=EQY11T01-EQY10TCSUB01
EQCSLB025=EQY11T025-EQY10TCSUB025
EQCSLB05=EQY11T05-EQY10TCSUB05
EQCSLB10=EQY11T10-EQY10TCSUB10
EQCSLB25=EQY11T25-EQY10TCSUB25
EQCSLB50=EQY11T50-EQY10TCSUB50

EQCSUB01=EQY11T01-EQY10TCSLB01
EQCSUB025=EQY11T025-EQY10TCSLB025
EQCSUB05=EQY11T05-EQY10TCSLB05
EQCSUB10=EQY11T10-EQY10TCSLB10
EQCSUB25=EQY11T25-EQY10TCSLB25
EQCSUB50=EQY11T50-EQY10TCSLB50

GQCSLB01=GQY11T01-GQY10TCSUB01
GQCSLB025=GQY11T025-GQY10TCSUB025
GQCSLB05=GQY11T05-GQY10TCSUB05
GQCSLB10=GQY11T10-GQY10TCSUB10
GQCSLB25=GQY11T25-GQY10TCSUB25
GQCSLB50=GQY11T50-GQY10TCSUB50

GQCSUB01=GQY11T01-GQY10TCSLB01
GQCSUB025=GQY11T025-GQY10TCSLB025
GQCSUB05=GQY11T05-GQY10TCSLB05
GQCSUB10=GQY11T10-GQY10TCSLB10
GQCSUB25=GQY11T25-GQY10TCSLB25
GQCSUB50=GQY11T50-GQY10TCSLB50


MCTCSUB01=EQCSUB01*(1-GiniY11T01)
MCTCSLB01=EQCSLB01*(1-GiniY11T01)

ICTCSUB01=MCTCSUB01-GQCSLB01
ICTCSLB01=MCTCSLB01-GQCSUB01


MCTCSUB025=EQCSUB025*(1-GiniY11T025)
MCTCSLB025=EQCSLB025*(1-GiniY11T025)

ICTCSUB025=MCTCSUB025-GQCSLB025
ICTCSLB025=MCTCSLB025-GQCSUB025

MCTCSUB05=EQCSUB05*(1-GiniY11T05)
MCTCSLB05=EQCSLB05*(1-GiniY11T05)

ICTCSUB05=MCTCSUB05-GQCSLB05
ICTCSLB05=MCTCSLB05-GQCSUB05


MCTCSUB10=EQCSUB10*(1-GiniY11T10)
MCTCSLB10=EQCSLB10*(1-GiniY11T10)


ICTCSUB10=MCTCSUB10-GQCSLB10
ICTCSLB10=MCTCSLB10-GQCSUB10


MCTCSUB25=EQCSUB25*(1-GiniY11T25)
MCTCSLB25=EQCSLB25*(1-GiniY11T25)

ICTCSUB25=MCTCSUB25-GQCSLB25
ICTCSLB25=MCTCSLB25-GQCSUB25


MCTCSUB50=EQCSUB50*(1-GiniY11T50)
MCTCSLB50=EQCSLB50*(1-GiniY11T50)

ICTCSUB50=MCTCSUB50-GQCSLB50
ICTCSLB50=MCTCSLB50-GQCSUB50


##############
### Table 4 
##############

#mean MW in treatment year, wbar = max in treatment year +0.5
#vary wlbar to be 10, 10.5 and 11 to show sensitivity of CiC results
wlbar = 0
mw = (Tperiod==2010)*(7.25+0.5*(premw==7.25)) + (Tperiod==2015)*(8+0.5*(premw==8))
wbar1 = (Tperiod==2010)*8 + (Tperiod==2015)*10 # how we compute the Wbar? 
wbar2 = (Tperiod==2010)*9 + (Tperiod==2015)*11
b11T  = FY11T(mw)    - FY11T(wlbar)
a11T1 = FY11T(wbar1) - FY11T(mw)
e11T1 = FY11T(wbar1) - FY11T(wlbar)
a11T2 = FY11T(wbar2) - FY11T(mw)
e11T2 = FY11T(wbar2) - FY11T(wlbar)

DeltabLB = FY11T(mw)   -FY11T(wlbar)-(FY10TUBCS[R==mw]-FY10TLBCS[R==wlbar])
DeltaaLB1= FY11T(wbar1)-FY11T(mw)-(FY10TUBCS[R==wbar1]-FY10TLBCS[R==mw])
DeltaeLB1= FY11T(wbar1)-FY11T(wlbar)-(FY10TUBCS[R==wbar1]-FY10TLBCS[R==wlbar])
DeltaaLB2= FY11T(wbar2)-FY11T(mw)-(FY10TUBCS[R==wbar2]-FY10TLBCS[R==mw])
DeltaeLB2= FY11T(wbar2)-FY11T(wlbar)-(FY10TUBCS[R==wbar2]-FY10TLBCS[R==wlbar])

DeltabUB = FY11T(mw)   -FY11T(wlbar)-(FY10TLBCS[R==mw]-FY10TUBCS[R==wlbar])
DeltaaUB1= FY11T(wbar1)-FY11T(mw) -(FY10TLBCS[R==wbar1]-FY10TUBCS[R==mw])
DeltaeUB1= FY11T(wbar1)-FY11T(wlbar)-(FY10TLBCS[R==wbar1]-FY10TUBCS[R==wlbar])
DeltaaUB2= FY11T(wbar2)-FY11T(mw)-(FY10TLBCS[R==wbar2]-FY10TUBCS[R==mw])
DeltaeUB2= FY11T(wbar2)-FY11T(wlbar)-(FY10TLBCS[R==wbar2]-FY10TUBCS[R==wlbar])

DeltaaDDID1=FY11T(wbar1)-FY11T(mw)-(FY10TDDID[R==wbar1]-FY10TDDID[R==mw])
DeltaeDDID1=FY11T(wbar1)-FY11T(wlbar)-(FY10TDDID[R==wbar1]-FY10TDDID[R==wlbar])

DeltabDDID=FY11T(mw)-FY11T(wlbar)-(FY10TDDID[R==mw]-FY10TDDID[R==wlbar])
DeltaaDDID2=FY11T(wbar2)-FY11T(mw)-(FY10TDDID[R==wbar2]-FY10TDDID[R==mw])
DeltaeDDID2=FY11T(wbar2)-FY11T(wlbar)-(FY10TDDID[R==wbar2]-FY10TDDID[R==wlbar])