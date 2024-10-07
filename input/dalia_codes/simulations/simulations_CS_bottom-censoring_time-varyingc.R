#install.packages("remotes")
#remotes::install_github("gsoutinho/survCopula")
#install.packages("copula")
#install.packages("latticeExtra")

rm(list=ls())
set.seed(12345)
#library(copula)
library(stats)
setwd("E:/Dropbox/Copula_Stability_Panel/Simulations")
#setwd("C:/Users/dghanem/Dropbox/Copula_Stability_Panel/Simulations")
#probability of being in control group
q=0.5;

#Clayton copula parameter
theta=1


#kt denotes the degrees of freedom of the chi^2 distribution for periods t=0,1
k0=3
k1=5

#ct denotes the bottom censoring point for period t=0,1
c0=5
delta=0
c1=c0+delta

grid=0.001
minR=-5
maxR=75
R=c(-Inf, seq(minR,maxR,grid),Inf)
Ysupp0=c(-Inf,seq(c0,maxR,grid),Inf)
Ysupp1=c(-Inf,seq(c1,maxR,grid),Inf)
Ysupp11T=Ysupp1
pchisqcens=function(y,k,c){
  pchisqcens=pchisq(c,df=k)*(y==c)+pchisq(y,df=k)*(y>c)
}

dchisqcens=function(y,k,c){
  dchisqcens=pchisq(c,df=k)*(y==c)+dchisq(y,df=k)*(y>c)
}
FY00C=function(y){
  FY00C=(max(q^(-theta)+pchisqcens(y,k0,c0)^(-theta)-1,0)^(-1/theta))/q
}
FY10C=function(ytilde){
  FY10C=(max(q^(-theta)+pchisqcens(ytilde,k1,c1)^(-theta)-1,0)^(-1/theta))/q
}
FY00T=function(y){
  FY00C=(max(q^(-theta)+pchisqcens(y,k0,c0)^(-theta)-1,0)^(-1/theta))/q
  FY00T=(pchisqcens(y,k0,c0)-(FY00C*q))/(1-q)
} 

FY10T=function(ytilde){
  FY10C=(max(q^(-theta)+pchisqcens(ytilde,k1,c1)^(-theta)-1,0)^(-1/theta))/q
  FY10T=(pchisqcens(ytilde,k1,c1)-(FY10C*q))/(1-q)
} 
qminus<-function(q,FY,y){
  #y and FY are vectors of the same length where FY is the value of the cdf at the corresponding y value
  y=y[-c(1)]
  FY=FY[-c(1)]
  qminus<-min(y[FY>=q])
}

qplus<-function(q,FY,y){
  #y and FY are vectors of the same length where FY is the value of the cdf at the corresponding y value
  qplus<-max(y[FY<=q])
}

FY00Cs=matrix(,nrow=1,ncol=length(Ysupp0))
FY10Cs=matrix(,nrow=1,ncol=length(Ysupp1))
FY00Ts=matrix(,nrow=,ncol=length(Ysupp0))
FY10Ts=matrix(,nrow=1,ncol=length(Ysupp1))
for (i in 1:length(Ysupp0)){
  
FY00Cs[1,i]=FY00C(Ysupp0[i])

FY00Ts[1,i]=FY00T(Ysupp0[i])
}
for (i in 1:length(Ysupp1)){
FY10Cs[1,i]=FY10C(Ysupp1[i])
FY10Ts[1,i]=FY10T(Ysupp1[i])
}


####plotting cdfs for Y00C, Y10C, Y00T
FY00CR=matrix(,nrow=1,ncol=length(R))
FY10CR=matrix(,nrow=1,ncol=length(R))
FY00TR=matrix(,nrow=1,ncol=length(R))
for (i in 1:length(R)){
  FY00CR[1,i]=FY00C(R[i])
  FY10CR[1,i]=FY10C(R[i])
  FY00TR[1,i]=FY00T(R[i])

}

pdf(sprintf("FY00C_bottom-censoring_timevaryingc_%s_%s_%s_%s_%s.pdf",theta,c0,c1,k0,k1))
plot(R,FY00CR,xlim=c(min(c0,c1)-1,min(c0,c1)+9),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
dev.off()

pdf(sprintf("FY10C_bottom-censoring_timevaryingc_%s_%s_%s_%s_%s.pdf",theta,c0,c1,k0,k1))
plot(R,FY10CR,xlim=c(min(c0,c1)-1,min(c0,c1)+9),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
dev.off()

pdf(sprintf("FY00T_bottom-censoring_timevaryingc_%s_%s_%s_%s_%s.pdf",theta,c0,c1,k0,k1))
plot(R,FY00TR,xlim=c(min(c0,c1)-1,min(c0,c1)+9),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
dev.off()





##plotting CS bounds
FY10TLBY=matrix(,nrow=1,ncol=length(Ysupp1))
FY10TUBY=matrix(,nrow=1,ncol=length(Ysupp1))
FY10TY=matrix(,nrow=1,ncol=length(Ysupp1))
for (i in 1:length(Ysupp1)){
  
  FY10TLBY[1,i]=FY00T(qplus(FY10C(Ysupp1[i]),FY00CR,R))
  if(Ysupp1[i]==-Inf){
    FY10TUBY[1,i]=0
  }else{
    FY10TUBY[1,i]=FY00T(qminus(FY10C(Ysupp1[i]),FY00Cs,Ysupp0))}
  FY10TY[1,i]=FY10T(Ysupp1[i])

}

FY10TLBCS=matrix(,nrow=1,ncol=length(R))
FY10TUBCS=matrix(,nrow=1,ncol=length(R))
FY10Te=matrix(,nrow=1,ncol=length(R))
for (i in 1:length(R)){
  FY10TLBCS[1,i]=max(FY10TLBY[,Ysupp1<=R[i]])
  FY10TUBCS[1,i]=max(FY10TUBY[,Ysupp1<=R[i]])
  FY10Te[1,i]=FY10T(R[i])
}

pdf(sprintf("FTCSbounds_bottom-censoring_timevaryingc_%s_%s_%s_%s_%s.pdf",theta,c0,c1,k0,k1))
plot(R,FY10TLBCS,xlim=c(min(c0,c1)-1,min(c0,c1)+9),ylim=c(0,1),type="l",col="red",xlab="y",ylab="cdf",cex.axis=1.5)
lines(R,FY10TUBCS,xlim=c(min(c0,c1)-1,min(c0,c1)+9),ylim=c(0,1),col="blue",lty=2)
lines(R,FY10Te,xlim=c(min(c0,c1)-1,min(c0,c1)+9),ylim=c(0,1),col="black",lty=4)
legend("bottomright",c("LB","UB", "CF"),cex=1.25,col=c("red","blue","black"),
       lty=c(1,2,4))

dev.off()