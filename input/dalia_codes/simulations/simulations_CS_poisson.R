#install.packages("remotes")
#remotes::install_github("gsoutinho/survCopula")
#install.packages("copula")
#install.packages("latticeExtra")
#install.packages("latex2exp")
rm(list=ls())
set.seed(12345)
library(base)
library(copula)
library(stats)
#library(latex2exp)
library(shape)
setwd("E:/Dropbox/Copula_Stability_Panel/Simulations")
#setwd("C:/Users/dghanem/Dropbox/Copula_Stability_Panel/Simulations")
#probability of treatment
q=0.5;
#indicator to generate data using copula stability
grid=0.001
R=c(-Inf, seq(-5,50,grid),Inf)
discrete=seq(0,50,1)
Ysupp=c(-Inf,seq(0,50,1),Inf)

#Clayton copula parameter
theta=1

#lambdat is the parameter of the poisson distribution for period t=0,1
lambda0=3
lambda1=2.5

FY00C=function(y){
  FY00C=(max(q^(-theta)+ppois(y,lambda0)^(-theta)-1,0)^(-1/theta))/q
}
FY10C=function(ytilde){
  FY10C=(max(q^(-theta)+ppois(ytilde,lambda1)^(-theta)-1,0)^(-1/theta))/q
}
FY00T=function(y){
  FY00C=(max(q^(-theta)+ppois(y,lambda0)^(-theta)-1,0)^(-1/theta))/q
  FY00T=(ppois(y,lambda0)-(FY00C*q))/(1-q)
} 

FY10T=function(ytilde){
  FY10C=(max(q^(-theta)+ppois(ytilde,lambda1)^(-theta)-1,0)^(-1/theta))/q
  FY10T=(ppois(ytilde,lambda1)-(FY10C*q))/(1-q)
} 

qminus<-function(q,FY,y){
  #AI2006 F^{-1}
  #y and FY are vectors of the same length where FY is the value of the cdf at the corresponding y value
  y=y[-c(1)]
  FY=FY[-c(1)]
  qminus<-min(y[FY>=q])
}

qplus<-function(q,FY,y){
  #AI2006 F^{(-1)}
  #y and FY are vectors of the same length where FY is the value of the cdf at the corresponding y valu
   qplus<-max(y[FY<=q])
}

FY00Cs=matrix(,nrow=1,ncol=length(Ysupp))
FY10Cs=matrix(,nrow=1,ncol=length(Ysupp))
FY00Ts=matrix(,nrow=,ncol=length(Ysupp))
FY10Ts=matrix(,nrow=1,ncol=length(Ysupp))
for (i in 1:length(Ysupp)){
  
  FY00Cs[1,i]=FY00C(Ysupp[i])
  FY10Cs[1,i]=FY10C(Ysupp[i])
  FY00Ts[1,i]=FY00T(Ysupp[i])
  FY10Ts[1,i]=FY10T(Ysupp[i])}
FY00s=q*FY00Cs+(1-q)*FY00Ts
graphics.off()
pdf(sprintf("FY00s_FY00Cs_poisson_%s_%s_%s.pdf",theta,lambda0,lambda1))
plot(FY00s,FY00Cs,xlim=c(0,1),ylim=c(0,1),col="red",cex=1.5,pch=16,xlab=expression("CDF of Y"[0][0]),ylab=expression("Control Group CDF of Y"[0][0]))
dev.off()


graphics.off()
pdf(sprintf("FY00s_FY00Cs_FY10Cs_poisson_%s_%s_%s.pdf",theta,lambda0,lambda1))
plot(FY00s,FY00Cs,xlim=c(0,1),ylim=c(0,1),col="red",cex=1.5,pch=16,xlab="F_{Y_{00}}",ylab="F_{Y_{00}|D=0}")
lines(seq(0,1,0.1),FY10C(1)*matrix(1,nrow=11,ncol=1),col="blue",lty=2,xlab="F_{Y_{00}}",ylab="Control Group CDF")

dev.off()


graphics.off()
pdf(sprintf("FY00s_FY00Cs_FY10Cs2_poisson_%s_%s_%s.pdf",theta,lambda0,lambda1))
plot(FY00s,FY00Cs,xlim=c(0,1),ylim=c(0,1),col="red",cex=1.5,pch=16,xlab="Marginal CDF",ylab="Control Group CDF")
lines(seq(0,1,0.1),FY10C(1)*matrix(1,nrow=11,ncol=1),col="blue",lty=2,xlab="Marginal CDF",ylab="Control Group CDF")
lines(seq(0,1,0.1),FY10C(2)*matrix(1,nrow=11,ncol=1),col="blue",lty=2,xlab="Marginal CDF",ylab="Control Group CDF")

dev.off()


FY00CR=matrix(,nrow=1,ncol=length(R))
FY10CR=matrix(,nrow=1,ncol=length(R))
FY00TR=matrix(,nrow=,ncol=length(R))
FY10TR=matrix(,nrow=1,ncol=length(R))
for (i in 1:length(R)){
FY00CR[1,i]=FY00C(R[i])
FY10CR[1,i]=FY10C(R[i])
FY00TR[1,i]=FY00T(R[i])
FY10TR[1,i]=FY10T(R[i])}


##plotting cdfs for Y00C, Y10C, Y00T
FY00CR=matrix(,nrow=1,ncol=length(R))
FY10CR=matrix(,nrow=1,ncol=length(R))
FY00TR=matrix(,nrow=,ncol=length(R))
for (i in 1:length(R)){
  FY00CR[1,i]=FY00C(R[i])
  FY10CR[1,i]=FY10C(R[i])
  FY00TR[1,i]=FY00T(R[i])}




pdf(sprintf("FY00C_poisson_%s_%s_%s.pdf",theta,lambda0,lambda1))
plot(R,FY00CR,xlim=c(0,5),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
dev.off()

pdf(sprintf("FY10C_poisson_%s_%s_%s.pdf",theta,lambda0,lambda1))
plot(R,FY10CR,xlim=c(0,5),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
dev.off()

pdf(sprintf("FY00T_poisson_%s_%s_%s.pdf",theta,lambda0,lambda1))
plot(R,FY00TR,xlim=c(0,5),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
dev.off()





##plotting CiC and CS bounds
FY10TLBCiC=matrix(,nrow=1,ncol=length(R))
FY10TUBCiC=matrix(,nrow=1,ncol=length(R))
FY10Te=matrix(,nrow=1,ncol=length(R))
  for (i in 1:length(R)){
  FY10TLBCiC[1,i]=FY00T(qplus(FY10C(R[i]),FY00Cs,Ysupp))
  FY10TUBCiC[1,i]=FY00T(qminus(FY10C(R[i]),FY00Cs,Ysupp))*(R[i]>=0)
  FY10Te[1,i]=FY10T(R[i])
  }
FY10TLBY=matrix(,nrow=1,ncol=length(Ysupp))
FY10TUBY=matrix(,nrow=1,ncol=length(Ysupp))
FY10TY=matrix(,nrow=1,ncol=length(Ysupp))
for (i in 1:length(Ysupp)){
  
  FY10TLBY[1,i]=FY00T(qplus(FY10C(Ysupp[i]),FY00Cs,Ysupp))
  if(Ysupp[i]==-Inf){
    FY10TUBY[1,i]=0
    }else{
    FY10TUBY[1,i]=FY00T(qminus(FY10C(Ysupp[i]),FY00Cs,Ysupp))}
  FY10TY[1,i]=FY10T(Ysupp[i])
}
graphics.off()

FY10TLBCS=matrix(,nrow=1,ncol=length(R))
FY10TUBCS=matrix(,nrow=1,ncol=length(R))
for (i in 1:length(R)){
  FY10TLBCS[1,i]=max(FY10TLBY[,Ysupp<=R[i]])
  FY10TUBCS[1,i]=max(FY10TUBY[,Ysupp<=R[i]])
}

graphics.off()
pdf(sprintf("FTCS_CiC_bounds_poisson_%s_%s_%s.pdf",theta,lambda0,lambda1))
plot(R,FY10TLBCS,xlim=c(-0.25,7),ylim=c(0,1),type="l",col="red",xlab="y",ylab="cdf")
lines(R,FY10TUBCS,xlim=c(-0.25,7),ylim=c(0,1),col="blue",lty=2)
lines(R,FY10TLBCiC,xlim=c(-0.25,7),ylim=c(0,1),col="red",lty=1)

lines(R,FY10TUBCiC,xlim=c(-0.25,7),ylim=c(0,1),col="blue",lty=1)

lines(R,FY10Te,xlim=c(-0.25,7),ylim=c(0,1),col="black",lty=4)
legend("bottomright",c("LB-AI2006","UB-AI2006","LB","UB", "CF"),
       cex=1.25,col=c("red","blue","red","blue","black"),
       lty=c(1,1,2,2,4))
dev.off()


## additional figures to help explain the intuition of CS bounds
p=seq(0,1,0.0001)
FY00CFY00=matrix(,nrow=1,ncol=length(p))

for (i in (1:length(p))){
  FY00CFY00[,i]=(max(q^(-theta)+p[i]^(-theta)-1,0)^(-1/theta))/q
}

graphics.off()
pdf(sprintf("FY00s_FY00Cs_onearrow_poisson_%s_%s_%s.pdf",theta,lambda0,lambda1))
plot(FY00s,FY00Cs,xlim=c(0,1),ylim=c(0,1),col="red",cex=1.5,pch=16,xlab=expression("CDF of Y"[0][0]),ylab=expression("Control Group CDF of Y"[0][0]))
Arrows(-0.2,FY00C(1), 0.11, FY00C(1),col="blue",lwd=2,arr.type="triangle")
dev.off()
graphics.off()
pdf(sprintf("FY00s_FY00Cs_twoarrows_poisson_%s_%s_%s.pdf",theta,lambda0,lambda1))
plot(FY00s,FY00Cs,xlim=c(0,1),ylim=c(0,1),col="red",cex=1.5,pch=16,xlab=expression("CDF of Y"[0][0]),ylab=expression("Control Group CDF of Y"[0][0]))
Arrows(-0.2,FY00C(1), 0.11, FY00C(1),col="blue",lwd=2,arr.type="triangle")
Arrows(-0.2,0.5, 0.11, 0.5,col="blue",lwd=2,arr.type="triangle")
dev.off()
#delta0bounds
delta0LB=matrix(,nrow=1,ncol=length(p))
delta0UB=matrix(,nrow=1,ncol=length(p))
for (i in 1:length(p)){
  delta0LB[1,i]=max(FY00Cs[FY00s<=p[i]])
  delta0UB[1,i]=min(FY00Cs[FY00s>=p[i]])
}
graphics.off()
pdf(sprintf("FY00s_FY00Cs_delta0bounds_poisson_%s_%s_%s.pdf",theta,lambda0,lambda1))
plot(FY00s,FY00Cs,xlim=c(0,1),ylim=c(0,1),col="red",cex=1.5,pch=16,xlab=expression("CDF of Y"[0][0]),ylab=expression("Control Group CDF of Y"[0][0]))
lines(p,delta0LB,xlim=c(0,1),ylim=c(0,1),col="red",lty=2)
lines(p,delta0UB,xlim=c(0,1),ylim=c(0,1),col="blue",lty=2)

dev.off()
