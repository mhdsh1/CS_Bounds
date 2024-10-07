#install.packages("remotes")
#remotes::install_github("gsoutinho/survCopula")
#install.packages("copula")
#install.packages("latticeExtra")

rm(list=ls())
set.seed(12345)
library(copula)
library(stats)
library(shape)
setwd("E:/Dropbox/Copula_Stability_Panel/Simulations")

#probability of being in the control group
q=0.5;

#Clayton copula parameter
theta=1
#kt denotes the degrees of freedom of the chi^2 distribution for periods t=0,1
k0=3
k1=5

#ct is the censoring point for periods t=0,1
c0=5
delta=5
c1=c0+delta
w=1
grid=0.001
R=c(-Inf, seq(-5,50,grid),Inf)
Ysupp0=c(-Inf,seq(0,c0,grid),c0)
Ysupp1=c(-Inf,seq(0,c1,grid),c1)
p=seq(0,1,0.001)

pchisqcens=function(y,k,c){
  pchisqcens=pchisq(y,df=k)*(y<c)+1*(y>=c)
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
  FY10C=(pchisqcens(ytilde,k1,c1)-(FY10C*q))/(1-q)
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

FY00s=q*FY00Cs+(1-q)*FY00Ts



####plotting cdfs for Y00C, Y10C, Y00T
FY00CR=matrix(,nrow=1,ncol=length(R))
FY10CR=matrix(,nrow=1,ncol=length(R))
FY00TR=matrix(,nrow=,ncol=length(R))
for (i in 1:length(R)){
  FY00CR[1,i]=FY00C(R[i])
  FY10CR[1,i]=FY10C(R[i])
  FY00TR[1,i]=FY00T(R[i])
}

#pre-treatment control group distribution
pdf(sprintf("FY00C_topcoding_%s_%s_%s_%s_%s.pdf",theta,c0,c1,k0,k1))
plot(R,FY00CR,xlim=c(0,max(c0,c1)+5),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
dev.off()

#post-treatment control group distribution
pdf(sprintf("FY10C_topcoding_%s_%s_%s_%s_%s.pdf",theta,c0,c1,k0,k1))
plot(R,FY10CR,xlim=c(0,max(c0,c1)+5),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
dev.off()

#pre-treatment treatment group distribution
pdf(sprintf("FY00T_topcoding_%s_%s_%s_%s_%s.pdf",theta,c0,c1,k0,k1))
plot(R,FY00TR,xlim=c(0,max(c0,c1)+5),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
dev.off()

##Plotting CS bounds on post-treatment counterfactual for treatment group
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



#bounds on post-treatment counterfactual distribution
pdf(sprintf("FTCSbounds_topcoding_%s_%s_%s_%s_%s.pdf",theta,c0,c1,k0,k1))
plot(R,FY10TLBCS,xlim=c(0,15),ylim=c(0,1),type="l",col="red",xlab="y",ylab="cdf")
lines(R,FY10TUBCS,xlim=c(0,15),ylim=c(0,1),col="blue",lty=2)
lines(R,FY10Te,xlim=c(0,15),ylim=c(0,1),col="black",lty=4)
legend("topleft",c("LB","UB", "CF"),cex=1.25,col=c("red","blue","black"),
       lty=c(1,2,4))
dev.off()


## additional figures to help understand the intuition behind the copula extension
FY00CFY00=matrix(,nrow=1,ncol=length(p))

toprange=(FY00C(4.999)+FY00T(4.999))*q

for (i in (1:length(p))){
  FY00CFY00[,i]=(max(q^(-theta)+p[i]^(-theta)-1,0)^(-1/theta))/q
  
}
poutsiderange=seq((FY00C(4.999)+FY00T(4.999))*q,1,0.001)

FY00CFY00LB=cbind(matrix(FY00C(4.999),nrow=1,ncol=length(poutsiderange)-1),c(1))
FY00CFY00UB=cbind(c(FY00C(4.999)),matrix(1,nrow=1,ncol=length(poutsiderange)-1))

prange=seq(0,(FY00C(4.999)+FY00T(4.999))/2,0.001)

FY00CFY00range=matrix(,nrow=1,ncol=length(prange))
for (i in (1:length(prange))){
  FY00CFY00range[,i]=(max(q^(-theta)+prange[i]^(-theta)-1,0)^(-1/theta))/q
}

graphics.off()
pdf(sprintf("FY00s_FY00Cs_topcoding_%s_%s_%s_%s_%s_nohollow.pdf",theta,c0,c1,k0,k1))
plot(prange,FY00CFY00range,xlim=c(0,1),ylim=c(0,1),col="red",type="l",xlab=expression("CDF of Y"[0][0]),ylab=expression("Control Group CDF of Y"[0][0]))
#points((FY00C(4.9999)+FY00T(4.9999))*q,FY00C(4.9999),col="red",cex=0.5)
points(1,1,col="red",cex=0.5,pch=16)
dev.off()


graphics.off()
pdf(sprintf("FY00s_FY00Cs_bounds_topcoding_%s_%s_%s_%s_%s.pdf",theta,c0,c1,k0,k1))
plot(prange,FY00CFY00range,xlim=c(0,1),ylim=c(0,1),col="red",type="l",xlab=expression("CDF of Y"[0][0]),ylab=expression("Control Group CDF of Y"[0][0]))
lines(poutsiderange,FY00CFY00LB,col="red",lty=2)
lines(poutsiderange,FY00CFY00UB,col="red",lty=2)
points(1,1,col="red",cex=0.5,pch=16)

dev.off()