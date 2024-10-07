#install.packages("remotes")
#install.packages("copula")
#install.packages("latticeExtra")

rm(list=ls())
set.seed(12345)
library(copula)
library(stats)
setwd("E:/Dropbox/Copula_Stability_Panel/Simulations")
#setwd("C:/Users/dghanem/Dropbox/Copula_Stability_Panel/Simulations")
#probability of treatment
q=0.5

#Clayton copula parameter
theta=1


mu0=1
sd0=0.5
mu1=mu0*2
sd1=sd0*sqrt(2)
c0=0.5
c1=2.5
b0=0.25
b1=0.75
grid=0.001
R=c(-Inf,seq(-5,5,grid),Inf)
Ysupp=R
pbunching=function(y,mu,sd,c,b){
  w=c+0.5
  pc=pnorm(c,mu,sd)
  pw=pnorm(w,mu,sd)
  pbunching=pnorm(y,mu,sd)*((y<c)+(y>=w))+(pc+b*(pw-pc))*(y==c)+(pc+b*(pw-pc)+(1-b)*(pnorm(y,mu,sd)-pc))*(y>c)*(y<w)
}

FY00C=function(y){
  FY00C=(max(q^(-theta)+pbunching(y,mu0,sd0,c0,b0)^(-theta)-1,0)^(-1/theta))/q
}
FY10C=function(ytilde){
  FY10C=(max(q^(-theta)+pbunching(ytilde,mu1,sd1,c1,b1)^(-theta)-1,0)^(-1/theta))/q
}
FY00T=function(y){
  FY00C=(max(q^(-theta)+pbunching(y,mu0,sd0,c0,b0)^(-theta)-1,0)^(-1/theta))/q
  FY00T=(pbunching(y,mu0,sd0,c0,b0)-(FY00C*q))/(1-q)
} 

FY10T=function(ytilde){
  FY10C=(max(q^(-theta)+pbunching(ytilde,mu1,sd1,c1,b1)^(-theta)-1,0)^(-1/theta))/q
  FY10T=(pbunching(ytilde,mu1,sd1,c1,b1)-(FY10C*q))/(1-q)
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

##plotting the cdfs of Y00C, Y10C, and Y00T

FY00CR=matrix(,nrow=1,ncol=length(R))
FY10CR=matrix(,nrow=1,ncol=length(R))
FY00TR=matrix(,nrow=,ncol=length(R))
for (i in 1:length(R)){
  FY00CR[1,i]=FY00C(R[i])
  FY10CR[1,i]=FY10C(R[i])
  FY00TR[1,i]=FY00T(R[i])}

pdf(sprintf("FY00C_bunching_%s_%s_%s_%s_%s.pdf",theta,c0,c1,b0,b1))
plot(R,FY00CR,xlim=c(-1,3),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
dev.off()

pdf(sprintf("FY10C_bunching_%s_%s_%s_%s_%s.pdf",theta,c0,c1,b0,b1))
plot(R,FY10CR,xlim=c(-1,3),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
dev.off()

pdf(sprintf("FY00T_bunching_%s_%s_%s_%s_%s.pdf",theta,c0,c1,b0,b1))
plot(R,FY00TR,xlim=c(-1,3),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
dev.off()

##plotting the CS bounds
FY10TLBCS=matrix(,nrow=1,ncol=length(R))
FY10TUBCS=matrix(,nrow=1,ncol=length(R))
FY10TR=matrix(,nrow=1,ncol=length(R))
  for (i in 1:length(R)){
  FY10TLBCS[1,i]=FY00T(qplus(FY10C(R[i]),FY00CR,R))
  FY10TUBCS[1,i]=FY00T(qminus(FY10C(R[i]),FY00CR,R))
  FY10TR[1,i]=FY10T(R[i])
  }
pdf(sprintf("FTCSbounds_bunching_%s_%s_%s_%s_%s.pdf",theta,c0,c1,b0,b1))
plot(R,FY10TLBCS,xlim=c(-1,3),ylim=c(0,1),type="l",col="red",xlab="y",ylab="cdf")
lines(R,FY10TUBCS,xlim=c(-1,3),ylim=c(0,1),col="blue",lty=2)
lines(R,FY10TR,xlim=c(-1,3),ylim=c(0,1),col="black",lty=3,lwd=1.5)
legend("topleft",c("LB","UB", "CF"),
       cex=1.25,col=c("red","blue","black"),
       lty=c(1,2,3))
dev.off()





