#this version uses the quantile of -Y and the left-limit definition in terms of the cdf of -Y 
rm(list=ls())
#install.packages("dplyr")
library(dplyr)
#install.packages("dineq")
#library(dineq)
#library(stargazer)
setwd("E:/Dropbox/Copula_Stability_Panel/Application/Cengizetal2019/")
qminus<-function(q,FY,y){
  #y and FY are vectors of the same length where FY is the value of the cdf at the corresponding y value
  #no longer removing -Inf since it is computed on R
  qminus<-min(y[FY>=q])
}
qplus<-function(q,FY,y){
  #y and FY are vectors of the same length where FY is the value of the cdf at the corresponding y value
 y=y[-c(length(y))]
 FY=FY[-c(length(y))]
 qplus<-max(y[FY<=q])
}

data<-read.csv("CPS_cleaned_merged_07_15.csv")
data$wage0<-data$wage/100
#[!is.na(data$statenum),]
#set wage to zero if NA
#data$wage0[is.na(data$wage0)]=0
data$state_mw2007=data$state_mw
data$state_mw2010=data$state_mw
data$state_mw2015=data$state_mw
states=na.omit(unique(data$statenum))
for (i in states){
  for (j in 1:12){
data$state_mw2007[data$statenum==i&data$month==j]=data$state_mw[which(data$statenum==i&data$month==j&data$year==2007)]
data$state_mw2010[data$statenum==i&data$month==j]=data$state_mw[which(data$statenum==i&data$month==j&data$year==2010)]
data$state_mw2015[data$statenum==i&data$month==j]=mean(data$state_mw[which(data$statenum==i&data$month==j&data$year==2015)])
  }
}
data$smw_increase0710=(data$state_mw2010-data$state_mw2007>0.25)
data$smw_increase1015=(data$state_mw2015-data$state_mw2010>0.25)
data$smw_increase0715=(data$state_mw2015-data$state_mw2007>0.5)

data2007=data[data$year==2007,]
data2010=data[data$year==2010,]
data2015=data[data$year==2015,]



#upremw does not rule out any states on the top in the sample
premw=8
upremw=Inf
preperiod=2010
Tperiod=2015


if((preperiod==2007)&(Tperiod==2010)){
  if (premw>0){
    data2007subsample=data2007[data2007$state_mw2007>=premw&data2007$state_mw2007<upremw,]
    summary_stats_pre<-data2007subsample%>%group_by(smw_increase0710)%>%
      summarise(mean.before = mean(wage0, na.rm=TRUE),
                var.before = var(wage0, na.rm=TRUE),
                n.before = sum(!is.na(wage0)))
    
    data2010subsample=data2010[data2010$state_mw2007>=premw&data2010$state_mw2007<upremw,]
    summary_stats_post<-data2010subsample%>%group_by(smw_increase0710)%>%
      summarise(mean.before = mean(wage0, na.rm=TRUE),
                var.before = var(wage0, na.rm=TRUE),
                n.before = sum(!is.na(wage0)))
    
Y00Tn<-na.omit(data2007$wage0[(data2007$smw_increase0710==1)&(data2007$state_mw2007>=premw)&(data2007$state_mw2007<upremw)])
Y00Cn<-na.omit(data2007$wage0[(data2007$smw_increase0710==0)&(data2007$state_mw2007>=premw)&(data2007$state_mw2007<upremw)])
Y11Tn<-na.omit(data2010$wage0[(data2010$smw_increase0710==1)&(data2010$state_mw2007>=premw)&(data2010$state_mw2007<upremw)])
Y10Cn<-na.omit(data2010$wage0[(data2010$smw_increase0710==0)&(data2010$state_mw2007>=premw)&(data2010$state_mw2007<upremw)])
  }else if (premw==0){
    Y00Tn<-na.omit(data2007$wage0[data2007$smw_increase0710==1])
    Y00Cn<-na.omit(data2007$wage0[data2007$smw_increase0710==0])
    Y11Tn<-na.omit(data2010$wage0[data2010$smw_increase0710==1])
    Y10Cn<-na.omit(data2010$wage0[data2010$smw_increase0710==0])
    
}
}else if((preperiod==2007)&(Tperiod==2015)){
  if (premw>0){
  Y00Tn<-na.omit(data2007$wage0[data2007$smw_increase0715==1&data2007$state_mw2007>=premw&data2007$state_mw2007<upremw])
  Y00Cn<-na.omit(data2007$wage0[data2007$smw_increase0715==0&data2007$state_mw2007>=premw&data2007$state_mw2007<upremw])
  Y11Tn<-na.omit(data2015$wage0[data2015$smw_increase0715==1&data2015$state_mw2007>=premw&data2015$state_mw2007<upremw])
  Y10Cn<-na.omit(data2015$wage0[data2015$smw_increase0715==0&data2015$state_mw2007>=premw&data2015$state_mw2007<upremw])
  }else if (premw==0){
    Y00Tn<-na.omit(data2007$wage0[data2007$smw_increase0715==1])
    Y00Cn<-na.omit(data2007$wage0[data2007$smw_increase0715==0])
    Y11Tn<-na.omit(data2015$wage0[data2015$smw_increase0715==1])
    Y10Cn<-na.omit(data2015$wage0[data2015$smw_increase0715==0])
  }
}else if((preperiod==2010)&(Tperiod==2015)){
  
  data2010subsample=data2010[data2010$state_mw2010>=premw&data2010$state_mw2010<upremw,]
  summary_stats_pre<-data2010subsample%>%group_by(smw_increase1015)%>%
    summarise(mean.before = mean(wage0, na.rm=TRUE),
              var.before = var(wage0, na.rm=TRUE),
              n.before = sum(!is.na(wage0)))
  
  data2015subsample=data2015[data2015$state_mw2010>=premw&data2015$state_mw2010<upremw,]
  summary_stats_post<-data2015subsample%>%group_by(smw_increase1015)%>%
    summarise(mean.before = mean(wage0, na.rm=TRUE),
              var.before = var(wage0, na.rm=TRUE),
              n.before = sum(!is.na(wage0)))
Y00Tn<-na.omit(data2010$wage0[data2010$smw_increase1015==1&data2010$state_mw2010>=premw&data2010$state_mw2010<upremw])
Y00Cn<-na.omit(data2010$wage0[data2010$smw_increase1015==0&data2010$state_mw2010>=premw&data2010$state_mw2010<upremw])
Y11Tn<-na.omit(data2015$wage0[data2015$smw_increase1015==1&data2015$state_mw2010>=premw&data2015$state_mw2010<upremw])
Y10Cn<-na.omit(data2015$wage0[data2015$smw_increase1015==0&data2015$state_mw2010>=premw&data2015$state_mw2010<upremw])
}



summary_stats<-cbind(summary_stats_pre,summary_stats_post)
write.csv(summary_stats, paste0("summarystats_wages_disaggregated06272024_06272024_",preperiod,"-",Tperiod,"_",premw,".csv"))

############################
# Define the empirical cdfs
############################

FY00C<-ecdf(Y00Cn)
FY10C<-ecdf(Y10Cn)
FY00T<-ecdf(Y00Tn)
FY11T<-ecdf(Y11Tn)

Ysupp0=c(-Inf,seq(0,max(Y00Cn,Y00Tn),0.01),Inf)
Ysupp1=c(-Inf,seq(0,max(Y10Cn),0.01),Inf)
R=c(-Inf, seq(-1,1+max(Y00Cn,Y00Tn,Y10Cn),0.01),Inf)

FY00Cs=matrix(,nrow=1,ncol=length(Ysupp0))

FY10Cs=matrix(,nrow=1,ncol=length(Ysupp1))
FY00Ts=matrix(,nrow=,ncol=length(Ysupp0))
for (i in 1:length(Ysupp0)){
  
  FY00Cs[1,i]=FY00C(Ysupp0[i])
  
  FY00Ts[1,i]=FY00T(Ysupp0[i])
}
for (i in 1:length(Ysupp1)){
  FY10Cs[1,i]=FY10C(Ysupp1[i])
}



####plotting cdfs for Y00C, Y10C, Y00T
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


#Distributional DiD
FY10TDDID=FY00TR+FY10CR-FY00CR
pdf(sprintf("DistDiDY10T_wages_Cengizetal2019_disaggregated06272024_06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TDDID,xlim=c(0,50),ylim=c(0,1),type="l",col="blue4",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(R,FY11TR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",cex.lab=1.5,xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
legend("topleft",c("DistDiD","Obs"),cex=1.25,col=c("blue4","black"),lty=c(1,1))
dev.off()
pdf(sprintf("DistDiDY10T_zoombottom_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TDDID,xlim=c(0,15),ylim=c(0,0.25),type="l",col="blue4",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(R,FY11TR,xlim=c(0,15),ylim=c(0,0.25),type="l",col="black",cex.lab=1.5,xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
legend("topleft",c("DistDiD","Obs"),cex=1.25,col=c("blue4","black"),lty=c(1,1))
dev.off()
pdf(sprintf("DistDiDY10T_zoomtop_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TDDID,xlim=c(30,75),ylim=c(0.75,1),type="l",col="blue4",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(R,FY11TR,xlim=c(30,75),ylim=c(0.75,1),type="l",col="black",cex.lab=1.5,xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
legend("topleft",c("DistDiD","Obs"),cex=1.25,col=c("blue4","black"),lty=c(1,1))
dev.off()


pdf(sprintf("FY00C_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY00CR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
dev.off()

pdf(sprintf("FY10C_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10CR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
dev.off()

pdf(sprintf("FY00T_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY00TR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
dev.off()


pdf(sprintf("FY11T_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY11TR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
dev.off()

mY00Tn=-Y00Tn
mY00Cn=-Y00Cn
FmY00T<-ecdf(mY00Tn)
FmY00C<-ecdf(mY00Cn)
FmY00CR=matrix(,nrow=1,ncol=length(R))
Rm=c(-Inf, seq(-1-max(Y00Cn,Y00Tn,Y10Cn),1,0.01),Inf)
for (i in seq(1,length(Rm),1)){
  FmY00CR[1,i]=FmY00C(Rm[i])
}

Ysupp00Cdiscrete=sort(unique(Y00Cn))
Ysupp00Cdiscrete=sort(matrix(Ysupp00Cdiscrete,nrow=1,ncol=length(Ysupp00Cdiscrete)))
FY00Cds=matrix(,nrow=1,ncol=length(Ysupp00Cdiscrete))
FmY00Cds=matrix(,nrow=1,ncol=length(Ysupp00Cdiscrete))
Ysupp00Cdiscretem=sort(matrix(-Ysupp00Cdiscrete,nrow=1,ncol=length(Ysupp00Cdiscrete)))
#discrete support
for (i in seq(1,length(FY00Cds),1)){
  FY00Cds[1,i]=FY00C(Ysupp00Cdiscrete[i])
  FmY00Cds[1,i]=FmY00C(Ysupp00Cdiscretem[i])
}

#computing CS bounds on support
Ysupp1noInf=Ysupp1[-c(length(Ysupp1))]
FY10TLBY=matrix(,nrow=1,ncol=length(Ysupp1noInf))
FY10TLBmY=matrix(,nrow=1,ncol=length(Ysupp1noInf))
FY10TUBY=matrix(,nrow=1,ncol=length(Ysupp1noInf))
FY11TY=matrix(,nrow=1,ncol=length(Ysupp1noInf))
for (i in 1:length(Ysupp1noInf)){
  FY10TLBY[1,i]=FY00T(qplus(FY10C(Ysupp1noInf[i]),FY00CR,R))
  
  FY10TLBmY[1,i]=1-FmY00T(qminus(1-FY10C(Ysupp1noInf[i]),FmY00CR,Rm))
  if(Ysupp1noInf[i]==-Inf){
    FY10TUBY[1,i]=0
  }else{
    FY10TUBY[1,i]=FY00T(qminus(FY10C(Ysupp1noInf[i]),FY00CR,R))}
  FY11TY[1,i]=FY11T(Ysupp1noInf[i])
}
 
#limsup transformation for right-continuity
FY10TLBCS=matrix(,nrow=1,ncol=length(R))
FY10TLBCSm=matrix(,nrow=1,ncol=length(R))

FY10TUBCS=matrix(,nrow=1,ncol=length(R))
for (i in 1:length(R)){
  FY10TLBCS[1,i]=max(FY10TLBY[,Ysupp1noInf<=R[i]])
  FY10TLBCSm[1,i]=max(FY10TLBmY[,Ysupp1noInf<=R[i]])
  FY10TUBCS[1,i]=max(FY10TUBY[,Ysupp1noInf<=R[i]])
}

FY10TCiC=FY10TUBCS
#CiCLB computed using discrete support

FY10TCiCLB=matrix(,nrow=1,ncol=length(R))
for(i in seq(1,length(R),1)){
FY10TCiCLB[1,i]=FY00T(qplus(FY10C(R[i]),FY00Cds,Ysupp00Cdiscrete))
}

#plot(R,FY10TLBCS,type="l",col="red",xlim=c(0,20))
#lines(R,FY10TCiCLB,type="l",col="blue")
#################
lowesty=min(min(Y10Cn),min(Y11Tn),min(Y00Cn),min(Y00Tn))
pdf(sprintf("FTCSbounds_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TLBCS,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(R,FY10TLBCSm,xlim=c(0,50),ylim=c(0,1),col="red",lty=1)
lines(R,FY10TUBCS,xlim=c(0,50),ylim=c(0,1),col="blue",lty=1)
lines(R,FY11TR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")
legend("topleft",c("CS-LB","CS-UB", "Obs"),cex=1.25,col=c("red","blue","black"),
       lty=c(1,1,1))
dev.off()

pdf(sprintf("FTCSbounds_noobs_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TLBCS,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(R,FY10TLBCSm,xlim=c(0,50),ylim=c(0,1),col="red",lty=1)
lines(R,FY10TUBCS,xlim=c(0,50),ylim=c(0,1),col="blue",lty=1)

legend("topleft",c("CS-LB","CS-UB"),cex=1.25,col=c("red","blue"),
       lty=c(1,1))
dev.off()
pdf(sprintf("FTCSbounds_zoombottom_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TLBCS,xlim=c(0,15),ylim=c(0,0.25),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(R,FY10TUBCS,xlim=c(0,15),ylim=c(0,0.25),col="blue",lty=1)
lines(R,FY10TLBCSm,xlim=c(0,50),ylim=c(0,1),col="red",lty=1)

lines(R,FY11TR,xlim=c(0,15),ylim=c(0,0.25),type="l",col="black",xlab="y",ylab="cdf")

legend("topleft",c("CS-LB","CS-UB", "Obs"),cex=1.25,col=c("red","blue","black"),
       lty=c(1,1,1))
dev.off()

pdf(sprintf("FTCSbounds_noobs_zoombottom_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TLBCS,xlim=c(0,15),ylim=c(0,0.25),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(R,FY10TUBCS,xlim=c(0,15),ylim=c(0,0.25),col="blue",lty=1)
lines(R,FY10TLBCSm,xlim=c(0,50),ylim=c(0,1),col="red",lty=1)
legend("topleft",c("CS-LB","CS-UB"),cex=1.25,col=c("red","blue"),
       lty=c(1,1))
dev.off()

pdf(sprintf("FTCSbounds_zoomtop_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TLBCS,xlim=c(30,75),ylim=c(0.75,1),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(R,FY10TUBCS,xlim=c(30,75),ylim=c(0.75,1),col="blue",lty=1)
lines(R,FY10TLBCSm,xlim=c(0,50),ylim=c(0,1),col="red",lty=1)
lines(R,FY11TR,xlim=c(0,10),ylim=c(0,0.1),type="l",col="black",xlab="y",ylab="cdf")
legend("topleft",c("CS-LB","CS-UB", "Obs"),cex=1.25,col=c("red","blue","black"),
       lty=c(1,1,1))
dev.off()
#CSbounds + DistDiD in one figure
pdf(sprintf("CSboundsDistDiDY10T_wages_Cengizetal2019_disaggregated06272024_06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TDDID,xlim=c(0,50),ylim=c(0,1),type="l",col="blue4",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(R,FY10TUBCS,xlim=c(0,15),ylim=c(0,0.25),col="blue",lty=1)
lines(R,FY10TLBCSm,xlim=c(0,50),ylim=c(0,1),col="red",lty=1)
legend("topleft",c("CS-LB","CS-UB","DistDiD"),cex=1.25,col=c("red","blue","blue4"),lty=c(1,1,1))
dev.off()
pdf(sprintf("CSboundsDistDiDY10T_zoombottom_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TDDID,xlim=c(0,15),ylim=c(0,0.25),type="l",col="blue4",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(R,FY10TUBCS,xlim=c(0,15),ylim=c(0,0.25),col="blue",lty=1)
lines(R,FY10TLBCSm,xlim=c(0,50),ylim=c(0,1),col="red",lty=1)
legend("topleft",c("CS-LB","CS-UB","DistDiD"),cex=1.25,col=c("red","blue","blue4"),lty=c(1,1,1))

dev.off()
pdf(sprintf("CSboundsDistDiDY10T_zoomtop_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TDDID,xlim=c(30,75),ylim=c(0.75,1),type="l",col="blue4",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(R,FY10TUBCS,xlim=c(0,15),ylim=c(0,0.25),col="blue",lty=1)
lines(R,FY10TLBCSm,xlim=c(0,50),ylim=c(0,1),col="red",lty=1)
legend("topleft",c("CS-LB","CS-UN","DistDiD"),cex=1.25,col=c("red","blue","blue4"),lty=c(1,1,1))
dev.off()



pdf(sprintf("FTCiC_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TCiC,xlim=c(0,50),ylim=c(0,1),type="l",col="green",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(R,FY10TCiCLB,xlim=c(0,50),ylim=c(0,1),type="l",col="magenta",xlab="y",ylab="cdf")
lines(R,FY10TLBCSm,xlim=c(30,75),ylim=c(0.75,1),col="red",lty=5)

lines(R,FY11TR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")

legend("topleft",c("CiC-PE", "Obs"),cex=1.25,col=c("green","black"),
       lty=c(1,1))
dev.off()

pdf(sprintf("FTCiC_zoombottom_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TCiC,xlim=c(0,15),ylim=c(0,0.25),type="l",col="green",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(R,FY11TR,xlim=c(0,15),ylim=c(0,0.25),type="l",col="black",xlab="y",ylab="cdf")
lines(R,FY10TCiCLB,xlim=c(0,50),ylim=c(0,1),type="l",col="magenta",xlab="y",ylab="cdf")
lines(R,FY10TLBCSm,xlim=c(30,75),ylim=c(0.75,1),col="red",lty=5)
legend("topleft",c("CiC-PE", "Obs"),cex=1.25,col=c("green","black"),
       lty=c(1,1))
dev.off()

pdf(sprintf("FTCiC_zoomtop_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TCiC,xlim=c(30,75),ylim=c(0.75,1),type="l",col="green",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(R,FY11TR,xlim=c(0,10),ylim=c(0,0.1),type="l",col="black",xlab="y",ylab="cdf")
lines(R,FY10TCiCLB,xlim=c(0,50),ylim=c(0,1),type="l",col="magenta",xlab="y",ylab="cdf")
lines(R,FY10TLBCSm,xlim=c(30,75),ylim=c(0.75,1),col="red",lty=5)

legend("topleft",c("CiC-PE", "Obs"),cex=1.25,col=c("green","black"),
       lty=c(1,1))
dev.off()


pdf(sprintf("FTCiC_wages_Cengizetal2019_disaggregated_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TCiC,xlim=c(0,50),ylim=c(0,1),type="l",col="green",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(R,FY10TCiCLB,xlim=c(0,50),ylim=c(0,1),type="l",col="magenta",xlab="y",ylab="cdf")
lines(R,FY10TLBCSm,xlim=c(30,75),ylim=c(0.75,1),col="red",lty=5)

lines(R,FY11TR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf")

legend("topleft",c("CiC-PE", "Obs"),cex=1.25,col=c("green","black"),
       lty=c(1,1))
dev.off()

pdf(sprintf("FTCiC_zoombottom_wages_Cengizetal2019_disaggregated_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TCiC,xlim=c(0,15),ylim=c(0,0.25),type="l",col="green",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(R,FY11TR,xlim=c(0,15),ylim=c(0,0.25),type="l",col="black",xlab="y",ylab="cdf")
legend("topleft",c("CiC-PE", "Obs"),cex=1.25,col=c("green","black"),
       lty=c(1,1))
dev.off()

pdf(sprintf("FTCiC_zoomtop_wages_Cengizetal2019_disaggregated_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,FY10TCiC,xlim=c(30,75),ylim=c(0.75,1),type="l",col="green",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(R,FY11TR,xlim=c(0,10),ylim=c(0,0.1),type="l",col="black",xlab="y",ylab="cdf")

legend("topleft",c("CiC-PE", "Obs"),cex=1.25,col=c("green","black"),
       lty=c(1,1))
dev.off()


###########################


# obtaining quantiles 
pgrid=0.001
p=seq(0,1,pgrid)
qplusY00C=matrix(,nrow=1,ncol=length(p))
qminusY00C=matrix(,nrow=1,ncol=length(p))
qminusY00R=matrix(,nrow=1,ncol=length(p))
qminusY10C=matrix(,nrow=1,ncol=length(p))
qminusY00T=matrix(,nrow=1,ncol=length(p))
for (i in 1:length(p)){
  qplusY00C[1,i]=qplus(p[i],FY00CR,R)
  qminusY00C[1,i]= qminus(p[i],FY00Cs,Ysupp0)
  qminusY00R[1,i]= qminus(p[i],FY00CR,R)
  qminusY10C[1,i]=qminus(p[i],FY10Cs,Ysupp1)
  qminusY00T[1,i]=qminus(p[i],FY00Ts,Ysupp0)
}

pdf(sprintf("QY00+-_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(p,qplusY00C,xlim=c(0,1),ylim=c(0,50),type="l", col="red",xlab="q",ylab="Quantile")
lines(p,qminusY00C,xlim=c(0,1),ylim=c(0,50),type="l", col="blue",xlab="q",ylab="Quantile",lty=1)
legend("topleft",c("Q+","Q-"),cex=1.25,col=c("red","blue"),
       lty=c(1,2,4))
dev.off()

qplusY00CFY10C=matrix(,nrow=1,ncol=length(R))
qminusY00CFY10C=matrix(,nrow=1,ncol=length(R))
for (i in 1:length(R)){
  qplusY00CFY10C[1,i]=qplus(FY10CR[i],FY00CR,R)
  qminusY00CFY10C[1,i]=qminus(FY10CR[i],FY00CR,R)
}
pdf(sprintf("QY00CFY10C+-_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(R,qplusY00CFY10C,xlim=c(10,40),ylim=c(0,50),type="l", col="red",xlab="y1",ylab="y0")
lines(R,qminusY00CFY10C,xlim=c(10,40),ylim=c(0,50),type="l", col="blue",xlab="y1",ylab="y0",lty=1)
legend("topleft",c("Q+","Q-"),cex=1.25,col=c("red","blue"),
       lty=c(1,2,4))
dev.off()
boundsSBS=rbind(R,FY10CR,qplusY00CFY10C,qminusY00CFY10C,FY10TLBCS,FY10TUBCS)
write.csv(boundsSBS,paste0("bounds_computation_wages_disaggregated06272024_",Tperiod,".csv"))

#obtain quantile of Distributional DiD, LB and UB of CS
QY10TDDID=matrix(,nrow=1,ncol=length(p))
QY10TCSLB=matrix(,nrow=1,ncol=length(p))
QY10TCSUB=matrix(,nrow=1,ncol=length(p))
QY10TCiC=matrix(,nrow=1,ncol=length(p))
QY10TCiCUB=matrix(,nrow=1,ncol=length(p))
QY10T=matrix(,nrow=1,ncol=length(p))
QY11T=matrix(,nrow=1,ncol=length(p))
for (i in 1:length(p)){
  QY10TDDID[1,i]=qminus(p[i],FY10TDDID,R)
  #this is the LB on the quantile function which is the quantile of the UB on cdf
  QY10TCSLB[1,i]=qminus(p[i],FY10TUBCS,R)
  #this is the UB on the quantile function which is the quantile of the LB on cdf
  QY10TCSUB[1,i]=qminus(p[i],FY10TLBCS,R)
  QY10TCiC[1,i]=qminus(p[i],FY10TCiC,R)
  #Quantile of LB of CiC using discrete support following kolesar recommendation
  QY10TCiCUB[1,i]=qminus(p[i],FY10TCiCLB,R)
  QY11T[1,i]=qminus(p[i],FY11TR,R)
}

pdf(sprintf("Quantiles_DDID_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(p,QY10TDDID,xlim=c(0,1),ylim=c(0,500),type="l",col="blue4",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(p,QY11T,xlim=c(0,1),ylim=c(0,500),type="l",col="black",cex.lab=1.5,xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
legend("topleft",c("DistDiD"),cex=1.25,col=c("blue4"),lty=c(1))
dev.off()

pdf(sprintf("Quantiles_DDID_zoom_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(p,QY10TDDID,xlim=c(0,0.25),ylim=c(0,15),type="l",col="blue4",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(p,QY11T,xlim=c(0,0.25),ylim=c(0,15),type="l",col="black",cex.lab=1.5,xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
legend("topleft",c("DistDiD"),cex=1.25,col=c("blue4"),lty=c(1,1))
dev.off()
pdf(sprintf("Quantiles_CS_zoom_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(p,QY10TCSLB,xlim=c(0,0.2),ylim=c(0,10),type="l",col="red",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(p,QY10TCSUB,xlim=c(0,0.2),ylim=c(0,10),type="l",col="blue",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(p,QY11T,xlim=c(0,0.2),ylim=c(0,10),type="l",col="black",cex.lab=1.5,xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
legend("topleft",c("CS-LB","CS-UB","Obs"),cex=1.25,col=c("red","blue","black"),lty=c(1,1,1))
dev.off()
pdf(sprintf("Quantiles_CiC_zoom_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",preperiod,Tperiod,premw))
plot(p,QY10TCiC,xlim=c(0,0.2),ylim=c(0,10),type="l",col="green",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(p,QY11T,xlim=c(0,0.2),ylim=c(0,10),type="l",col="black",cex.lab=1.5,xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
legend("topleft",c("CiC","Obs"),cex=1.25,col=c("green","black"),lty=c(1,1))
dev.off()





maxR=2*max(R[-c(length(R))])
#DiD
fY10TDDID=matrix(0,nrow=1,ncol=length(R))
for(i in seq(2,length(R),1)){
  fY10TDDID[1,i]=FY10TDDID[1,i]-FY10TDDID[1,i-1]
}
EY10TDDIDd=sum(R[R<Inf&R>-Inf]*fY10TDDID[R<Inf&R>-Inf])
EY11T=mean(Y11Tn)
EY00C=mean(Y00Cn)
EY10C=mean(Y10Cn)
EY00T=mean(Y00Tn)
EY10TDID=EY00T+EY10C-EY00C


# finding expectation of the quantile function
EY10TDDID=sum(QY10TDDID[QY10TDDID<=maxR&QY10TDDID>=min(Ysupp1[-c(1)])])*pgrid

EY10TCSLB=sum(QY10TCSLB[QY10TCSLB<=maxR&QY10TCSLB>=min(Ysupp1[-c(1)])])*pgrid
EY10TCSUB=sum(QY10TCSUB[QY10TCSUB<=maxR&QY10TCSUB>=min(Ysupp1[-c(1)])])*pgrid
EY10TCiC=sum(QY10TCiC[QY10TCiC<=maxR&QY10TCiC>=min(Ysupp1[-c(1)])])*pgrid
#CiC lower bound

EY10TCiCUB=sum(QY10TCiCUB[QY10TCiCUB<=maxR&QY10TCiCUB>=min(Ysupp1[-c(1)])])*pgrid

#CiC lower bound - Kolesar computation
FY00Cminus<-ecdf(-Y00Cn)
Ysupp10Cdiscrete=sort(unique(Y10Cn))
Ysupp10Cdiscrete=matrix(Ysupp10Cdiscrete,nrow=length(Ysupp10Cdiscrete),ncol=1)
FY10Cds=matrix(,nrow=length(Ysupp10Cdiscrete),ncol=1)
for (i in seq(1,length(FY10Cds),1)){
FY10Cds[i,1]=FY10C(Ysupp10Cdiscrete[i])
}

kappaLB=matrix(,nrow=length(Y00Tn),ncol=1)
for (i in seq(1,length(Y00Tn),1)){
  kappaLB[i,1]=qminus(1-FY00Cminus(-Y00Tn[i]),FY10Cds,Ysupp10Cdiscrete)
}
EY10TCiCLB=mean(kappaLB)


# SW_lambda  p is tau .. you can change it tau 

G11T=sum(2*(1-p[QY11T<=maxR&QY11T>=min(Ysupp1[-c(1)])])*(QY11T[QY11T<=maxR&QY11T>=min(Ysupp1[-c(1)])]))*pgrid
G10TDDID=sum(2*(1-p[QY10TDDID<=maxR&QY10TDDID>=min(Ysupp1[-c(1)])])*QY10TDDID[QY10TDDID<=maxR&QY10TDDID>=min(Ysupp1[-c(1)])])*pgrid
G10TCSLB=sum(2*(1-p[QY10TCSLB<=maxR&QY10TCSLB>=min(Ysupp1[-c(1)])])*QY10TCSLB[QY10TCSLB<=maxR&QY10TCSLB>=min(Ysupp1[-c(1)])])*pgrid
G10TCSUB=sum(2*(1-p[QY10TCSUB<=maxR&QY10TCSUB>=min(Ysupp1[-c(1)])])*QY10TCSUB[QY10TCSUB<=maxR&QY10TCSUB>=min(Ysupp1[-c(1)])])*pgrid
G10TCiC=sum(2*(1-p[QY10TCiC<=maxR&QY10TCiC>=min(Ysupp1[-c(1)])])*QY10TCiC[QY10TCiC<=maxR&QY10TCiC>=min(Ysupp1[-c(1)])])*pgrid

GiniY11T=1-G11T/EY11T

# gini swtt 

GTTDDID=G11T-G10TDDID
GTTCSUB=G11T-G10TCSLB
GTTCSLB=G11T-G10TCSUB
GTTCiC=G11T-G10TCiC

# min component 
MCTDDID=(EY11T-EY10TDDID)*(1-GiniY11T)
# ineq component
ICTDDID=-(GTTDDID-MCTDDID)

MCTCSLB=(EY11T-EY10TCSUB)*(1-GiniY11T)
MCTCSUB=(EY11T-EY10TCSLB)*(1-GiniY11T)
MCTCiC=(EY11T-EY10TCiC)*(1-GiniY11T)

ICTCSUB=MCTCSUB-GTTCSLB
ICTCSLB=MCTCSLB-GTTCSUB
ICTCiC=MCTCiC-GTTCiC

EQY10TDDID01d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.01]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.01])
EQY10TDDID025d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.025]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.025])
EQY10TDDID05d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.05]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.05])
EQY10TDDID10d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.1]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.1])
EQY10TDDID25d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.25]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.25])
EQY10TDDID50d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.5]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.5])
EQY10TDDID75d=sum(R[R<Inf&R>-Inf&FY10TDDID<=0.75]*fY10TDDID[R<Inf&R>-Inf&FY10TDDID<=0.75])

EQY11T01=sum(QY11T[p<=0.01&p>0])*pgrid/0.01
EQY11T025=sum(QY11T[p<=0.025&p>0])*pgrid/0.025
EQY11T05=sum(QY11T[p<=0.05&p>0])*pgrid/0.05
EQY11T10=sum(QY11T[p<=0.1&p>0])*pgrid/0.1
EQY11T25=sum(QY11T[p<=0.25&p>0])*pgrid/0.25
EQY11T50=sum(QY11T[p<=0.5&p>0])*pgrid/0.5


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

EQY10TCiC01=sum(QY10TCiC[p<=0.01&p>0])*pgrid/0.01
EQY10TCiC025=sum(QY10TCiC[p<=0.025&p>0])*pgrid/0.025
EQY10TCiC05=sum(QY10TCiC[p<=0.05&p>0])*pgrid/0.05
EQY10TCiC10=sum(QY10TCiC[p<=0.1&p>0])*pgrid/0.1
EQY10TCiC25=sum(QY10TCiC[p<=0.25&p>0])*pgrid/0.25
EQY10TCiC50=sum(QY10TCiC[p<=0.5&p>0])*pgrid/0.5


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

GQY10TCiC01=sum(2*(0.01-p[p<=0.01&p>0])*QY10TCiC[p<=0.01&p>0])*pgrid/(0.01)^2
GQY10TCiC025=sum(2*(0.025-p[p<=0.025&p>0])*QY10TCiC[p<=0.025&p>0])*pgrid/(0.025)^2
GQY10TCiC05=sum(2*(0.05-p[p<=0.05&p>0])*QY10TCiC[p<=0.05&p>0])*pgrid/(0.05)^2
GQY10TCiC10=sum(2*(0.10-p[p<=0.1&p>0])*QY10TCiC[p<=0.1&p>0])*pgrid/(0.1)^2
GQY10TCiC25=sum(2*(0.25-p[p<=0.25&p>0])*QY10TCiC[p<=0.25&p>0])*pgrid/(0.25)^2
GQY10TCiC50=sum(2*(0.5-p[p<=0.5&p>0])*QY10TCiC[p<=0.5&p>0])*pgrid/(0.5)^2


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

EQCiC01=EQY11T01-EQY10TCiC01
EQCiC025=EQY11T025-EQY10TCiC025
EQCiC05=EQY11T05-EQY10TCiC05
EQCiC10=EQY11T10-EQY10TCiC10
EQCiC25=EQY11T25-EQY10TCiC25
EQCiC50=EQY11T50-EQY10TCiC50

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


GQCiC01=GQY11T01-GQY10TCiC01
GQCiC025=GQY11T025-GQY10TCiC025
GQCiC05=GQY11T05-GQY10TCiC05
GQCiC10=GQY11T10-GQY10TCiC10
GQCiC25=GQY11T25-GQY10TCiC25
GQCiC50=GQY11T50-GQY10TCiC50

MCTCSUB01=EQCSUB01*(1-GiniY11T01)
MCTCSLB01=EQCSLB01*(1-GiniY11T01)
MCTCiC01=EQCiC01*(1-GiniY11T01)

ICTCSUB01=MCTCSUB01-GQCSLB01
ICTCSLB01=MCTCSLB01-GQCSUB01
ICTCiC01=MCTCiC01-GQCiC01


MCTCSUB025=EQCSUB025*(1-GiniY11T025)
MCTCSLB025=EQCSLB025*(1-GiniY11T025)
MCTCiC025=EQCiC025*(1-GiniY11T025)

ICTCSUB025=MCTCSUB025-GQCSLB025
ICTCSLB025=MCTCSLB025-GQCSUB025
ICTCiC025=MCTCiC025-GQCiC025

MCTCSUB05=EQCSUB05*(1-GiniY11T05)
MCTCSLB05=EQCSLB05*(1-GiniY11T05)
MCTCiC05=EQCiC05*(1-GiniY11T05)

ICTCSUB05=MCTCSUB05-GQCSLB05
ICTCSLB05=MCTCSLB05-GQCSUB05
ICTCiC05=MCTCiC05-GQCSUB05


MCTCSUB10=EQCSUB10*(1-GiniY11T10)
MCTCSLB10=EQCSLB10*(1-GiniY11T10)
MCTCiC10=EQCiC10*(1-GiniY11T10)


ICTCSUB10=MCTCSUB10-GQCSLB10
ICTCSLB10=MCTCSLB10-GQCSUB10
ICTCiC10=MCTCiC10-GQCiC10


MCTCSUB25=EQCSUB25*(1-GiniY11T25)
MCTCSLB25=EQCSLB25*(1-GiniY11T25)
MCTCiC25=EQCiC25*(1-GiniY11T25)


ICTCSUB25=MCTCSUB25-GQCSLB25
ICTCSLB25=MCTCSLB25-GQCSUB25
ICTCiC25=MCTCiC25-GQCiC25


MCTCSUB50=EQCSUB50*(1-GiniY11T50)
MCTCSLB50=EQCSLB50*(1-GiniY11T50)
MCTCiC50=EQCiC50*(1-GiniY11T50)

ICTCSUB50=MCTCSUB50-GQCSLB50
ICTCSLB50=MCTCSLB50-GQCSUB50
ICTCiC50=MCTCiC50-GQCiC50


QY11T025=QY11T[p==0.025]
QY11T05=QY11T[p==0.05]
QY11T10=QY11T[p==0.10]
QY11T25=QY11T[p==0.25]
QY11T50=QY11T[p==0.50]
QY11T75=QY11T[p==0.75]
QY11T90=QY11T[p==0.90]

QTTCSLB01=QY11T[p==0.01]-QY10TCSUB[p==0.01]
QTTCSLB025=QY11T025-QY10TCSUB[p==0.025]
QTTCSLB05=QY11T05-QY10TCSUB[p==0.05]
QTTCSLB10=QY11T10-QY10TCSUB[p==0.10]
QTTCSLB25=QY11T25-QY10TCSUB[p==0.25]
QTTCSLB50=QY11T50-QY10TCSUB[p==0.50]
QTTCSLB75=QY11T75-QY10TCSUB[p==0.75]
QTTCSLB90=QY11T90-QY10TCSUB[p==0.90]

QTTCSUB01=QY11T[p==0.01]-QY10TCSLB[p==0.01]
QTTCSUB025=QY11T025-QY10TCSLB[p==0.025]
QTTCSUB05=QY11T05-QY10TCSLB[p==0.05]
QTTCSUB10=QY11T10-QY10TCSLB[p==0.10]
QTTCSUB25=QY11T25-QY10TCSLB[p==0.25]
QTTCSUB50=QY11T50-QY10TCSLB[p==0.50]
QTTCSUB75=QY11T75-QY10TCSLB[p==0.75]
QTTCSUB90=QY11T90-QY10TCSLB[p==0.90]


QTTDDID01=QY11T[p==0.01]-QY10TDDID[p==0.01]
QTTDDID025=QY11T025-QY10TDDID[p==0.025]
QTTDDID05=QY11T05-QY10TDDID[p==0.05]

QTTDDID10=QY11T10-QY10TDDID[p==0.10]
QTTDDID25=QY11T25-QY10TDDID[p==0.25]
QTTDDID50=QY11T50-QY10TDDID[p==0.50]
QTTDDID75=QY11T75-QY10TDDID[p==0.75]
QTTDDID90=QY11T90-QY10TDDID[p==0.90]

resultsACF=c(EY11T,EY10TCSLB,EY10TCSUB,EY10TDID,EY10TDDID,EY10TDDIDd)
resultsATT=c(EY11T,EY11T-EY10TCSUB,EY11T-EY10TCSLB,EY11T-EY10TDID,EY11T-EY10TDDID,EY11T-EY10TCiC)
resultsGTT=c(G11T,GTTCSLB,GTTCSUB,"--",GTTDDID,GTTCiC)
resultsMCGTT=c("--",MCTCSLB,MCTCSUB,"--",MCTDDID,MCTCiC)
resultsICGTT=c("--",ICTCSLB,ICTCSUB,"--",ICTDDID,ICTCiC)

resultsEQTT01=c(EQY11T01,EQCSLB01,EQCSUB01,"--",EQDDID01,EQCiC01)
resultsEQTT025=c(EQY11T025,EQCSLB025,EQCSUB025,"--",EQDDID025,EQCiC025)
resultsEQTT05=c(EQY11T05,EQCSLB05,EQCSUB05,"--",EQDDID025,EQCiC05)
resultsEQTT10=c(EQY11T10,EQCSLB10,EQCSUB10,"--",EQDDID10,EQCiC10)
resultsEQTT25=c(EQY11T25,EQCSLB25,EQCSUB25,"--",EQDDID25,EQCiC25)
resultsEQTT50=c(EQY11T50,EQCSLB50,EQCSUB50,"--",EQDDID50,EQCiC50)

resultsGQTT01=c(GQY11T01,GQCSLB01,GQCSUB01,"--",GQDDID01,GQCiC01)
resultsMCTT01=c("--",MCTCSLB01,MCTCSUB01,"--",MCTDDID01,MCTCiC01)
resultsICTT01=c("--",ICTCSLB01,ICTCSUB01,"--",ICTDDID01,ICTCiC01)
resultsGQTT025=c(GQY11T025,GQCSLB025,GQCSUB025,"--",GQDDID025,GQCiC025)
resultsMCTT025=c("--",MCTCSLB025,MCTCSUB025,"--",MCTDDID025,MCTCiC025)
resultsICTT025=c("--",ICTCSLB025,ICTCSUB025,"--",ICTDDID025,ICTCiC025)

resultsGQTT05=c(GQY11T05,GQCSLB05,GQCSUB05,"--",GQDDID05,GQCiC05)
resultsMCTT05=c("--",MCTCSLB05,MCTCSUB05,"--",MCTDDID05,MCTCiC05)
resultsICTT05=c("--",ICTCSLB05,ICTCSUB05,"--",ICTDDID05,ICTCiC05)
resultsGQTT10=c(GQY11T10,GQCSLB10,GQCSUB10,"--",GQDDID10,GQCiC10)
resultsMCTT10=c("--",MCTCSLB10,MCTCSUB10,"--",MCTDDID10,MCTCiC10)
resultsICTT10=c("--",ICTCSLB10,ICTCSUB10,"--",ICTDDID10,ICTCiC10)
resultsGQTT25=c(GQY11T25,GQCSLB25,GQCSUB25,"--",GQDDID25,GQCiC25)
resultsMCTT25=c("--",MCTCSLB25,MCTCSUB25,"--",MCTDDID25,MCTCiC25)
resultsICTT25=c("--",ICTCSLB25,ICTCSUB25,"--",ICTDDID25,ICTCiC25)
resultsGQTT50=c(GQY11T50,GQCSLB50,GQCSUB50,"--",GQDDID50,GQCiC50)
resultsMCTT50=c("--",MCTCSLB50,MCTCSUB50,"--",MCTDDID50,MCTCiC50)
resultsICTT50=c("--",ICTCSLB50,ICTCSUB50,"--",ICTDDID50,ICTCiC50)

resultsEQTT=rbind(resultsEQTT01,resultsEQTT025,resultsEQTT05,resultsEQTT10,resultsEQTT25,resultsEQTT50)
resultsGQTT=rbind(resultsGQTT01,resultsMCTT01,resultsICTT01,resultsGQTT025,resultsMCTT025,resultsICTT025,resultsGQTT05,resultsMCTT05,resultsICTT05,resultsGQTT10,resultsMCTT10,resultsICTT10,resultsGQTT25,resultsMCTT25,resultsICTT25,resultsGQTT50,resultsMCTT50,resultsICTT50)


resultsQTT01=c(QY11T[p==0.01],QTTCSLB01,QTTCSUB01,"--",QTTDDID01,"--")
resultsQTT025=c(QY11T025,QTTCSLB025,QTTCSUB025,"--",QTTDDID025,"--")
resultsQTT05=c(QY11T05,QTTCSLB05,QTTCSUB05,"--",QTTDDID05,"--")
resultsQTT10=c(QY11T10,QTTCSLB10,QTTCSUB10,"--",QTTDDID10,"--")
resultsQTT25=c(QY11T25,QTTCSLB25,QTTCSUB25,"--",QTTDDID25,"--")
resultsQTT50=c(QY11T50,QTTCSLB50,QTTCSUB50,"--",QTTDDID50,"--")
resultsQTT75=c(QY11T75,QTTCSLB75,QTTCSUB75,"--",QTTDDID75,"--")
resultsQTT90=c(QY11T90,QTTCSLB90,QTTCSUB90,"--",QTTDDID90,"--")

#mean MW in treatment year, wbar = max in treatment year +0.5
#vary wlbar to be 10, 10.5 and 11 to show sensitivity of CiC results
wlbar=0
mw=(Tperiod==2010)*(7.25+0.5*(premw==7.25))+(Tperiod==2015)*(8+0.5*(premw==8))
wbar1=(Tperiod==2010)*8+(Tperiod==2015)*10
wbar2=(Tperiod==2010)*9+(Tperiod==2015)*11
b11T=FY11T(mw)-FY11T(wlbar)
a11T1=FY11T(wbar1)-FY11T(mw)
e11T1=FY11T(wbar1)-FY11T(wlbar)
a11T2=FY11T(wbar2)-FY11T(mw)
e11T2=FY11T(wbar2)-FY11T(wlbar)
DeltabLB=FY11T(mw)-FY11T(wlbar)-(FY10TUBCS[R==mw]-FY10TLBCS[R==wlbar])
DeltaaLB1=FY11T(wbar1)-FY11T(mw)-(FY10TUBCS[R==wbar1]-FY10TLBCS[R==mw])
DeltaeLB1=FY11T(wbar1)-FY11T(wlbar)-(FY10TUBCS[R==wbar1]-FY10TLBCS[R==wlbar])
DeltaaLB2=FY11T(wbar2)-FY11T(mw)-(FY10TUBCS[R==wbar2]-FY10TLBCS[R==mw])
DeltaeLB2=FY11T(wbar2)-FY11T(wlbar)-(FY10TUBCS[R==wbar2]-FY10TLBCS[R==wlbar])

DeltabUB=FY11T(mw)-FY11T(wlbar)-(FY10TLBCS[R==mw]-FY10TUBCS[R==wlbar])
DeltaaUB1=FY11T(wbar1)-FY11T(mw)-(FY10TLBCS[R==wbar1]-FY10TUBCS[R==mw])
DeltaeUB1=FY11T(wbar1)-FY11T(wlbar)-(FY10TLBCS[R==wbar1]-FY10TUBCS[R==wlbar])
DeltaaUB2=FY11T(wbar2)-FY11T(mw)-(FY10TLBCS[R==wbar2]-FY10TUBCS[R==mw])
DeltaeUB2=FY11T(wbar2)-FY11T(wlbar)-(FY10TLBCS[R==wbar2]-FY10TUBCS[R==wlbar])

DeltabCiC=FY11T(mw)-FY11T(wlbar)-(FY10TCiC[R==mw]-FY10TCiC[R==wlbar])
DeltaaCiC1=FY11T(wbar1)-FY11T(mw)-(FY10TCiC[R==wbar1]-FY10TCiC[R==mw])
DeltaeCiC1=FY11T(wbar1)-FY11T(wlbar)-(FY10TCiC[R==wbar1]-FY10TCiC[R==wlbar])
DeltaaCiC2=FY11T(wbar2)-FY11T(mw)-(FY10TCiC[R==wbar2]-FY10TCiC[R==mw])
DeltaeCiC2=FY11T(wbar2)-FY11T(wlbar)-(FY10TCiC[R==wbar2]-FY10TCiC[R==wlbar])

DeltabDDID=FY11T(mw)-FY11T(wlbar)-(FY10TDDID[R==mw]-FY10TDDID[R==wlbar])
DeltaaDDID1=FY11T(wbar1)-FY11T(mw)-(FY10TDDID[R==wbar1]-FY10TDDID[R==mw])
DeltaeDDID1=FY11T(wbar1)-FY11T(wlbar)-(FY10TDDID[R==wbar1]-FY10TDDID[R==wlbar])
DeltaaDDID2=FY11T(wbar2)-FY11T(mw)-(FY10TDDID[R==wbar2]-FY10TDDID[R==mw])
DeltaeDDID2=FY11T(wbar2)-FY11T(wlbar)-(FY10TDDID[R==wbar2]-FY10TDDID[R==wlbar])

resultsdeltab=rbind(c("Obs","CS-LB","CS-UB","DID","DistDID","CiC"),c(b11T,DeltabLB,DeltabUB,"--",DeltabDDID,DeltabCiC))
resultsdeltaa1=c(a11T1,DeltaaLB1,DeltaaUB1,"--",DeltaaDDID1,DeltaaCiC1)
resultsdeltaa2=c(a11T2,DeltaaLB2,DeltaaUB2,"--",DeltaaDDID2,DeltaaCiC2)
resultsdeltae1=c(e11T1,DeltaeLB1,DeltaeUB1,"--",DeltaeDDID1,DeltaeCiC1)
resultsdeltae2=c(e11T2,DeltaeLB2,DeltaeUB2,"--",DeltaeDDID2,DeltaeCiC2)

results=rbind(resultsdeltab,resultsdeltaa1,resultsdeltae1,resultsdeltaa2,resultsdeltae2,resultsATT,resultsGTT,resultsMCGTT,resultsICGTT,resultsEQTT,resultsGQTT,resultsACF,resultsQTT01,resultsQTT025,resultsQTT05,resultsQTT10,resultsQTT25,resultsQTT50,resultsQTT75,resultsQTT90)
write.csv(results,paste0("ACF_wages_Cengizetal2019_disaggregated06272024_",preperiod,Tperiod,"_",premw,"_02262024.csv"))
