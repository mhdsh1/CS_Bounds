# Mahdi Shams 
# Date:
# Des: 

###########
#   Setup
###########

#this version uses the quantile of -Y and the left-limit definition in terms of the cdf of -Y 
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
  #y and FY are vectors of the same length where FY is the value of the cdf at the corresponding y value
  #no longer removing -Inf since it is computed on R
  qminus<-min(y[FY>=q])
}

qplus<-function(q,FY,y){
  #y and FY are vectors of the same length where FY is the value of the cdf at the corresponding y value
 y  = y[-c(length(y))]
 FY = FY[-c(length(y))]
 qplus<-max(y[FY<=q])
}

###############
# Reading Data
###############


data<-read.csv(paste0(input_path,"CPS_cleaned_merged_07_15.csv"))

data$wage0<-data$wage/100

#[!is.na(data$statenum),]
#set wage to zero if NA
#data$wage0[is.na(data$wage0)]=0

data$state_mw2007 = data$state_mw
data$state_mw2010 = data$state_mw
data$state_mw2015 = data$state_mw

states=na.omit(unique(data$statenum))
for (i in states){
  for (j in 1:12){
data$state_mw2007[data$statenum==i&data$month==j] = 
    data$state_mw[which(data$statenum==i&data$month==j&data$year==2007)]

data$state_mw2010[data$statenum==i&data$month==j] = 
    data$state_mw[which(data$statenum==i&data$month==j&data$year==2010)]

data$state_mw2015[data$statenum==i&data$month==j] = 
    mean(data$state_mw[which(data$statenum==i&data$month==j&data$year==2015)])
  }
}

# Mahdi: why we only have the mean for the last one?

data$smw_increase0710 = (data$state_mw2010-data$state_mw2007 > 0.25)
data$smw_increase1015 = (data$state_mw2015-data$state_mw2010 > 0.25)
data$smw_increase0715 = (data$state_mw2015-data$state_mw2007 > 0.5)

data2007 = data[data$year==2007,]
data2010 = data[data$year==2010,]
data2015 = data[data$year==2015,]


#################
# making the outcomes Tn and Cn
################


#upremw does not rule out any states on the top in the sample
premw     = 8 # state minimum wage filtering.
upremw    = Inf

# 3 cases:
# Case 1: (preperiod == 2007) & (Tperiod == 2010)
# Case 2: (preperiod == 2007) & (Tperiod == 2015)
# Case 3: (preperiod == 2010) & (Tperiod == 2015)

# We only focus on case 3
preperiod = 2010
Tperiod   = 2015


# different groups representing different wage samples: 
# Tn & Cn groups before (00) and after (11/10) the mw change

# Case 1
if((preperiod==2007)&(Tperiod==2010)){
  if (premw>0){
    data2007subsample = 
        data2007[data2007$state_mw2007 >= premw & data2007$state_mw2007 < upremw,]
    summary_stats_pre <- data2007subsample%>%group_by(smw_increase0710)%>%
      summarise(mean.before = mean(wage0, na.rm=TRUE),
                var.before  = var(wage0, na.rm=TRUE),
                n.before    = sum(!is.na(wage0)))
    
    data2010subsample = 
        data2010[data2010$state_mw2007>=premw&data2010$state_mw2007<upremw,]
    summary_stats_post < -data2010subsample%>%group_by(smw_increase0710)%>%
      summarise(mean.before = mean(wage0, na.rm=TRUE),
                var.before  = var(wage0, na.rm=TRUE),
                n.before    = sum(!is.na(wage0)))
    
Y00Tn <- na.omit(data2007$wage0[(data2007$smw_increase0710==1)&
                                (data2007$state_mw2007>=premw)&
                                (data2007$state_mw2007<upremw)])

Y00Cn<-na.omit(data2007$wage0[(data2007$smw_increase0710==0)&
                              (data2007$state_mw2007>=premw)&
                              (data2007$state_mw2007<upremw)])

Y11Tn<-na.omit(data2010$wage0[(data2010$smw_increase0710==1)&
                              (data2010$state_mw2007>=premw)&
                              (data2010$state_mw2007<upremw)])

Y10Cn<-na.omit(data2010$wage0[(data2010$smw_increase0710==0)&
                              (data2010$state_mw2007>=premw)&
                              (data2010$state_mw2007<upremw)])
  }
  # Mahdi: Whats this additional cond for?
  else if (premw==0){
    Y00Tn<-na.omit(data2007$wage0[data2007$smw_increase0710==1])
    Y00Cn<-na.omit(data2007$wage0[data2007$smw_increase0710==0])
    Y11Tn<-na.omit(data2010$wage0[data2010$smw_increase0710==1])
    Y10Cn<-na.omit(data2010$wage0[data2010$smw_increase0710==0])    
}
# Case 2
}else if((preperiod==2007)&(Tperiod==2015)){
  if (premw>0){
  Y00Tn<-na.omit(data2007$wage0[data2007$smw_increase0715==1&
                                data2007$state_mw2007>=premw&
                                data2007$state_mw2007<upremw])
                                
  Y00Cn<-na.omit(data2007$wage0[data2007$smw_increase0715==0&
                                data2007$state_mw2007>=premw&
                                data2007$state_mw2007<upremw])

  Y11Tn<-na.omit(data2015$wage0[data2015$smw_increase0715==1&
                                data2015$state_mw2007>=premw&
                                data2015$state_mw2007<upremw])

  Y10Cn<-na.omit(data2015$wage0[data2015$smw_increase0715==0&
                                data2015$state_mw2007>=premw&
                                data2015$state_mw2007<upremw])
  }else if (premw==0){
    Y00Tn<-na.omit(data2007$wage0[data2007$smw_increase0715==1])
    Y00Cn<-na.omit(data2007$wage0[data2007$smw_increase0715==0])
    Y11Tn<-na.omit(data2015$wage0[data2015$smw_increase0715==1])
    Y10Cn<-na.omit(data2015$wage0[data2015$smw_increase0715==0])
  }
# Case 3
}else if((preperiod==2010)&(Tperiod==2015)){
  
  data2010subsample=data2010[data2010$state_mw2010>=premw&
                             data2010$state_mw2010<upremw,]
  summary_stats_pre<-data2010subsample%>%group_by(smw_increase1015)%>%
    summarise(mean.before = mean(wage0, na.rm=TRUE),
              var.before = var(wage0, na.rm=TRUE),
              n.before = sum(!is.na(wage0)))
  
  data2015subsample=data2015[data2015$state_mw2010>=premw&
                             data2015$state_mw2010<upremw,]
  summary_stats_post<-data2015subsample%>%group_by(smw_increase1015)%>%
    summarise(mean.before = mean(wage0, na.rm=TRUE),
              var.before = var(wage0, na.rm=TRUE),
              n.before = sum(!is.na(wage0)))

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
}


### Summary Statistics 

summary_stats<-cbind(summary_stats_pre,summary_stats_post)

write.csv(summary_stats, 
          paste0(output_path, "summarystats_wages_disaggregated06272024_06272024_",
          preperiod,"-",Tperiod,"_",premw,".csv"))



### Empirical CDFs

FY00C<-ecdf(Y00Cn)
FY10C<-ecdf(Y10Cn)
FY00T<-ecdf(Y00Tn)
FY11T<-ecdf(Y11Tn)

Ysupp0 = c(-Inf,seq(0,max(Y00Cn,Y00Tn),0.01),Inf)
Ysupp1 = c(-Inf,seq(0,max(Y10Cn),0.01),Inf)

R = c(-Inf, seq(-1,1+max(Y00Cn,Y00Tn,Y10Cn),0.01),Inf)

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


#### plotting cdfs for Y00C, Y10C, Y00T

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

FY10TDDID = FY00TR + FY10CR - FY00CR

pdf(paste0(output_path, sprintf(
    "DistDiDY10T_wages_Cengizetal2019_disaggregated06272024_06272024_%s-%s_%s.pdf",
    preperiod,Tperiod,premw)))
plot(R,FY10TDDID,xlim=c(0,50),ylim=c(0,1),type="l",col="blue4",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(R,FY11TR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",cex.lab=1.5,xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
legend("topleft",c("DistDiD","Obs"),cex=1.25,col=c("blue4","black"),lty=c(1,1))
dev.off()

pdf(paste0(output_path, sprintf(
    "DistDiDY10T_zoombottom_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",
    preperiod,Tperiod,premw)))
plot(R,FY10TDDID,xlim=c(0,15),ylim=c(0,0.25),type="l",col="blue4",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(R,FY11TR,xlim=c(0,15),ylim=c(0,0.25),type="l",col="black",cex.lab=1.5,xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
legend("topleft",c("DistDiD","Obs"),cex=1.25,col=c("blue4","black"),lty=c(1,1))
dev.off()

pdf(paste0(output_path, sprintf(
    "DistDiDY10T_zoomtop_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",
    preperiod,Tperiod,premw)))
plot(R,FY10TDDID,xlim=c(30,75),ylim=c(0.75,1),type="l",col="blue4",xlab="y",ylab="cdf",cex.lab=1.5,cex.axis=1.5,cex.lab=1.5)
lines(R,FY11TR,xlim=c(30,75),ylim=c(0.75,1),type="l",col="black",cex.lab=1.5,xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
legend("topleft",c("DistDiD","Obs"),cex=1.25,col=c("blue4","black"),lty=c(1,1))
dev.off()

pdf(paste0(output_path, sprintf(
    "FY00C_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",
    preperiod,Tperiod,premw)))
plot(R,FY00CR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
dev.off()

pdf(paste0(output_path, sprintf(
    "FY10C_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",
    preperiod,Tperiod,premw)))
plot(R,FY10CR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
dev.off()

pdf(paste0(output_path, sprintf(
    "FY00T_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",
    preperiod,Tperiod,premw)))
plot(R,FY00TR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
dev.off()


pdf(paste0(output_path, sprintf(
    "FY11T_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",
    preperiod,Tperiod,premw)))
plot(R,FY11TR,xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
dev.off()



 mY00Tn = -Y00Tn # Mahdi: What's this?
 mY00Cn = -Y00Cn

 FmY00T<-ecdf(mY00Tn)
 FmY00C<-ecdf(mY00Cn)

 FmY00CR = matrix(,nrow=1,ncol=length(R))

 Rm=c(-Inf, seq(-1-max(Y00Cn,Y00Tn,Y10Cn),1,0.01),Inf)

 for (i in seq(1,length(Rm),1)){
   FmY00CR[1,i]=FmY00C(Rm[i])
 }

# Ysupp00Cdiscrete = sort(unique(Y00Cn))
# Ysupp00Cdiscrete = sort(matrix(Ysupp00Cdiscrete,nrow=1,ncol=length(Ysupp00Cdiscrete)))
# FY00Cds  = matrix(,nrow=1,ncol=length(Ysupp00Cdiscrete))
# FmY00Cds = matrix(,nrow=1,ncol=length(Ysupp00Cdiscrete))
# Ysupp00Cdiscretem = sort(matrix(-Ysupp00Cdiscrete,nrow=1,ncol=length(Ysupp00Cdiscrete)))

# #discrete support
# for (i in seq(1,length(FY00Cds),1)){
#   FY00Cds[1,i]  = FY00C(Ysupp00Cdiscrete[i])
#   FmY00Cds[1,i] = FmY00C(Ysupp00Cdiscretem[i])
# }

#computing CS bounds on support
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
 
#limsup transformation for right-continuity -- Slide 17/29
FY10TLBCS  = matrix(,nrow=1,ncol=length(R))
FY10TLBCSm = matrix(,nrow=1,ncol=length(R))
FY10TUBCS  = matrix(,nrow=1,ncol=length(R))

for (i in 1:length(R)){
  FY10TLBCS [1,i] = max(FY10TLBY [,Ysupp1noInf<=R[i]])
  FY10TLBCSm[1,i] = max(FY10TLBmY[,Ysupp1noInf<=R[i]])
  FY10TUBCS [1,i] = max(FY10TUBY [,Ysupp1noInf<=R[i]])
}

FY10TCiC = FY10TUBCS

lowesty = min(min(Y10Cn),min(Y11Tn),min(Y00Cn),min(Y00Tn))

pdf(paste0(output_path, sprintf(
    "FTCSbounds_wages_Cengizetal2019_disaggregated06272024_%s-%s_%s.pdf",
    preperiod,Tperiod,premw)))
plot (R,FY10TLBCS, xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf",cex.axis=1.5,cex.lab=1.5)
lines(R,FY10TLBCSm,xlim=c(0,50),ylim=c(0,1),col="red",lty=1)
lines(R,FY10TUBCS, xlim=c(0,50),ylim=c(0,1),col="blue",lty=1)
lines(R,FY11TR,    xlim=c(0,50),ylim=c(0,1),type="l",col="black",xlab="y",ylab="cdf") # observations
legend("topleft",c("CS-LB","CS-UB", "Obs"),cex=1.25,col=c("red","blue","black"),
       lty=c(1,1,1))
dev.off()