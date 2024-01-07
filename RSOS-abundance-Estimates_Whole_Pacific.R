# make pooled 3-year estimates of abundance for the North Pacific from
# Chapman-Petersen comparisons of wintering areas to summering areas
# using balanced sampling to minimize geographic bias differences between estimates
set.seed(0)
library(ggplot2) # for plots
par(xaxs="i",yaxs="i",cex.axis=1.3,cex.lab=1.5,font=2,font.lab=2)
nJK= 20 #jackknife sample size (NOTE: cannot be zero)

# read data
# setwd("D:/Work/May 2023/Happywhales 3")
setwd("C:\\Jay\\SPLASH\\SPLASH-2\\Happywhale Pacific Data&Code FromTed")

AllData= read.csv(file="AllData2 woDupl in SeasonYear & MetaRegion.csv")
hist(AllData$DeltaDays)

# limit samples to within 90 days of peak season
# AllData= AllData[AllData$DeltaDays <= 90,]
# hist(AllData$DeltaDays)
Title= "All Areas, Jackknife Samples & +/- 2SE"
AllData$MetaRegion= as.factor(AllData$MetaRegion)
levels(AllData$MetaRegion)

# estimates based on 3 winters and 2 summers around mid-point year
MidYears= 2002:2021
EstDF= data.frame(MidYear=0,Jackknife=NA,Winter=NA,Summer=NA,Matches=NA,
                  Abundance=NA,SPLASHabund=NA,BiasFactor=NA,BiasCorrAbund=NA)
iSample= 0
AllData$JK_OutSample= ceiling(runif(length(AllData$enc_id))*nJK)

for (Jackknife in 0:nJK) {
  cat("Jackknife =",Jackknife,"\n")

# take jackknife sample from whole data set, leaving out 1/nJK th sample
# except for first pass (Jackknife = 0)
  JK_Data= AllData[AllData$JK_OutSample != Jackknife,]  

# using all samples (w/o balanced sampling)
  cat("\n Unbalanced geographic sampling:  \n")
for (iYear in MidYears) {
  # create samples from wintering and summering areas within pooled years
  WinterSample= JK_Data[JK_Data$WinterAreaTF,]
  WinterSample= WinterSample[(WinterSample$SeasonYear >= (iYear-1))&
                   (WinterSample$SeasonYear <= (iYear+1)),]
  SummerSample= JK_Data[!JK_Data$WinterAreaTF,]
  SummerSample= SummerSample[(SummerSample$SeasonYear >= (iYear))&
                     (SummerSample$SeasonYear <= (iYear+1)),]
  
  # eliminate duplicates in winter and summer samples
  WinterSample= WinterSample[!duplicated(WinterSample$ind_id),]
  SummerSample= SummerSample[!duplicated(SummerSample$ind_id),]
  
  # save splash year samples
  if (iYear == 2005) {
    SPLASHsample= rbind(WinterSample,SummerSample)
    SPLASHsampleSize= summary(SPLASHsample$MetaRegion)
  }
  
  # make Chapman-Petersen estimates from these two samples
  Nwinter= length(WinterSample$ind_id)
  Nsummer= length(SummerSample$ind_id)
  Nmatches= sum(duplicated(c(WinterSample$ind_id,SummerSample$ind_id)))
  Nest= 1 + ((Nwinter+1) * (Nsummer+1) / (Nmatches+1))
  cat(paste("Yr",iYear-1,"-",iYear+1,sep="")," Estimate=",Nest," Samples=",Nmatches,Nwinter,Nsummer,"\n")
}


# using balanced sampling (same size sample from each region for  pooled years)

cat("\n Balanced geographic sampling:  \n")
Regions= levels(JK_Data$MetaRegion)
for (iYear in MidYears) {
  # create samples from wintering and summering areas within pooled years
  WinterSample= JK_Data[JK_Data$WinterAreaTF,]
  WinterSample= WinterSample[(WinterSample$SeasonYear >= (iYear-1))&
                               (WinterSample$SeasonYear <= (iYear+1)),]
  SummerSample= JK_Data[!JK_Data$WinterAreaTF,]
  SummerSample= SummerSample[(SummerSample$SeasonYear >= (iYear))&
                               (SummerSample$SeasonYear <= (iYear+1)),]
  # eliminate duplicates in winter and summer samples
  WinterSample= WinterSample[!duplicated(WinterSample$ind_id),]
  SummerSample= SummerSample[!duplicated(SummerSample$ind_id),]
  iYearSample= rbind(WinterSample,SummerSample)
  iYearSampleSize= summary(iYearSample$MetaRegion)

  # estimate target sample size for each region based on the lesser of current sample and SPLASH sample
  TargetSampleSize= array()
  for (iRegion in 1:length(Regions)) {
    TargetSampleSize[iRegion]= min(SPLASHsampleSize[iRegion],iYearSampleSize[iRegion])
  }
  cat()
  cat(" SPLASH sample size  \n",SPLASHsampleSize,"\n")
  cat(" iYear  sample size  \n",iYearSampleSize,"\n")
  cat(" Largest common sample size w/ SPLASH \n",TargetSampleSize,"\n")
  
  # subsample regions with balanced sample equal to target sample size
  for (iRegion in 1:length(Regions)) {
    RegionSubset= iYearSample[iYearSample$MetaRegion==Regions[iRegion],]
    SubsetSize= TargetSampleSize[iRegion]
    SampleSize= length(RegionSubset$MetaRegion)  
    if (SubsetSize <= SampleSize) {
      index= sample(1:SampleSize,SubsetSize,replace=FALSE)
      SampleSubset= RegionSubset[index,]
    }
    if (iRegion == 1) {
      BalancedSample= SampleSubset
    } else {
      BalancedSample= rbind(BalancedSample,SampleSubset)
    }

  
    # subsample SPLASH to get balanced sample equal to target sample size
    RegionSubset= SPLASHsample[SPLASHsample$MetaRegion==Regions[iRegion],]
    SubsetSize= TargetSampleSize[iRegion]
    SampleSize= length(RegionSubset$MetaRegion)  
    if (SubsetSize <= SampleSize) {
      index= sample(1:SampleSize,SubsetSize,replace=FALSE)
      SampleSubset= RegionSubset[index,]
    }
    if (iRegion == 1) {
      BalancedSplashSample= SampleSubset
    } else {
      BalancedSplashSample= rbind(BalancedSplashSample,SampleSubset)
    }
  }

  # make population estimates from balanced samples
  WinterSample= BalancedSample[BalancedSample$WinterAreaTF,]
  SummerSample= BalancedSample[!BalancedSample$WinterAreaTF,]
  # remove duplicates within samples
  WinterSample= WinterSample[!duplicated(WinterSample$ind_id),]
  SummerSample= SummerSample[!duplicated(SummerSample$ind_id),]
  # make Chapman-Petersen estimates from these two samples
  Nwinter= length(WinterSample$ind_id)
  Nsummer= length(SummerSample$ind_id)
  Nmatches= sum(duplicated(c(WinterSample$ind_id,SummerSample$ind_id)))
  Nest= 1 + ((Nwinter+1) * (Nsummer+1) / (Nmatches+1))
  cat(iYear," Estimate=",Nest," Samples=",Nmatches,Nwinter,Nsummer,"\n")
 
  # make population estimates from balanced SPLASH samples
  WinterSample= BalancedSplashSample[BalancedSplashSample$WinterAreaTF,]
  SummerSample= BalancedSplashSample[!BalancedSplashSample$WinterAreaTF,]
  # remove duplicates within samples
  WinterSample= WinterSample[!duplicated(WinterSample$ind_id),]
  SummerSample= SummerSample[!duplicated(SummerSample$ind_id),]
  # make Chapman-Petersen estimates from these two samples
  Nwinter1= length(WinterSample$ind_id)
  Nsummer1= length(SummerSample$ind_id)
  Nmatches1= sum(duplicated(c(WinterSample$ind_id,SummerSample$ind_id)))
  Nest1= 1 + ((Nwinter1+1) * (Nsummer1+1) / (Nmatches1+1))
  cat(iYear," SPLASH Estimate=",Nest1," Samples=",Nmatches1,Nwinter1,Nsummer1,"\n")

  # estimate bias corrections and write estimates to dataframe.
  iSample= iSample + 1
  BiasCorrFactor= 21063/Nest1
  BiasCorrAbund= Nest * BiasCorrFactor
  EstDF[iSample,]= c(iYear,Jackknife,Nwinter,Nsummer,Nmatches,Nest,Nest1,BiasCorrFactor,BiasCorrAbund)
  
}
}

write.csv(EstDF,file=paste("Estimates wo MidSeasonFilter for all areas w JK-SE",".csv",sep=""))

#estimate standard errors from jackknife samples
JK_EstDF= EstDF[EstDF$Jackknife != 0,]
JK_MeanAbund= array()
JK_StdErr= array()
for (iYear in 1:length(MidYears)) {
  JK_abund= JK_EstDF$BiasCorrAbund[JK_EstDF$MidYear == MidYears[iYear]]
  JK_MeanAbund[iYear]= mean(JK_abund)
  JK_StdErr[iYear]= sqrt( sum((JK_abund-JK_MeanAbund[iYear])^2) * (nJK-1)/nJK )
}

# create estimate dataframe with jackknife SE
EstDFwSE= EstDF[EstDF$Jackknife==0,]

# add a component of variance from the SPLASH abund estimate (CV= 0.04) to the JK_StdErr
CVsplash= 0.04
JK_CV= JK_StdErr / EstDFwSE$BiasCorrAbund
Tot_CV= sqrt(CVsplash*CVsplash + JK_CV*JK_CV)
Tot_SE= Tot_CV * EstDFwSE$BiasCorrAbund
plot(Tot_SE,JK_StdErr)

# plot long time series abundance estimates without title
fict.data <- data.frame(Year = c("1975", "1993"), Abund = c(1300, 7005))
svg(filename="LongTimeSeriesAbundEstimates.svg",width=10,height=6,pointsize=14)
Title= NA
plotxlim <- c(1974,2022)
plot(c(1975, 1993, as.numeric(EstDFwSE$MidYear)), 
     c(1300, 7005, as.numeric(EstDFwSE$BiasCorrAbund)),main=Title,
     ylim=c(0,52000),xlim=plotxlim,pch=20,xlab=expression(bold("Mid-sample year")),
     ylab=expression(bold("Abundance")), xaxt = "none", col = "black")
abline(v = 1974:2022, h = seq(0, 50000, 10000), col = "lightgray", lty = "dotted",
       lwd = par("lwd"))
axis(side = 1, at = 1974:2023)
# points(c(1975, 1993, MidYears), c(1300, 7005, EstDFwSE$BiasCorrAbund),pch=20,col="red",cex=1)
MAline <- spline(x = MidYears, 
                 y = c(head(EstDFwSE$BiasCorrAbund, 1), 
                       na.omit(frollmean(EstDFwSE$BiasCorrAbund, 3, align = "center")), 
                       tail(EstDFwSE$BiasCorrAbund, 1)), 
                 n = 10 * diff(range(MidYears)))
lines(MAline,
      col="red", lwd = 3) ### solid lines
lines(c(1975, 1993, 2002), 
      c(1300, 7005, EstDFwSE$BiasCorrAbund[1]), 
      lty = 3, col = "red") ### dashed lines
# plot 2*SE bars
lines(c(1975, 1975), c(1200, 1400))
lines(c(1993, 1993), c(6010, 8000))
for (iYear in 1:length(MidYears)) {
  lines(c(MidYears[iYear],MidYears[iYear]),
        c(EstDFwSE$BiasCorrAbund[iYear]-2*Tot_SE[iYear],
          EstDFwSE$BiasCorrAbund[iYear]+2*Tot_SE[iYear]),
        col = "#777777",lwd=2)
}
dev.off()

# plot short time series abundance estimates without title
svg(filename="ShortTimeSeriesAbundEstimates.svg",width=10,height=6,pointsize=14)
Title= NA
plotxlim <- c(2001,2022)
plot(c(as.numeric(EstDFwSE$MidYear)), 
     c(as.numeric(EstDFwSE$BiasCorrAbund)),main=Title,
     ylim=c(0,52000),xlim=plotxlim,pch=20,xlab=expression(bold("Mid-sample year")),
     ylab=expression(bold("Abundance")), xaxt = "none", col = "black")
abline(v = 2001:2023, h = seq(0, 50000, 10000), col = "lightgray", lty = "dotted",
       lwd = par("lwd"))
axis(side = 1, at = 2001:2022)
MAline <- spline(x = MidYears, 
                 y = c(head(EstDFwSE$BiasCorrAbund, 1), 
                       na.omit(frollmean(EstDFwSE$BiasCorrAbund, 3, align = "center")), 
                       tail(EstDFwSE$BiasCorrAbund, 1)), 
                 n = 10 * diff(range(MidYears)))
lines(MAline,
      col="red", lwd = 3) ### solid lines

# plot 2*SE bars
for (iYear in 1:length(MidYears)) {
  lines(c(MidYears[iYear],MidYears[iYear]),
        c(EstDFwSE$BiasCorrAbund[iYear]-2*Tot_SE[iYear],
          EstDFwSE$BiasCorrAbund[iYear]+2*Tot_SE[iYear]),
        col = "#777777",lwd=2)
}
dev.off()

# add estimate of total standard error to the estimate dataframe
EstDFwSE$SE= Tot_SE
EstDFwSE= subset(EstDFwSE,select= -2)
setDT(EstDFwSE)
EstDFwSE[i = , j = BiasCorrAbundMA := frollmean(BiasCorrAbund, 3, align = "center"), by = ]
write.csv(EstDFwSE,file="NPacAbundEst wJK-SE v3 woMidSeasonFilter.csv")
# use jackknife mean values

MeanRawAbund= by(EstDF$Abundance,EstDF$MidYear,mean)
as.numeric(MeanRawAbund)
MeanBC_Abund= by(EstDF$BiasCorrAbund,EstDF$MidYear,mean)
as.numeric(MeanBC_Abund)

save.image("WholePacificEnvSave woMidSeasonFilter.RData")
