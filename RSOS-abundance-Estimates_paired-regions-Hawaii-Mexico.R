# make pooled 3-year estimates of abundance for the North Pacific from
# Chapman-Petersen comparisons of wintering areas to summering areas
# using balanced sampling to minimize geographic bias differences between estimates
library(data.table)
set.seed(0)
par(xaxs="i",yaxs="i",cex.axis=1.3,cex.lab=1.5,font=2,font.lab=2)
nJK= 20 #jackknife sample size

# read data
# setwd("D:/Work/May 2023/Happywhales 3")
# AllData= read.csv(file="AllData2 woDupl in SeasonYear & MetaRegion.csv")
setwd("C:\\Jay\\SPLASH\\SPLASH-2\\Happywhale Pacific Data&Code FromTed")
AllData= read.csv(file="AllData2 woDupl in SeasonYear & MetaRegion.csv")

# limit samples to within 90 days of peak season
# AllData= AllData[AllData$DeltaDays <= 90,]

# include only CA&OR and Mexico Mainland
AllData1= AllData[AllData$MetaRegion %in% c("CA&OR","MexMld"),]
cat("\n CA&OR vs MexMld only \n")
Title= "CA&OR vs MexMld- JK Samples & 2SE"
MidYears= 2002:2021

# estimates based on 3 winters and 2 summers around mid-point year
EstDF1= data.frame(MidYear=0,Jackknife=NA,Abundance=NA,Matches=NA,
                  Winter=NA,Summer=NA)
iSample= 0
Jackknife= 0
AllData1$JK_OutSample= ceiling(runif(length(AllData1$enc_id))*nJK)

for (Jackknife in 0:nJK) {
JK_Data= AllData1[AllData1$JK_OutSample != Jackknife,]  #take jackknife sample leaving 10% out
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
  
  # make Chapman-Petersen estimates from these two samples
  Nwinter= length(WinterSample$ind_id)
  Nsummer= length(SummerSample$ind_id)
  Nmatches= sum(duplicated(c(WinterSample$ind_id,SummerSample$ind_id)))
  Nest= 1 + ((Nwinter+1) * (Nsummer+1) / (Nmatches+1))
  cat(paste("Yr",iYear,"-",iYear+1,sep="")," Estimate=",Nest," Samples=",Nmatches,Nwinter,Nsummer,"\n")
  iSample= iSample + 1
  EstDF1[iSample,]= c(iYear,Jackknife,Nest,Nmatches,Nwinter,Nsummer)
}
}

write.csv(EstDF1,file=paste("Estimates for",Title,".csv",sep=""))

#estimate standard errors from jackknife samples
JK_EstDF1= EstDF1[EstDF1$Jackknife != 0,]
JK_MeanAbund= array()
JK_StdErr= array()
for (iYear in 1:length(MidYears)) {
  JK_abund= JK_EstDF1$Abundance[JK_EstDF1$MidYear == MidYears[iYear]]
  JK_MeanAbund[iYear]= mean(JK_abund)
  JK_StdErr[iYear]= sqrt( sum((JK_abund-JK_MeanAbund[iYear])^2) * (nJK-1)/nJK )
}

# create estimate dataframe with jackknife SE
EstDF1wSE= EstDF1[EstDF1$Jackknife==0,]
EstDF1wSE$SE= JK_StdErr
EstDF1wSE$L_CI= EstDF1wSE$Abundance - 2*JK_StdErr
EstDF1wSE$U_CI= EstDF1wSE$Abundance + 2*JK_StdErr
EstDF1wSE= subset(EstDF1wSE,select= -2)

### moving average estimates
setDT(EstDF1wSE)
EstDF1wSE[i = , j = AbundanceMA := round(frollmean(Abundance, n = 3, align = "center")), by = ]
write.csv(EstDF1wSE,file=paste(Title,".csv"))

# create plot
svg(filename="Mexico vs CAOR Abundance.svg",width=10,height=6,pointsize=14)
Title=NA
plotxlim <- c(2001,2022)
plot(as.numeric(EstDF1wSE$MidYear), as.numeric(EstDF1wSE$Abundance), main = Title,
     xlim = plotxlim, ylim = c(0,max(EstDF1wSE$Abundance)*1.5),xaxt = "none",
     pch = 20, xlab = expression(bold("Mid-sample year")), ylab = expression(bold("Relative abundance")), col = "black")
axis(side = 1, at = MidYears)
abline(v = seq(range(MidYears)[1] - 1, range(MidYears)[2] + 1), h = seq(0, 50000, 2000), col = "lightgray", lty = "dotted",
       lwd = 1)

MAline <- spline(x = MidYears, 
                 y = c(head(EstDF1wSE$Abundance, 1), 
                       na.omit(frollmean(EstDF1wSE$Abundance, 3, align = "center")), 
                       tail(EstDF1wSE$Abundance, 1)), 
                 n = 10 * diff(range(MidYears)))
lines(MAline,
      col="red", lwd = 3)

for (iYear in 1:length(MidYears)) {
  lines(c(MidYears[iYear],MidYears[iYear]),
        c(EstDF1wSE$Abundance[iYear]-2*EstDF1wSE$SE[iYear],
          EstDF1wSE$Abundance[iYear]+2*EstDF1wSE$SE[iYear]),col = "#777777",lwd=2)
}
dev.off()
MeanRawAbund= by(EstDF1$Abundance,EstDF1$MidYear,mean)
as.numeric(MeanRawAbund)

#...............................................................................
# include only SEAK&NorthBC and Hawaii
AllData2= AllData[AllData$MetaRegion %in% c("SEAK&NorthBC","Hawaii"),]
cat("\n HAWAII vs SEAK&NBC only \n")
Title= "SEAK&NBC vs Hawaii- JK Samples & 2SE"
MidYears= 2002:2021

# estimates based on 3 winters and 2 summers around mid-point year
EstDF2= data.frame(MidYear=0,Jackknife=NA,Abundance=NA,Matches=NA,
                   Winter=NA,Summer=NA)
iSample= 0
Jackknife= 0
AllData2$JK_OutSample= ceiling(runif(length(AllData2$enc_id))*nJK)

for (Jackknife in 0:nJK) {
  JK_Data= AllData2[AllData2$JK_OutSample != Jackknife,]  #take jackknife sample leaving 10% out
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
    
    # make Chapman-Petersen estimates from these two samples
    Nwinter= length(WinterSample$ind_id)
    Nsummer= length(SummerSample$ind_id)
    Nmatches= sum(duplicated(c(WinterSample$ind_id,SummerSample$ind_id)))
    Nest= 1 + ((Nwinter+1) * (Nsummer+1) / (Nmatches+1))
    cat(paste("Yr",iYear,"-",iYear+1,sep="")," Estimate=",Nest," Samples=",Nmatches,Nwinter,Nsummer,"\n")
    iSample= iSample + 1
    EstDF2[iSample,]= c(iYear,Jackknife,Nest,Nmatches,Nwinter,Nsummer)
  }
}

write.csv(EstDF2,file=paste("Estimates for",Title,".csv",sep=""))

#estimate standard errors from jackknife samples
JK_EstDF2= EstDF2[EstDF2$Jackknife != 0,]
JK_MeanAbund= array()
JK_StdErr= array()
for (iYear in 1:length(MidYears)) {
  JK_abund= JK_EstDF2$Abundance[JK_EstDF2$MidYear == MidYears[iYear]]
  JK_MeanAbund[iYear]= mean(JK_abund)
  JK_StdErr[iYear]= sqrt( sum((JK_abund-JK_MeanAbund[iYear])^2) * (nJK-1)/nJK )
}

# create estimate dataframe with jackknife SE
EstDF2wSE= EstDF2[EstDF2$Jackknife==0,]
EstDF2wSE$SE= JK_StdErr
EstDF2wSE$L_CI= EstDF2wSE$Abundance - 2*JK_StdErr
EstDF2wSE$U_CI= EstDF2wSE$Abundance + 2*JK_StdErr
EstDF2wSE= subset(EstDF2wSE,select= -2)

### moving average estimates
setDT(EstDF2wSE)
EstDF2wSE[i = , j = AbundanceMA := round(frollmean(Abundance, n = 3, align = "center")), by = ]
write.csv(EstDF2wSE,file=paste(Title,".csv"))

Title=NA
svg(filename="Hawaii vs SEAK&NBC Abundance.svg",width=10,height=6,pointsize=14)
Title=NA
plotxlim <- c(2001,2022)
plot(as.numeric(EstDF2wSE$MidYear), as.numeric(EstDF2wSE$Abundance), main = Title,
     xlim = plotxlim, ylim = c(0,max(EstDF2wSE$Abundance)*1.5),xaxt = "none",
     pch = 20, xlab = expression(bold("Mid-sample year")), ylab = expression(bold("Relative abundance")), col = "black")
axis(side = 1, at = MidYears)
abline(v = seq(range(MidYears)[1] - 1, range(MidYears)[2] + 1), h = seq(0, 50000, 2000), col = "lightgray", lty = "dotted",
       lwd = 1)

MAline <- spline(x = MidYears, 
                 y = c(head(EstDF2wSE$Abundance, 1), 
                       na.omit(frollmean(EstDF2wSE$Abundance, 3, align = "center")), 
                       tail(EstDF2wSE$Abundance, 1)), 
                 n = 10 * diff(range(MidYears)))
lines(MAline,
      col="red", lwd = 3)

for (iYear in 1:length(MidYears)) {
  lines(c(MidYears[iYear],MidYears[iYear]),
        c(EstDF2wSE$Abundance[iYear]-2*EstDF2wSE$SE[iYear],
          EstDF2wSE$Abundance[iYear]+2*EstDF2wSE$SE[iYear]),col = "#777777",lwd=2)
}
dev.off()

MeanRawAbund= by(EstDF2$Abundance,EstDF2$MidYear,mean)
as.numeric(MeanRawAbund)

# plot Hawaii and Mexico relative abundance together
svg(filename="Hawaii & Mex Relative Abundance.svg",width=10,height=6,pointsize=14)
Title=NA
plotxlim <- c(2001,2022)
plot(as.numeric(EstDF2wSE$MidYear), as.numeric(EstDF2wSE$Abundance), main = Title,
     xlim = plotxlim, ylim = c(0,25000),xaxt = "none",
     pch = 20, xlab = expression(bold("Mid-sample year")), 
     ylab = expression(bold("Relative abundance")), col = "red")
points(as.numeric(EstDF1wSE$MidYear), as.numeric(EstDF1wSE$Abundance),
       pch=20,col="black")
axis(side = 1, at = MidYears)
abline(v = seq(range(MidYears)[1] - 1, range(MidYears)[2] + 1), h = seq(0, 50000, 2000), col = "lightgray", lty = "dotted",
       lwd = 1)
MAline <- spline(x = MidYears, 
                 y = c(head(EstDF2wSE$Abundance, 1), 
                       na.omit(frollmean(EstDF2wSE$Abundance, 3, align = "center")), 
                       tail(EstDF2wSE$Abundance, 1)), 
                 n = 10 * diff(range(MidYears)))
lines(MAline,
      col="red", lwd = 3)
MAline <- spline(x = MidYears, 
                 y = c(head(EstDF1wSE$Abundance, 1), 
                       na.omit(frollmean(EstDF1wSE$Abundance, 3, align = "center")), 
                       tail(EstDF1wSE$Abundance, 1)), 
                 n = 10 * diff(range(MidYears)))
lines(MAline,
      col="black", lwd = 3)

for (iYear in 1:length(MidYears)) {
  lines(c(MidYears[iYear],MidYears[iYear]),
        c(EstDF2wSE$Abundance[iYear]-2*EstDF2wSE$SE[iYear],
          EstDF2wSE$Abundance[iYear]+2*EstDF2wSE$SE[iYear]),col = "#777777",lwd=2)
  lines(c(MidYears[iYear],MidYears[iYear]),
        c(EstDF1wSE$Abundance[iYear]-2*EstDF1wSE$SE[iYear],
          EstDF1wSE$Abundance[iYear]+2*EstDF1wSE$SE[iYear]),col = "#777777",lwd=2)
}
dev.off()


# plot the ratio of relative abundance for Hawaii vs Hawaii plus Mexico
Ratio= EstDF2wSE$Abundance / (EstDF1wSE$Abundance+EstDF2wSE$Abundance)
svg(filename="ProportionHawaii.svg",width=6,height=6,pointsize=14)
plot(MidYears,Ratio,xlab=expression(bold("Mid-sample year")),ylab=expression(bold("Proportion Hawai'i")),
     xlim=c(2001,2022),ylim=c(0,1),type="p",pch=20)
Regress= lm(Ratio~1+MidYears)
lines(MidYears,Regress$fitted.values,lty="dashed",col="red")
dev.off()

# Exploratory analyses were completed with alternative subsets based on:

# # include only SEAK and Hawaii
# AllData= AllData[AllData$MetaRegion %in% c("SEAK","Hawaii"),]
# cat("\n HAWAII vs SEAK only \n")
# Title= "SEAK vs Hawaii- JK Samples & 2SE"
# MidYears= 2002:2021

# # include only NorthBC and Hawaii
# AllData= AllData[AllData$MetaRegion %in% c("NorthBC","Hawaii"),]
# cat("\n HAWAII vs NBC only \n")
# Title= "NBC vs Hawaii- JK Samples & 2SE"
# MidYears= 2002:2020

# include only CA&OR and Mexico Mainland/Central America
# AllData= AllData[AllData$MetaRegion %in% c("CA&OR","MexMld","CenAm_SMex"),]
# cat("\n CA&OR vs MexMld/CenAm_SMex only \n")
# Title= "CA&OR vs MexMld./CenAm_SMex- JK Samples & 2SE"
# MidYears= 2002:2020

# # include only WPac and Kamchatka and WBerSea
# AllData= AllData[AllData$MetaRegion %in% c("Kamchatka","WBerSea","WPac"),]
# cat("\n Kamchatka WBerSea and WPac only \n")
# Title= "Rus&WBerSea vs WPac- JK Samples & 2SE"
# MidYears= c(2005,2009:2018)

# # include only WPac and WBerSea
# AllData= AllData[AllData$MetaRegion %in% c("WBerSea","WPac"),]
# cat("\n WBerSea and WPac only \n")
# Title= "WBerSea vs WPac- JK Samples & 2SE"
# MidYears= c(2005,2009:2018)

# include only WPac and Kamchatka
# AllData= AllData[AllData$MetaRegion %in% c("Kamchatka","WPac"),]
# cat("\n Kamchatka and WPac only \n")
# Title= "Rus vs WPac- JK Samples & 2SE"
# MidYears= c(2005,2009,2015,2018)
