# pre-process HappyWhale data to create region & season strata
#  whale season-year is based on year, with Nov-Dec lumped with next year
library(lubridate)
library(data.table)
library(readxl)
library(ggplot2)
library(RColorBrewer)
# input mid-season date for each region 
MidSeason= c("08-01","03-01")  #corresponding to March 1 and August 1, per SPLASH protocols

# setwd("C:\\Jay\\SPLASH\\SPLASH-2\\HappyWhaleWholeDataset")
# AllData= read.csv("all-n-pacific-humpbacks-through-2021-from-2022-01-14.csv")
# setwd("C:\\Jay\\SPLASH\\SPLASH-2\\HappyWhaleWholeDataset\\Feb2022Data\\2022-02-14-dataset")
# AllData= read.csv("all-n-pacific-humpbacks-through-2021-from-2022-02-14.csv")
# setwd("D:/Work/July 2023/Happywhales 4/Data")
# setwd("C:\\Jay\\SPLASH\\SPLASH-2\\Happywhale Pacific Data&Code FromTed")
# AllData <- read_excel("2023-07-all N Pacific data.xlsx") |>
#   as("data.table")
setwd("/Users/tedcheeseman/Documents/work/R")
AllData <- read_excel("2024-01-Heather-Riley-all-matches.xlsx") |>
  as("data.table")

# convert region variable to factor and summarize
AllData$enc_region= as.factor(AllData$enc_region)
summary(AllData$enc_region)

# create datetime variables
AllData$Date <- as.POSIXct(AllData$enc_date, "%Y-%m-%d", tz="GMT")  
AllData$Year <- as.integer(substr(AllData$enc_date, 1, 4))
summary(AllData$Date)
summary(AllData$Year)
  
### MetaRegion based on enc_region
### enc_region dictionary
enc_region_dict <- data.table(Key = c("Russia", "Alaska", "Oregon", "(^((?!Baja).)*California)", "Mexico", "Nicaragua", "Guatemala",
                                      "Salvador", "Costa Rica", "Panama", "Hawaii", "Canada", "Washington", "Phillippines",
                                      "Mariana", "Japan"),
                              Value = c("Russia", "Alaska", "CA&OR", "CA&OR", "Mexico", rep("CenAm", 5),
                                        "Hawaii", "Canada", "SouthBC&WA", rep("WPac", 3)))

index_value <- lapply(enc_region_dict$Key, function(X) data.table(INDEX = grep(X, AllData$enc_region, perl = TRUE))) |>
  setNames(enc_region_dict$Value) |>
  rbindlist(idcol = "MetaRegion")
nrow(index_value) ### total number of matches (w/duplicates)
length(unique(index_value$INDEX)) ### unique indices

index_duplicates <- index_value[i = , j = .N, by = INDEX][i = N > 1, j = ] |>
  merge(index_value)

AllData[i = unique(index_duplicates$INDEX), j = unique(enc_region), by = ]

AllData$MetaRegion= NA
Index <- grep(pattern="Russia", AllData$enc_region)
AllData$MetaRegion[Index] <- "Russia"
Index <- grep(pattern="Alaska", AllData$enc_region)
AllData$MetaRegion[Index] <- "Alaska"
Index <- grep(pattern="Oregon", AllData$enc_region)
AllData$MetaRegion[Index] <- "CA&OR"
Index <- grep("(^((?!Baja).)*California)", AllData$enc_region, perl = TRUE)
AllData$MetaRegion[Index] <- "CA&OR"
Index <- grep(pattern="Mexico", AllData$enc_region)
AllData$MetaRegion[Index] <- "Mexico"
Index <- grep(pattern="Nicaragua", AllData$enc_region)
AllData$MetaRegion[Index] <- "CenAm"
Index <- grep(pattern="Guatemala", AllData$enc_region)
AllData$MetaRegion[Index] <- "CenAm"
Index <- grep(pattern="Salvador", AllData$enc_region)
AllData$MetaRegion[Index] <- "CenAm"
Index <- grep(pattern="Costa Rica", AllData$enc_region)
AllData$MetaRegion[Index] <- "CenAm"
Index <- grep(pattern="Panama", AllData$enc_region)
AllData$MetaRegion[Index] <- "CenAm"
Index <- grep(pattern="Hawaii", AllData$enc_region)
AllData$MetaRegion[Index] <- "Hawaii"
Index <- grep(pattern="Canada", AllData$enc_region)
AllData$MetaRegion[Index] <- "Canada"
Index <- grep(pattern="Washington", AllData$enc_region)
AllData$MetaRegion[Index] <- "SouthBC&WA"
Index <- grep(pattern="Philippines", AllData$enc_region)
AllData$MetaRegion[Index] <- "WPac"
Index <- grep(pattern="Mariana", AllData$enc_region)
AllData$MetaRegion[Index] <- "WPac"
Index <- grep(pattern="Japan", AllData$enc_region)
AllData$MetaRegion[Index] <- "WPac"

### MetaRegion based on enc_loc_verbatim
### enc_loc_region dictionary

enc_loc_verbatim_dict <- data.table(Key = c("Revillagigedo", "Partida", "Glacier"),
                                  Value = c("MexIsl", "MexIsl", "SEAK&NorthBC"))

index_value <- lapply(enc_loc_verbatim_dict$Key, function(X) data.table(INDEX = grep(X, AllData$enc_loc_verbatim, perl = TRUE))) |>
  setNames(enc_loc_verbatim_dict$Value) |>
  rbindlist(idcol = "MetaRegion")
nrow(index_value) ### total number of matches (w/duplicates)
length(unique(index_value$INDEX)) ### unique indices

index_duplicates <- index_value[i = , j = .N, by = INDEX][i = N > 1, j = ] |>
  merge(index_value)

Index <- grep(pattern = "Revillagigedo", AllData$enc_loc_verbatim)
AllData$MetaRegion[Index] <- "MexIsl"
Index <- grep(pattern = "Partida", AllData$enc_loc_verbatim)
AllData$MetaRegion[Index] <- "MexIsl"
Index <- grep(pattern = "Glacier", AllData$enc_loc_verbatim)
AllData$MetaRegion[Index] <- "SEAK&NorthBC"

AllData[i = unique(index_duplicates$INDEX), j = unique(enc_loc_verbatim), by = ]

# MetaRegion based at least partially on Lat/Lon
# Combine southern Mexico with Central America

### Igor: changed latitude border from 19.2 to 20
Index <- which(
  (AllData$MetaRegion == "Mexico" & AllData$enc_lat <= 20) | AllData$MetaRegion == "CenAm"
  )
AllData$MetaRegion[Index] <- "CenAm_SMex"

# Include southern BC with Washington
### IGOR: the border between South and North British Columbia is not well-defined and,
### on top of that, does not run all the way through a single latitude.
### The coastal border might be anywhere between the Northern tip of Vancouver Island (50.9 N)

Index <- which(AllData$MetaRegion == "Canada" & AllData$enc_lat <= 51.0)   #lat is only approx.
AllData$MetaRegion[Index] <- "SouthBC&WA"
Index <- which(AllData$MetaRegion == "Canada")   
AllData$MetaRegion[Index] <- "SEAK&NorthBC"

# Separate offshore (Commander Is & western Bering) from Russia
Index <- which(AllData$MetaRegion == "Russia" & AllData$enc_lat <= 56.0 & AllData$enc_long > 164.5)  
AllData$MetaRegion[Index] <- "WBerSea"

# Combine Russian Bering Strait w/ Eastern Bering Sea 
Index <- which(AllData$MetaRegion == "Russia" & AllData$enc_long < 0)  
AllData$MetaRegion[Index] <- "E&NBerSea"

# Combine Russia coast north of Kamchatka with the eastern Bering
Index <- which(AllData$MetaRegion == "Russia" & AllData$enc_long > 172)  
AllData$MetaRegion[Index] <- "E&NBerSea"

# Remainder of Russia is Kamchatka
Index <- which(AllData$MetaRegion == "Russia")  
AllData$MetaRegion[Index] <- "Kamchatka"

# Separate Alaska into regions
# Separate BerE from Alaska
Index <- which(AllData$MetaRegion == "Alaska" & AllData$enc_long > 0.0)  
AllData$MetaRegion[Index] <- "WBerSea"
# Separate SEAk from Alaska
Index <- which(AllData$MetaRegion == "Alaska" & AllData$enc_long > -141.0)  
AllData$MetaRegion[Index] <- "SEAK&NorthBC"
# Separate AlBer from Alaska
Index <- which(AllData$MetaRegion == "Alaska" & AllData$enc_long < -157.0)  
AllData$MetaRegion[Index] <- "E&NBerSea"
# Remainder of Alaska is Gulf of AK
Index <- which(AllData$enc_long > -157.0 & AllData$enc_long < -141.0 & AllData$enc_lat > 50.0)  
AllData$MetaRegion[Index] <- "GulfOfAK"

# Separate Baja from Mexico
Index <- which(AllData$MetaRegion == "Mexico" & AllData$enc_long < -108.4)  
AllData$MetaRegion[Index] <- "MexBajaCal"
# Remainder of Mexico is mainland
Index <- which(AllData$MetaRegion == "Mexico")  
AllData$MetaRegion[Index] <- "MexMld"

### IGOR: here, I explicitly remove MexIsl whales above 20 N
AllData[i = MetaRegion == "MexIsl" &enc_lat > 20, j = MetaRegion := NA , by = ]

# Add Color codes to avoid same color in adjacent area
AllData$Color= NA
AllData$Color[AllData$MetaRegion=="CenAm_SMex"]= 12
AllData$Color[AllData$MetaRegion=="MexMld"]= 11
AllData$Color[AllData$MetaRegion=="MexBajaCal"]= 9
AllData$Color[AllData$MetaRegion=="MexIsl"]= 10
AllData$Color[AllData$MetaRegion=="CA&OR"]= 8
AllData$Color[AllData$MetaRegion=="SouthBC&WA"]= 7
AllData$Color[AllData$MetaRegion=="SEAK&NorthBC"]= 5
AllData$Color[AllData$MetaRegion=="GulfOfAK"]= 4
AllData$Color[AllData$MetaRegion=="E&NBerSea"]= 3
AllData$Color[AllData$MetaRegion=="WBerSea"]= 2
AllData$Color[AllData$MetaRegion=="Kamchatka"]= 1
AllData$Color[AllData$MetaRegion=="Hawaii"]= 13
AllData$Color[AllData$MetaRegion=="WPac"]= 14

# Eliminate whales from Western Pacific migr route
Index= which(AllData$enc_long > 0.0 & AllData$enc_lat > 40 & AllData$enc_lat < 48)  
AllData$MetaRegion[Index]= NA
AllData$Color[Index]= NA

### AllData$Colour as factor
AllData$Color <- as.factor(AllData$Color)

AllData$MetaRegion= as.factor(AllData$MetaRegion)
summary(AllData$MetaRegion)
outcasts= AllData[is.na(AllData$MetaRegion),]
AllData= AllData[!is.na(AllData$MetaRegion),]

# convert east longitude to west and plot detection locations
AllData$enc_long[is.na(AllData$enc_long)]= 0
AllData$enc_long[AllData$enc_long>0]= AllData$enc_long[AllData$enc_long>0] - 360
#plot(AllData$enc_long,AllData$enc_lat,col=AllData$Color,pch=".")
# plot(AllData$enc_long,AllData$enc_lat,col=AllData$Color,pch=20,xlim=c(-250,-80),
#      xlab="West Longitude",ylab="North Latitude",main="Sample Distr by Region")

Regions= levels(AllData$MetaRegion)

# define winter areas (and by default, summer areas)
AllData$WinterAreaTF= FALSE
AllData$WinterAreaTF[AllData$MetaRegion %in% 
            c("CenAm_SMex","Hawaii","MexBajaCal","MexIsl","MexMld","WPac"  )]= TRUE

# create new varaible season-year to include Nov-Dec with next year for wintering areas
AllData$SeasonYear= AllData$Year
#if late in the year (after day 250), winter whale year is Year plus 1
AllData$SeasonYear[yday(AllData$Date)>250 & AllData$WinterAreaTF]= AllData$SeasonYear[yday(AllData$Date)>250 & AllData$WinterAreaTF]+1

# output a table of raw sample sizes by SeasonYear and MetaRegion
output= table(AllData$SeasonYear,AllData$MetaRegion)
write.csv(output,file="TableByMetaRegion&SeasonYear wDuplicates.csv")

# determine time in days from mid-season date
iRegion= AllData$WinterAreaTF + 1  # iRegion= 1 for summer 2 for winter
AllData$MidSeason= as.POSIXct(paste(AllData$SeasonYear,MidSeason[iRegion],sep="-"),format="%Y-%m-%d")
#calculate the number of days before or after mid-season and show histogram
AllData$DeltaDays= abs(as.numeric(difftime(AllData$Date,AllData$MidSeason,units="days")))
hist(AllData$DeltaDays,breaks=30)
AllData$SeasonYear= as.factor(AllData$SeasonYear)

# Eliminate data without individual ID
AllData= AllData[!is.na(AllData$ind_id),]
# Save data with duplicates within season year and meta region
write.csv(AllData,file="AllData wDupl in SeasonYear & MetaRegion.csv")

# find unique IDs within each MetaRegion/Year stratum and keep the
#    one that is closest to mid-season
# Note this section is slow and inefficient, but I found it to be intuitive
# for (iYear in levels(AllData$SeasonYear)) {
#   for (iRegion in levels(AllData$MetaRegion)) {
#     Index= which(AllData$MetaRegion==iRegion & AllData$SeasonYear==iYear)
#     AllDataStratum= AllData[Index,]
#     # Find duplicated individual ids
#     UniqueIds= unique(AllDataStratum$ind_id)
#     # find sample closest in time to mid-season for each unique ID
#     cat(" Eliminating duplicates for Region:",iRegion," Year:",iYear,"\n")
#     for (ID in UniqueIds) {
#       IDencounters= AllDataStratum[AllDataStratum$ind_id == ID,]
#       MidSample= which.min(IDencounters$DeltaDays)
#       nNew= nNew + 1
#       AllData2[nNew,]= IDencounters[MidSample,] #many warnings, can be ignored
#     }
#   }
# }

### This section takes 45 ~ 60 sec to run 
AllDatawoDupl <- split(AllData, list(AllData$SeasonYear, AllData$MetaRegion)) |>
  lapply(function(X) split(X, X$ind_id)) |>
  unlist(recursive = FALSE) |>
  lapply(function(X) X[i = which.min(DeltaDays), j = , by = ]) |>
  rbindlist()

hist(AllDatawoDupl$DeltaDays, breaks = 30)
fwrite(AllDatawoDupl, file = "AllData2 woDupl in SeasonYear & MetaRegion.csv")
# AllData2= read.csv(file="AllData2 woDupl in SeasonYear & MetaRegion.csv")
# 

# output a table of raw sample sizes by SeasonYear and MetaRegion
output <- table(AllDatawoDupl$SeasonYear,AllDatawoDupl$MetaRegion)
fwrite(output, file = "SummaryByMetaRegion&SeasonYear woDuplicates.csv")
write.csv(output, file = "TableByMetaRegion&SeasonYear woDuplicates.csv")

# convert east longitude to west and plot detection locations
AllDatawoDupl$enc_long[AllDatawoDupl$enc_long > 0] <- AllDatawoDupl$enc_long[AllDatawoDupl$enc_long > 0] - 360
#plot(AllData$enc_long,AllData$enc_lat,col=AllData$Color,pch=".")
svg(filename="Map_wo_SeasonAreaDuplicates.svg",width=7.25,height=3.75,pointsize=16)

map_data("world") |>
  as.data.table() |>
  _[i = long > 0, j = long := long - 360, by = ] |>
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_path() +
  lims(x = c(-250, -50), y = c(-10, 80)) +
  geom_point(data = AllDatawoDupl, aes(x = enc_long, y = enc_lat, col = MetaRegion, group = MetaRegion), pch = 20, cex = 3) +
  scale_colour_manual(values = c(brewer.pal(9, "Set1"), brewer.pal(5, "Set2")), guide = "none") +
  theme_bw()
dev.off()

# ### Checking if my code produces an identical result compared to original code
# AllData= read.csv(file="AllData2 woDupl in SeasonYear & MetaRegion.csv") 
# setDT(AllData)
# X1 <- AllData[, -1][order(enc_id)][, list(enc_id, enc_lat, enc_long, month, day, year, ind_id, DeltaDays)] |> na.omit()
# X2 <- AllDatawoDupl[order(enc_id)][, list(enc_id, enc_lat, enc_long, month, day, year, ind_id, DeltaDays)] |> na.omit()
# lapply(seq_len(ncol(X1)), function(X) range(X1[, ..X] - X2[, ..X])) ### the differences are negligible
