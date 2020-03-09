# Exploration of DBS Data Spring 2015-2018

install.packages("amt")
library(dplyr)
library(ggplot2)
library(amt)
library(tidyr)
library(dplyr)

DesertDataDanielle <- read.csv("DesertDataDanielle.csv", header = TRUE, sep = ",")
View(DesertDataDanielle)
str(DesertDataDanielle)
DesertDataDanielle$date <- as.character(DesertDataDanielle$date)
DesertDataDanielle$date <- as.Date(DesertDataDanielle$date, format = "%Y%m%d")
DesertDataDanielle$DateTime <- as.character(DesertDataDanielle$DateTime)
DesertDataDanielle$DateTimeC <- as.POSIXct(DesertDataDanielle$DateTime, format = "%m/%d/%Y %H:%M")
DesertDataDanielle$UTME <- as.numeric(DesertDataDanielle$UTME)
DesertDataDanielle$UTMN <- as.numeric(DesertDataDanielle$UTMN)

# check if any coordinates are missing (and if so, remove the relocation)
all(complete.cases(DesertDataDanielle))
# FALSE
all(complete.cases(DesertDataDanielle$DateTimeC))
# TRUE
all(complete.cases(DesertDataDanielle$UTME))
# TRUE
all(complete.cases(DesertDataDanielle$UTMN))
# TRUE - None of the coordinates are missing.

#adeHabitatlt package

#basics of movement
install.packages("adehabitatLT")
library(adehabitatLT)

#here, object "data" needs (at a minimum) columns id, data (in POSIXct), x, y. Coordinates can be in UTM

# data.lt<-as.ltraj(xy=data[,c("x","y")], date=data$date, id=data$id) #bursts are animal id

#the ltraj object is a list, so getting location data for first id would be achieved by data.lt[[1]]

#here we plot movement parameters for the second individual in the list

hist(data.lt[[2]]$dist) #step length, Weibull distribution

hist(data.lt[[2]]$rel.angle) #turning angle, wrapped Cauchy distribution

plot(data.lt[[2]]$date, data.lt[[2]]$R2n) #plot net squared displacement profile, Net squared displacement calculates the squared distance
# between each GPS location in an individual's track and the
# individual's original location. Distances are squared to remove
# directional information

DesertDataDanielleNoDuplicates <- DesertDataDanielle[!duplicated(DesertDataDanielle), ]
View(DesertDataDanielleNoDuplicates)

DesertDataDanielleNoDuplicates$IDDateTime <- paste(DesertDataDanielleNoDuplicates$ID, DesertDataDanielleNoDuplicates$DateTime, sep = " ")
str(DesertDataDanielleNoDuplicates)
table(duplicated(DesertDataDanielleNoDuplicates$IDDateTime))
# There are still duplicates in the dataframe.
DesertDataDanielleNoDuplicates <- DesertDataDanielleNoDuplicates[!duplicated(DesertDataDanielleNoDuplicates$IDDateTime), ]
table(duplicated(DesertDataDanielleNoDuplicates$IDDateTime))
# No more duplicates in the dataframe in terms of IDDateTime. 

# THIS IS AS THE CROW FLIES. When do for real analyses, need to take into account topography (and cite that paper).
data.
data.lt_Desert <- as.ltraj(xy = DesertDataDanielleNoDuplicates[, c("UTME", "UTMN")], date = DesertDataDanielleNoDuplicates$DateTimeC, id = DesertDataDanielleNoDuplicates$ID)
View(data.lt_Desert)
hist(data.lt_Desert[[2]]$dist)
str(data.lt_Desert[[1]])
# Can see which sheep it is under str() attribute
hist(data.lt_Desert[[id = "BHS_ 1321"]]$dist)
data.lt_Desert
head(data.lt_Desert)
# Checked the math in 1 case:
data.lt_Desert[[burst]]
length(unique(DesertDataDanielle$ID))
# There are 307 unique bighorn IDs. This is the same as the number of dataframes in the data.lt_Desert list, so there is one dataframe/ID in data.lt_Desert.

# Checking that the # of step lengths is right for individual bighorn
length(data.lt_Desert[[305]]$dist)
nrow(DesertDataDanielleNoDuplicates[DesertDataDanielleNoDuplicates$ID == "BHS_1661", ])
data.lt_Desert[[305]]$dist
# So how this program works is it assigns the step length to the first point, therefore making the last one be an NA.
# Also checked the time for this bighorn, it is correct. How this works is time is in seconds, the program assigns the time between points to the first point, therefore making the last one be an NA.

# In each dataframe within the list, need to divide the dist by the dt to get distance/time. 
for(i in 1:length(data.lt_Desert)){data.lt_Desert[[i]]$distancebytime  <- data.lt_Desert[[i]]$dist / (data.lt_Desert[[i]]$dt / 3600) }
# going to do meters/hour

# Explore the different values of dt for every dataframe in the list. How many of each dt.

# for(i in 1:length(data.lt_Desert)){unique(data.lt_Desert[[i]]$dt) / 3600} WRONG

# For a given dt, compare dist/time by month for all bighorn (aka across all dataframes in the list) repeated measures ANOVA?

# for the four hour fix rate, can compare distance/time if the dt is between 3.5 - 4.5 hours

ids<-unique(DesertDataDanielleNoDuplicates$ID)
FourHourFixRate<-NULL
for(i in 1:length(data.lt_Desert)){ # i =1
  FourHourFixRate.tmp<-NULL
  potential.rows<-12600 <= data.lt_Desert[[i]]$dt & data.lt_Desert[[i]]$dt <= 16200
  rows<- potential.rows[!is.na(potential.rows)]
  FourHourFixRate.tmp <-  data.lt_Desert[[i]][rows,] 
  FourHourFixRate.tmp$ID<-rep(ids[[i]],nrow(FourHourFixRate.tmp))
  if(i==1 & sum(rows)>0){
    FourHourFixRate<-FourHourFixRate.tmp
  }else{
      FourHourFixRate<-rbind(FourHourFixRate,FourHourFixRate.tmp)}
}

head(FourHourFixRate)
str(FourHourFixRate)
data.lt_Desert[[1]]
test<-FourHourFixRate %>% group_by(ID,months(date)) %>% summarize(sl.mean=mean(distancebytime))
test1<-as.data.frame(test)
head(test1)
str(test1)
test1$month <- as.factor(test1$`months(date)`)
plot(sl.mean ~ month, data = test1, na.rm = TRUE, ylim = c(0, 1000), main = "Every 4 Hours")

summary(FourHourFixRate$ID)
# 100 individual bighorn in this plot


# for every 12 hour fix rate (2x/day with equal length in between the times)

ids<-unique(DesertDataDanielleNoDuplicates$ID)
TwelveHourFixRate<-NULL
for(i in 1:length(data.lt_Desert)){ # i =1
  TwelveHourFixRate.tmp<-NULL
  potential.rows2 <-41400 <= data.lt_Desert[[i]]$dt & data.lt_Desert[[i]]$dt <= 45000
  rows2<- potential.rows2[!is.na(potential.rows2)]
  TwelveHourFixRate.tmp <-  data.lt_Desert[[i]][rows2,] 
  TwelveHourFixRate.tmp$ID<-rep(ids[[i]],nrow(TwelveHourFixRate.tmp))
  if(i==1 & sum(rows2)>0){
    TwelveHourFixRate<-TwelveHourFixRate.tmp
  }else{
    TwelveHourFixRate<-rbind(TwelveHourFixRate,TwelveHourFixRate.tmp)}
}
head(TwelveHourFixRate)
str(TwelveHourFixRate)
data.lt_Desert[[1]]
test3<-TwelveHourFixRate %>% group_by(ID,months(date)) %>% summarize(sl.mean=mean(distancebytime))
test4<-as.data.frame(test3)
head(test4)
str(test4)
test4$month <- as.factor(test4$`months(date)`)
plot(sl.mean ~ month, data = test4, na.rm = TRUE, ylim = c(0, 1000), main = "Every 12 hours")

summary(TwelveHourFixRate$ID)

# Less than hour and a half fix rate

ids<-unique(DesertDataDanielleNoDuplicates$ID)
OneandHalfHourFixRate<-NULL
for(i in 1:length(data.lt_Desert)){ # i =1
  OneandHalfHourFixRate.tmp<-NULL
  potential.rows3<-data.lt_Desert[[i]]$dt <= 5400
  rows3<- potential.rows3[!is.na(potential.rows3)]
  OneandHalfHourFixRate.tmp <-  data.lt_Desert[[i]][rows3,] 
  OneandHalfHourFixRate.tmp$ID<-rep(ids[[i]],nrow(OneandHalfHourFixRate.tmp))
  if(i==1 & sum(rows)>0){
    OneandHalfHourFixRate<-OneandHalfHourFixRate.tmp
  }else{
    OneandHalfHourFixRate<-rbind(OneandHalfHourFixRate,OneandHalfHourFixRate.tmp)}
}
head(OneandHalfHourFixRate)
str(OneandHalfHourFixRate)
data.lt_Desert[[1]]
test5<-OneandHalfHourFixRate %>% group_by(ID,months(date)) %>% summarize(sl.mean=mean(distancebytime))
test6<-as.data.frame(test5)
head(test6)
str(test6)
test6$month <- as.factor(test6$`months(date)`)
plot(sl.mean ~ month, data = test6, na.rm = TRUE, ylim = c(0, 1000), main = "Less than One and a Half Hours")


# mean dt/animal
# subsample 

# [data.lt_Desert[[i]]$date == ]


# plot boxplots for all months for all bighorn
# then repeat, but separate for ewes and rams












View(steps1580)
summary(steps1580$sl_)
plot(steps1580$sl_)
hist(steps1580$sl_)
smallsteps1580 <- steps1580[steps1580$sl_ < 2000]

# TRYING 1580 USING THE moveHMM Package
install.packages("moveHMM")
library(moveHMM)
attempt <- prepData(Data_1580_noduplicates, type = "UTM", coordNames = c("UTME", "UTMN"))


## SOME THOUGHTS
# 1589 is every four hours for 5/1-9/4 last summer. 1,175 entries. Marb ewe.
any(duplicated(Data_1589$DateTimeC))
# TRUE, we have some duplicated time stamps. Therefore, removing duplicates.
Data_1589_v2 <- Data_1589[!duplicated(Data_1589$DateTimeC), ]
View(Data_1589_v2)
# Now there are only 1,060 entries.

# +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0

track1589 <- make_track(Data_1589_v2, UTME, UTMN, DateTimeC, ID = ID, Pop = Pop, Sex = Sex, crs = sp::CRS("+proj=utm +zone=11 +datum=WGS84"), order_by_ts = TRUE)
str(track1589)
# Below doesn't work
steps(track1589, lonlat = TRUE, keep_cols = "start", diff_time_units = "auto")


# Individual BHS Playing with Data:

Data_1578 <- DesertDataDanielle[DesertDataDanielle$ID == "BHS_1578", ]
View(Data_1578)
#397 total, 
count(DesertDataDanielle[DesertDataDanielle$ID == "BHS_ 1492", ])

Data_1596 <- DesertDataDanielle[DesertDataDanielle$ID == "BHS_ 1596", ]
nrow(Data_1596)
# 65 data points
Data_1598 <- DesertDataDanielle[DesertDataDanielle$ID == "BHS_1598", ]
View (Data_1598)
Data_1589 <- DesertDataDanielle[DesertDataDanielle$ID == "BHS_ 1589", ]
nrow(Data_1589)
# 341 GPS points
Data_1599 <- DesertDataDanielle[DesertDataDanielle$ID == "BHS_1599", ]
View(Data_1599)
Data_1492 <- DesertDataDanielle[DesertDataDanielle$ID == "BHS_1492", ]
View(Data_1492)
Data_1580 <- DesertDataDanielle[DesertDataDanielle$ID == "BHS_ 1580", ]
View(Data_1580)
Data_1596 <- DesertDataDanielle[DesertDataDanielle$ID == "BHS_ 1596", ]
View(Data_1596)

#1580: 1100 entries
any(duplicated(Data_1580$DateTimeC))
# but duplicates
Data_1580_noduplicates <- Data_1580[!duplicated(Data_1580), ]
View(Data_1580_noduplicates)
# Now 1079 entries.
track1580 <- make_track(tbl = Data_1580_noduplicates, .x = UTME, .y = UTMN, .t = DateTimeC, ID = ID, Pop = Pop, Sex = Sex, crs = sp::CRS("+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"), order_by_ts = TRUE)
plot(track1580)
# WHY DOES BELOW WORK IN lonlat is FALSE, but not if it's true?
steps1580 <- steps(track1580, lonlat = FALSE, keep_cols = "start", diff_time_units = "auto")
steps1580 <- track1580 %>% mutate(sl_ = step_lengths(.))

data.lt_1580 <- as.ltraj(xy=Data_1580_noduplicates[,c("UTME","UTMN")], date = Data_1580_noduplicates$DateTimeC, id= Data_1580_noduplicates$ID) 

hist(data.lt_1580[[1]]$dist)


dupes <- DesertDataDanielleNoDuplicates$DateTimeC[duplicated(DesertDataDanielleNoDuplicates$DateTimeC)]


DesertDataDanielleNoDuplicates <- distinct(DesertDataDanielleNoDuplicates)

DesertDataDanielleNoDuplicates$DateTimeasFactor <- as.factor(DesertDataDanielleNoDuplicates$DateTime)







