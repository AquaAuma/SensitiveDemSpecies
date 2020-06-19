Sys.setenv(LANG = "en")
rm(list=ls())

### Libraries
#------------
library(data.table)
library(dplyr)

re.estimate.SA <- 'FALSE'

##########################################################################################
#### LOAD FILES
##########################################################################################
# Haul info
setwd('E:/DATA/ICES Surveys/exchange.data/HH')
hh.ns <- read.csv('HH.NS-IBTS.csv')
hh.baltic <- read.csv('HH.BITS.csv')
hh.evhoe <- read.csv('HH.EVHOE.csv')
hh.fcgs <- read.csv('HH.FR-FCGS.csv')
hh.igfs <- read.csv('HH.IE-IGFS.csv')
hh.nigfs <- read.csv('HH.NIGFS.csv')
pb.nigfs <- data.frame(sort(unique(hh.nigfs$StatRec)))
pb.nigfs[,2] <- c('34E4','35E4','36E4','33E5','34E5','35E5','36E5','37E5','38E5','33E6','34E6','35E6','36E6',
                  '37E6','38E6','35E7','36E7','37E7','38E7')
names(pb.nigfs) <- c('StatRec','StatRec2')
hh.nigfs <- left_join(hh.nigfs, pb.nigfs, by='StatRec')
hh.nigfs$StatRec <- NULL
setnames(hh.nigfs, old='StatRec2',new='StatRec')
hh.pt <- read.csv('HH.PT-IBTS.csv')
hh.rock <- read.csv('HH.ROCKALL.csv')
hh.spa <- read.csv('HH.SP-ARSA.csv')
pb.spa <- data.frame(sort(unique(hh.spa$StatRec)))
pb.spa[,2] <- c('01E2','02E2','03E2','01E3','02E3','03E3')
names(pb.spa) <- c('StatRec','StatRec2')
hh.spa <- left_join(hh.spa, pb.spa, by='StatRec')
hh.spa$StatRec <- NULL
setnames(hh.spa, old='StatRec2',new='StatRec')
hh.spn <- read.csv('HH.SP-NORTH.csv')
pb.spn <- data.frame(sort(unique(hh.spn$StatRec)))
pb.spn[,2] <- c('12E1','13E1','14E1','15E1','16E1','13E2','15E2','16E2','17E2','16E3','17E3','16E4',
                '16E5','15E6','16E6','15E7','16E7','15E8','16E8','16E9')
names(pb.spn) <- c('StatRec','StatRec2')
hh.spn <- left_join(hh.spn, pb.spn, by='StatRec')
hh.spn$StatRec <- NULL
setnames(hh.spn, old='StatRec2',new='StatRec')
hh.spp <- read.csv('HH.SP-PORC.csv')
hh.swc <- read.csv('HH.SWC.IBTS.csv')

# new surveys to homogeneize with old ones
hh.sns <- read.csv('HH.SNS.csv') # a part of HL is in HH, and I removed it
hh.sns <- hh.sns %>% 
  filter(RecordType=='HH')
hh.sns$StNo <- as.factor(as.character(hh.sns$StNo))
hh.sns$Stratum <- as.factor(as.character(hh.sns$Stratum))
hh.sns$Rigging <- as.factor(as.character(hh.sns$Rigging))
hh.sns$HydroStNo <- as.factor(as.character(hh.sns$HydroStNo))

hh.bts <- read.csv('HH.BTS.csv')
hh.bts8 <- read.csv('HH.BTSVIII.csv')
pb.bts8 <- data.frame(sort(unique(hh.bts8$StatRec)))
pb.bts8[,2] <- c('24E6','23E7','24E7','20E8','21E8','22E8','23E8','17E9','18E9','19E9','20E9','21E9')
names(pb.bts8) <- c('StatRec','StatRec2')
hh.bts8 <- left_join(hh.bts8, pb.bts8, by='StatRec')
hh.bts8$StatRec <- NULL
setnames(hh.bts8, old='StatRec2',new='StatRec')
hh.bts8$StNo <- as.factor(as.character(hh.bts8$StNo))
hh.bts8$Stratum <- as.factor(as.character(hh.bts8$Stratum))
hh.bts8$Rigging <- as.factor(as.character(hh.bts8$Rigging))
hh.bts8$HydroStNo <- as.factor(as.character(hh.bts8$HydroStNo))
hh.bts8$StatRec <- as.factor(as.character(hh.bts8$StatRec))

hh.dyfs <- read.csv('HH.DYFS.csv')
identical(colnames(hh.sns), colnames(hh.dyfs))
hh.new1 <- rbind(hh.dyfs, hh.sns)
identical(colnames(hh.bts), colnames(hh.bts8))
hh.new2 <- rbind(hh.bts, hh.bts8)
hh.new1 <- hh.new1 %>% 
  select(colnames(hh.ns))

identical(colnames(hh.new1), colnames(hh.ns))
identical(colnames(hh.new2), colnames(hh.ns))
hh.new <- rbind(hh.new1, hh.new2)

# Length info
setwd('E:/DATA/ICES Surveys/exchange.data/HL')
hl.ns1 <- read.csv('HL.NS-IBTS-1990-2000.csv')
hl.ns2 <- read.csv('HL.NS-IBTS-2001-2010.csv')
hl.ns3 <- read.csv('HL.NS-IBTS-2011-2017.csv')
hl.ns4 <- read.csv('HL.NS-IBTS-1965-1989.csv')
hl.ns <- rbind(hl.ns1, hl.ns2, hl.ns3, hl.ns4)

hl.baltic1 <- read.csv('HL.BITS-1991-2000.csv')
hl.baltic2 <- read.csv('HL.BITS-2001-2010.csv')
hl.baltic3 <- read.csv('HL.BITS-2011-2017.csv')
hl.baltic <- rbind(hl.baltic1, hl.baltic2, hl.baltic3)

hl.evhoe1 <- read.csv('HL.EVHOE-1997-2007.csv')
hl.evhoe2 <- read.csv('HL.EVHOE-2008-2017.csv')
hl.evhoe <- rbind(hl.evhoe1, hl.evhoe2)

hl.cgfs <- read.csv('HL.FR-CGFS.csv')
hl.igfs <- read.csv('HL.IE-IGFS.csv')
hl.nigfs <- read.csv('HL.NIGFS.csv')
hl.pt <- read.csv('HL.PT-IBTS.csv')
hl.rock <- read.csv('HL.ROCK.csv')
hl.spa <- read.csv('HL.SP-ARSA.csv')
hl.spn <- read.csv('HL.SP-NORTH.csv')
hl.spp <- read.csv('HL.SP-PORC.csv')
hl.swc <- read.csv('HL.SWC-IBTS.csv')

hl.bts <- read.csv('HL.BTS.csv') 
hl.bts8 <- read.csv('HL.BTSVIII.csv') 
hl.dyfs <- read.csv('HL.DYFS.csv')
hl.sns <- read.csv('HL.SNS.csv')
hl.new1 <- rbind(hl.bts, hl.bts8)
hl.new1$ScientificName_WoRMS <- NULL
hl.sns$StNo <- as.factor(as.character(hl.sns$StNo))
hl.new2 <- rbind(hl.dyfs, hl.sns)
hl.new2$ScientificName_WoRMS <- hl.new2$DevStage <- NULL
identical(colnames(hl.new2), colnames(hl.new1))
hl.new <- rbind(hl.new1, hl.new2)
hl.new <- hl.new %>% 
  select(colnames(hl.baltic))
identical(colnames(hl.new), colnames(hl.baltic))

hh.nigfs$StatRec <- as.factor(as.character(hh.nigfs$StatRec))
hh.nigfs$HydroStNo <- as.factor(as.character(hh.nigfs$HydroStNo))
hh.swc$StNo <- as.factor(as.character(hh.swc$StNo))
hh.pt$StNo <- as.factor(as.character(hh.pt$StNo))
hh.spa$StNo <- as.factor(as.character(hh.spa$StNo))
hh.spa$HydroStNo <- as.factor(as.character(hh.spa$HydroStNo))
hh.spa$Rigging <- as.factor(as.character(hh.spa$Rigging))
hh.spa$StatRec <- as.factor(as.character(hh.spa$StatRec))
hh.spp$StatRec <- as.factor(as.character(hh.spp$StatRec))
hh.spn$StatRec <- as.factor(as.character(hh.spn$StatRec))
hh.spp$StNo <- as.factor(as.character(hh.spp$StNo))
hh.spn$StNo <- as.factor(as.character(hh.spn$StNo))
hl.swc$StNo <- as.factor(as.character(hl.swc$StNo))
hl.pt$StNo <- as.factor(as.character(hl.pt$StNo))

hh <- rbind(hh.ns, hh.evhoe, hh.fcgs, hh.igfs, hh.nigfs, hh.swc, hh.rock, hh.pt, hh.baltic, hh.spa, hh.spp, hh.spn, hh.new)#, hh.spa, hh.spp, hh.spn)
hl <- rbind(hl.ns, hl.evhoe, hl.cgfs, hl.igfs, hl.nigfs, hl.swc, hl.rock, hl.pt, hl.baltic, hl.spa, hl.spp, hl.spn, hl.new)# hl.spa, hl.spp, hl.spn)

rm(hh.ns, hh.evhoe, hh.fcgs, hh.igfs, hh.nigfs, hh.swc, hh.rock, hh.pt, hh.baltic)
rm(hl.ns, hl.evhoe, hl.cgfs, hl.igfs, hl.nigfs, hl.swc, hl.rock, hl.pt, hl.baltic)
rm(hl.baltic1, hl.baltic2, hl.baltic3, hl.evhoe1, hl.evhoe2, hl.ns1, hl.ns2, hl.ns3, hl.ns4)
rm(hh.spa, hh.spn, hh.spp, hl.spa, hl.spn, hl.spp)
rm(hh.bts, hh.bts8, hh.dyfs, hh.new, hh.new1, hh.new2, hh.sns)
rm(hl.bts, hl.bts8, hl.dyfs, hl.new1, hl.new2, hl.sns, hl.new)

##########################################################################################
#### CREATE A UNIQUE HAUL ID
##########################################################################################


hl$HaulID <- paste(hl$Survey, hl$Year,hl$Quarter, hl$Country, hl$Ship, hl$Gear, hl$StNo, hl$HaulNo)
hl$SweepLngt <- hl$SpecCodeType <- hl$SpecCode <- hl$Sex <- hl$DateofCalculation <- hl$RecordType <- NULL

hh$HaulID <- paste(hh$Survey, hh$Year,hh$Quarter, hh$Country, hh$Ship, hh$Gear, hh$StNo, hh$HaulNo)

# Is the HaulID unique?
hhn <- unique(hh$HaulID)
length(hhn)==nrow(hh)

#pb <- c()
#for (i in 1:length(hhn)){
#  j <- which(hh$HaulID==hhn[i])
#  if(length(j)>1){pb <- hhn[i]}
#}

hh$DateofCalculation <- hh$ThClineDepth <- hh$ThermoCline <- hh$SwellHeight <- hh$SwellDir <- hh$WindSpeed <- hh$WindDir <- hh$BotCurSpeed <- NULL
hh$BotCurDir <- hh$SurCurSpeed <- hh$SurCurDir <- hh$SpeedWater <- hh$TowDir <- hh$WgtGroundRope <- hh$KiteDim <- hh$Buoyancy <- NULL
hh$DoorWgt <- hh$DoorSurface <- hh$WarpDen <- hh$Warpdia <- hh$Warplngt <- hh$Tickler <- hh$Rigging <- hh$Netopening <- NULL
hh$HydroStNo <- hh$HaulLat <- hh$SweepLngt <- hh$HaulLong <- hh$DayNight <- hh$Stratum <- hh$TimeShot <- hh$Day <- hh$RecordType <- hh$GearExp <- hh$DoorType <- NULL

# Only keep hauls where there is the length composition. 60162 hauls in hh and 60135 in hl
hh <- subset(hh, hh$HaulID %in% hl$HaulID)
identical(sort(unique(hh$HaulID)),sort(unique(hl$HaulID)))



##########################################################################################
#### MERGE HH and HL FILES
##########################################################################################

haulidhl <- sort(unique(hl$HaulID))
haulidhh <- sort(unique(hh$HaulID))
identical(haulidhh, haulidhl)

#survey <- merge(hh, hl, by='HaulID', all.x=FALSE, all.y=TRUE)
survey <- right_join(hh, hl, by='HaulID')
nrow(survey)==nrow(hl)


### Check if the HaulID is unique
### Not the case for the baltic sea, a lot of duplicates!!!
#ids <- unique(hh$HaulID)
#pb <- vector()
#for(i in 1:length(ids)){
#  x <- which(hh$HaulID==ids[i])
#  if(length(x)>1){pb[length(pb)+1] <- ids[i]}
#}
#print(pb) # dim 0 ok!

survey$Survey.y <- survey$Quarter.y <- survey$StNo.y <- survey$Country.y <- survey$Ship.y <- survey$Gear.y <- survey$HaulNo.y <- survey$Year.y <- NULL
setnames(survey, old=c('Survey.x','Quarter.x','Country.x','Ship.x','Gear.x','StNo.x','HaulNo.x','Year.x','ValidAphiaID','GroundSpeed',
                       'BotTemp','SurTemp'),
         new=c('Survey','Quarter','Country','Ship','Gear','StNo','HaulNo','Year','AphiaID','Speed','SBT','SST'))


##########################################################################################
#### REMOVE INVALID DATA
##########################################################################################

#Remove invalid hauls
survey_raw <- survey
survey <- subset(survey_raw, subset=(survey_raw$HaulVal %in% c('V')))

### Remove invalid species records
survey <- subset(survey, subset=(!is.na(survey$AphiaID)))
survey <- subset(survey, survey$SpecVal %in% c(1,10,4,7))

# Transform mm into cm
survey[(survey$LngtCode=='0'|survey$LngtCode=='.'),]$LngtClass <- survey[(survey$LngtCode=='0'|survey$LngtCode=='.'),]$LngtClass/10

# Take out rows without length or weight info => abundance only is not enough
#survey <- subset(survey, subset=((survey$HLNoAtLngt!=(-9) & survey$CatCatchWgt!=(-9)) || (survey$HLNoAtLngt==(-9) & survey$CatCatchWgt!=(-9)) || (survey$HLNoAtLngt!=(-9) & survey$CatCatchWgt==(-9))))
survey <- subset(survey, survey$DataType %in% c('S','R','C'))



##########################################################################################
#### RESCALE DATA INTO ABUNDANCE FOR THE HAUL DURATION AND ABUNDANCE AT LENGTH
##########################################################################################

# Remove no abundance at length 
#survey <- subset(survey, survey$HLNoAtLngt>0)

# If Data Type=='C', abundance at length already readjusted with time so get back the abundance for the actual duration of the haul.
surveyC <- subset(survey, survey$DataType=='C')
surveyC$HLNoAtLngt <- surveyC$HLNoAtLngt*surveyC$SubFactor*surveyC$HaulDur/60
surveyC$TotalNo <- surveyC$TotalNo*surveyC$HaulDur/60
surveyC$CatCatchWgt <- surveyC$CatCatchWgt*surveyC$HaulDur/60

# If data type=='R', abundance at length is mulitplied by subfactor and adjusted to time
surveyR <- subset(survey, survey$DataType %in% c('S','R'))
surveyR$HLNoAtLngt <- surveyR$HLNoAtLngt*surveyR$SubFactor

survey <- rbind(surveyC, surveyR)
survey$HaulVal <- survey$DataType <- survey$StdSpecRecCode <- survey$SpecVal <- survey$CatIdentifier <- survey$SubWgt <- survey$SubFactor <- NULL

##########################################################################################
#### GET THE SWEPT AREA
##########################################################################################

survey[survey$WingSpread==-9,]$WingSpread <- NA # wing spread less available so take doorspread
survey[survey$DoorSpread==-9,]$DoorSpread <- NA
survey[survey$Speed==-9,]$Speed <- NA
survey[survey$Distance==-9,]$Distance <- NA
survey[survey$Depth==-9,]$Depth <- NA

survey$Area.swept <- survey$Distance*0.001*survey$DoorSpread*0.001
survey[is.na(survey$Area.swept),]$Area.swept <- survey[is.na(survey$Area.swept),]$Speed*1.852*survey[is.na(survey$Area.swept),]$HaulDur/60*survey[is.na(survey$Area.swept),]$DoorSpread*0.001
# FR-CGFS, NIGFS and PT have no swept area calculation possible
# For EVHOE, NS-IBTS, IE-IGFS, SWC-IBTS and ROCKALL = all values around 0.3km2

if(re.estimate.SA==TRUE){
# Re-estimate the swept area from a linear model per survey
### EVHOE ###
evhoe <- survey %>%
  filter(Survey=='EVHOE') %>%
  select(-TotalNo, -NoMeas, -CatCatchWgt, -LngtCode, -LngtClass, -HLNoAtLngt, -AphiaID) %>%
  distinct()

par(mfrow=c(2,2))
plot(Area.swept ~ Depth, data=evhoe)
plot(Area.swept ~ HaulDur, data=evhoe)
plot(Area.swept ~ Speed, data=evhoe)
plot(Area.swept ~ Distance, data=evhoe)

evhoe$Depth <- log(evhoe$Depth)
lm0 <- lm(Area.swept ~ HaulDur + Depth, data=evhoe)
summary(lm0)

pred0 <- predict.lm (object=lm0, newdata=evhoe, interval='confidence', level=0.95)
evhoe <- cbind(evhoe, pred0)
evhoe[is.na(evhoe$Area.swept),]$Area.swept <- evhoe[is.na(evhoe$Area.swept),]$fit

evhoe <- evhoe %>%
  select(HaulID, Area.swept) %>%
  rename(Area2=Area.swept) # a few NA's
area2 <- evhoe

### North Sea ###
nsibts <- survey %>%
  filter(Survey=='NS-IBTS',
         Year>1989,
         !is.na(Depth)) %>%
  select(Year, HaulID, HaulDur, Area.swept, Depth, Ship, Gear, GearExp, DoorType, Speed, Distance) %>%
  distinct()

par(mfrow=c(2,2))
plot(Area.swept ~ HaulDur, data=nsibts, log='xy')
plot(Area.swept ~ Depth, data=nsibts)
plot(Area.swept ~ Distance, data=nsibts)
plot(Area.swept ~ Speed, data=nsibts)

nsibts$Depth <- log(nsibts$Depth)
lm0 <- lm(Area.swept ~ HaulDur + Depth, data=nsibts)

pred0 <- predict(lm0, newdata=nsibts, interval='confidence', level=0.95)
nsibts <- cbind(nsibts,pred0)
nsibts[is.na(nsibts$Area.swept),]$Area.swept <- nsibts[is.na(nsibts$Area.swept),]$fit

nsibts <- nsibts %>%
  select(HaulID, Area.swept) %>%
  rename(Area2=Area.swept)
area2 <- rbind(nsibts, area2)

### SWC-IBTS ###
swc <- survey %>%
  filter(Survey=='SWC-IBTS',
         Year>1989) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, Speed, Distance) %>%
  distinct()

plot(Area.swept ~ HaulDur, data=swc)
plot(Area.swept ~ Depth, data=swc, log='x')
swc$Depth <- log(swc$Depth)
lm0 <- lm(Area.swept ~ HaulDur + Depth, data=swc)

pred0 <- predict(lm0, newdata=swc, interval='confidence', level=0.95)
swc <- cbind(swc,pred0)
swc[is.na(swc$Area.swept),]$Area.swept <- swc[is.na(swc$Area.swept),]$fit

swc <- swc %>%
  select(HaulID, Area.swept) %>%
  rename(Area2=Area.swept)
area2 <- rbind(area2, swc)

### BITS ###
bits <- survey %>%
  filter(Survey=='BITS',
         Year>1989) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID) %>%
  distinct()

plot(Area.swept ~ HaulDur, data=bits)
plot(Area.swept ~ Depth, data=bits, log='x')
lm0 <- lm(Area.swept ~ HaulDur + log(Depth), data=bits)

pred0 <- predict(lm0, newdata=bits, interval='confidence', level=0.95)
bits <- cbind(bits,pred0)
bits[is.na(bits$Area.swept),]$Area.swept <- bits[is.na(bits$Area.swept),]$fit

bits <- bits %>%
  select(HaulID, Area.swept) %>%
  rename(Area2=Area.swept)
area2 <- rbind(area2, bits)

### IE-IGFS ###
ie <- survey %>%
  filter(Survey=='IE-IGFS',
         Year>1989) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID) %>%
  distinct()

plot(Area.swept ~ HaulDur, data=ie)
plot(Area.swept ~ Depth, data=ie, log='x')
ie$Depth <- log(ie$Depth)
lm0 <- lm(Area.swept ~ HaulDur + Depth, data=ie)

pred0 <- predict(lm0, newdata=ie, interval='confidence', level=0.95)
ie <- cbind(ie,pred0)
ie[is.na(ie$Area.swept),]$Area.swept <- ie[is.na(ie$Area.swept),]$fit

ie <- ie %>%
  select(HaulID, Area.swept) %>%
  rename(Area2=Area.swept)
area2 <- rbind(area2, ie)

# FR-CGFS very few hauls with swept area data
cgfs <- survey %>%
  filter(Survey=='FR-CGFS',
         Year>1989) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, Speed, Distance) %>%
  distinct()

par(mfrow=c(2,2))
plot(Area.swept ~ HaulDur, data=cgfs)
plot(Area.swept ~ Depth, data=cgfs)
plot(Area.swept ~ Distance, data=cgfs) # Distance always reported

lm0 <- lm(Area.swept ~ HaulDur + Depth + Distance, data=cgfs)

pred0 <- predict(lm0, newdata=cgfs, interval='confidence', level=0.95)
cgfs <- cbind(cgfs,pred0)
cgfs[is.na(cgfs$Area.swept),]$Area.swept <- cgfs[is.na(cgfs$Area.swept),]$fit

cgfs <- cgfs %>%
  select(HaulID, Area.swept) %>%
  rename(Area2=Area.swept)
area2 <- rbind(area2, cgfs)

### NIGFS ###
nigfs <- survey %>%
  filter(Survey=='NIGFS',
         Year>1989) %>%
  mutate(DurQ = ifelse(HaulDur<40,'S','L')) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, DurQ, Speed, Distance) %>%
  distinct()

par(mfrow=c(1,2))
plot(Area.swept ~ HaulDur, data=nigfs)
plot(Area.swept ~ Depth, data=nigfs)

# Model for short hauls
nigfsS <- nigfs %>%
  filter(DurQ=='S',
         !is.na(Depth))
par(mfrow=c(2,2))
plot(Area.swept ~ HaulDur, data=nigfsS)
plot(Area.swept ~ Depth, data=nigfsS)
plot(Area.swept ~ Speed, data=nigfsS)
nigfsS$Depth2 <- (nigfsS$Depth-mean(nigfsS$Depth))^2
lm0 <- lm(Area.swept ~ HaulDur + Depth + Depth2, data=nigfsS)

pred0 <- predict(lm0, newdata=nigfsS, interval='confidence', level=0.95)
nigfsS <- cbind(nigfsS,pred0)
nigfsS[is.na(nigfsS$Area.swept),]$Area.swept <- nigfsS[is.na(nigfsS$Area.swept),]$fit

# Model for short hauls
nigfsL <- nigfs %>%
  filter(DurQ=='L',
         !is.na(Depth))
par(mfrow=c(2,2))
plot(Area.swept ~ HaulDur, data=nigfsL)
plot(Area.swept ~ Depth, data=nigfsL)
plot(Area.swept ~ Speed, data=nigfsL)
nigfsL$Depth2 <- (nigfsL$Depth-mean(nigfsL$Depth))^2
lm0 <- lm(Area.swept ~ HaulDur + Depth + Depth2, data=nigfsL)

pred0 <- predict(lm0, newdata=nigfsL, interval='confidence', level=0.95)
nigfsL <- cbind(nigfsL,pred0)
nigfsL[is.na(nigfsL$Area.swept),]$Area.swept <- nigfsL[is.na(nigfsL$Area.swept),]$fit

nigfs <- rbind(nigfsL, nigfsS)
nigfs <- nigfs %>%
  select(HaulID, Area.swept) %>%
  rename(Area2=Area.swept)
area2 <- rbind(area2, nigfs)

### ROCKALL ###
rock <- survey %>%
  filter(Survey=='ROCKALL',
         Year>1989) %>%
  select(Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, Speed, Distance) %>%
  distinct()

par(mfrow=c(2,2))
plot(Area.swept ~ HaulDur, data=rock)
plot(Area.swept ~ Depth, data=rock)
plot(Area.swept ~ Speed, data=rock)
plot(Area.swept ~ Distance, data=rock, log='x')

lm0 <- lm(Area.swept ~ HaulDur + Depth, data=rock)

pred0 <- predict(lm0, newdata=rock, interval='confidence', level=0.95)
rock <- cbind(rock,pred0)
rock[is.na(rock$Area.swept),]$Area.swept <- rock[is.na(rock$Area.swept),]$fit

rock <- rock %>%
  select(HaulID, Area.swept) %>%
  rename(Area2=Area.swept)
area2 <- rbind(area2, rock)

### PORTUGAL ###
pt <- survey %>%
  filter(!is.na(HaulDur),
         !is.na(Depth),
         !is.na(Speed),
         Year>1989)%>%
  select(Survey, Year, HaulDur, Area.swept, Depth, Ship, Gear, HaulID, Speed, Distance) %>%
  distinct()

par(mfrow=c(2,2))
plot(Area.swept ~ HaulDur, data=pt)
plot(Area.swept ~ Depth, data=pt)
plot(Area.swept ~ Speed, data=pt)
plot(Area.swept ~ Distance, data=pt)
lm0 <- lm(Area.swept ~ HaulDur + Depth + Speed, data=pt)

pred0 <- predict(lm0, newdata=pt, interval='confidence', level=0.95)
pt <- cbind(pt,pred0)
pt[is.na(pt$Area.swept),]$Area.swept <- pt[is.na(pt$Area.swept),]$fit

pt <- pt %>%
  filter(Survey=='PT-IBTS') %>%
  select(HaulID, Area.swept) %>%
  rename(Area2=Area.swept)
area2 <- rbind(area2, pt)


rm(bits, cgfs, ie, nsibts, pt, hl.spa, hl.spn, hl.spp, nigfsL, nigfsS, nigfs, pred0, lm0, evhoe, swc, rock, xx)

# Paste new estimates to survey data frame
survey0 <- left_join(survey, area2, by='HaulID')
survey0 <- survey0 %>%
  mutate(Area.swept = coalesce(Area.swept, Area2)) %>%
  select(-Area2) %>%
  filter(is.na(Area.swept) | Area.swept>0)
survey <- survey0

}

##########################################################################################
#### GET CPUEs AND RIGHT COLUMNS NAMES
##########################################################################################

# Keep Length
#survey <- survey %>%
#  mutate(numcpue = TotalNo/Area.swept,
#         wtcpue = CatCatchWgt/(Area.swept*1000),
#         numh = TotalNo*60/HaulDur,
#         wgth = CatCatchWgt*60/(HaulDur*1000),
#         numlencpue = HLNoAtLngt/Area.swept,
#         numlenh = HLNoAtLngt*60/HaulDur,
#         wtlencpue = NA,
#         wgtlenh = NA,
#         Season = 'NA',
#         Length = LngtClass,
#         Depth = replace(Depth, Depth<0, NA),
#         SBT = replace(SBT, SBT<0, NA),
#         SST = replace(SST, SST<0, NA),
#         num=TotalNo) %>%
#  group_by(Survey, HaulID, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST, AphiaID, Length) %>%
#  summarize_at(.vars=c('numcpue', 'wtcpue', 'numh', 'wgth', 'numlencpue', 'wtlencpue', 'numlenh', 'wgtlenh'), .funs=function(x) sum(x, na.rm=T)) %>%
#  select(Survey, HaulID, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST,
#         AphiaID, numcpue, wtcpue, numh, wgth, Length, numlencpue, wtlencpue, numlenh, wgtlenh)

# Only keep abundances/weight
survey <- survey %>%
  mutate(numcpue = TotalNo/Area.swept,
         wtcpue = CatCatchWgt/(Area.swept*1000),
         numh = TotalNo*60/HaulDur,
         wgth = CatCatchWgt*60/(HaulDur*1000),
         num = TotalNo,
         wgt = CatCatchWgt,
         Season = 'NA',
         Depth = replace(Depth, Depth<0, NA),
         SBT = replace(SBT, SBT<0, NA),
         SST = replace(SST, SST<0, NA)) %>%
  mutate(num = if_else(Gear=='BT6' & Survey=='DYFS', num*3/6, num),
         num = if_else(Gear=='BT7', num*4/7, num),
         num = if_else(Gear=='BT8', num*4/8, num),
         Survey = as.character(Survey),
         Gear = as.character(Gear),
         Survey = dplyr::if_else(Survey=='BITS' & Gear=='TVS', 'BITSS', Survey), 
#two different gear sizes are used in the Baltic - they cannot converted reliably for sensitives, so they are treated as separate surveys in my analysis.
         Survey = if_else(Survey=='BITS' & Gear=='TVL', 'BITSL', Survey),
         Survey = if_else(Survey=='SCOWGFS', 'SWC-IBTS', Survey),
         Survey = if_else(Survey=='SCOROC', 'ROCKALL', Survey)) %>% 
#there have been minor changes to the gear in these surveys, it should not affect sensitives
  group_by(Survey, HaulID, StatRec, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST, AphiaID, BycSpecRecCode) %>%
  summarize_at(.vars=c('numcpue', 'wtcpue', 'numh', 'wgth', 'num', 'wgt'), .funs=function(x) sum(x, na.rm=T)) %>%
  select(Survey, HaulID, StatRec, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST,
         AphiaID, BycSpecRecCode, numcpue, wtcpue, numh, wgth, num, wgt)
survey <- data.frame(survey)


##########################################################################################
#### Clean species names
##########################################################################################

survey$Species <- NA

library(worms)
library(worrms)
library(crul)
library(urltools)

dat.ices <- survey
aphia_list <- unique(dat.ices$AphiaID)
aphia_list <- aphia_list[!duplicated(aphia_list)]

# creating taxonomy tables for each species
my_sp_taxo <- wm_record_(id = aphia_list)

# row binds all the results and pass to data frame. 
df_test <- data.frame(do.call(rbind, my_sp_taxo))
df_test$url <- df_test$lsid <- df_test$citation <- NULL
df_test$isExtinct <- df_test$modified <- df_test$valid_authority <- df_test$unacceptreason <- NULL
df_test$authority <- df_test$status <- df_test$taxonRankID <- df_test$isBrackish <- df_test$isFreshwater <- df_test$isTerrestrial <- df_test$match_type <- NULL
#check if it identified everything
dim(subset(df_test, is.na(df_test$phylum))) # ok


# In the class column, we only keep the 5 groups we want. 
df_test <- subset(df_test, class %in% c("Elasmobranchii","Actinopterygii","Holocephali","Myxini","Petromyzonti")) 

# Only keep species rank
#df_test <- subset(df_test, df_test$rank=='Species')

keep_sp <- data.frame(df_test) # subsetting
keep_sp <- data.frame(unlist(keep_sp$scientificname)) #unlisting
names(keep_sp) <- 'ScientificName'
keep_ap <- data.frame(df_test) # subsetting
keep_ap <- data.frame(unlist(keep_ap$AphiaID))
names(keep_ap) <- 'AphiaID'
keep <- cbind(keep_ap, keep_sp)

dat.ices <- subset(dat.ices, dat.ices$AphiaID %in% keep_ap$AphiaID)
dat.ices <- left_join(dat.ices, keep, by='AphiaID')
dat.ices$Species <- dat.ices$ScientificName
dat.ices$ScientificName <- NULL
survey <- dat.ices

survey <- survey %>%
  select(Survey, HaulID, StatRec, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST,
         Species, BycSpecRecCode, numcpue, wtcpue, numh, wgth, num, wgt)
survey$AphiaID <- NULL


### Code to integrate from Anna on species bycatch corrections                                                         'Melanogrammus aeglefinus','Merlangius merlangus','Trisopterus esmarkii')))
survey <- data.frame(survey)
survey <- survey %>% 
  mutate(Species = as.character(Species)) %>% 
  #mutate(Survey = as.factor(Survey),
  #       Species = as.factor(Species)) %>% 
  
  filter(!(BycSpecRecCode==0 & Survey=='NS-IBTS' & !Species %in% c('Clupea harengus','Sprattus sprattus','Scomber scombrus','Gadus morhua',
                                                                   'Melanogrammus aeglefinus','Merlangius merlangus','Trisopterus esmarkii')),
         !(BycSpecRecCode==2 & !Species %in% c('Ammodytidae','Anarhichas lupus','Argentina silus','Argentina sphyraena',
                                                'Chelidonichthys cuculus','Callionymus lyra','Eutrigla gurnardus','Lumpenus lampretaeformis',
                                                'Mullus surmuletus','Squalus acanthias','Trachurus trachurus',
                                                'Platichthys flesus','Pleuronectes platessa','Limanda limanda','Lepidorhombus whiffiagoni','Hippoglossus hippoglossus','Hippoglossoides platessoi',
                                                'Glyptocephalus cynoglossu','Microstomus kitt','Scophthalmus maximus','Scophthalmus rhombus','Solea solea',
                                                'Pollachius virens','Pollachius pollachius','Trisopterus luscus','Trisopterus minutus','Micromesistius poutassou','Molva molva',
                                                'Merluccius merluccius','Brosme brosme','Clupea harengus','Sprattus sprattus','Scomber scombrus','Gadus morhua','Melanogrammus aeglefinus',
                                                'Merlangius merlangus','Trisopterus esmarkii')),
         !(BycSpecRecCode==3 & !Species %in% c('Pollachius virens','Pollachius pollachius','Trisopterus luscus','Trisopterus minutus','Micromesistius poutassou','Molva molva',
                                               'Merluccius merluccius','Brosme brosme','Clupea harengus','Sprattus sprattus',
                                               'Scomber scombrus','Gadus morhua','Melanogrammus aeglefinus','Merlangius merlangus','Trisopterus esmarkii')),
         !(BycSpecRecCode==4 & !Species %in% c('Platichthys flesus','Pleuronectes platessa','Limanda limanda','Lepidorhombus whiffiagoni','Hippoglossus hippoglossus','Hippoglossoides platessoi',
                                               'Glyptocephalus cynoglossu','Microstomus kitt','Scophthalmus maximus','Scophthalmus rhombus','Solea solea',
                                               'Clupea harengus','Sprattus sprattus','Scomber scombrus','Gadus morhua','Melanogrammus aeglefinus',
                                               'Merlangius merlangus','Trisopterus esmarkii')),
         !(BycSpecRecCode==5 & !Species %in% c('Ammodytidae','Anarhichas lupus','Argentina silus','Argentina sphyraena',
                                               'Chelidonichthys cuculus','Callionymus lyra','Eutrigla gurnardus','Lumpenus lampretaeformis',
                                               'Mullus surmuletus','Squalus acanthias','Trachurus trachurus','Clupea harengus',
                                               'Sprattus sprattus','Scomber scombrus','Gadus morhua','Melanogrammus aeglefinus',
                                               'Merlangius merlangus','Trisopterus esmarkii'))) %>% 
  mutate(Species = if_else(Species %in% c('Dipturus batis','Dipturus flossada','Dipturus intermedia','Dipturus'),'Dipturus spp', Species),
         Species = if_else(Sepcies %in% c('Liparis montagui','Liparis liparis','Liparis liparis liparis'),'Liparis spp', Species),
         Species = if_else(Species %in% c('Chelon aurata','Chelon ramada'),'Chelon spp',Species),
         Species = if_else(Species %in% c('Mustelus','Mustelus mustelus','Mustelus asterias'), 'Mustelus spp', Species),
         Species = if_else(Species %in% c('Alosa','Alosa alosa','Alosa fallax'),'Alosa spp', Species),
         Species = if_else(Species %in% c('Argentina','Argentinidae','Argentina silus','Argentina sphyraena'),'Argentina spp', Species),
         Species = if_else(Species %in% c('Callionymus reticulatus','Callionymus maculatus'),'Callionymus spp', Species),
         Species = if_else(Species %in% c('Ciliata mustela','Ciliata septentrionalis'), 'Ciliata spp', Species),
         Species = if_else(Species %in% c('Gaidropsarus','Gaidropsaurus macrophthalmus','Gaidropsaurus mediterraneus','Gaidropsaurus vulgaris'),'Gaidropsarus spp', Species),
         Species = if_else(Species %in% c('Sebastes','Sebastes norvegicus','Sebastes mentella','Sebastes marinus'),'Sebastes spp', Species),
         Species = if_else(Species %in% c('Syngnathus','Syngnathus rostellatus','Syngnathus acus','Syngnathus typhle','Nerophis ophidion'),'Syngnatus spp', Species),
         Species = if_else(Species %in% c('Pomatoschistus','Pomatoschistus microps','Pomatoschistus minutus','Pomatoschistus pictus'), 'Pomatoschistus spp', Species),
         Species = if_else(Species %in% c('Lesueurigobius','Gobius cobitis','Gobius niger','Leusueurigobius friesii','Neogobius melanostomus','Neogobius'),'Gobius spp', Species))


##########################################################################################
#### Select quarters
##########################################################################################
survey <- survey %>% 
  filter(!(Survey=='NS-IBTS' & Quarter %in% c(2,4)),
         !(Survey=='NS-IBTS' & Quarter==1 & Year<1967),
         !(Survey=='SWC-IBTS' & Quarter %in% c(2,3)),
         !(Survey=='BITSS' & Quarter %in% c(2,3)),
         !(Survey %in% c('BITS','BITSL')),
         !(Survey=='BTS' & Year<1987),
         !(Survey=='IE-IGFS' & Year<2003),
         !(Survey=='NIGFS' & Year<2006))

#xx <- data.frame(survey2) %>% 
#  group_by(Survey, Quarter) %>% 
#  dplyr::summarize(Hauls=length(unique(HaulID)), Year=paste(min(Year), max(Year), sep='-'))



##########################################################################################
#### SAVE DATA
##########################################################################################
setwd('~/RA_DTUAqua/SensitiveFish/SensitiveDemSpecies/data')
save(survey, file='ICESsurveysByc18062020.RData')
#load('ICESsurveysByc18062020.RData')
