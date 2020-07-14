#############################################################
#### Code to download and clean survey data from DATRAS
#### Adapted to build abundance indices of sensitive and by-caught species
#### Related paper: Are fish sensitive to trawling recovering in the Northeast Atlantic?
#### Coding: Aurore Maureaud, July 2020
#############################################################
Sys.setenv(LANG = "en")
rm(list=ls())

##########################################################################################
#### LOAD LIBRARIES
##########################################################################################
library(data.table)
library(dplyr)
library(icesDatras)
library(worms)
library(worrms)
library(crul)
library(urltools)


##########################################################################################
#### LOAD FILES
##########################################################################################
last.year <- 2019

# Haul info from Datras
hh.ns <- getDATRAS(record='HH', survey='NS-IBTS', years=c(1967:last.year), quarters=c(1,3))
hh.baltic <- getDATRAS(record='HH', survey='BITS', years=c(1991:last.year), quarters=c(1,4))
hh.evhoe <- getDATRAS(record='HH', survey='EVHOE', years=c(1997:last.year), quarters=4)
hh.cgfs <- getDATRAS(record='HH', survey='FR-CGFS', years=c(1998:last.year), quarters=4)
hh.igfs <- getDATRAS(record='HH', survey='IE-IGFS', years=c(2003:last.year), quarters=4)
hh.nigfs <- getDATRAS(record='HH', survey='NIGFS', years=c(2005:last.year), quarters=c(1:4))
hh.pt <- getDATRAS(record='HH', survey='PT-IBTS', years=c(2002:last.year), quarters=c(3:4))
hh.rock <- getDATRAS(record='HH', survey='ROCKALL', years=c(1999:2009), quarters=3)
hh.scorock <- getDATRAS(record='HH', survey='SCOROC', years=c(2011:last.year), quarters=3)
hh.spa <- getDATRAS(record='HH', survey='SP-ARSA', years=c(1996:last.year), quarters=c(1:4))
hh.spn <- getDATRAS(record='HH', survey='SP-NORTH', years=c(1990:last.year), quarters=c(3:4))
hh.spp <- getDATRAS(record='HH', survey='SP-PORC', years=c(2001:last.year), quarters=c(3:4))
hh.sns <- getDATRAS(record='HH', survey='SNS', years=c(2002:last.year), quarters=c(3:4))
hh.swc <- getDATRAS(record='HH', survey='SWC-IBTS', years=c(1985:2010), quarters=c(1:4))
hh.scowcgfs <- getDATRAS(record='HH', survey='SCOWCGFS', years=c(2011:last.year), quarters=c(1:4))
hh.bts <- getDATRAS(record='HH', survey='BTS', years=c(1985:last.year), quarters=c(1:4))
hh.bts8 <- getDATRAS(record='HH', survey='BTS-VIII', years=c(2011:last.year), quarters=4)
hh.dyfs <- getDATRAS(record='HH', survey='DYFS', years=c(2002:last.year), quarters=c(3,4))

hh <- rbind(hh.ns, hh.baltic, hh.evhoe, hh.cgfs, hh.igfs, hh.nigfs, hh.pt, hh.rock, hh.scorock, hh.spa, 
            hh.spn, hh.spp, hh.sns, hh.swc, hh.scowcgfs, hh.bts, hh.bts8, hh.dyfs)

# Length info from DATRAS
hl.ns <- getDATRAS(record='HL', survey='NS-IBTS', years=c(1967:last.year), quarters=c(1,3))
hl.baltic <- getDATRAS(record='HL', survey='BITS', years=c(1991:last.year), quarters=c(1,4))
hl.evhoe <- getDATRAS(record='HL', survey='EVHOE', years=c(1997:last.year), quarters=4)
hl.cgfs <- getDATRAS(record='HL', survey='FR-CGFS', years=c(1998:last.year), quarters=4)
hl.igfs <- getDATRAS(record='HL', survey='IE-IGFS', years=c(2003:last.year), quarters=4)
hl.nigfs <- getDATRAS(record='HL', survey='NIGFS', years=c(2005:last.year), quarters=c(1:4))
hl.pt <- getDATRAS(record='HL', survey='PT-IBTS', years=c(2002:last.year), quarters=c(3:4))
hl.rock <- getDATRAS(record='HL', survey='ROCKALL', years=c(1999:2009), quarters=3)
hl.scorock <- getDATRAS(record='HL', survey='SCOROC', years=c(2011:last.year), quarters=3)
hl.spa <- getDATRAS(record='HL', survey='SP-ARSA', years=c(1996:last.year), quarters=c(1:4))
hl.spn <- getDATRAS(record='HL', survey='SP-NORTH', years=c(1990:2012), quarters=c(3:4))
hl.spn2 <- getDATRAS(record='HL', survey='SP-NORTH', years=c(2014:last.year), quarters=c(3:4))
# there is a problem with SPNorth for year 2013, so did not load the data for that year
hl.spn <- rbind(hl.spn, hl.spn2)
hl.spp <- getDATRAS(record='HL', survey='SP-PORC', years=c(2001:last.year), quarters=c(3:4))
hl.sns <- getDATRAS(record='HL', survey='SNS', years=c(2002:last.year), quarters=c(3:4))
hl.swc <- getDATRAS(record='HL', survey='SWC-IBTS', years=c(1985:2010), quarters=c(1:4))
hl.scowcgfs <- getDATRAS(record='HL', survey='SCOWCGFS', years=c(2011:last.year), quarters=c(1:4))
hl.bts <- getDATRAS(record='HL', survey='BTS', years=c(1985:last.year), quarters=c(1:4))
hl.bts8 <- getDATRAS(record='HL', survey='BTS-VIII', years=c(2011:last.year), quarters=4)
hl.dyfs <- getDATRAS(record='HL', survey='DYFS', years=c(2002:last.year), quarters=c(3,4))

hl <- rbind(hl.ns, hl.baltic, hl.evhoe, hl.cgfs, hl.igfs, hl.nigfs, hl.pt, hl.rock, hl.scorock, hl.spa, 
            hl.spn, hl.spp, hl.sns, hl.swc, hl.scowcgfs, hl.bts, hl.bts8, hl.dyfs)

rm(hl.ns, hl.baltic, hl.evhoe, hl.cgfs, hl.igfs, hl.nigfs, hl.pt, hl.rock, hl.scorock, hl.spa, 
   hl.spn, hl.spn2, hl.spp, hl.sns, hl.swc, hl.scowcgfs, hl.bts, hl.bts8, hl.dyfs, 
   hh.ns, hh.baltic, hh.evhoe, hh.cgfs, hh.igfs, hh.nigfs, hh.pt, hh.rock, hh.scorock, hh.spa, 
   hh.spn, hh.spp, hh.sns, hh.swc, hh.scowcgfs, hh.bts, hh.bts8, hh.dyfs)


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
hl <- subset(hl, hl$HaulID %in% hh$HaulID)

#if(identical(sort(unique(hh$HaulID)),sort(unique(hl$HaulID)))){
#  save(hh, file='~/RA_DTUAqua/SensitiveFish/SensitiveDemSpecies/data/HH.09.07.2020.RData')
#  save(hl, file='~/RA_DTUAqua/SensitiveFish/SensitiveDemSpecies/data/HL.09.07.2020.RData')
#}
#load('data/HH.09.07.2020.RData')
#load('data/HL.09.07.2020.RData')

##########################################################################################
#### MERGE HH and HL FILES
##########################################################################################

haulidhl <- sort(unique(hl$HaulID))
haulidhh <- sort(unique(hh$HaulID))
identical(haulidhh, haulidhl)

#survey <- merge(hh, hl, by='HaulID', all.x=FALSE, all.y=TRUE)
survey <- right_join(hh, hl, by=c('HaulID','Survey','Quarter','Country','Ship','Gear','StNo','HaulNo','Year'))
nrow(survey)==nrow(hl)

survey <- survey %>% 
  dplyr::rename(SBT = BotTemp,
         SST = SurTemp,
         Speed = GroundSpeed,
         AphiaID = Valid_Aphia)

### Check if the HaulID is unique
### Not the case for the baltic sea, a lot of duplicates!!!
#ids <- unique(hh$HaulID)
#pb <- vector()
#for(i in 1:length(ids)){
#  x <- which(hh$HaulID==ids[i])
#  if(length(x)>1){pb[length(pb)+1] <- ids[i]}
#}
#print(pb) # dim 0 ok!


##########################################################################################
#### REMOVE INVALID DATA
##########################################################################################
survey <- survey %>% 
  filter(HaulVal %in% 'V', #Remove invalid hauls
         !is.na(AphiaID), # Remove invalid species records
         SpecVal %in% c(1,10,4,7),
         DataType %in% c('S','R','C'))


##########################################################################################
#### RESCALE DATA INTO ABUNDANCE FOR THE HAUL DURATION AND ABUNDANCE AT LENGTH
##########################################################################################
# If Data Type=='C', abundance at length already readjusted with time so get back the abundance for the actual duration of the haul.
# If data type=='R', abundance at length is mulitplied by subfactor and adjusted to time
survey <- survey %>% 
  mutate(HLNoAtLngt = if_else(DataType=='C', HLNoAtLngt*SubFactor*HaulDur/60, HLNoAtLngt),
         TotalNo = if_else(DataType=='C', TotalNo*HaulDur/60, TotalNo),
         #CatCatchWgt = if_else(DataType=='C', CatCatchWgt*HaulDur/60, CatCatchWgt),
         HLNoAtLngt = if_else(DataType %in% c('S','R'),HLNoAtLngt*SubFactor,HLNoAtLngt)) %>% 
  select(-HaulVal, -DataType, -StdSpecRecCode, -SpecVal, -CatIdentifier, -SubWgt, -SubFactor)


##########################################################################################
#### GET THE SWEPT AREA
##########################################################################################
survey <- survey %>% 
  mutate(WingSpread = replace(WingSpread, WingSpread==-9, NA),
         DoorSpread = replace(DoorSpread, DoorSpread==-9, NA),
         Speed = replace(Speed, Speed==-9, NA),
         Distance = replace(Distance, Distance==-9, NA),
         Depth = replace(Depth, Depth==-9, NA),
         Area.swept = Distance*0.001*DoorSpread*0.001,
         Area.swept = if_else(is.na(Area.swept), Speed*1.852*HaulDur/60*DoorSpread*0.001, Area.swept))


##########################################################################################
#### GET CPUEs AND RIGHT COLUMNS NAMES
##########################################################################################
# Only keep abundances/weight
survey <- survey %>%
  mutate(numcpue = TotalNo/Area.swept,
         wtcpue = CatCatchWgt/(Area.swept*1000),
         numh = TotalNo*30/HaulDur, # per 30', not per hour
         wgth = CatCatchWgt*60/(HaulDur*1000),
         num = TotalNo,
         wgt = CatCatchWgt,
         Season = 'NA',
         Depth = replace(Depth, Depth<0, NA),
         SBT = replace(SBT, SBT<0, NA),
         SST = replace(SST, SST<0, NA)) %>%
  mutate(numh = if_else(Gear=='BT6' & Survey=='DYFS', numh*3/6, numh)) %>% 
  mutate(numh = if_else(Gear=='BT7', numh*4/7, numh)) %>% 
  mutate(numh = if_else(Gear=='BT8', numh*4/8, numh)) %>% 
  #there have been minor changes to the gear in these surveys, it should not affect sensitives
  mutate(Survey = if_else(Survey=='SCOWCGFS', 'SWC-IBTS', Survey)) %>% 
  mutate(Survey = if_else(Survey=='SCOROC','ROCKALL',Survey)) %>% 
  #two different gear sizes are used in the Baltic - they cannot converted reliably for sensitives, so they are treated as separate surveys in my analysis.
  mutate(Survey = if_else(Survey=='BITS' & Gear=='TVS', 'BITSS', Survey)) %>% 
  mutate(Survey = if_else(Survey=='BITS' & Gear=='TVL', 'BITSL', Survey)) %>% 
  group_by(Survey, HaulID, StatRec, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST, AphiaID, BycSpecRecCode) %>%
  summarize_at(.vars=c('numcpue', 'wtcpue', 'numh', 'wgth', 'num', 'wgt'), .funs=function(x) sum(x, na.rm=T)) %>%
  select(Survey, HaulID, StatRec, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, Gear, Depth, SBT, SST,
         AphiaID, BycSpecRecCode, numcpue, wtcpue, numh, wgth, num, wgt)
survey <- data.frame(survey)


##########################################################################################
#### Clean species names
##########################################################################################

survey$Species <- NA
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

keep_sp <- data.frame(df_test) # subsetting
keep_sp <- data.frame(unlist(keep_sp$valid_name)) #unlisting
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
  select(Survey, HaulID, StatRec, Year, Month, Quarter, Season, ShootLat, ShootLong, HaulDur, Area.swept, 
         Gear, Depth, SBT, SST, Species, BycSpecRecCode, numcpue, wtcpue, numh, wgth, num, wgt)
survey$AphiaID <- NULL


### Code to integrate from Anna on species bycatch corrections
survey <- data.frame(survey)
survey <- survey %>%
  mutate(Species = recode(Species, 'Synaphobranchus kaupii'='Synaphobranchus kaupi',
                          'Dipturus batis'='Dipturus spp','Dipturus flossada'='Dipturus spp',
                          'Dipturus batis-complex'='Dipturus spp','Dipturus intermedia'='Dipturus spp',
                          'Dipturus'='Dipturus spp','Liparis montagui'='Liparis spp',
                          'Liparis liparis'='Liparis spp','Liparis liparis liparis'='Liparis spp',
                          'Chelon aurata'='Chelon spp','Chelon ramada'='Chelon spp',
                          'Mustelus mustelus/asterias'='Mustelus spp','Mustelus'='Mustelus spp',
                          'Mustelus mustelus'='Mustelus spp','Mustelus asterias'='Mustelus spp',
                          'Alosa'='Alosa spp','Alosa alosa'='Alosa spp','Alosa fallax'='Alosa spp',
                          'Argentina'='Argentina spp','Argentinidae'='Argentina spp',
                          'Argentina silus'='Argentina spp','Argentina sphyraena'='Argentina spp',
                          'Callionymus reticulatus'='Callionymus spp','Callionymus maculatus'='Callionymus spp',
                          'Ciliata mustela'='Ciliata spp','Ciliata septentrionalis'='Ciliata spp',
                          'Gaidropsarus'='Gaidropsarus spp','Gaidropsaurus macrophthalmus'='Gaidropsarus spp',
                          'Gaidropsaurus mediterraneus'='Gaidropsarus spp','Gaidropsaurus vulgaris'='Gaidropsarus spp',
                          'Sebastes'='Sebastes spp','Sebastes norvegicus'='Sebastes spp','Sebastes mentella'='Sebastes spp',
                          'Sebastes marinus'='Sebastes spp','Syngnathus'='Syngnatus spp',
                          'Syngnathus rostellatus'='Syngnatus spp','Syngnathus acus'='Syngnatus spp',
                          'Syngnathus typhle'='Syngnatus spp','Nerophis ophidion'='Syngnatus spp',
                          'Pomatoschistus'='Pomatoschistus spp','Pomatoschistus microps'='Pomatoschistus spp',
                          'Pomatoschistus minutus'='Pomatoschistus spp','Pomatoschistus pictus'='Pomatoschistus spp',
                          'Lesueurigobius'='Gobius spp','Gobius cobitis'='Gobius spp','Gobius niger'='Gobius spp',
                          'Leusueurigobius friesii'='Gobius spp','Neogobius melanostomus'='Gobius spp',
                          'Neogobius'='Gobius spp')) %>% 
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
                                               'Merlangius merlangus','Trisopterus esmarkii')))


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

#xx <- data.frame(survey) %>% 
#  group_by(Survey, Quarter) %>% 
#  dplyr::summarize(Hauls=length(unique(HaulID)), Year=paste(min(Year), max(Year), sep='-'))


##########################################################################################
#### SAVE DATA
##########################################################################################
save(survey, file='data/ICESsurveysByc10072020.RData')
