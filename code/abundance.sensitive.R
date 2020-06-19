rm(list=ls())
Sys.setenv(LANG = "en")
memory.limit(size = 30000)

##########################################################################################
### Libraries
##########################################################################################
library(dplyr)
library(ggplot2)
library(egg)
library(RColorBrewer)
library(rgdal)
library(tidyr)

##########################################################################################
### Load data
##########################################################################################
### Load sensi. species index
sensi <- read.csv('results/Sensi.csv')
sensi <- sensi %>% 
  mutate(Species = as.character(Species),
         Species = if_else(Species=='Dipturus batis-complex', 'Dipturus spp', Species),
         Species = if_else(Species=='Mustelus mustelus/asterias', 'Mustelus spp', Species)
  )

### Load species abundances across surveys
load('data/ICESsurveysByc18062020.RData')
survey <- survey %>% 
  mutate(Species = if_else(Species %in% c('Mustelus','Mustelus asterias','Mustelus mustelus'),'Mustelus spp',Species))

### Load ICES rectangles shapefiles
rect <- readOGR(dsn = "data/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp",layer="ICES_Statistical_Rectangles_Eco")
# subset shapefile for ICES rectangles sampled by the surveys
surveyed.rect <- subset(rect, ICESNAME %in% unique(survey$StatRec))


##########################################################################################
### Select sensi spp
##########################################################################################
# Threshold to define sensitive species
sensi.spp <- sensi %>% 
  filter(Fsensi<=0.43) %>% # based on Rindorf et al., 2020 Journal of Applied Ecology
  select(Species) 
sensi.spp <- as.vector(sensi.spp[,1])
length(sensi.spp) # as in Rindorf et al., 2020, 59 species are found sensitive and kept in the analysis

# Select snesitive spp in surveys
survey <- survey %>% 
  mutate(Species = as.character(Species)) %>% 
  filter(survey$Species %in% sensi.spp)
length(unique(survey$Species))


##########################################################################################
### Select species with 50% observed in time-series
##########################################################################################

# Species coverage
num.obs <- survey %>% 
  group_by(Year, Species) %>% 
  summarise(ICESsq=length(unique(StatRec)))

windows(60,50)
ggplot(num.obs, aes(x=Year, y=Species, fill=ICESsq)) + geom_tile() + theme_bw() +
  scale_fill_distiller(palette = "RdYlBu")

# Survey coverage
num.surv <- survey %>% 
  mutate(SurvQ = paste(Survey, Quarter, sep='-')) %>% 
  group_by(Year, SurvQ) %>% 
  summarise(ICESsq=length(unique(StatRec)))

windows(60,50)
ggplot(num.surv, aes(x=Year, y=SurvQ, fill=ICESsq)) + geom_tile() + theme_bw() +
  scale_fill_distiller(palette = "RdYlBu")


##########################################################################################
### Try for one spp
##########################################################################################
survey.spp <- survey %>% 
  filter(Species=='Molva molva')

# Extent of available data
summary.spp <- survey.spp %>% 
  mutate(StatRec=as.character(StatRec)) %>% 
  group_by(StatRec, Year) %>% 
  summarize(haul=length(unique(HaulID)))

windows(60,50)
ggplot(summary.spp, aes(x=Year, y=StatRec, fill=haul)) + geom_tile() + theme_bw() +
  scale_fill_distiller(palette='RdYlBu')

# Map of log abundance per haul
summary.spp.h <- survey.spp %>% 
  group_by(StatRec, ShootLat, ShootLong) %>% 
  summarize(num=mean(num, na.rm=T))

world <- map_data("world")
windows(60,50)
ggplot() + scale_color_distiller(palette = "RdYlBu")+ 
  geom_polygon(data=surveyed.rect, aes(long, lat, group = group), colour = 'grey', fill='lightgrey', size = 0.7) +
  geom_point(data=summary.spp.h, aes(x=ShootLong, y=ShootLat, col=log(num)))+
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(xlim=c(-25,30), ylim=c(35,62)) + theme_bw()


# Map with average num per ICES square and all hauls
summary.spp.sq <- survey.spp %>% 
  mutate(StatRec = as.character(StatRec)) %>% 
  rename(ICESNAME = StatRec) %>% 
  group_by(ICESNAME) %>% 
  summarize(num=mean(num, na.rm=T))

map_data_fortified <- fortify(surveyed.rect, region = "ICESNAME")
map_data <- map_data_fortified %>% left_join(summary.spp.sq, by = c("id" = "ICESNAME"))

world <- map_data("world")
windows(60,50)
ggplot(data=map_data, aes(long, lat, group = group, fill = log(num))) +
  geom_polygon(colour = 'grey',  size = 0.7) +
  scale_fill_distiller(palette = "RdYlBu", na.value='lightgrey')+ 
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(xlim=c(-25,30), ylim=c(35,62)) + theme_bw()


### Realized habitat
realized.habitat <- survey %>% 
  filter(Species=='Molva molva') %>% 
  group_by(StatRec) %>% 
  summarize(num=1)
  
map_data_fortified <- fortify(surveyed.rect, region = "ICESNAME")
map_data <- map_data_fortified %>% left_join(realized.habitat, by = c("id" = "StatRec"))

world <- map_data("world")
ggplot(data=map_data, aes(long, lat, group = group, fill = num)) +
  geom_polygon(colour = 'grey',  size = 0.7) +
  #scale_fill_distiller(colour='lightblue', na.value='lightgrey')+ 
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(xlim=c(-25,30), ylim=c(35,62)) + theme_bw()

# only keep ices squares where species is present
ices.keep <- sort(unique(realized.habitat$StatRec))

# subset data with the right ICES squares --> in order to assign 0
survey.spp <- survey %>% 
  filter(StatRec %in% ices.keep)

# add 0 and only keep the spp of interest
survey.spp.0 <- survey.spp %>% 
  complete(HaulID, Species,
           fill=list(numcpue=0, wtcpue=0, numh=0, wgth=0, num=0, wgt=0)) %>% 
  as.data.frame() %>% 
  filter(Species=='Molva molva') %>% 
  select(HaulID, Species, numcpue, wtcpue, numh, wgth, num, wgt)

survey.spp <- survey.spp %>% select(-numcpue, -wtcpue, -numh, -wgth, -num, -wgt, -Species)
survey.spp <- left_join(survey.spp.0, survey.spp, by=c('HaulID'))

# total number of hauls
length(unique(survey.spp$HaulID))

yearly.s.mean <- survey.spp %>% 
  group_by(Year, Survey) %>% 
  summarize_at(.vars=c('numcpue', 'numh', 'num'), .funs=function(x) mean(x, na.rm=T))

ggplot(yearly.s.mean, aes(x=Year, y=numh, group=Survey, col=Survey)) + geom_point() + geom_line() +
  theme_bw()

yearly.sq.mean <- survey.spp %>% 
  group_by(Year, StatRec) %>% 
  summarize_at(.vars=c('numcpue', 'numh', 'num'), .funs=function(x) mean(x, na.rm=T))

ggplot(yearly.sq.mean, aes(x=Year, y=numh)) + geom_point() + geom_smooth(method='loess')









