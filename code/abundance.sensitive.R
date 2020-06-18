rm(list=ls())
Sys.setenv(LANG = "en")

##########################################################################################
### Libraries
##########################################################################################
library(dplyr)
library(ggplot2)
library(egg)
library(RColorBrewer)

##########################################################################################
### Load data
##########################################################################################
### Load sensi. species index
sensi <- read.csv('results/Sensi.csv')

### Load species abundances across surveys
load('data/ICESsurveysByc18062020.RData')


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
  filter(Species=='Dipturus oxyrinchus')

# Extent of available data
summary.spp <- survey.spp %>% 
  group_by(StatRec, Year) %>% 
  summarize(haul=length(unique(HaulID)))

windows(60,50)
ggplot(summary.spp, aes(x=Year, y=StatRec, fill=haul)) + geom_tile() + theme_bw() +
  scale_fill_distiller(palette='RdYlBu')


summary.spp.sq <- survey.spp %>% 
  group_by(StatRec, ShootLat, ShootLong) %>% 
  summarize(num=mean(num, na.rm=T))

# Map of ICES squares with presence/absence
world <- map_data("world")
ggplot() + scale_color_distiller(palette = "RdYlBu")+
  geom_point(data=summary.spp.sq, aes(x=ShootLong, y=ShootLat, col=log(num)))+
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(xlim=c(-25,30), ylim=c(35,65)) + theme_bw()






















