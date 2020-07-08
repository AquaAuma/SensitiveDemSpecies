rm(list=ls())
Sys.setenv(LANG = "en")

##########################################################################################
### Libraries
##########################################################################################
library(dplyr)
library(ggplot2)
library(egg)
library(RColorBrewer)
library(rgdal)
library(tidyr)
library(tidyverse)
library(sjstats)
library(msir)

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

ggplot(num.obs, aes(x=Year, y=Species, fill=ICESsq)) + geom_tile() + theme_bw() +
  scale_fill_distiller(palette = "RdYlBu")

# Survey coverage
num.surv <- survey %>% 
  mutate(SurvQ = paste(Survey, Quarter, sep='-')) %>% 
  group_by(Year, SurvQ) %>% 
  summarise(ICESsq=length(unique(StatRec)))

ggplot(num.surv, aes(x=Year, y=SurvQ, fill=ICESsq)) + geom_tile() + theme_bw() +
  scale_fill_distiller(palette = "RdYlBu")

# Selection of species present in 50% of observed time-series
# memo on how to use spread: https://jtr13.github.io/spring19/hx2259_qz2351.html 
replace_by_one <- function(x){if(x>0){x <- 1}
  return(x)}
select50 <- survey %>% 
  group_by(Species, Year) %>% 
  summarize(numh=max(numh, na.rm=T)) %>% 
  spread(Year, numh, fill=0) %>% 
  mutate_all(replace_by_one) %>%
  as.data.frame() %>% 
  column_to_rownames("Species")
select50$prop <- apply(select50, 1, FUN=function(x) sum(x)/ncol(select50))
select50 <- subset(select50, prop>=0.5)
select50 <- rownames(select50)
print(select50) # list of selected spp


##########################################################################################
### Try for one spp
##########################################################################################

Survey <- sort(unique(survey$Survey))
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(length(Survey))
cols <- data.frame(cbind(Survey, mycolors))

plot.loess='FALSE'
pdf(file='results/Index.Std.after.loess.pdf')
for (i in 1:length(select50)){
species <- select50[i]
print(species)
#species <- 'Lepidorhombus whiffiagonis'

survey.spp <- survey %>% 
  filter(Species==species)

### 1. Realized habitat
realized.habitat <- survey %>% 
  filter(Species==species) %>% 
  group_by(StatRec) %>% 
  summarize(num=1)

#map_data_fortified <- fortify(surveyed.rect, region = "ICESNAME")
#map_data <- map_data_fortified %>% left_join(realized.habitat, by = c("id" = "StatRec"))
#world <- map_data("world")
#ggplot(data=map_data, aes(long, lat, group = group, fill = num)) +
#  geom_polygon(colour = 'grey',  size = 0.7) +
#  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
#  coord_quickmap(xlim=c(-25,30), ylim=c(35,62)) + theme_bw()

# only keep ices squares where species is present
ices.keep <- sort(unique(realized.habitat$StatRec))

# subset data with the right ICES squares --> in order to assign 0
survey.spp <- survey %>% 
  filter(StatRec %in% ices.keep)
length(unique(survey.spp$HaulID)) # remaining hauls


### 2. Add missing zeros
# add 0 and only keep the spp of interest
survey.spp.0 <- survey.spp %>% 
  complete(HaulID, Species,
           fill=list(numcpue=0, wtcpue=0, numh=0, wgth=0, num=0, wgt=0)) %>% 
  as.data.frame() %>% 
  filter(Species==species) %>% 
  select(HaulID, Species, numcpue, wtcpue, numh, wgth, num, wgt)

survey.spp <- survey.spp %>% select(-numcpue, -wtcpue, -numh, -wgth, -num, -wgt, -Species)
survey.spp <- left_join(survey.spp.0, survey.spp, by=c('HaulID'))
length(unique(survey.spp$HaulID)) # remaining hauls


### 3. Calculate average abundance/per hour per Year/ICESsq/Survey
# yearly average per survey
yearly.s.mean <- survey.spp %>% 
  mutate(numh = numh/2) %>% # transform the /h into /30', matters when taking the average with 0
  group_by(Year, Survey) %>% 
  summarize_at(.vars=c('numcpue', 'numh', 'num'), .funs=function(x) mean(x, na.rm=T))

mean.per.survey <- yearly.s.mean %>% # to remove surveys with only 0
  group_by(Survey) %>% 
  summarize(numh=mean(numh)) %>% 
  filter(numh>0)

yearly.s.mean <- yearly.s.mean %>% 
  filter(Survey %in% mean.per.survey$Survey)

#mycolors <- colorRampPalette(brewer.pal(8, "RdYlBu"))(length(unique(yearly.s.mean$Survey)))
#ggplot(yearly.s.mean, aes(x=Year, y=numh, group=Survey, col=Survey)) + geom_line(lwd=2) +
#  theme_bw() + scale_color_manual(values=mycolors) + ylab('NUMCPUE') + ggtitle(species)


### 4. Divide by long-term average: the last 10 years
# relative to last decade
last.decade <- sort(unique(survey.spp$Year), decreasing=TRUE)[1:10]
mean.recent <- yearly.s.mean %>% 
  filter(Year %in% last.decade) %>% 
  group_by(Survey) %>% 
  summarize(mean.recent=mean(numh))
mean.recent <- mean.recent %>% 
  filter(mean.recent!=0)# sometimes, mean.recent is 0 for some surveys, remove the surveys with recent mean=0

yearly.s.mean <- data.frame(yearly.s.mean)
yearly.s.mean <- yearly.s.mean %>%
  filter(Survey %in% mean.recent$Survey) %>% 
  left_join(mean.recent, by='Survey') %>% 
  mutate(mean.recent = mean.recent,
         numh = numh/mean.recent,
         num = num/mean.recent,
         numcpue = numcpue/mean.recent) %>% 
  filter(numh<=5) # remove year with 5 times the long-term average

mycolors <- colorRampPalette(brewer.pal(8, "RdYlBu"))(length(unique(yearly.s.mean$Survey)))
#ggplot(yearly.s.mean, aes(x=Year, y=numh, group=Survey, col=Survey)) + geom_line(lwd=2) +
#  theme_bw() + scale_color_manual(values=mycolors) +ylab(paste('numcpue/average ',last.decade[10],'-',last.decade[1], sep='')) +
#  geom_hline(yintercept=1, lwd=1, lty=2, col='black') + ggtitle(species)


### 6. Get Loess per survey
# fitted to logged positive catches of each species in each survey

to.loess <- yearly.s.mean %>% 
  filter(numh>0)

#ggplot(to.loess, aes(x=Year, y=numh, group=Survey, col=Survey)) + geom_line(lwd=2) +
#  theme_bw() + scale_color_manual(values=mycolors) +ylab(paste('numcpue/average ',last.decade[10],'-',last.decade[1], sep='')) +
#  geom_hline(yintercept=1, lwd=1, lty=2, col='black')


dat.index <- data.frame()
surveys <- unique(to.loess$Survey)
weights <- rep(NA, times=length(surveys))
comb.surveys <- c()
#windows()
#par(mfrow=c(5,5))
for(s in 1:length(surveys)){
  data.s <- subset(to.loess, Survey==surveys[s])
  if(nrow(data.s)>6){
  loess.s <- loess(log(numh) ~ Year, data=data.s)
  loess.p <- predict(loess.s, se=TRUE)
  if(plot.loess==TRUE){
  plot(log(numh) ~ Year, data=data.s, main=surveys[s], ylim=c(min(loess.p$fit-2*loess.p$se.fit), max(loess.p$fit+2*loess.p$se.fit)))
  lines(loess.p$fit, x=sort(unique(data.s$Year)), col="black", lwd=2)
  lines(loess.p$fit-2*loess.p$se.fit, x=sort(unique(data.s$Year)), col="black", lty=2)
  lines(loess.p$fit+2*loess.p$se.fit, x=sort(unique(data.s$Year)), col="black", lty=2)
  text(x=data.s$Year[3], y=min(log(data.s$numh))+0.9*(range(log(data.s$numh))[2]-range(log(data.s$numh))[1]),
       labels=paste('Var: ', round(var(loess.p$fit), 2), sep=''),col="red")
  text(x=data.s$Year[3], y=min(log(data.s$numh))+0.8*(range(log(data.s$numh))[2]-range(log(data.s$numh))[1]),
       labels=paste('RSE: ', round((loess.s$s), 2), sep=''),col="blue")
  }
  
  # Calculate cv of the residuals from the loess
  loess.r <- resid(loess.s)
  #print(paste(surveys[s],': ',round(sd(loess.r),2), sep=''))
  
  #plot(loess.r ~ data.s$Year, main=(paste('CV resid: ', round(sd(loess.r),2), sep='')))
  #abline(h=0, lty=2)
  
  # if the sd of the residuals is lower than 0.75, we keep the survey-species combination
  if(sd(loess.r)<=0.9){comb.surveys[length(comb.surveys)+1] <- surveys[s]}
  
  data.s$weight <- rep(1/sd(loess.p$fit), times=nrow(data.s))
  dat.index <- rbind(dat.index, data.s)
  rm(data.s, loess.p, loess.s, loess.r)
  }
}


# plot indices per surveys after selecting combinations of spp-surveys to keep basedon loess
yearly.s.mean <- yearly.s.mean %>% 
  filter(Survey %in% comb.surveys)
mycolors <- colorRampPalette(brewer.pal(8, "RdYlBu"))(length(comb.surveys))
print(ggplot(yearly.s.mean, aes(x=Year, y=numh, group=Survey, col=Survey)) + geom_line(lwd=2) +
        theme_bw() + scale_color_manual(values=mycolors) + 
        ylab(paste('numcpue/average ',last.decade[10],'-',last.decade[1], sep='')) +
        geom_hline(yintercept=1, lwd=1, lty=2, col='black') + ggtitle(species) + xlim(1965,2020) + ylim(0,6))

}
dev.off()


try1 <- ggplot(dat.index, aes(x=Year, y=numh, size=weight, weight=weight)) + geom_point() +
  theme_bw() + ylab('Abundance/long-term average') + geom_smooth(method='loess', col='black', span=0.75) +
  theme(legend.position = 'none')

try2 <- ggplot(dat.index, aes(x=Year, y=numh)) + geom_point() +
  theme_bw() + ylab('Abundance/long-term average') + geom_smooth(aes(weight=weight), method='loess', col='black', span=0.75) +
  theme(legend.position = 'none')

egg::ggarrange(try1, try2, labels=c('',''), nrow=1)

loess.p <- fitted(loess(numh~Year,weights=weight,span=0.75, data=dat.index))


loess.spp <- loess(numh ~ Year, data=dat.index, span=0.75)
loess.p <- predict(loess.spp, se=TRUE)
plot(numh ~ Year, data=dat.index, ylim=c(min(loess.p$fit-2*loess.p$se.fit), max(loess.p$fit+2*loess.p$se.fit)))
lines(loess.p$fit, x=sort(dat.index$Year), col="black", lwd=2)
lines(loess.p$fit-2*loess.p$se.fit, x=sort(dat.index$Year), col="black", lty=2)
lines(loess.p$fit+2*loess.p$se.fit, x=sort(dat.index$Year), col="black", lty=2)

# plot annual index with loess smooth across surveys
ggplot(dat.index, aes(x=Year, y=index)) + geom_point() +
  theme_bw() + geom_smooth(method='loess', span=0.75, col='black') + xlim(1965,2019)






### Junk R code
# Extent of available data
summary.spp <- survey.spp %>% 
  mutate(StatRec=as.character(StatRec)) %>% 
  group_by(StatRec, Year) %>% 
  summarize(haul=length(unique(HaulID)))

ggplot(summary.spp, aes(x=Year, y=StatRec, fill=haul)) + geom_tile() + theme_bw() +
  scale_fill_distiller(palette='RdYlBu')

# Map of log abundance per haul
summary.spp.h <- survey.spp %>% 
  group_by(StatRec, ShootLat, ShootLong) %>% 
  summarize(num=mean(num, na.rm=T))

world <- map_data("world")
#windows(60,50)
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
#windows(60,50)
ggplot(data=map_data, aes(long, lat, group = group, fill = log(num))) +
  geom_polygon(colour = 'grey',  size = 0.7) +
  scale_fill_distiller(palette = "RdYlBu", na.value='lightgrey')+ 
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
  coord_quickmap(xlim=c(-25,30), ylim=c(35,62)) + theme_bw()












