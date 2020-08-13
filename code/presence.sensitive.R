#############################################################
#### Code to identify sensitive species to fishing in Northwestern Europe
#### and build abundance indices
#### Related paper: Are fish sensitive to trawling recovering in the Northeast Atlantic?
#### Coding: Aurore Maureaud, July 2020
#############################################################
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
### Needed functions
##########################################################################################

# from https://gist.github.com/kylebgorman/6444612 
source('code/autoloess.R')

replace_by_one <- function(x){if(x>0){x <- 1}
  return(x)}

##########################################################################################
### Load data
##########################################################################################
### Load sensi. species index
sensi <- read.csv('results/Sensi.csv')
sensi <- sensi %>% 
  mutate(Species = recode(Species, 'Dipturus batis-complex'='Dipturus spp','Mustelus mustelus/asterias'='Mustelus spp'))

### Load species abundances across surveys
load('data/ICESsurveysByc11082020.RData')

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
  filter(survey$Species %in% sensi.spp)
length(unique(survey$Species))

# Check that all sensitive spp are in the survey data frame
setdiff(sensi.spp, unique(survey$Species)) # is 0, ok!

##########################################################################################
### Select species with 50% observed in time-series
##########################################################################################
# Selection of species present in 50% of observed time-series
select50 <- survey %>% 
  group_by(Species, Year) %>% 
  summarize(numh=max(numh, na.rm=T)) %>% 
  spread(Year, numh, fill=0) %>% 
  mutate_all(replace_by_one) %>%
  as.data.frame() %>% 
  column_to_rownames("Species")
select50$prop <- apply(select50, 1, FUN=function(x) sum(x)/ncol(select50))
select50 <- subset(select50, prop>=0.45)
select50 <- rownames(select50)
print(select50) # list of selected spp

select50 <- sensi.spp

##########################################################################################
### Build presence/absence indices for all species
##########################################################################################
### Choice to make
last.year <- 2019

### Run to get the index
pdf(file='results/Prob.presence.spp.11.08.pdf')
for (i in 1:length(sensi.spp)){
  species <- sensi.spp[i]
  print(species)
  
  survey.spp <- survey %>% 
    filter(Species==species)
  
  ### 1. Realized habitat
  realized.habitat <- survey %>% 
    filter(Species==species) %>% 
    group_by(StatRec) %>% 
    summarize(num=1)
  
  map_data_fortified <- fortify(surveyed.rect, region = "ICESNAME")
  map_data <- map_data_fortified %>% left_join(realized.habitat, by = c("id" = "StatRec"))
  world <- map_data("world")
  habitat <- ggplot(data=map_data, aes(long, lat, group = group, fill = num)) +
    geom_polygon(colour = 'grey',  size = 0.2) +
    geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='black', color="black") +
    coord_quickmap(xlim=c(-25,30), ylim=c(35,62)) + theme_bw() + ggtitle(species) + theme(legend.position = 'none') +
    xlab('') + ylab('')
  
  # only keep ices squares where species is present
  ices.keep <- sort(unique(realized.habitat$StatRec))
  survey.spp <- survey %>% 
    filter(StatRec %in% ices.keep)
  
  
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
  
  
  ### 3. Calculate proportion of presence per Year/ICESsq
  # yearly average per ICEs rectangle
  yearly.s.mean <- survey.spp %>% 
    group_by(Year, StatRec, HaulID) %>% 
    summarize(numh=sum(numh)) %>% 
    ungroup() %>% 
    mutate(numh = if_else(numh>0, 1, 0)) %>%     
    group_by(Year, StatRec) %>% 
    summarize(prop = sum(numh)/length(unique(HaulID)))
  
  # Loess fit
  dat.index <- yearly.s.mean[order(yearly.s.mean$Year),]
  loess.spp <- loess(prop ~ Year, data=dat.index, span=0.75)
  # auto-optimization, returning the best span value
  span <- autoloess(loess.spp)
  print(span)
  loess.spp <- loess(prop ~ Year, data=dat.index, span=span)
  loess.p <- predict(loess.spp, newdata=seq(from=min(dat.index$Year),to=last.year, by=1), se=TRUE)
  
  dat.fit <- data.frame(loess.p$fit)
  names(dat.fit) <- 'fit'
  dat.fit$se.low <- dat.fit$fit-2*loess.p$se.fit
  dat.fit$se.high <- dat.fit$fit+2*loess.p$se.fit
  dat.fit$Year <- seq(from=min(dat.index$Year),to=last.year, by=1)
  
  loess.ices.year <- ggplot(dat.index, aes(x=Year, y=prop)) + geom_point(shape=21, fill='lightgrey') + theme_bw() +
    xlab('Year') + ylab('Prob. of presence by ICES rect.') + theme(text=element_text(size=10)) + 
    theme(legend.position = 'none') + xlim(1960,2020) + ylim(0,1) + ggtitle('Loess ICES rect.') +
    geom_line(data=dat.fit, aes(x=Year, y=fit), lwd=1, col='black') +
    geom_line(data=dat.fit, aes(x=Year, y=se.low), lwd=0.5, col='black') +
    geom_line(data=dat.fit, aes(x=Year, y=se.high), lwd=0.5, col='black')

  
  ### 4. Calculate proportion of presence per Year
  yearly.mean <- survey.spp %>% 
    group_by(Year, StatRec, HaulID) %>% 
    summarize(numh=sum(numh)) %>% 
    ungroup() %>% 
    mutate(numh = if_else(numh>0, 1, 0)) %>%     
    group_by(Year, StatRec) %>% 
    summarize(prop = sum(numh)/length(unique(HaulID))) %>% 
    ungroup() %>% group_by(Year) %>% 
    summarize(prop = mean(prop))
  
  ### Loess fit
  dat.index <- yearly.mean[order(yearly.mean$Year),]
  loess.spp <- loess(prop ~ Year, data=dat.index, span=0.75)
  # auto-optimization, returning the best span value
  span <- autoloess(loess.spp)
  print(span)
  loess.spp <- loess(prop ~ Year, data=dat.index, span=span)
  loess.p <- predict(loess.spp, newdata=seq(from=min(dat.index$Year),to=last.year, by=1), se=TRUE)
  
  dat.fit <- data.frame(loess.p$fit)
  names(dat.fit) <- 'fit'
  dat.fit$se.low <- dat.fit$fit-2*loess.p$se.fit
  dat.fit$se.high <- dat.fit$fit+2*loess.p$se.fit
  dat.fit$Year <- seq(from=min(dat.index$Year),to=last.year, by=1)
  
  loess.year <- ggplot(dat.index, aes(x=Year, y=prop)) + geom_point(shape=21, fill='lightgrey') + theme_bw() +
    xlab('Year') + ylab('Presence/ICES rect.') + theme(text=element_text(size=10)) + 
    theme(legend.position = 'none') + xlim(1960,2020) + ylim(0,1) + ggtitle('Loess on prop.') +
    geom_line(data=dat.fit, aes(x=Year, y=fit), lwd=1, col='black') +
    geom_line(data=dat.fit, aes(x=Year, y=se.low), lwd=0.5, col='black') +
    geom_line(data=dat.fit, aes(x=Year, y=se.high), lwd=0.5, col='black')
  
  
  ### 5. Calculate proportion of presence per Year/survey
  # yearly average per Survey
  yearly.s.mean <- survey.spp %>% 
    group_by(Year, Survey, HaulID) %>% 
    summarize(numh=sum(numh)) %>% 
    ungroup() %>% 
    mutate(numh = if_else(numh>0, 1, 0)) %>%     
    group_by(Year, Survey) %>% 
    summarize(prop = sum(numh)/length(unique(HaulID)))
  
  means.survey <- yearly.s.mean %>% 
    group_by(Survey) %>%
    summarize(prop=mean(prop)) %>% 
    filter(prop>0)
  
  yearly.s.mean <- yearly.s.mean %>% 
    filter(Survey %in% means.survey$Survey)
  
  mycolors <- colorRampPalette(brewer.pal(8, "RdYlBu"))(length(unique(yearly.s.mean$Survey)))
  index <- ggplot() + geom_line(data=yearly.s.mean, aes(x=Year, y=prop, group=Survey, col=Survey), lwd=1) +
    theme_bw() + scale_color_manual(values=mycolors) + ylab('Presence per Survey/All') + 
    theme(legend.position = 'none') + theme(text=element_text(size=10)) +
    geom_line(data=yearly.mean, aes(x=Year, y=prop), lwd=2) + ggtitle('Presence per Survey/All')
  
  # Loess fit per survey
  surveys <- unique(yearly.s.mean$Survey)
  dat.index <- data.frame()
  weights <- c()
  comb.surveys <- c()
  for (s in 1:length(surveys)){
    dat.s <- subset(yearly.s.mean, Survey==surveys[s])
    dat.s <- dat.s[order(dat.s$Year),]
    loess.spp <- loess(prop ~ Year, data=dat.s, span=0.75)
    # auto-optimization, returning the best span value
    span <- autoloess(loess.spp)
    print(span)
    loess.spp <- loess(prop ~ Year, data=dat.s, span=span)
    if(!is.infinite(loess.spp$s)){
      if(!is.na(loess.spp$s)){
        loess.p <- predict(loess.spp, newdata=seq(from=min(dat.s$Year),to=last.year, by=1), se=TRUE)
        dat.fit <- data.frame(loess.p$fit)
        names(dat.fit) <- 'fit'
        dat.fit$Survey <- rep(surveys[s], times=nrow(dat.fit))
        dat.fit$se.low <- dat.fit$fit-2*loess.p$se.fit
        dat.fit$se.high <- dat.fit$fit+2*loess.p$se.fit
        dat.fit$Year <- seq(from=min(dat.s$Year),to=last.year, by=1)
        weights[length(weights)+1] <- 1/10^(sd(loess.p$fit)^2)
        comb.surveys[length(comb.surveys)+1] <- surveys[s]
        dat.index <- rbind(dat.index, dat.fit)}}
  }
  yearly.s.mean <- yearly.s.mean %>% 
    filter(Survey %in% unique(dat.index$Survey))
  loess.survey.year <- ggplot(yearly.s.mean, aes(x=Year, y=prop, group=Survey, colour=Survey)) + 
    geom_point(shape=21, fill='lightgrey') + theme_bw() +
    xlab('Year') + ylab('Presence/ICES rect.') + theme(text=element_text(size=10)) + 
    theme(legend.position = 'none') + xlim(1960,2020) + ylim(0,1) + ggtitle('Loess per survey') +
    geom_line(data=dat.index, aes(x=Year, y=fit, group=Survey, colour=Survey), lwd=1) +
    geom_line(data=dat.index, aes(x=Year, y=se.low, group=Survey, colour=Survey), lwd=0.5) +
    geom_line(data=dat.index, aes(x=Year, y=se.high, group=Survey, colour=Survey), lwd=0.5)
  
  # Loess based on selected surveys
  loess.spp <- loess(prop ~ Year, data=yearly.s.mean, span=0.75)
  # auto-optimization, returning the best span value
  span <- autoloess(loess.spp)
  print(span)
  loess.spp <- loess(prop ~ Year, data=yearly.s.mean, span=span)
  loess.p <- predict(loess.spp, newdata=seq(from=min(yearly.s.mean$Year),to=last.year, by=1), se=TRUE)
  dat.fit <- data.frame(loess.p$fit)
  names(dat.fit) <- 'fit'
  dat.fit$se.low <- dat.fit$fit-2*loess.p$se.fit
  dat.fit$se.high <- dat.fit$fit+2*loess.p$se.fit
  dat.fit$Year <- seq(from=min(yearly.s.mean$Year),to=last.year, by=1)
  loess.surveys <- ggplot(yearly.s.mean, aes(x=Year, y=prop)) + geom_point(shape=21, fill='lightgrey') + theme_bw() +
    xlab('Year') + ylab('Presence/Survey') + theme(text=element_text(size=10)) + 
    theme(legend.position = 'none') + xlim(1960,2020) + ylim(0,1) + ggtitle('Loess on surveys') +
    geom_line(data=dat.fit, aes(x=Year, y=fit), lwd=1, col='black') +
    geom_line(data=dat.fit, aes(x=Year, y=se.low), lwd=0.5, col='black') +
    geom_line(data=dat.fit, aes(x=Year, y=se.high), lwd=0.5, col='black')
  
  # Weighted Loess based on selected surveys
  weights <- data.frame(cbind(comb.surveys, weights))
  yearly.s.mean <- yearly.s.mean %>% 
    filter(Survey %in% weights$comb.surveys)
  weights$comb.surveys <- as.character(weights$comb.surveys)
  weights$weights <- as.numeric(as.vector(weights$weights))
  yearly.s.mean <- left_join(yearly.s.mean, weights, by=c('Survey'='comb.surveys'))
  loess.spp <- loess(prop ~ Year, weights=weights, data=yearly.s.mean, span=0.75)
  # auto-optimization, returning the best span value
  span <- autoloess(loess.spp)
  print(span)
  loess.spp <- loess(prop ~ Year, weights=weights, data=yearly.s.mean, span=span)
  loess.p <- predict(loess.spp, newdata=seq(from=min(yearly.s.mean$Year),to=last.year, by=1), se=TRUE)
  dat.fit <- data.frame(loess.p$fit)
  names(dat.fit) <- 'fit'
  dat.fit$se.low <- dat.fit$fit-2*loess.p$se.fit
  dat.fit$se.high <- dat.fit$fit+2*loess.p$se.fit
  dat.fit$Year <- seq(from=min(yearly.s.mean$Year),to=last.year, by=1)
  loess.surveys.w <- ggplot(yearly.s.mean, aes(x=Year, y=prop, size=weights)) + geom_point(shape=21, fill='lightgrey') + theme_bw() +
    xlab('Year') + ylab('Presence/Survey') + theme(text=element_text(size=10)) + 
    theme(legend.position = 'none') + xlim(1960,2020) + ylim(0,1) + ggtitle('Weighted Loess Surveys') +
    geom_line(data=dat.fit, aes(x=Year, y=fit), lwd=1, col='black') +
    geom_line(data=dat.fit, aes(x=Year, y=se.low), lwd=0.5, col='black') +
    geom_line(data=dat.fit, aes(x=Year, y=se.high), lwd=0.5, col='black')
  
  egg::ggarrange(habitat, loess.ices.year, index, loess.survey.year, loess.surveys, loess.surveys.w,
                 labels=c('','','','','',''), nrow=2)
  rm(dat.index, loess.now, index, habitat, loess.ices.year, loess.year, loess.survey.year, loess.surveys, loess.surveys.w)
}
dev.off()
