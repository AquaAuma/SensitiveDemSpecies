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
load('data/ICESsurveysByc10072020.RData')

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


##########################################################################################
### Build indices for all species
##########################################################################################

Survey <- sort(unique(survey$Survey))
mycolors <- colorRampPalette(brewer.pal(11, "RdYlBu"))(length(Survey))
cols <- data.frame(cbind(Survey, mycolors))

### Choice to make
plot.loess='FALSE'
last.year <- 2019

### Run to get the index
pdf(file='results/Map.Index.Loess.spp.pdf')
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

# subset data with the right ICES squares --> in order to assign 0
survey.spp <- survey %>% 
  filter(StatRec %in% ices.keep)
#length(unique(survey.spp$HaulID)) # remaining hauls


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
#length(unique(survey.spp$HaulID)) # remaining hauls


### 3. Calculate average abundance/per hour per Year/ICESsq/Survey
# yearly average per survey
yearly.s.mean <- survey.spp %>% 
#  mutate(numh = numh/2) %>% # transform the /h into /30', matters when taking the average with 0
  group_by(Year, Survey) %>% 
  summarize_at(.vars=c('numcpue', 'numh', 'num'), .funs=function(x) mean(x, na.rm=T))

mean.per.survey <- yearly.s.mean %>% # to remove surveys with only 0
  group_by(Survey) %>% 
  summarize(numh=mean(numh)) %>% 
  filter(numh>0)

yearly.s.mean <- yearly.s.mean %>% 
  filter(Survey %in% mean.per.survey$Survey)


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
index.bstd <- ggplot(yearly.s.mean, aes(x=Year, y=numh, group=Survey, col=Survey)) + geom_line(lwd=2) +
  theme_bw() + scale_color_manual(values=mycolors) +ylab(paste('numcpue/average ',last.decade[10],'-',last.decade[1], sep='')) +
  geom_hline(yintercept=1, lwd=1, lty=2, col='black') + theme(legend.position = 'none')+
  theme(text=element_text(size=11))


### 6. Get Loess per survey
# fitted to logged positive catches of each species in each survey
to.loess <- yearly.s.mean %>% 
  filter(numh>0)

dat.index <- data.frame()
surveys <- unique(to.loess$Survey)
weights <- rep(NA, times=length(surveys))
comb.surveys <- c()
weight <- c()
#windows()
#par(mfrow=c(5,5))
for(s in 1:length(surveys)){
    data.s <- subset(to.loess, Survey==surveys[s])
    if(nrow(data.s)>2){
      # loess with whatever span, default is 0.75
      loess.s <- loess(log(numh) ~ Year, data=data.s)
      # auto-optimization, returning the best span value
      span <- autoloess(loess.s)
      loess.s <- loess(log(numh) ~ Year, data=data.s, span=span)
      # if the optimized value is not good, we keep the default value at 0.75
      if(is.infinite(loess.s$s)){span <- 0.75
      loess.s <- loess(log(numh) ~ Year, data=data.s, span=span)}

      if(!is.infinite(loess.s$s)){
      if(!is.na(loess.s$s)){
      # fit the loess with the right span
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
    #plot(loess.r ~ data.s$Year, main=(paste('CV resid: ', round(sd(loess.r),2), sep='')))
    #abline(h=0, lty=2)
    
    # if the sd of the residuals is lower than 0.75, we keep the survey-species combination
    print(sd(loess.r))
    if(sd(loess.r)<=1){
      comb.surveys[length(comb.surveys)+1] <- surveys[s]
      weight[length(weight)+1] <- 1/sd(loess.p$fit)^2
      dat.index <- rbind(dat.index, data.s)
    }
      }
        }
    rm(data.s, loess.p, loess.s, loess.r)
    }
}

# plot indices per surveys after selecting combinations of spp-surveys to keep basedon loess
if(length(comb.surveys)>0){
  weights <- data.frame(cbind(comb.surveys, weight))
  yearly.s.mean <- yearly.s.mean %>% 
    filter(Survey %in% weights$comb.surveys)
  yearly.s.mean <- left_join(yearly.s.mean, weights, by=c('Survey'='comb.surveys'))
  
  # keep same survey legend colors each time
  mycolors <- colorRampPalette(brewer.pal(8, "RdYlBu"))(length(unique(survey$Survey)))
  dat.col <- data.frame(cbind(Survey, mycolors)) %>% 
    filter(Survey %in% comb.surveys)
  index <- ggplot(yearly.s.mean, aes(x=Year, y=numh, group=Survey, col=Survey)) + geom_line(lwd=1) +
          theme_bw() + scale_color_manual(name=dat.col$Survey, values=as.character(dat.col$mycolors)) + 
          ylab(paste('numcpue/average ',last.decade[10],'-',last.decade[1], sep='')) +
          geom_hline(yintercept=1, lwd=1, lty=2, col='black') + xlim(1960,2020) + ylim(0,6) +
          theme(text=element_text(size=11)) + theme(legend.position = 'none')

  ### Final loess with temporal change
  ### With weights
  dat.index <- yearly.s.mean[order(yearly.s.mean$Year),]
  dat.index$weight <- as.numeric(as.vector(dat.index$weight))
  loess.spp.w <- loess(numh ~ Year, weights=weight, data=dat.index, span=0.75)
  # auto-optimization, returning the best span value
  span <- autoloess(loess.spp.w)
  print(span)
  loess.spp.w <- loess(numh ~ Year, weights=weight, data=dat.index, span=span)
  loess.p.w <- predict(loess.spp.w, newdata=seq(from=min(dat.index$Year),to=last.year, by=1), se=TRUE)
  
  dat.fit <- data.frame(loess.p.w$fit)
  names(dat.fit) <- 'fit'
  dat.fit$se.low <- dat.fit$fit-2*loess.p.w$se.fit
  dat.fit$se.high <- dat.fit$fit+2*loess.p.w$se.fit
  dat.fit$Year <- seq(from=min(dat.index$Year),to=last.year, by=1)
  
  loess.w <- ggplot(dat.index, aes(x=Year, y=numh, size=weight)) + geom_point(shape=21, fill='lightgrey') + theme_bw() +
    xlab('Year') + ylab('numcpue/average 2010???2019') + theme(text=element_text(size=11)) + 
    theme(legend.position = 'none') + xlim(1960,2020) + ylim(0,5) + ggtitle('Weighted Loess') +
    geom_line(data=dat.fit, aes(x=Year, y=fit), lwd=1, col='black') +
    geom_line(data=dat.fit, aes(x=Year, y=se.low), lwd=0.5, col='black') +
    geom_line(data=dat.fit, aes(x=Year, y=se.high), lwd=0.5, col='black')
  
  ### Without weights
  dat.index <- yearly.s.mean[order(yearly.s.mean$Year),]
  loess.spp <- loess(numh ~ Year, data=dat.index, span=0.75)
  # auto-optimization, returning the best span value
  span <- autoloess(loess.spp)
  print(span)
  loess.spp <- loess(numh ~ Year, data=dat.index, span=span)
  loess.p <- predict(loess.spp, newdata=seq(from=min(dat.index$Year),to=last.year, by=1), se=TRUE)
  
  dat.fit <- data.frame(loess.p$fit)
  names(dat.fit) <- 'fit'
  dat.fit$se.low <- dat.fit$fit-2*loess.p$se.fit
  dat.fit$se.high <- dat.fit$fit+2*loess.p$se.fit
  dat.fit$Year <- seq(from=min(dat.index$Year),to=last.year, by=1)
  
  loess.now <- ggplot(dat.index, aes(x=Year, y=numh)) + geom_point(shape=21, fill='lightgrey') + theme_bw() +
    xlab('Year') + ylab('numcpue/average 2010???2019') + theme(text=element_text(size=11)) + 
    theme(legend.position = 'none') + xlim(1960,2020) + ylim(0,5) + ggtitle('Classic Loess') +
    geom_line(data=dat.fit, aes(x=Year, y=fit), lwd=1, col='black') +
    geom_line(data=dat.fit, aes(x=Year, y=se.low), lwd=0.5, col='black') +
    geom_line(data=dat.fit, aes(x=Year, y=se.high), lwd=0.5, col='black')

  egg::ggarrange(habitat, index, loess.now, loess.w, labels=c('','','',''), nrow=2)
  rm(dat.index, loess.now, loess.w, index, habitat)
}
}
dev.off()






