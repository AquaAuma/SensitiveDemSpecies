### Libraries
library(dplyr)
library(egg)
library(ggplot2)

### 1. Load data
# Load species abundances across surveys
load('data/ICESsurveysByc11082020.RData')

# Load species abundances across surveys from Anna
survey.AR <- read.csv("check/survey_data_from_AR.csv")


### 2. Summarize survey data
survey <- survey %>% 
  group_by(Species, Survey, Year) %>% 
  summarize(numh=mean(numh, na.rm=T))

### 3. Plot
spe <- unique(survey.AR$Species)
survey <- survey %>% 
  filter(Species %in% unique(survey.AR$Species))

pdf('check/check.abundances.pdf')
for (i in 1:length(spe)){
  sub.surv <- subset(survey, Species==as.character(spe[i]))
  sub.surv.AR <- subset(survey.AR, Species==as.character(spe[i]))
  
  plot.surv <- ggplot(sub.surv, aes(x=Year, y=numh)) + geom_line() + geom_point() + ggtitle(spe[i]) + xlim(1960,2020)
  plot.surv.AR <- ggplot(sub.surv.AR, aes(x=year, y=nolength)) + geom_line() + geom_point() + xlim(1960,2020)
  egg::ggarrange(plot.surv, plot.surv.AR, labels=c('',''), nrow=1)
  rm(sub.surv, sub.surv.AR, plot.surv, plot.surv.AR)
}
dev.off()