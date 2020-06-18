Sys.setenv(LANG = "en")

#########################
### Libraries
#########################
library(dplyr)
library(ggplot2)
library(egg)


#########################
### Load data
#########################

### Load sensi. species index
sensi <- read.csv('results/Sensi.csv')

### Load species abundances across surveys
survey <- load('data/ICESsurveysByc18062020.RData')