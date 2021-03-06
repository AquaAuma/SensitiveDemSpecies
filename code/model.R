#############################################################
#### Model code for estimating species sensitivity to fishing
#### 'GISLASIM'
#### Developed with Anna Rindorf and Henrik Gislason
#### Paper: Are fish sensitive to trawling recovering in the Northeast Atlantic?
#### Coding: Aurore Maureaud, July 2020
#############################################################

rm(list=ls())
Sys.setenv(LANG = "en")

##########################################################################################
### Libraries
##########################################################################################
library(readr)
library(readxl)


##########################################################################################
### Exploitation effiency per group
##########################################################################################
EE <- read_excel("data/Walker_catchability.xlsx")
EE$max <- apply(EE[,2:8], 1, FUN=max)
EE$eff1 <- EE$eff2 <- EE$eff3 <- EE$eff4 <- EE$eff5 <- EE$eff6 <- EE$eff7 <- NA

EE$eff1 <- EE$G1/EE$max
EE$eff2 <- EE$G2/EE$max
EE$eff3 <- EE$G3/EE$max
EE$eff4 <- EE$G4/EE$max
EE$eff5 <- EE$G5/EE$max
EE$eff6 <- EE$G6/EE$max
EE$eff7 <- EE$G7/EE$max
EE[1:2,10:16] <- 0


##########################################################################################
### Parameters/traits collected or re-estimated
##########################################################################################

# Load trait data
trait <- read_excel("data/Traits.xlsx")
trait$Fsensi <- NA

# lmax maximum observed length
# linf infinity length
# K growth coefficient
# lmat length at maturity
# lmet length at birth/metamorphosis
# repro reproduction type
# explo explotation pattern from Walkter t al., 2017, code from 1-7
#1	Predominatly buried in sediment
#2	on or near sebed - anguilliform of fusiform
#3	predominatly on seabed - flat
#4	predominatly close to seabed, but not on it
#5	midwater species with some seabed association
#6	pelagic
#7	lumpiform


##########################################################################################
#### Set up trait relationships
##########################################################################################
# Model to estimate K from Linf
plot(K~Linf, data=trait, log='xy')
lmK <- lm(log(K, base=10)~log(Linf, base=10), data=trait)

# Model to estimate Lmat from Linf
plot(log(Lmat, base=10)~log(Linf, base=10), data=trait)
lmLmat <- lm(log(Lmat, base=10)~log(Linf, base=10), data=trait)

# Model to estimate Lmet from Linf for oviparous spp.
trait.ovi <- subset(trait, Taxonomy=='Oviparous' & Lmet>2)
plot(Lmet ~ Linf, data=trait.ovi, log='xy')
lmLmet.ovi <- lm(log(Lmet, base=10) ~ log(Linf, base=10), data=trait.ovi)

# Model to estimate Lmet from Linf for Ovoviparous and viviparous spp.
trait.vivi <- subset(trait, Taxonomy %in% c('Viviparous','Ovoviparous'))
plot(Lmet ~ Linf, data=trait.vivi, log='xy')
lmLmet.vivi <- lm(log(Lmet, base=10) ~ log(Linf, base=10), data=trait.vivi)


##########################################################################################
### Get Sensitivity to fishing for all spp
##########################################################################################

for(s in 1:nrow(trait)){

# select a spp
spp <- trait$Species[s]
print(paste(s,spp,sep=' '))

# Assign traits
lmax <- trait$Lmax[s]
linf <- trait$Linf[s]
K <- trait$K[s]
lmat <- trait$Lmat[s]
lmet <- trait$Lmet[s]
explo <- as.numeric(as.vector(trait[s,7]))
repro <- trait$Taxonomy[s]


### Estimation of missing traits
##########################################################################################
if (is.na(linf)){linf <- 10^(0.044+0.9841*log(lmax, base=10))}
# equation from Froese, R. and Binohlan, C., 2000. Journal of fish biology, 56(4), pp.758-773.

if (is.na(K)){K <- 10^(lmK$coefficients[1])*linf^(lmK$coefficients[2])}
# from the present study

if (is.na(lmat)){lmat <- 10^(lmLmat$coefficients[1])*linf^(lmLmat$coefficients[2])}
# from the present study

if (is.na(lmet)){
  #if(repro == 'Oviparous'){lmet <- round(10^(lmLmet.ovi$coefficients[1])*linf^(lmLmet.ovi$coefficients[2]))}
  #if(repro == 'Ovoviviparous' | repro == 'Viviparous'){lmet <- round(10^(lmLmet.vivi$coefficients[1]))*linf^(lmLmet.vivi$coefficients[2])}
  #if(repro == 'Oviparous'){lmet <- round(exp(-0.2639+0.6009*log(linf)))} # NB round on R transforms 2.5 in 2, but 2.5 --> 3 in EXCEL
  #if(repro == 'Ovoviviparous' | repro == 'Viviparous'){lmet <- round(exp(-0.6164+0.7977*log(linf)))}
  if(repro == 'Oviparous'){lmet <- round(0.7917*linf^(0.5921))}
  if(repro == 'Ovoviviparous' | repro == 'Viviparous'){lmet <- round(0.5398*linf^(0.7977))}
  
  }
# from the present study


# Simulations
##########################################################################################
sensi <- 99
f.factor=-0.01
fishSensi <- data.frame(matrix(nrow=2001, ncol=2))
names(fishSensi) <- c('f.factor','fish.sensi')
k <- 0

# while loop to find when fishSensi==0.25
while(sensi>0.25 & k<2001){
  f.factor <- f.factor + 0.01
  k <- k + 1
  fishSensi$f.factor[k] <- f.factor
  
l <- c(lmet:1001)
sim <- data.frame(l)

# Assign the exploitation pattern
sim$expl <- NA
if(explo==1){sim$expl <- as.numeric(as.vector(EE$G1[(lmet+1):nrow(EE)]))}
if(explo==2){sim$expl <- as.numeric(as.vector(EE$G2[(lmet+1):nrow(EE)]))}
if(explo==3){sim$expl <- as.numeric(as.vector(EE$G3[(lmet+1):nrow(EE)]))}
if(explo==4){sim$expl <- as.numeric(as.vector(EE$G4[(lmet+1):nrow(EE)]))}
if(explo==5){sim$expl <- as.numeric(as.vector(EE$G5[(lmet+1):nrow(EE)]))}
if(explo==6){sim$expl <- as.numeric(as.vector(EE$G6[(lmet+1):nrow(EE)]))}
if(explo==7){sim$expl <- as.numeric(as.vector(EE$G7[(lmet+1):nrow(EE)]))}

for(i in 1:nrow(sim)){
  if(sim$expl[i]<=0){sim$expl[i] <- 1}
}

# Fishing mortality
for (i in 1:nrow(sim)){
  if(sim$l[i]<linf+0.001){sim$Fi[i] <- f.factor*sim$expl[i]}
  else{sim$Fi[i] <- 0}
}

# Natural mortality
sim$M <- K*((sim$l+0.5)/linf)^(-1.5)

# Individuals
sim$N[1] <- 1000000000 
for (i in 2:nrow(sim)){
  if(round(sim$l[i]+0.5)<linf) 
  {sim$N[i] <- sim$N[i-1]*((linf-sim$l[i])/(linf-sim$l[i-1]))^((sim$Fi[i-1]+sim$M[i-1])/K)}
  else{sim$N[i] <- 0}
}

# Nav
sim$Nav <- NA
for (i in 1:nrow(sim)){
  if(sim$l[i]<linf){sim$Nav[i] <- (sim$N[i]-sim$N[i+1])/(sim$Fi[i]+sim$M[i])}
  else{sim$Nav[i] <- 0}
}

# Prop mature
lmatoverlinf <- lmat/linf
s1 <- lmatoverlinf*linf*log(3)/((1.2*lmatoverlinf-lmatoverlinf)*linf)
s2 <- s1/(lmatoverlinf*linf)
  
for (i in 1:nrow(sim)){
  if(i==1){
    if(1/(1+exp(s1-s2*(sim$l[i]+0.5)))<0.1){sim$propM[i] <- 0}
    else{sim$propM[i] <- s1}
  }
  if(i>1){
    if(1/(1+exp(s1-s2*(sim$l[i]+0.5)))<0.1){sim$propM[i] <- 0}
    else{sim$propM[i] <- 1/(1+exp(s1-s2*(sim$l[i]+0.5)))}
  }
}

# SSB
for (i in 1:nrow(sim)){
  if(sim$l[i]<linf & (sim$l[i+1]>linf | sim$propM[i]>0)){sim$SSB[i] <- sim$propM[i]*sim$Nav[i]*0.01*(sim$l[i]+0.5)^3*1}
  else{sim$SSB[i] <- 0}
}


# Age & days
for (i in 1:nrow(sim)){
  if(sim$l[i]<linf){sim$age[i] <- (1/K)*log((linf-lmet)/(linf-sim$l[i]))}
  else{sim$age[i] <- 0}
  
  sim$days[i] <- sim$age[i]*365
}

# N1 & spawners
sim$N1[1] <- NA
for(i in 2:nrow(sim)){
  # Get N1
  if(sim$days[i-1]<365){
    if(sim$days[i]>365){sim$N1[i] <- sim$N[i]}
    else{sim$N1[i] <- 0}
  }
  else{sim$N1[i] <- 0}
  
  # Get spawners
  if(sim$l[i]>lmat+0.001){sim$S[i] <- sim$Nav[i]}
  else{sim$S[i] <- 0}
}


# Get species sensitivity
##########################################################################################
totSSB <- sum(sim$SSB)
SSBoverR <- totSSB/sim$N[1]
N1overSSB <- sum(sim$N1)*1000/totSSB
N1overSpawners <- sum(sim$N1)/sum(sim$S)

if(f.factor==0){SSBoverR0 <- SSBoverR}
fishSensi$fish.sensi[k] <- SSBoverR/SSBoverR0
sensi <- SSBoverR/SSBoverR0

}


fishSensi <- subset(fishSensi, !is.na(f.factor) & fish.sensi>0.25)
trait$Fsensi[s] <- fishSensi$f.factor[nrow(fishSensi)]
}

write.csv(trait, 'results/Sensi.csv')
