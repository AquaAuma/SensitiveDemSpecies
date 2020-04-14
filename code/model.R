#############################################################
#### Model code for estimating species sensitivity to fishing
#### Developed with Anna Rindorf and Henrik Gislason
#### Paper: Are fish sensitive to trawling recovering in the Northeast Atlantic?
#### Coding: Aurore Maureaud
#############################################################

# Libraries
library(readr)
library(readxl)

# Parameters/traits collected or re-estimated
#############################################
lmax # maximum observed length

linf # infinity length

K # growth coefficient

lmat # length at maturity

lmet # length at birth/metamorphosis

repro # reproduction type

explo # explotation pattern from Walkter t al., 2017, code from 1-7
#1	Predominatly buried in sediment
#2	on or near sebed - anguilliform of fusiform
#3	predominatly on seabed - flat
#4	predominatly close to seabed, but not on it
#5	midwater species with some seabed association
#6	pelagic
#7	lumpiform



# Estimation of missing traits
##############################
if (is.na(linf)){linf <- 0.044+0.9841*log(lmax, base=10)}
# equation from Froese, R. and Binohlan, C., 2000. Journal of fish biology, 56(4), pp.758-773.

if (is.na(K)){K <- 1.54953*linf^(0.53141)}
# from the present study

if (is.na(lmat)){lmat <- 0.6019*linf^(0.9726)}
# from the present study

if (is.na(lmet)){
  if(repro == 'Oviparous'){lmet <- round(0.7917*linf^(0.5921))} # NB round on R transforms 2.5 in 2, but 2.5 --> 3 in EXCEL
  if(repro == 'Ovoviviparous' | repro == 'Vivipar'){lmet <- round(0.5398*linf^0.7977)}
}
# from the present study


# Exploitation effiency per group
#################################
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


# Simulations
#############
lmet=2
repro=5
linf=152
f.factor=1
K=0.073
  
l <- c(lmet:1001)
sim <- data.frame(l)

# Assign the exploitation pattern
if(repro==1){sim$expl <- EE$G1[3:nrow(EE)]
             for(i in 1:nrow(sim))
              if(sim$expl[i]>1){sim$expl[i] <- 1}
             }

if(repro==2){sim$expl <- EE$G2[3:nrow(EE)]
            for(i in 1:nrow(sim))
              if(sim$expl[i]>1){sim$expl[i] <- 1}
}

if(repro==3){sim$expl <- EE$G3[3:nrow(EE)]
            for(i in 1:nrow(sim))
              if(sim$expl[i]>1){sim$expl[i] <- 1}
}

if(repro==4){sim$expl <- EE$G4[3:nrow(EE)]
            for(i in 1:nrow(sim))
              if(sim$expl[i]>1){sim$expl[i] <- 1}
}

if(repro==5){sim$expl <- EE$G5[3:nrow(EE)]
            for(i in 1:nrow(sim))
              if(sim$expl[i]>1){sim$expl[i] <- 1}
}

if(repro==6){sim$expl <- EE$G6[3:nrow(EE)]
            for(i in 1:nrow(sim))
              if(sim$expl[i]>1){sim$expl[i] <- 1}
}

if(repro==7){sim$expl <- EE$G7[3:nrow(EE)]
            for(i in 1:nrow(sim))
              if(sim$expl[i]>1){sim$expl[i] <- 1}
}

# Fishing mortality
for (i in 1:nrow(sim)){
  if(sim$l<linf+0.001){sim$Fi[i] <- linf*f.factor}
  else{sim$Fi[i] <- 0}
}

# Natural mortality
sim$M <- K*((l+0.5)/linf)^(-1.5)

# Individuals
sim$N[1] <- 1000000000 
for (i in 2:nrow(sim)){
  if(round(sim$l+0.5)<linf) 
  {sim$N[i] <- sim$N[i-1]*((linf-sim$l[i])/(linf-sim$l[i-1]))^((sim$F[i-1]+sim$M[i-1])/K)}
  else{sim$N[i] <- 0}
}

# Nav
for (i in 1:nrow(sim)){
  if(sim$l[i]<linf){sim$Nav[i] <- (sim$N[i]-sim$N[i+1])/(sim$F[i]+sim$G[i])}
  else{sim$Nav[i] <- 0}
}

# Prop mature
for (i in 1:nrow(sim)){
  if(i==1){
    if(1/(1+exp(s1-s2*(simuations$l[i]+0.5)))<0.1){sim$propM[i] <- 0}
    else{sim$propM[i] <- s1}
  }
  if(i>1){
    if(1/(1+exp(s1-s2*(simuations$l[i]+0.5)))<0.1){sim$propM[i] <- 0}
    else{sim$propM[i] <- 1/(1+exp(s1-s2*(sim$l[i]+0.5)))}
  }
}

# SSB
for (i in 1:nrow(sim)){
  if(sim$l[i]<linf){sim$SSB[i] <- sim$propM[i]*sim$Nav[i]*0.01*(sim$l[i]+0.5)^3}
  else if(sim$l[i+1]>linf | sim$propM[i]>0){sim$SSB[i] <- 1}
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
  if(sim$days[i]<365){
    if(sim$days[i+1]>365){sim$N1[i] <- sim$N[i+1]}
    else{sim$N1[i] <- 0}
  }
  else{sim$N1[i] <- 0}
  
  # Get spawners
  if(sim$l[i]>lmat+0.001){sim$S[i] <- sim$Nav[i]}
  else{sim$S[i] <- 0}
}

totSSB <- sum(sim$SSB)
SSSoverR <- totSSB/sim$N[1]
N1overSSB <- sum(sim$N1)*1000/totSSB
N1overSpawners <- sum(sim$N1)/sum(sim$S)

### Get fish sensitivity
fishSensi <- data.frame()


