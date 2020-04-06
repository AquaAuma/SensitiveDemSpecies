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
simulations <- data.frame(l)

# Assign the exploitation pattern
if(repro==1){simulations$expl <- EE$G1[3:nrow(EE)]
             for(i in 1:nrow(simulations))
              if(simulations$expl[i]>1){simulations$expl[i] <- 1}
             }

if(repro==2){simulations$expl <- EE$G2[3:nrow(EE)]
            for(i in 1:nrow(simulations))
              if(simulations$expl[i]>1){simulations$expl[i] <- 1}
}

if(repro==3){simulations$expl <- EE$G3[3:nrow(EE)]
            for(i in 1:nrow(simulations))
              if(simulations$expl[i]>1){simulations$expl[i] <- 1}
}

if(repro==4){simulations$expl <- EE$G4[3:nrow(EE)]
            for(i in 1:nrow(simulations))
              if(simulations$expl[i]>1){simulations$expl[i] <- 1}
}

if(repro==5){simulations$expl <- EE$G5[3:nrow(EE)]
            for(i in 1:nrow(simulations))
              if(simulations$expl[i]>1){simulations$expl[i] <- 1}
}

if(repro==6){simulations$expl <- EE$G6[3:nrow(EE)]
            for(i in 1:nrow(simulations))
              if(simulations$expl[i]>1){simulations$expl[i] <- 1}
}

if(repro==7){simulations$expl <- EE$G7[3:nrow(EE)]
            for(i in 1:nrow(simulations))
              if(simulations$expl[i]>1){simulations$expl[i] <- 1}
}

# Fishing mortality
for (i in 1:nrow(simulations)){
  if(simulations$l<linf+0.001){simulations$Fi[i] <- linf*f.factor}
  else{simulations$Fi[i] <- 0}
}

# Natural mortality
simulations$M <- K*((l+0.5)/linf)^(-1.5)

# Individuals
simulations$N[1] <- 1000000000 
for (i in 2:nrow(simulations)){
  if(round(simulations$l+0.5)<linf) 
  {simulations$N[i] <- simulations$N[i-1]*((linf-simulations$l[i])/(linf-simulations$l[i-1]))^((simulations$F[i-1]+simulations$M[i-1])/K)}
  else{simulations$N[i] <- 0}
}

# Nav
for (i in 1:nrow(simulations)){
  if(simulations$l[i]<linf){simulations$Nav[i] <- (simulations$N[i]-simulations$N[i+1])/(simulations$F[i]+simulations$G[i])}
  else{simulations$Nav[i] <- 0}
}

# Prop mature
for (i in 1:nrow(simulations)){
  if(i==1){
    if(1/(1+exp(s1-s2*(simuations$l[i]+0.5)))<0.1){simulations$propM[i] <- 0}
    else{simulations$propM[i] <- s1}
  }
  if(i>1){
    if(1/(1+exp(s1-s2*(simuations$l[i]+0.5)))<0.1){simulations$propM[i] <- 0}
    else{simulations$propM[i] <- 1/(1+exp(s1-s2*(simulations$l[i]+0.5)))}
  }
}

# SSB
for (i in 1:nrow(simulations)){
  if(simulations$l[i]<linf){simulations$SSB[i] <- simulations$propM[i]*simulations$Nav[i]*0.01*(simulations$l[i]+0.5)^3}
  else if(simulations$l[i+1]>linf | simulations$propM[i]>0){simulations$SSB[i] <- 1}
  else{simulations$SSB[i] <- 0}
}

# Age
for (i in 1:nrow(simulations)){
  if(simulations$l[i]<linf){simulations$age[i] <- (1/K)*log((linf-lmet)/(linf-simulations$l[i]))}
  else{simulations$age[i] <- 0}
  
  simulations$days[i] <- simulations$age[i]*365
}


