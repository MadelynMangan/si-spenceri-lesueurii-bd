# File: High_exclude_set.R
# Description: runs High-elevation simulations across levels of L. lesueurii removal or exclusion
# Author: Madelyn J. Mangan
# Date: 2024/04/28

# load deSolve
library(deSolve)

# set working directory, change manually if not using Rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load('High_manage.RData')

nPoints <- 11 # number of points along parameter scale at which to simulate
nSims <- nrow(parm_eq) # number of simulation per point on parameter scale

All_Results_All <- list() # set list to store sublists of results for each points along intervention scale

LE_mod <- 1    # L. lesueurii set to 100% of normal size
beta_mod <- 1  # no added resistance (100% transmission)
mu_mod <- 1    # no added tolerance (100% added mortality)
m_mod <- 1     # no increase in postmetamorphic Lesueurii mortality
gamma_mod <- 1 # no increase in recovery rate 

parm_draw <- parm_eq # set filtered equilibrated parameter sets to run simulations
init_draw <- init_eq # set filtered equilibrated densities to run simulations

# create sliding scale of fecundity adjustment from 0% to 100%
mS1_slide <- matrix(nrow=nSims, ncol = nPoints, data=NA) 
mS2_slide <- matrix(nrow=nSims, ncol = nPoints, data=NA) 
mS3_slide <- matrix(nrow=nSims, ncol = nPoints, data=NA) 
mA1_slide <- matrix(nrow=nSims, ncol = nPoints, data=NA) 
mA2_slide <- matrix(nrow=nSims, ncol = nPoints, data=NA) 

# create scales of increasing mortality from L. lesueruii to L. spenceri levels
for (i in 1:nrow(mS1_slide)){
  mS1_slide[i,] <- seq(parm_draw$m.S1_LE[i], parm_draw$m.S1_SP[i], length.out = nPoints)
  mS2_slide[i,] <- seq(parm_draw$m.S2_LE[i], parm_draw$m.S2_SP[i], length.out = nPoints) 
  mS3_slide[i,] <- seq(parm_draw$m.S3_LE[i], parm_draw$m.S3_SP[i], length.out = nPoints) 
  mA1_slide[i,] <- seq(parm_draw$m.A1_LE[i], parm_draw$m.A1_SP[i], length.out = nPoints) 
  mA2_slide[i,] <- seq(parm_draw$m.A2_LE[i], parm_draw$m.A2_SP[i], length.out = nPoints) 
}



# RUN SIMULATIONS - cycle through each npoint for nSims parameter sets
for (i in 1:nPoints){
  All_Results_Point <- list() # set list to store results
  for (j in 1:nSims){
    set <- parm_draw[j,] # assign parameter set for current model
    init <- init_draw[[j]] # assign initial conditinos for current model
    set$m.S1_LE <- mS1_slide[j,i] # adjust subadult 1 mortality level
    set$m.S2_LE <- mS2_slide[j,i] # adjust subadult 2 mortality level
    set$m.S3_LE <- mS3_slide[j,i] # adjust subadult 3 mortality level
    set$m.A1_LE <- mA1_slide[j,i] # adjust adult 1 mortality level
    set$m.A2_LE <- mA2_slide[j,i] # adjust adult 2 mortality level
    source(file = 'High_manage.R') # call script to run 20-year simulation
    All_Results_Point[[j]] <- results # store results for this nPoint
    print(paste("Point", i,"of", nPoints,"; parm set", j, "of",nSims))  # print log
  }
  All_Results_All[[i]] <- All_Results_Point # add all nPoint results into master df
}

save(All_Results_All, parm_draw, init_draw, mS1_slide, file = 'High_exclude.RData') 