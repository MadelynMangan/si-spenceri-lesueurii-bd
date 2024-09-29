# File: High_trout_set
# Description: runs High-elevation simulations across levels of trout predation
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

m_Trout <- parm_draw$m.L_SP # SP mortality when trout are present
m_noTrout <- parm_draw$m.L_LE # SP mortality when trout are absent

# create sliding scale of trout-related mortality from 100% to 0%
m_slide <- matrix(nrow=nSims, ncol = nPoints, data=NA) 
for (i in 1:nrow(m_slide)){
  m_slide[i,] <- seq(m_Trout[i], m_noTrout[i], length.out = nPoints) # create scale of decreasing mortality
}

# RUN SIMULATIONS - cycle through each npoint for nSims parameter sets
for (i in 1:nPoints){
  All_Results_Point <- list() # set list to store results
  for (j in 1:nSims){
    set <- parm_draw[j,] # assign parameter set for current model
    init <- init_draw[[j]] # assign initial conditinos for current model
    set$m.L_SP <- m_slide[j,i] # adjust larval mortality rate for trout predation
    set$m.J_SP <- m_slide[j,i] # adjust juvenile mortality rate for trout predation
    source(file = 'High_manage.R') # call script to run 20-year simulation
    All_Results_Point[[j]] <- results # store results for this nPoint
    print(paste("Point", i,"of", nPoints,"; parm set", j, "of",nSims))  # print log
  }
  All_Results_All[[i]] <- All_Results_Point # add all nPoint results into master df
}

save(All_Results_All, parm_draw, init_draw, m_slide, file = 'High_trout.RData') 