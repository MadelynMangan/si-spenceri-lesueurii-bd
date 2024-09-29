# File: 3D_set.R
# Title: Compile quasi-equilibrated system outcomes across contours of captive breeding and 
#       trout removal intervention
# Author: Madelyn J. Mangan
# Date: 2024/09/27

# load deSolve
library(deSolve)


# set working directory, change manually if not using Rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


nPoints <- 100 # number of points along parameter scale at which to simulate

LE_mod <- 1 # L. lesueurii set to 100% of normal size
beta_mod <- 1 # whole-system transmision modifier
beta_int <- 1 # transmission modifier
mu_mod <- 1 # no added tolerance (100% added mortality)
m_mod <- 1 # no increase in postmetamorphic Lesueurii mortality
gamma_mod <-1 # no change in recovery rate


# Load quasi-equilibrated data (tabulated into median by Convert_Object.R
load("High_manage3D.RData")
load("Moderate_manage3D.RData")
load("Low_manage3D.RData")


# Loop through each elevation, generating contour results
for (i in c("High", "Moderate", "Low")){
  if (i == "High"){
    set <- as.list(High_parms)
    initial <- High_init
  }
  if (i == "Moderate"){
    set <- as.list(Moderate_parms)
    initial <- Moderate_init
  }
  if (i == "Low"){
    set <- as.list(Low_parms)
    initial <- Low_init
  }
  
  m.L_slide <- seq(set$m.L_SP, set$m.L_LE, length.out = nPoints) # create scale of decreasing larval SP predation
  m.J_slide <- seq(set$m.J_SP, set$m.J_LE, length.out = nPoints) # create scale of decreasing juvenile SP predation
  f_slide <- seq(set$f_SP, set$f_LE, length.out = nPoints) # create scale of increasing SP fecundity
  
  all_results <- data.frame("Trout_reduction" = NA,
                            "Captive_breeding" = NA,
                            "Larval_mortality" = NA,
                            "Juvenile_mortality" = NA,
                            "Fecundity" = NA,
                            "SP_adult"= NA,
                            "LE_adult"= NA,
                            "Ratio_SP_LE"= NA,
                            "Zoospores"= NA,
                            "Year_EQ"= NA)
  row_count <- 1
  for (j in 1:length(m.L_slide)){
    for (k in 1:length(f_slide)){
      set$m.L_SP <- m.L_slide[j] # adjust larval mortality rate for trout predation
      set$m.J_SP <- m.J_slide[j] # adjust juvenile mortality rate for trout predation
      set$f_SP <- f_slide[k] # adjust fecundity for captive breeding
      source(file = paste(i, "_3D_quasi.R", sep=''))
      all_results[row_count,] <- c((j-1)/(nPoints-1)*100, (k-1)/(nPoints-1)*100, m.L_slide[j], m.J_slide[j], f_slide[k], SP, LE,
                                   SP_LE, Bd, years.to.quasi)
      print(paste("Trout Reduction:", j,"of", nPoints))
      print(paste("Captive Breeding:", k,"of", nPoints))
      print(paste("Ratio SP-to-LE:", SP_LE))
      row_count <- row_count + 1
    }
  }
  save(all_results, set, initial, file = paste(i, "_3D_out.RData", sep=''))
  print(paste("FINISH", i,""))
}


