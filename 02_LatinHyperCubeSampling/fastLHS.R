# File: fastLHS.R
# Title: Latin Hypercube Sampler
# Author: Madelyn Mangan
# Date: 2023/08/04

# Description:
#
#     This script generates a Latin Hypercube Sample across parameter space, resulting in N total
#     parameters sets. (N is specified at the top of this script). This script calls a .csv input file which should contain the following columns:
#
#          - 'elevation': the elevation of the model in which the parameter will be used
#          - 'species': the species in which the parameter will be used
#          - 'symbol': shorthand name of the parameter
#          - 'dist': the probability distribution of the parameter. Choices are 'normal' or 'uniform'
#          - 'mean': the mean (the estimated value) of the parameter (unnecessary if dist = uniform)
#          - 'sd': the standard deviation of the parameter (unnecessary if dist = uniform)
#          - 'lower': the lower bound of the parameter distribution (unnecessary for dist = normal)
#          - 'upper': the upper bound of the parameter distribution (unnecessary for dist = normal)
#    

# set working directory, change manually if not using Rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##########################################
# SET 'N' - NUMBER OF PARAMETER SETS HERE
N <- 10000

# load latin hypercube sample package
library(lhs)

# set seed for reproducibility
set.seed(204)


# Define function to generate Latin Hypercube Sample parameter set from parameter file
make_lhs <- function(parmList, n){
  n_parm <- nrow(parmList)
  LHS <- randomLHS(n, n_parm)
  for (i in 1:n_parm){
    if (parmList$dist[i] == 'normal'){
      LHS[,i] <- qnorm(LHS[,i], mean = parmList$mean[i], sd = parmList$sd[i])
    }
    if (parmList$dist[i] == 'uniform'){
      LHS[,i] <- qunif(LHS[,i], min = parmList$lower[i], max = parmList$upper[i])
    }
  }
  colnames(LHS) <- parmList$symbol
  return(as.data.frame(LHS))
}

# Generate LHS sets  
input_low <- read.csv("LowLHS.csv", header =TRUE) # load low elevation parms
parameters_shuffled <- make_lhs(input_low, N) # generate LHS
save(parameters_shuffled, file = 'LHS_low.RData') # save output

input_moderate <- read.csv("ModerateLHS.csv", header=TRUE) # load moderate elevation parms
parameters_shuffled <- make_lhs(input_moderate, N) # generate LHS
save(parameters_shuffled, file = 'LHS_moderate.RData')# save output

input_high <- read.csv("HighLHS.csv", header=TRUE) # load high elevation parms
parameters_shuffled <- make_lhs(input_high, N) # generate LHS
save(parameters_shuffled, file = 'LHS_high.RData') # save output



