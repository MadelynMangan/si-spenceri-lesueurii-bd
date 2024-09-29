# File: Moderate_quasi_set.R
# Description: Runs a group of LHS parameter draws to equilibrium, and filters for realism
# Author: Madelyn J. Mangan
# Date: 2024/04/28


library(deSolve) # load deSolve
# set working directory, change manually if not using Rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("LHS_moderate.RData") # load Latin Hypercube sample of parameter sets

# Set filters for parameter set realism
max_LE_to_SP_ratio <- 50 # maximum LE-to-SP adult ratio at quasi-eq
min_prevalence_eq <- 0.05 # minimum prevalence at quasi-eq
max_prevalence_eq <- 0.70 # maximum prevalence at quasi-eq

# Extract shuffled parameter sets
parameter_sets <-parameters_shuffled

# Set modifiers to 1 (no change) 
beta_mod <- 1 # no added resistance (100% transmission)
mu_mod <- 1 # no added tolerance (100% added mortality)
m_mod <- 1 # no increase in postmetamorphic Lesueurii mortality

# Create empy lists/vectors for tracking output
initial_conditions <- list() # stores quasi-eq states of all compartments
filter_vals <- list() # stores realism filter values for all parameter set
fail <- c() # tracks whether each parameter set passes or fails the filter
year_of_quasiEQ <- c() # tracks years to reach quasi-eq in each parameter set
adult_densities <- list() # tracks yearly adult densities for all parameter sets

# Function to determine if a quasi-equilibrated parameter set fulfills all
#   filtering criteria (TRUE = Fails criteria)
fail_filter <- function(vector, LE_SP, pMin, pMax){
  out_eq <- as.list(vector)
  failure <- with(out_eq, {
    any(ratio > LE_SP,
        prev_SP < pMin, 
        prev_SP5 < pMin, 
        prev_SP > pMax,
        prev_SP5 > pMax,
        prev_LE < pMin,
        prev_LE5 < pMin,
        prev_LE > pMax,
        prev_LE5 > pMax)
  })
  return(failure)
}

# Run all parameter sets to quasi-eq
for (j in 1:nrow(parameter_sets)){    # for each LHS parameter set
  set <- parameter_sets[j,]           # assign parameter set for proper calling 
  print(paste("Parameter set #", j))  # print to indicate set number
  source(file = 'Moderate_quasi.R')   # simulate to quasi-eq
  initial_conditions[[j]] <- init.out # store quasi-eq states across compartments
  filter_vals[[j]] <- filters         # store filter values at quasi-eq
  fail <- c(fail,                     # determine realism fail/pass of parameter set 
            fail_filter(vector = filters, 
                        LE_SP = max_LE_to_SP_ratio, 
                        pMin = min_prevalence_eq,  
                        pMax = max_prevalence_eq))
  if (fail[j] == 1) {print("Parameter set FAILURE")} 
  year_of_quasiEQ <- c(year_of_quasiEQ, years.to.quasi) # store year of quasi-eq
  adult_densities[[j]] <- run_results # store yearly adult densities 
}

# save all tracked output to datafile
save(parameter_sets, filter_vals, fail, initial_conditions, year_of_quasiEQ, adult_densities, file = 'Moderate_quasi.RData')

