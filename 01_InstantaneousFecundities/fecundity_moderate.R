### fecundity_moderate.R
# This script runs SI models to find the instantaneous daily fecundity for Litoria lesueurii 
# and Litoria spenceri in the absence of (1) Bd, (2) carrying capacities, and 
# (3) seasonality in larval hatching. Using the bisection method with a tolerance 
# of 0.00001 and and arbitrary initial age structure , this script converges on the 
# instantaneous daily fecundity that matches Bd-free discrete annual growth rates 
# displayed in West et al. (2020)

# Load/install packages using "pacman"
# install.packages("pacman") # Uncomment this code if pacman not already installed
library(pacman)
pacman::p_load(deSolve, ggplot2, gridExtra, grid, ggpubr)

###############################
### Part 1 - Set Parameters ###
###############################

### Set number of years to run simulation
start_day <- 183 # day of 365 to start simulation
year <- 15       # number of years
interval <- 1    # interval (in days) at which to generate values
time <- seq(start_day, 365*year, interval) # generate time sequence for ODE solver

### Set initial population sizes by age 
nL <- 0  # larvae
n0 <- 40 # year 0 (juveniles)
n1 <- 35 # year 1 (Subadults year 1)
n2 <- 30 # year 2
n3 <- 25 # year 3
n4 <- 20 # year 4
n5 <- 15 # year 5
n6 <- 10 # year 6

### Scale initial density of each species (applied evenly across life stages)
#   A value of "1" corresponds to sizes nL through n6 defined above
z.SP <- 1   # L. spenceri
z.LE <- 1   # L. lesueurii

# Set minimum ambient zoospore concentration unrelated to focal species 
min.Z <- 0  # also functions as initial zoospore concentration

### Set initial states for Litoria spenceri, Litoria lesueurii, and zoospore pool
initial <- c(
   
   ## Litoria spenceri 
   
   # Larvae
   S_L1_SP = nL*z.SP,     # Susceptible 
   I_L1_SP = 0,     # Infected 

   # Juveniles          
   S_J1_SP = n0*z.SP,     # Susceptible 
   I_J1_SP = 0,     # Infected 

   # Subadults, Year 1
   S_S1.1_SP = n1*z.SP,  # Susceptible
   I_S1.1_SP = 0,   # Infected 

   # Subadults, Year 2
   S_S2.1_SP = n2*z.SP,  # Susceptible
   I_S2.1_SP = 0,   # Infected 

   # Subadults, Year 3
   S_S3.1_SP = n3*z.SP,  # Susceptible
   I_S3.1_SP = 0,   # Infected 

   # Adults, Year 1
   S_A1.1_SP = n5*z.SP,  # Susceptible
   I_A1.1_SP = 0,   # Infected 

   #Adults, Year 2
   S_A2_SP = (n5+n6)*z.SP,    # Susceptible 
   I_A2_SP = 0,     # Infected 
   
   ## Litoria lesueurii
   
   # Larvae
   S_L1_LE = nL*z.LE,     # Susceptible 
   I_L1_LE = 0,     # Infected 

   # Juveniles     
   S_J1_LE = n0*z.LE,     # Susceptible 
   I_J1_LE = 0,     # Infected 

   # Subadults, Year 1
   S_S1.1_LE = n1*z.LE,  # Susceptible
   I_S1.1_LE = 0,   # Infected 

   # Subadults, Year 2
   S_S2.1_LE = n2*z.LE,  # Susceptible
   I_S2.1_LE = 0,   # Infected 

   # Adults, Year 1
   S_A1.1_LE = n3*z.LE,  # Susceptible
   I_A1.1_LE = 0,   # Infected 

   # Adults, Year 2
   S_A2_LE = (n4+n5+n6)*z.LE,    # Susceptible 
   I_A2_LE = 0,     # Infected 
   
   ## Zoospore Pool
   Z = min.Z)         # Zoospore density in zoospore pool


### Set parameter values for differential equations 
parameters = c(
   
   ## Litoria spenceri
   
   # Offspring per capita = sex ratio * prob(breeding) * fecundity
   alpha1.SP = 1,    # Adults, Year 1
   alpha2.SP = 1,      # Adults, Year 2
   # Transmission rates from zoospore pool to host
   beta.L_SP = 0.002,                # Larvae
   beta.J_SP = 0.0002,               # Juveniles
   beta.S1_SP = 0.0002,              # Subadults, Year 1
   beta.S2_SP = 0.0002,              # Subadults, Year 2
   beta.S3_SP = 0.0002,              # Subadults, Year 3
   beta.A1_SP = 0.0002,              # Adults, Year 1
   beta.A2_SP = 0.0002,              # Adults, Year 2
   # Natural mortality rates
   m.L_SP = log(0.011)/(-365),     # Larvae
   m.J_SP = log(0.011)/(-365),     # Juveniles
   m.S1_SP = log(1-.391)/(-365),     # Subadults, Year 1
   m.S2_SP = log(1-.376)/(-365),     # Subadults, Year 2
   m.S3_SP = log(1-.309)/(-365),     # Subadults, Year 3
   m.A1_SP = log(1-.306)/(-365),     # Adults, Year 1
   m.A2_SP = log(1-.306)/(-365),     # Adults, Year 2
   # Rate of ageing
   l.L_SP = 1/(365*0.25),          # Larvae to Juveniles
   l.J_SP = 1/(365*0.75),          # Juveniles to Subadults, Year 1
   l.S1_SP = 1/365,                # Subadults, Year 1 to Subadults, Year 2
   l.S2_SP = 1/365,                # Subadults, Year 2 to Subadutls, Year 3
   l.S3_SP = 1/365,                # Subadults, Year 3 to Adults, Year 1
   l.A1_SP = 1/365,                # Adults, Year 1 to Adults, Year 2
   # Added mortality rate due to infection
   mu.L_SP = 0,                      # Larvae
   mu.J_SP = log(1-.257)/(-365),     # Juveniles
   mu.S1_SP = log(1-.257)/(-365),    # Subadults, Year 1
   mu.S2_SP = log(1-.260)/(-365),    # Subadults, Year 2
   mu.S3_SP = log(1-.272)/(-365),    # Subadults, Year 3
   mu.A1_SP = log(1-.272)/(-365),    # Adults, Year 1
   mu.A2_SP = log(1-.272)/(-365),    # Adults, Year 2
   # recovery rate from infection
   gamma.L_SP = 0,                   # Larvae
   gamma.J_SP = log(1-.956)/(-365),  # Juveniles
   gamma.S1_SP = log(1-.956)/(-365), # Subadults, Year 1
   gamma.S2_SP = log(1-.956)/(-365), # Subadults, Year 2
   gamma.S3_SP = log(1-.956)/(-365), # Subadults, Year 3
   gamma.A1_SP = log(1-.956)/(-365), # Adults, Year 1
   gamma.A2_SP = log(1-.956)/(-365), # Adults, Year 2
   # zoospore shedding rate
   lambda.L_SP = 0.01,               # Larvae
   lambda.J_SP = 0.05,               # Juveniles
   lambda.S1_SP = 0.1,               # Subadults, Year 1
   lambda.S2_SP = 0.1,               # Subadults, Year 2
   lambda.S3_SP = 0.1,               # Subadults, Year 3
   lambda.A1_SP = 0.1,               # Adults, Year 1
   lambda.A2_SP = 0.1,               # Adults, Year 2
 
   ## Litoria lesueurii 
   
   # Offspring per capita = sex ratio * prob(breeding) * fecundity 
   alpha1.LE = 1,
   alpha2.LE = 1, 
   # Transmission rates from zoospore pool to host
   beta.L_LE = 0.002,                # Larvae
   beta.J_LE = 0.0002,               # Juveniles
   beta.S1_LE = 0.0002,              # Subadults, Year 1
   beta.S2_LE = 0.0002,              # Subadults, Year 2
   beta.A1_LE = 0.0002,              # Adults, Year 1
   beta.A2_LE = 0.0002,              # Adults, Year 2
   # Natural mortality rates
   m.L_LE = log(0.052)/(-365),     # Larvae
   m.J_LE = log(0.052)/(-365),      # Juveniles
   m.S1_LE = log(1-.226)/(-365),     # Subadults, Year 1
   m.S2_LE = log(1-.209)/(-365),     # Subadults, Year 2
   m.A1_LE = log(1-.128)/(-365),     # Adults, Year 1
   m.A2_LE = log(1-.128)/(-365),     # Adults, Year 2
   # Rate of ageing
   l.L_LE = 1/(365*0.25),          # Larvae to Juveniles
   l.J_LE = 1/(365*0.75),          # Juveniles to Subadults, Year 1
   l.S1_LE = 1/365,                # Subadults, Year 1 to Subadults, Year 2
   l.S2_LE = 1/365,                # Subadults, Year 2 to Adults, Year 1
   l.A1_LE = 1/365,                # Adults, Year 1 to Adults, Year 2
   # Added mortality rate due to infection
   mu.L_LE = 0,                      # Larvae
   mu.J_LE = log(1-.648)/(-365),     # Juveniles 
   mu.S1_LE = log(1-.648)/(-365),    # Subadults, Year 1
   mu.S2_LE = log(1-.661)/(-365),    # Subadults, Year 2
   mu.A1_LE = log(1-.720)/(-365),    # Adults, Year 1
   mu.A2_LE = log(1-.720)/(-365),    # Adults, Year 2
   # recovery rate from infection
   gamma.L_LE = 0,                   # Larvae
   gamma.J_LE = log(1-.949)/(-365),  # Juveniles 
   gamma.S1_LE = log(1-.949)/(-365), # Subadults, Year 1
   gamma.S2_LE = log(1-.949)/(-365), # Subadults, Year 2
   gamma.A1_LE = log(1-.949)/(-365), # Adults, Year 1
   gamma.A2_LE = log(1-.949)/(-365), # Adults, Year 2
   # zoospore shedding rate
   lambda.L_LE = 0.01,               # Larvae
   lambda.J_LE = 0.05,               # Juveniles 
   lambda.S1_LE = 0.1,               # Subadults, Year 1
   lambda.S2_LE = 0.1,               # Subadults, Year 2
   lambda.A1_LE = 0.1,               # Adults, Year 1
   lambda.A2_LE = 0.1,               # Adults, Year 2
   
   ## Zoospore decay rate
   gamma.Z = 1/2,
   ## Minimum Zoospore Density
   mZ = min.Z)                    



##############################################
### Part 2 - Define Functions for Modeling ###
##############################################


### Define SIS model 
SIS_2host <- function (t, x, params) {
  with(as.list(c(x, params)),{
    year_time <- t/365-floor(t/365) # Convert time of year to a proportion
    # Calculate total L. spenceri Adults, Year 1
    N_A1_SP <- S_A1.1_SP + I_A1.1_SP
    # Calculate total L. spenceri Adults, Year 2
    N_A2_SP <- S_A2_SP + I_A2_SP 
    # Calculate total L. lesueurii Adults, Year 1
    N_A1_LE <- S_A1.1_LE + I_A1.1_LE 
    # Calculate total L. lesueurii Adults, Year 2
    N_A2_LE <- S_A2_LE + I_A2_LE 
    # Calculates seasonal forcing on hatching
    R <- 1 
      
      
      
    # Differential equations, with variables as defined above
    
    ## L. spenceri
    
    # Larvae, Substage 1
    dS_L1_SP.dt = (R*alpha1.SP*N_A1_SP + R*alpha2.SP*N_A2_SP)- beta.L_SP*S_L1_SP*Z - m.L_SP*S_L1_SP + gamma.L_SP*I_L1_SP -l.L_SP*S_L1_SP 
    dI_L1_SP.dt = beta.L_SP*S_L1_SP*Z - m.L_SP*I_L1_SP - mu.L_SP*I_L1_SP - gamma.L_SP*I_L1_SP -l.L_SP*I_L1_SP 
    # Juveniles, Substage 1
    dS_J1_SP.dt = l.L_SP*S_L1_SP -beta.J_SP*S_J1_SP*Z - m.J_SP*S_J1_SP + gamma.J_SP*I_J1_SP -l.J_SP*S_J1_SP                      
    dI_J1_SP.dt = l.L_SP*I_L1_SP +beta.J_SP*S_J1_SP*Z - m.J_SP*I_J1_SP - mu.J_SP*I_J1_SP  - gamma.J_SP*I_J1_SP -l.J_SP*I_J1_SP  
    # Subadults, Year 1, Substage 1
    dS_S1.1_SP.dt = l.J_SP*S_J1_SP-beta.S1_SP*S_S1.1_SP*Z - m.S1_SP*S_S1.1_SP + gamma.S1_SP*I_S1.1_SP - l.S1_SP*S_S1.1_SP              
    dI_S1.1_SP.dt = l.J_SP*I_J1_SP+beta.S1_SP*S_S1.1_SP*Z - m.S1_SP*I_S1.1_SP - mu.S1_SP*I_S1.1_SP - gamma.S1_SP*I_S1.1_SP - l.S1_SP*I_S1.1_SP    
    # Subadults, Year 2, Substage 1
    dS_S2.1_SP.dt = l.S1_SP*S_S1.1_SP-beta.S2_SP*S_S2.1_SP*Z - m.S2_SP*S_S2.1_SP + gamma.S2_SP*I_S2.1_SP - l.S2_SP*S_S2.1_SP              
    dI_S2.1_SP.dt = l.S1_SP*I_S1.1_SP+beta.S2_SP*S_S2.1_SP*Z - m.S2_SP*I_S2.1_SP - mu.S2_SP*I_S2.1_SP - gamma.S2_SP*I_S2.1_SP - l.S2_SP*I_S2.1_SP    
    # Subadults, Year 3, Substage 1
    dS_S3.1_SP.dt = l.S2_SP*S_S2.1_SP-beta.S3_SP*S_S3.1_SP*Z - m.S3_SP*S_S3.1_SP + gamma.S3_SP*I_S3.1_SP - l.S3_SP*S_S3.1_SP              
    dI_S3.1_SP.dt = l.S2_SP*I_S2.1_SP+beta.S3_SP*S_S3.1_SP*Z - m.S3_SP*I_S3.1_SP - mu.S3_SP*I_S3.1_SP - gamma.S3_SP*I_S3.1_SP - l.S3_SP*I_S3.1_SP    
    # Adult, Year 1, Substage 1
    dS_A1.1_SP.dt = l.S3_SP*S_S3.1_SP-beta.A1_SP*S_A1.1_SP*Z - m.A1_SP*S_A1.1_SP + gamma.A1_SP*I_A1.1_SP - l.A1_SP*S_A1.1_SP              
    dI_A1.1_SP.dt = l.S3_SP*I_S3.1_SP+beta.A1_SP*S_A1.1_SP*Z - m.A1_SP*I_A1.1_SP - mu.A1_SP*I_A1.1_SP - gamma.A1_SP*I_A1.1_SP - l.A1_SP*I_A1.1_SP    
    # Adults, Year 2
    dS_A2_SP.dt = l.A1_SP*S_A1.1_SP - beta.A2_SP*S_A2_SP*Z - m.A2_SP*S_A2_SP + gamma.A2_SP*I_A2_SP                          
    dI_A2_SP.dt = l.A1_SP*I_A1.1_SP + beta.A2_SP*S_A2_SP*Z - m.A2_SP*I_A2_SP - mu.A2_SP*I_A2_SP - gamma.A2_SP*I_A2_SP               
    
    ## L. lesueurii
    
    # Larvae, Substage 1
    dS_L1_LE.dt = (R*alpha1.LE*N_A1_LE + R*alpha2.LE*N_A2_LE)- beta.L_LE*S_L1_LE*Z - m.L_LE*S_L1_LE + gamma.L_LE*I_L1_LE -l.L_LE*S_L1_LE 
    dI_L1_LE.dt = beta.L_LE*S_L1_LE*Z - m.L_LE*I_L1_LE - mu.L_LE*I_L1_LE - gamma.L_LE*I_L1_LE -l.L_LE*I_L1_LE 
    # Juveniles, Substage 1
    dS_J1_LE.dt = l.L_LE*S_L1_LE -beta.J_LE*S_J1_LE*Z - m.J_LE*S_J1_LE + gamma.J_LE*I_J1_LE -l.J_LE*S_J1_LE                      
    dI_J1_LE.dt = l.L_LE*I_L1_LE +beta.J_LE*S_J1_LE*Z - m.J_LE*I_J1_LE - mu.J_LE*I_J1_LE  - gamma.J_LE*I_J1_LE -l.J_LE*I_J1_LE  
    # Subadults, Year 1, Substage 1
    dS_S1.1_LE.dt = l.J_LE*S_J1_LE-beta.S1_LE*S_S1.1_LE*Z - m.S1_LE*S_S1.1_LE + gamma.S1_LE*I_S1.1_LE - l.S1_LE*S_S1.1_LE              
    dI_S1.1_LE.dt = l.J_LE*I_J1_LE+beta.S1_LE*S_S1.1_LE*Z - m.S1_LE*I_S1.1_LE - mu.S1_LE*I_S1.1_LE - gamma.S1_LE*I_S1.1_LE - l.S1_LE*I_S1.1_LE    
    # Subadults, Year 2, Substage 1
    dS_S2.1_LE.dt = l.S1_LE*S_S1.1_LE-beta.S2_LE*S_S2.1_LE*Z - m.S2_LE*S_S2.1_LE + gamma.S2_LE*I_S2.1_LE - l.S2_LE*S_S2.1_LE              
    dI_S2.1_LE.dt = l.S1_LE*I_S1.1_LE+beta.S2_LE*S_S2.1_LE*Z - m.S2_LE*I_S2.1_LE - mu.S2_LE*I_S2.1_LE - gamma.S2_LE*I_S2.1_LE - l.S2_LE*I_S2.1_LE    
    # Adult, Year 1, Substage 1
    dS_A1.1_LE.dt = l.S2_LE*S_S2.1_LE-beta.A1_LE*S_A1.1_LE*Z - m.A1_LE*S_A1.1_LE + gamma.A1_LE*I_A1.1_LE - l.A1_LE*S_A1.1_LE              
    dI_A1.1_LE.dt = l.S2_LE*I_S2.1_LE+beta.A1_LE*S_A1.1_LE*Z - m.A1_LE*I_A1.1_LE - mu.A1_LE*I_A1.1_LE - gamma.A1_LE*I_A1.1_LE - l.A1_LE*I_A1.1_LE    
    # Adults, Year 2
    dS_A2_LE.dt = l.A1_LE*S_A1.1_LE - beta.A2_LE*S_A2_LE*Z - m.A2_LE*S_A2_LE + gamma.A2_LE*I_A2_LE                            # susceptible
    dI_A2_LE.dt = l.A1_LE*I_A1.1_LE + beta.A2_LE*S_A2_LE*Z - m.A2_LE*I_A2_LE - mu.A2_LE*I_A2_LE - gamma.A2_LE*I_A2_LE            # infected
    
    ## Zoospore pool
    dZ.dt = lambda.L_SP*I_L1_SP + lambda.L_LE*I_L1_LE + 
      lambda.J_SP*I_J1_SP + lambda.J_LE*I_J1_LE + 
      lambda.S1_SP*I_S1.1_SP + lambda.S1_LE*I_S1.1_LE +
      lambda.S2_SP*I_S2.1_SP + lambda.S2_LE*I_S2.1_LE +
      lambda.S3_SP*I_S3.1_SP + 
      lambda.A1_SP*I_A1.1_SP + lambda.A1_LE*I_A1.1_LE +
      lambda.A2_SP*I_A2_SP + lambda.A2_LE*I_A2_LE - gamma.Z*Z + gamma.Z*mZ
    
    return(list(c(
      # L. spenceri
      dS_L1_SP.dt, dI_L1_SP.dt,      # Larvae, Substage 1, susceptible & infected
      dS_J1_SP.dt, dI_J1_SP.dt,      # Juveniles, Substage 1, susceptible & infected
      dS_S1.1_SP.dt, dI_S1.1_SP.dt,  # Subadults, Year 1, Substage 1, susceptible & infected
      dS_S2.1_SP.dt, dI_S2.1_SP.dt,  # Subadults, Year 2, Substage 1, susceptible & infected
      dS_S3.1_SP.dt, dI_S3.1_SP.dt,  # Subadults, Year 3, Substage 1, susceptible & infected
      dS_A1.1_SP.dt, dI_A1.1_SP.dt,  # Adults, Year 1, Substage 1, susceptible & infected
      dS_A2_SP.dt, dI_A2_SP.dt,      # Adults, Year 2, susceptible & infected
      # L. lesueurii
      dS_L1_LE.dt, dI_L1_LE.dt,      # Larvae, Substage 1, susceptible & infected
      dS_J1_LE.dt, dI_J1_LE.dt,      # Juveniles, Substage 1, susceptible & infected
      dS_S1.1_LE.dt, dI_S1.1_LE.dt,  # Subadults, Year 1, Substage 1, susceptible & infected
      dS_S2.1_LE.dt, dI_S2.1_LE.dt,  # Subadults, Year 2, Substage 1, susceptible & infected
      dS_A1.1_LE.dt, dI_A1.1_LE.dt,  # Adults, Year 1, Substage 1, susceptible & infected
      dS_A2_LE.dt, dI_A2_LE.dt,       # Adults, Year 2, susceptible & infected
      dZ.dt))) 
  })
  
}


###########################################
### PART 3 - RUN MODEL AND SUM  RESULTS ###
###########################################

# Define function to run model past transient dynamics (15 years) and calculate
# the mean finite annual growth rate across all life stages for comparison to 
# values listed in West et al. (2020)

growth <- function(parms) {   
  
  ### Run model
  results <- as.data.frame(ode(times = time, y = initial, parms = parms, func = SIS_2host))
  results<-tail(results, 365) # extract last year of growth 
  xvals <- seq(0,1,length.out = 365) # generate days in year as x-variable
  
  # Calculate L. spenceri growth rate
  GrowthSP <- exp(mean(c(
    summary(lm(log(results$S_S1.1_SP)~xvals))$coefficients[2,1],
    summary(lm(log(results$S_S2.1_SP)~xvals))$coefficients[2,1],
    summary(lm(log(results$S_S3.1_SP)~xvals))$coefficients[2,1],
    summary(lm(log(results$S_A1.1_SP)~xvals))$coefficients[2,1],
    summary(lm(log(results$S_A2_SP)~xvals))$coefficients[2,1])))
  
  # Calculate L. lesueurii growth rate
  GrowthLE <- exp(mean(c(
    summary(lm(log(results$S_S1.1_LE)~xvals))$coefficients[2,1],
    summary(lm(log(results$S_S2.1_LE)~xvals))$coefficients[2,1],
    summary(lm(log(results$S_A1.1_LE)~xvals))$coefficients[2,1],
    summary(lm(log(results$S_A2_LE)~xvals))$coefficients[2,1])))
  # return vector of growth rates
  return(c(GrowthSP, GrowthLE))
}


# Define function using bisection method to converge on instantaneous daily fecundity 
# which matches the Bd-free finite annual growth rates observed in West et al. (2020)

# L. spenceri 
findRootSP <- function(lower, upper, target, tolerance){
  repeat {
    c <- (lower+upper)/2
    parameters['alpha1.SP'] = 0.5*0.5*c   
    parameters['alpha2.SP'] = 1*0.5*c
    lambda <- growth(parameters)[1]
    if (abs(lambda - target) < tolerance){break}
    if (lambda > target) {upper <- c}
    if (lambda < target) {lower <- c}
    print(c(lower, upper,lambda))
  }
  return(c(c,lambda))
}

# L. lesueurii
findRootLE <- function(lower, upper, target, tolerance){
  repeat {
    c <- (lower+upper)/2
    parameters['alpha1.LE'] = 0.5*0.5*c   
    parameters['alpha2.LE'] = 1*0.5*c
    lambda <- growth(parameters)[2]
    if (abs(lambda - target) < tolerance){break}
    if (lambda > target) {upper <- c}
    if (lambda < target) {lower <- c}
    print(c(lower, upper,lambda))
  }
  return(c(c,lambda))
}

################
# EXECUTE CODE #
################

# L. spenceri bisection exploring instantaneous daily growth rates from 0 to 5
findRootSP(0, 5, 1.149384,0.00001)
# L. spenceri bisection exploring instantaneous daily growth rates from 0 to 5
findRootLE(0, 5, 2.826110,0.00001)

