# File: High_manage.R
# Title: High-elevation SI model in Litoria spenceri/lesueurii system
# Author: Madelyn J. Mangan
# Date: 2024/04/28

# Description: 
#
#     This script runs a Susceptible-Infected compartmental model of Batrachochytrium dendrobatidis
#     (chytrid fungus) infection dynamics in a two-host amphibian community. This community contains
#     the focal species Litoria spenceri and the suspected reservoir species Litoria lesueurii. All 
#     pathogen transmission occurs through a dynamic zoospore pool rather than by host-to-host 
#     transmission.
#     
#     This script is called by High_*_set.R where * corresponds to the management scenario being 
#	  evalutated. Initial compartment densities for a given parameter set are obtained from the 
#	  quasi-equilibrated output in 03_Quasi-Equilibration. The script defaults to running 20 years 
#	  and calculating output daily densities across life stages. 


###############################
### Part 1 - Set Parameters ###
###############################

### Set number of years to run simulation
start_day <- 183 # day of 365 to start simulation
year <- 20   # number of years
interval <- 73  # interval (in days) at which to generate values
time <- seq(start_day, 365*year+183, interval) # generate time sequence for ODE solve

### Set initial states for Litoria spenceri, Litoria lesueurii, and zoospore pool
initial <- init

### Set parameter values for differential equations 
parameters = c(
   
   ## Litoria spenceri
   
   # Offspring per capita = sex ratio * prob(breeding) * fecundity
   f_SP = set$f_SP, # fecundity (added here for easier documenting)
   alpha1.SP = 0.5*0.5*set$f_SP, # Adults, Year 1
   alpha2.SP = 1*0.5*set$f_SP,   # Adults, Year 2
   # Transmission rates from zoospore pool to host
   beta.L_SP = set$beta.L_SP*beta_mod,   # Larvae
   beta.J_SP = set$beta.J_SP*beta_mod,   # Juveniles
   beta.S1_SP = set$beta.S1_SP*beta_mod, # Subadults, Year 1
   beta.S2_SP = set$beta.S2_SP*beta_mod, # Subadults, Year 2
   beta.S3_SP = set$beta.S3_SP*beta_mod, # Subadults, Year 3
   beta.S4_SP = set$beta.S4_SP*beta_mod, # Subadults, Year 4
   beta.A1_SP = set$beta.A1_SP*beta_mod, # Adults, Year 1
   beta.A2_SP = set$beta.A2_SP*beta_mod, # Adults, Year 2
   # Natural mortality rates
   m.L_SP = set$m.L_SP,    # Larvae
   m.J_SP = set$m.J_SP,    # Juveniles
   m.S1_SP = set$m.S1_SP,  # Subadults, Year 1
   m.S2_SP = set$m.S2_SP,  # Subadults, Year 2
   m.S3_SP = set$m.S3_SP,  # Subadults, Year 3
   m.S4_SP = set$m.S4_SP,  # Subadults, Year 4
   m.A1_SP = set$m.A1_SP,  # Adults, Year 1
   m.A2_SP = set$m.A2_SP,  # Adults, Year 2
   # Rate of ageing
   l.L_SP = 1/(365*0.25), # Larvae to Juveniles
   l.J_SP = 1/(365*0.75), # Juveniles to Subadults, Year 1
   l.S1_SP = 1/365,       # Subadults, Year 1 to Subadults, Year 2
   l.S2_SP = 1/365,       # Subadults, Year 2 to Subadults, Year 3
   l.S3_SP = 1/365,       # Subadults, Year 3 to Subadults, Year 4
   l.S4_SP = 1/365,       # Subadults, Year 4 to Adults, Year 1
   l.A1_SP = 1/365,       # Adults, Year 1 to Adults, Year 2
   # Added mortality rate due to infection
   mu.L_SP = 0,                    # Larvae
   mu.J_SP = set$mu.J_SP*mu_mod,   # Juveniles
   mu.S1_SP = set$mu.S1_SP*mu_mod, # Subadults, Year 1
   mu.S2_SP = set$mu.S2_SP*mu_mod, # Subadults, Year 2
   mu.S3_SP = set$mu.S3_SP*mu_mod, # Subadults, Year 3
   mu.S4_SP = set$mu.S4_SP*mu_mod, # Subadults, Year 4
   mu.A1_SP = set$mu.A1_SP*mu_mod, # Adults, Year 1
   mu.A2_SP = set$mu.A2_SP*mu_mod, # Adults, Year 2
   # recovery rate from infection
   gamma.L_SP = 0,                # Larvae
   gamma.J_SP = set$gamma.J_SP*gamma_mod,   # Juveniles
   gamma.S1_SP = set$gamma.S1_SP*gamma_mod, # Subadults, Year 1
   gamma.S2_SP = set$gamma.S2_SP*gamma_mod, # Subadults, Year 2
   gamma.S3_SP = set$gamma.S3_SP*gamma_mod, # Subadults, Year 3
   gamma.S4_SP = set$gamma.S4_SP*gamma_mod, # Subadults, Year 4
   gamma.A1_SP = set$gamma.A1_SP*gamma_mod, # Adults, Year 1
   gamma.A2_SP = set$gamma.A2_SP*gamma_mod, # Adults, Year 2
   # zoospore shedding rate
   lambda.L_SP = set$lambda.L_SP,   # Larvae
   lambda.J_SP = set$lambda.J_SP,   # Juveniles
   lambda.S1_SP = set$lambda.S1_SP, # Subadults, Year 1
   lambda.S2_SP = set$lambda.S2_SP, # Subadults, Year 2
   lambda.S3_SP = set$lambda.S3_SP, # Subadults, Year 3
   lambda.S4_SP = set$lambda.S4_SP, # Subadults, Year 4
   lambda.A1_SP = set$lambda.A1_SP, # Adults, Year 1
   lambda.A2_SP = set$lambda.A2_SP, # Adults, Year 2
 
   ## Litoria lesueurii 
   
   # Offspring per capita = sex ratio * prob(breeding) * fecundity 
   f_LE = set$f_LE, # fecundity (added here for easier documenting)
   alpha1.LE = 0.5*0.5*set$f_LE, # Adults, Year 1
   alpha2.LE = 1*0.5*set$f_LE,   # Adults, Year 2
   # Transmission rates from zoospore pool to host
   beta.L_LE = set$beta.L_LE,    # Larvae
   beta.J_LE = set$beta.J_LE,    # Juveniles
   beta.S1_LE = set$beta.S1_LE,  # Subadults, Year 1
   beta.S2_LE = set$beta.S2_LE,  # Subadults, Year 2
   beta.S3_LE = set$beta.S3_LE,  # Subadults, Year 3
   beta.A1_LE = set$beta.A1_LE,  # Adults, Year 1
   beta.A2_LE = set$beta.A2_LE,  # Adults, Year 2
   # Natural mortality rates
   m.L_LE = set$m.L_LE,   # Larvae
   m.J_LE = set$m.J_LE*m_mod,   # Juveniles
   m.S1_LE = set$m.S1_LE*m_mod, # Subadults, Year 1
   m.S2_LE = set$m.S2_LE*m_mod, # Subadults, Year 2
   m.S3_LE = set$m.S3_LE*m_mod, # Subadults, Year 3
   m.A1_LE = set$m.A1_LE*m_mod, # Adults, Year 1
   m.A2_LE = set$m.A2_LE*m_mod, # Adults, Year 2
   # Rate of ageing
   l.L_LE = 1/(365*0.25),  # Larvae to Juveniles
   l.J_LE = 1/(365*0.75),  # Juveniles to Subadults, Year 1
   l.S1_LE = 1/365,        # Subadults, Year 1 to Subadults, Year 2
   l.S2_LE = 1/365,        # Subadults, Year 2 to Subadults, Year 3
   l.S3_LE = 1/365,        # Subadults, Year 3 to Adults, Year 1
   l.A1_LE = 1/365,        # Adults, Year 1 to Adults, Year 2
   # Added mortality rate due to infection
   mu.L_LE = 0,               # Larvae
   mu.J_LE = set$mu.J_LE,     # Juveniles 
   mu.S1_LE = set$mu.S1_LE,   # Subadults, Year 1
   mu.S2_LE = set$mu.S2_LE,   # Subadults, Year 2
   mu.S3_LE = set$mu.S3_LE,   # Subadults, Year 3
   mu.A1_LE = set$mu.A1_LE,   # Adults, Year 1
   mu.A2_LE = set$mu.A2_LE,   # Adults, Year 2
   # recovery rate from infection
   gamma.L_LE = 0,                # Larvae
   gamma.J_LE = set$gamma.J_LE,   # Juveniles 
   gamma.S1_LE = set$gamma.S1_LE, # Subadults, Year 1
   gamma.S2_LE = set$gamma.S2_LE, # Subadults, Year 2
   gamma.S3_LE = set$gamma.S3_LE, # Subadults, Year 3
   gamma.A1_LE = set$gamma.A1_LE, # Adults, Year 1
   gamma.A2_LE = set$gamma.A2_LE, # Adults, Year 2
   # zoospore shedding rate
   lambda.L_LE = set$lambda.L_LE,   # Larvae
   lambda.J_LE = set$lambda.J_LE,   # Juveniles 
   lambda.S1_LE = set$lambda.S1_LE, # Subadults, Year 1
   lambda.S2_LE = set$lambda.S2_LE, # Subadults, Year 2
   lambda.S3_LE = set$lambda.S3_LE, # Subadults, Year 3
   lambda.A1_LE = set$lambda.A1_LE, # Adults, Year 1
   lambda.A2_LE = set$lambda.A2_LE, # Adults, Year 2
   
   # Carrying capacity for each species
   K_SP_L <- 2500,
   K_LE_L <- 2500,
   
   ## Zoospore decay rate
   gamma.Z = set$gamma.Z,
   ## Minimum Zoospore Density
   mZ = 1)

##############################################
### Part 2 - Define Functions for Modeling ###
##############################################

### Calculate scaling factor for truncated seasonal forcing of larval births
num <- seq(0,1, length.out=100000) # values at which to generate curve
cos_vals <- 1-cos(2*pi*((num-0.5))) # generate positive cosine curve
cos_vals2 <- (1-2*cos(4*pi*(num-0.4))) # generate cos curve with 2x amplitude, 2 periods/year, and 0.4 x-shift
cos_vals2 <- ifelse(cos_vals2>0, cos_vals2, 0) # truncated curve to exclude negative values
cos_vals2[(100000*.4):(100000*0.9)] <- 0 # remove second period of sinusoidal curve in late year
Adj_forcing <- sum(cos_vals)/sum(cos_vals2) # calculate scaling factor to bring AUC back to 1

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
     # Calculate total L. spenceri Larvae
     N_L_SP <- S_L1_SP + I_L1_SP
     # Calculate density-dependent growth penalty for larval births
     Penalty_SP <- (1-N_L_SP/K_SP_L)
     # Calculate total L. lesueurii Larvae
     N_L_LE <- S_L1_LE + I_L1_LE
     # Calculate density-dependent growth penalty for larval births
     Penalty_LE <- (1-N_L_LE/K_LE_L)
     # Calculates seasonal forcing on hatching
     R <- Adj_forcing*(1-2*cos(4*pi*(year_time-0.4)))
     R <- ifelse(R>0, R, 0) # removes negative values
     R <- ifelse(any(year_time<0.4, year_time>0.9), R, 0) 
      
   # Differential equations, with variables as defined above
      
      ## L. spenceri
      
      # Larvae, Substage 1
      dS_L1_SP.dt = Penalty_SP*(R*alpha1.SP*N_A1_SP + R*alpha2.SP*N_A2_SP) - beta.L_SP*S_L1_SP*Z - m.L_SP*S_L1_SP + gamma.L_SP*I_L1_SP -l.L_SP*S_L1_SP 
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
      # Subadults, Year 4, Substage 1
      dS_S4.1_SP.dt = l.S3_SP*S_S3.1_SP-beta.S4_SP*S_S4.1_SP*Z - m.S4_SP*S_S4.1_SP + gamma.S4_SP*I_S4.1_SP - l.S4_SP*S_S4.1_SP              
      dI_S4.1_SP.dt = l.S3_SP*I_S3.1_SP+beta.S4_SP*S_S4.1_SP*Z - m.S4_SP*I_S4.1_SP - mu.S4_SP*I_S4.1_SP - gamma.S4_SP*I_S4.1_SP - l.S4_SP*I_S4.1_SP    
      # Adult, Year 1, Substage 1
      dS_A1.1_SP.dt = l.S4_SP*S_S4.1_SP-beta.A1_SP*S_A1.1_SP*Z - m.A1_SP*S_A1.1_SP + gamma.A1_SP*I_A1.1_SP - l.A1_SP*S_A1.1_SP              
      dI_A1.1_SP.dt = l.S4_SP*I_S4.1_SP+beta.A1_SP*S_A1.1_SP*Z - m.A1_SP*I_A1.1_SP - mu.A1_SP*I_A1.1_SP - gamma.A1_SP*I_A1.1_SP - l.A1_SP*I_A1.1_SP    
      # Adults, Year 2
      dS_A2_SP.dt = l.A1_SP*S_A1.1_SP - beta.A2_SP*S_A2_SP*Z - m.A2_SP*S_A2_SP + gamma.A2_SP*I_A2_SP                          
      dI_A2_SP.dt = l.A1_SP*I_A1.1_SP + beta.A2_SP*S_A2_SP*Z - m.A2_SP*I_A2_SP - mu.A2_SP*I_A2_SP - gamma.A2_SP*I_A2_SP               
      
      ## L. lesueurii
   
      # Larvae, Substage 1
      dS_L1_LE.dt = Penalty_LE*(R*alpha1.LE*N_A1_LE + R*alpha2.LE*N_A2_LE)- beta.L_LE*S_L1_LE*Z - m.L_LE*S_L1_LE + gamma.L_LE*I_L1_LE -l.L_LE*S_L1_LE 
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
      # Subadults, Year 3, Substage 1
      dS_S3.1_LE.dt = l.S2_LE*S_S2.1_LE-beta.S3_LE*S_S3.1_LE*Z - m.S3_LE*S_S3.1_LE + gamma.S3_LE*I_S3.1_LE - l.S3_LE*S_S3.1_LE              
      dI_S3.1_LE.dt = l.S2_LE*I_S2.1_LE+beta.S3_LE*S_S3.1_LE*Z - m.S3_LE*I_S3.1_LE - mu.S3_LE*I_S3.1_LE - gamma.S3_LE*I_S3.1_LE - l.S3_LE*I_S3.1_LE    
      # Adult, Year 1, Substage 1
      dS_A1.1_LE.dt = l.S3_LE*S_S3.1_LE-beta.A1_LE*S_A1.1_LE*Z - m.A1_LE*S_A1.1_LE + gamma.A1_LE*I_A1.1_LE - l.A1_LE*S_A1.1_LE              
      dI_A1.1_LE.dt = l.S3_LE*I_S3.1_LE+beta.A1_LE*S_A1.1_LE*Z - m.A1_LE*I_A1.1_LE - mu.A1_LE*I_A1.1_LE - gamma.A1_LE*I_A1.1_LE - l.A1_LE*I_A1.1_LE    
      # Adults, Year 2
      dS_A2_LE.dt = l.A1_LE*S_A1.1_LE - beta.A2_LE*S_A2_LE*Z - m.A2_LE*S_A2_LE + gamma.A2_LE*I_A2_LE                            
      dI_A2_LE.dt = l.A1_LE*I_A1.1_LE + beta.A2_LE*S_A2_LE*Z - m.A2_LE*I_A2_LE - mu.A2_LE*I_A2_LE - gamma.A2_LE*I_A2_LE           
      
      ## Zoospore pool
      dZ.dt = lambda.L_SP*I_L1_SP + lambda.L_LE*I_L1_LE + 
         lambda.J_SP*I_J1_SP + lambda.J_LE*I_J1_LE + 
         lambda.S1_SP*I_S1.1_SP + lambda.S1_LE*I_S1.1_LE +
         lambda.S2_SP*I_S2.1_SP + lambda.S2_LE*I_S2.1_LE +
         lambda.S3_SP*I_S3.1_SP + lambda.S3_LE*I_S3.1_LE ++
         lambda.S4_SP*I_S4.1_SP + 
         lambda.A1_SP*I_A1.1_SP + lambda.A1_LE*I_A1.1_LE +
         lambda.A2_SP*I_A2_SP + lambda.A2_LE*I_A2_LE - gamma.Z*Z +gamma.Z*mZ
         
      return(list(c(
         # L. spenceri
         dS_L1_SP.dt, dI_L1_SP.dt,      # Larvae, Substage 1, susceptible & infected
         dS_J1_SP.dt, dI_J1_SP.dt,      # Juveniles, Substage 1, susceptible & infected
         dS_S1.1_SP.dt, dI_S1.1_SP.dt,  # Subadults, Year 1, Substage 1, susceptible & infected
         dS_S2.1_SP.dt, dI_S2.1_SP.dt,  # Subadults, Year 2, Substage 1, susceptible & infected
         dS_S3.1_SP.dt, dI_S3.1_SP.dt,  # Subadults, Year 3, Substage 1, susceptible & infected
         dS_S4.1_SP.dt, dI_S4.1_SP.dt,  # Subadults, Year 4, Substage 1, susceptible & infected
         dS_A1.1_SP.dt, dI_A1.1_SP.dt,  # Adults, Year 1, Substage 1, susceptible & infected
         dS_A2_SP.dt, dI_A2_SP.dt,      # Adults, Year 2, susceptible & infected
         # L. lesueurii
         dS_L1_LE.dt, dI_L1_LE.dt,      # Larvae, Substage 1, susceptible & infected
         dS_J1_LE.dt, dI_J1_LE.dt,      # Juveniles, Substage 1, susceptible & infectedted
         dS_S1.1_LE.dt, dI_S1.1_LE.dt,  # Subadults, Year 1, Substage 1, susceptible & infected
         dS_S2.1_LE.dt, dI_S2.1_LE.dt,  # Subadults, Year 2, Substage 1, susceptible & infected
         dS_S3.1_LE.dt, dI_S3.1_LE.dt,  # Subadults, Year 3, Substage 1, susceptible & infected
         dS_A1.1_LE.dt, dI_A1.1_LE.dt,  # Adults, Year 1, Substage 1, susceptible & infected
         dS_A2_LE.dt, dI_A2_LE.dt,       # Adults, Year 2, susceptible & infected
         dZ.dt))) 
   })
  
}

##########################################
### PART 3 - Run model and sum results ###
##########################################

### Run model
results <- as.data.frame(ode(times = time, y = initial, parms = parameters, func = SIS_2host))

# Adult Population Total
results$A_SP_Total <- with(results, S_A1.1_SP+ I_A1.1_SP + S_A2_SP + I_A2_SP)
results$A_LE_Total <- with(results, S_A1.1_LE + I_A1.1_LE + S_A2_LE + I_A2_LE)
avg365_SP <- mean(tail(results$A_SP_Total, round(365/interval, 0)))
avg365_LE <- mean(tail(results$A_LE_Total, round(365/interval, 0)))
