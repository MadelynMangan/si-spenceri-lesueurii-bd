# File: quasi-eq_diagnostics.R
# Description: filters quasi-equilibrated parameter sets for realism and generates diagnostic plots
# Author: Madelyn J. Mangan
# Date: 2024/09/27

# load packages for plotting

library(ggplot2) 
library(gridExtra)
library(grid)
library(tidyr)
library(dplyr)
library(scales)

############################################
### Compile and generate subplot objects ###
############################################
pal<- c("#7D0101","#1E88E5","#A08C4F")

# define RData files to process
data_names <- c("Low_quasi.RData", "Moderate_quasi.RData", "High_quasi.RData")

# set working directory, change manually if not using Rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

n_keep_all <- c()
### For loop to compile ggplots within/across datasets
for (j in 1:length(data_names)){
  population <- data_names[j] # set scenario datafile name
  load(population)            # load scenario quasi-eq output
  elevation <- strsplit(population, "[_]")[[1]][1] # extract scenario name from file name
  
# Set empty vectors to extract filter values of passing parameter sets
ratio <- c()    # ratio of LE-to-SP adults at quasi-eq
prevSP <- c()   # winter prevalence of infection at quasi-eq in SP
prevSP5 <- c()  # summer prevalence of infection at quasi-eq in SP
prevLE <- c()   # winter prevalence of infection at quasi-eq in LE
prevLE5 <- c()  # summer prevalence of infection at quasi-eq in LE

# Keep only the parameter sets which passed realism filter
keep <- filter_vals[which(fail==F)]
n_keep <- length(keep) # store number of passing parameter sets
n_keep_all <- c(n_keep_all, n_keep)

# Consolidate passing eq state (init_eq) and parameters (parm_eq) into data object
init_eq <- initial_conditions[which(fail==F)] # gather passing eq states
parm_eq <- parameter_sets[which(fail==F),] # gather passing parameter sets
save(init_eq, parm_eq, file = paste(elevation, "_manage.RData", sep='' )) # save

# Convert passing eq states from list to data frame
init_eq_df <- as.data.frame(matrix(ncol = length(init_eq[[1]]), nrow = 0))
names(init_eq_df) <- names(init_eq[[1]])
  for (k in 1:length(init_eq)){
    init_eq_df[k,] <- init_eq[[k]]
  }

# Add Elevation identifier column for merged data frame across scenarios
init_eq_df$Elevation <- rep(elevation, n_keep)

# initialize (j=1) or row-bind (j>1) eq states into master data frame initial_pass
ifelse (j==1,
  initial_pass <- init_eq_df,
  initial_pass <- bind_rows(initial_pass, init_eq_df))

# Extract filter values from passing parameter sets
for (i in 1:length(keep)){
  ratio <- c(ratio, keep[[i]][1])
  prevSP <- c(prevSP, keep[[i]][2])
  prevSP5 <- c(prevSP5, keep[[i]][3])
  prevLE <- c(prevLE, keep[[i]][4])
  prevLE5 <- c(prevLE5, keep[[i]][5])
}



# Combine SP winter/summer prevalence into single df for plotting
df_prev <- data.frame(
  "prevSP" = c(prevSP, prevSP5), # bind winter/summer SP prev to one column
  "prevLE" = c(prevLE, prevLE5), # bind winter/summer LE prev to one column
  # create season index column to distinguish winter/summer in aes()
  'Season' = c(rep("Winter", length(keep)), rep('Summer', length(keep))))




# compile single 'ratio' and year of quasiEQ df across all scenarios
ifelse(j == 1,
       df_ratio <- data.frame('ratio'=ratio, 
                              'yearEQ' = year_of_quasiEQ[which(fail==F)],
                              'Elevation' =rep(elevation, length(ratio))),
       df_ratio <- rbind(df_ratio, 
                         data.frame('ratio'=ratio, 
                                    'yearEQ' = year_of_quasiEQ[which(fail==F)],
                                    'Elevation'=rep(elevation, length(ratio)))))
                    
# Make ggplot object of LE-to-SP adult ratio in filtered/equilibrated parm sets
if (j == length(data_names)){
  
  df_ratio$Elevation <- factor(df_ratio$Elevation, levels = c('High', 'Moderate', 'Low'))
  
  ratio_med <- round(tapply(df_ratio$ratio, df_ratio$Elevation, median),2)
  year_med <- tapply(df_ratio$yearEQ, df_ratio$Elevation, median)
  
  max <- max(df_ratio$ratio)
  LEtoSP <- ggplot(df_ratio, aes(x=ratio, color = Elevation)) + geom_density(size=1.5) +
    ggtitle(expression(paste("Adult ", italic('L. lesueurii '), 'to ',
                             italic('L. spenceri '),'\nratio at quasi-equilibrium'))) +
    xlab("Ratio") + ylab("Density") + theme_classic(base_size=20) +
    annotate(geom='text', x=rep(max*0.70, 3), y=c(0.066, 0.056, 0.046), 
             label=c(paste("High:", ratio_med[1]),
                     paste("Moderate:", ratio_med[2]),
                     paste("Low:", ratio_med[3])), col=pal,
             size=c(8,8,8), hjust=0) + xlim(0, max*1.1)+
    geom_vline(xintercept = 1, linetype = 2) + scale_color_manual(values = pal) +
    theme(legend.position="none")
    
  
  max <- max(df_ratio$yearEQ)
  YearofEQ_plot <- ggplot(df_ratio, aes(x=yearEQ, color = Elevation)) + geom_density(size=1.5) +
    xlab("Year") + ylab("Density") + theme_classic(base_size=20)+
    ggtitle("Year of quasi-equilibrium")+
    annotate(geom='text', x=rep(max*0.70, 3), y=c(0.055, 0.0475, 0.04), 
             label=c(paste("High:", year_med[1]),
                     paste("Moderate:", year_med[2]),
                     paste("Low:", year_med[3])), col=pal,
             size=c(8,8,8), hjust=0) + xlim(0, max*1.1) + scale_color_manual(values = pal)+
    theme(legend.position="none")
}

# Make ggplot object of SP prevalence in filtered/equilibrated parm sets
Prev_SP <- ggplot(df_prev) + geom_density(aes(x=prevSP, color=Season)) + 
  ggtitle(paste("N = ", n_keep," par. sets", sep='')) +
  xlab("Adult prevalence of infection") + ylab("Density") + theme_bw(base_size = 14) +
  scale_color_manual(values = pal[1:2])

# Make ggplot object of LE prevalence in filtered/equilibrated parm sets
Prev_LE <- ggplot(df_prev) + geom_density(aes(x=prevLE, color=Season)) + 
  ggtitle(paste("N = ", n_keep," par. sets", sep='')) +
  xlab("Adult prevalence of infection") + ylab("Density") + theme_bw(base_size = 14) +
  scale_color_manual(values = pal[1:2])

# Rename based on which population dataset is currently loaded 
if (population == 'High_quasi.RData') { # High elevation model
  # LEtoSP_High <- LEtoSP
  PrevSP_High <- Prev_SP + theme(legend.position="none") 
  PrevLE_High <- Prev_LE + theme(legend.position="none") 
}

if (population == 'Moderate_quasi.RData') {  # Moderate elevation model
  # LEtoSP_Moderate <- LEtoSP
  PrevSP_Moderate <- Prev_SP + theme(legend.position="none") 
  PrevLE_Moderate <- Prev_LE + theme(legend.position="none") 
}

if (population == 'Low_quasi.RData') {  # Low elevation model
  # LEtoSP_Low <- LEtoSP
  PrevSP_Low <- Prev_SP + theme(legend.position="none") 
  PrevLE_Low <- Prev_LE + theme(legend.position="none") 
}


}
##########################################
### Generate combined diagnostic plots ###
########################################## 

### LE-to-SP Ratio Plot
n <- 7

png(filename = 'LEtoSP_Ratio.png', width = 1300*n, height = 600*n, res=600)
LEtoSP
dev.off()

### Year of quasi-equilibrium Plot
png(filename = 'Year_of_EQ.png', width = 1300*n, height = 600*n, res=600)
YearofEQ_plot
dev.off()


# Define text blocks for plotting
blank <- grid.text("") # blank grid block
SP_title <- grid.text(expression(paste(italic("L. spenceri"))), gp=gpar(fontsize=22)) # SP column title
LE_title <- grid.text(expression(paste(italic("L. lesueurii"))), gp=gpar(fontsize=22)) # LE column title
High_title <- grid.text("High", gp=gpar(fontsize=22)) # High row title
Moderate_title <- grid.text("Moderate", gp=gpar(fontsize=22)) # Moderate row title
Low_title <- grid.text("Low", gp=gpar(fontsize=22)) # Low row title


# Define grid layout of subplots
layout = rbind(c(1,1,2,2,2,1,3,3,3,1),
               c(4,4,5,5,5,5,6,6,6,6),
               c(4,4,5,5,5,5,6,6,6,6),
               c(4,4,5,5,5,5,6,6,6,6),
               c(7,7,8,8,8,8,9,9,9,9),
               c(7,7,8,8,8,8,9,9,9,9),
               c(7,7,8,8,8,8,9,9,9,9),
               c(10,10,11,11,11,11,12,12,12,12),
               c(10,10,11,11,11,11,12,12,12,12),
               c(10,10,11,11,11,11,12,12,12,12))

# Map subplots to grid and output
png(filename = 'Equilibrium_Prevalence.png', width=800*n*1.25, height=800*n*1.25, res=600)
grid.arrange(blank, SP_title, LE_title,
             High_title, PrevSP_High, PrevLE_High,
             Moderate_title, PrevSP_Moderate, PrevLE_Moderate,
             Low_title, PrevSP_Low, PrevLE_Low,
             nrow=4, ncol=3, layout_matrix = layout)
dev.off()

#### Compartment Densities Plot
initial_pass_index <- length(initial_pass)-1
initial_pass[is.na(initial_pass)] <- 0

initial_pass$SP_L <- with(initial_pass,
                          S_L1_SP + I_L1_SP)
                        
initial_pass$LE_L <- with(initial_pass,
                          S_L1_LE + I_L1_LE)

initial_pass$SP_J <- with(initial_pass,
                          S_J1_SP + I_J1_SP)

initial_pass$LE_J <- with(initial_pass,
                          S_J1_LE + I_J1_LE)

initial_pass$SP_S <- with(initial_pass,
                          S_S1.1_SP + I_S1.1_SP +
                            S_S2.1_SP + I_S2.1_SP +
                            S_S3.1_SP + I_S3.1_SP +
                            S_S4.1_SP + I_S4.1_SP)

initial_pass$LE_S <- with(initial_pass,
                          S_S1.1_LE + I_S1.1_LE +
                            S_S2.1_LE + I_S2.1_LE +
                            S_S3.1_LE + I_S3.1_LE)

initial_pass$SP_A <- with(initial_pass,
                          S_A1.1_SP + I_A1.1_SP +
                            S_A2_SP + I_A2_LE)

initial_pass$LE_A <- with(initial_pass,
                          S_A1.1_LE + I_A1.1_LE +
                            S_A2_LE + I_A2_LE)


summary_initial_pass <- initial_pass
summary_initial_pass$Elevation <- factor(summary_initial_pass$Elevation,
                                         levels = c("High", "Moderate","Low"))

df_labels <- data.frame(
  "eq_labels" = c("Larval\nSP", "Larval\nLE", "Juvenile\nSP", "Juvenile\nLE", "Subadult\nSP", 
                  "Subadult\nLE", "Adult\nSP", "Adult\nLE", "Zoospore"),
  "xvals" = c(0, 1, 2.25, 3.25, 4.5,5.5, 6.75, 7.75, 9)+0.25
)             

summary_initial_pass$xvals <- c(rep(0, n_keep_all[1]),
           rep(0.25, n_keep_all[2]),
           rep(0.5, n_keep_all[3]))
eq_densities <- ggplot(summary_initial_pass) + 
  geom_jitter(aes(x=xvals, y=SP_L, color = Elevation), alpha = 0.05, width = 0.08, size = 1.95)+
  geom_jitter(aes(x=1+xvals, y=LE_L, color = Elevation), alpha = 0.05, width = 0.08, size = 1.95)+
  geom_jitter(aes(x=2.25+xvals, y=SP_J, color = Elevation), alpha = 0.05, width = 0.08, size = 1.95)+
  geom_jitter(aes(x=3.25+xvals, y=LE_J, color = Elevation), alpha = 0.05, width = 0.08, size = 1.95)+
  geom_jitter(aes(x=4.5+xvals, y=SP_S, color = Elevation), alpha = 0.05, width = 0.08, size = 1.95)+
  geom_jitter(aes(x=5.5+xvals, y=LE_S, color = Elevation), alpha = 0.05, width = 0.08, size = 1.95)+
  geom_jitter(aes(x=6.75+xvals, y=SP_A, color = Elevation), alpha = 0.05, width = 0.08, size = 1.95)+
  geom_jitter(aes(x=7.75+xvals, y=LE_A, color = Elevation), alpha = 0.05, width = 0.08, size = 1.95)+
  geom_jitter(aes(x=9+xvals, y=Z, color = Elevation), alpha = 0.05, width = 0.08, size = 1.95)+
  geom_boxplot(aes(x=xvals, y=SP_L, group=Elevation), size=0.25, outlier.shape = NA, alpha=0)+
  geom_boxplot(aes(x=1+xvals, y=LE_L, group=Elevation), size = 0.25, outlier.shape = NA, alpha=0)+
  geom_boxplot(aes(x=2.25+xvals, y=SP_J, group=Elevation), size=0.25, outlier.shape = NA, alpha=0)+
  geom_boxplot(aes(x=3.25+xvals, y=LE_J, group=Elevation), size=0.25, outlier.shape = NA, alpha=0)+
  geom_boxplot(aes(x=4.5+xvals, y=SP_S, group=Elevation),  size=0.25, outlier.shape = NA, alpha=0)+
  geom_boxplot(aes(x=5.5+xvals, y=LE_S, group=Elevation), size=0.25, outlier.shape = NA, alpha=0)+
  geom_boxplot(aes(x=6.75+xvals, y=SP_A, group=Elevation), size=0.25, outlier.shape = NA, alpha=0)+
  geom_boxplot(aes(x=7.75+xvals, y=LE_A, group=Elevation), size=0.25, outlier.shape = NA, alpha=0)+
  geom_boxplot(aes(x=9+xvals, y=Z, group=Elevation), size=0.25, outlier.shape = NA, alpha=0)+
  geom_text(data=df_labels, aes(x=xvals, y=rep(-300,9), label = eq_labels), size=4.5) +xlab('')+
  theme_bw(base_size=20)+
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) + ylab('Density')+
  ggtitle("Compartment densities at quasi-equilibrium (July 2nd)") +
  geom_abline(intercept=0, slope=0, linetype=3, alpha=0.5 ) +
  ylim(-301,8000)+scale_color_manual(values = pal) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))
  
  
png(filename = 'Equilibrium_Densities.png', width = 1000*n*1.1, height = 600*n*1.1, res=600)
eq_densities
dev.off()

#########################################################
### GENERATE INPUT FILES FOR '06_RecruitmentContours' ###
#########################################################

# clear environment
rm(list = ls())
library(dplyr)

# Function to calculate median initial conditions of quasi-eq parm sets
median_init <- function(init){
  all_init <- init[[1]]
  for (i in 2:length(init)){
    all_init <- bind_rows(all_init, init[[i]])
  }
  return(all_init)
}

### Median quasi_EQ High elevation
load("High_manage.RData")
High_parms <- apply(parm_eq, MARGIN = 2, FUN = median)
High_init <- apply(median_init(init_eq), MARGIN=2, FUN=median)

### Median quasi_EQ Moderate elevation
load("Moderate_manage.RData")
Moderate_parms <- apply(parm_eq, MARGIN = 2, FUN = median)
Moderate_init <- apply(median_init(init_eq), MARGIN=2, FUN=median)

### Median quasi_EQ Low elevation
load("Low_manage.RData")
Low_parms <- apply(parm_eq, MARGIN = 2, FUN = median)
Low_init <- apply(median_init(init_eq), MARGIN=2, FUN=median)

save(Low_parms, Low_init, file = 'Low_manage3D.RData')
save(Moderate_parms, Moderate_init, file = 'Moderate_manage3D.RData')
save(High_parms, High_init, file = 'High_manage3D.RData')