# File: Manage_breed.R
# Title: Aggregates management scenario results for captive breeding/release and
#        creates management plots for the appendix
# Author: Madelyn J. Mangan
# Date: 2024/09/27

# Load ggplot
library(ggplot2)
library(gridExtra)
library(grid)

# Define function to aggregate final year of data from all simulations into single dataframe
agg_final <- function(bigList, slide){
  scale <- seq(0, 100, length.out = ncol(slide))
  for (i in 1:length(bigList)){
    for (j in 1:length(bigList[[i]])){
      slide_val <- slide[j,i]
      scale_val <- scale[i]
      final <- tail(bigList[[i]][[j]], 1)
      ifelse((i==1 & j==1),
             df <- as.data.frame(cbind(slide_val, scale_val, final)),
             df <- rbind(df, cbind(slide_val, scale_val, final)))
    }
  }
  return(df)
}

# Define function for calculating median and quartiles for outcomes
find_quant <- function(densities){
  median <- median(densities)
  lower <- quantile(densities, p = 0.25)
  upper <- quantile(densities, p = 0.75)
  out <- c('lower' = lower, 'median' =median, 'upper' = upper)
  return(out)
}


############################################
### Compile and generate subplot objects ###
############################################

# define .RData files to process
data_names <- c("High_breed.RData", "Moderate_breed.RData", "Low_breed.RData") 

### For loop to compile ggplots within/across datasets
for (j in 1:length(data_names)){
  population <- data_names[j] # set scenario datafile name
  load(population)            # load scenario quasi-eq output
  elevation <- strsplit(population, "[_]")[[1]][1] # extract scenario name from file name
  print(paste("Begin", elevation, "elevation compilation..."))
  # aggregate final year of data across simulation in single dataframe using agg_final()
  breed_out <- agg_final(All_Results_All,  f_slide)
  
  # Compute median and quantiles for L. spenceri for each point on the management scale
  breed_sumSP <-tapply(breed_out$A_SP_Total, list(breed_out$scale_val), find_quant) # tapply
  df_breedSP <- data.frame(scale = numeric(), # make empty data frame to store tapply output
                             lower = numeric(), 
                             median = numeric(), 
                             upper = numeric(),
                             Species = character())
  for (i in 1:length(breed_sumSP)){ # add tapply output to dataframe
    df_breedSP[i,] <- c(
      as.numeric(names(breed_sumSP))[i], # store percent management applied
      breed_sumSP[[i]][1], # lower bound
      breed_sumSP[[i]][2], # median
      breed_sumSP[[i]][3], # upper bound
      "L. spenceri") # species name
  }

  # Compute median quantiles for L. lesueurii for each point on the management scale
  breed_sumLE <-tapply(breed_out$A_LE_Total, list(breed_out$scale_val), find_quant) # tapply
  df_breedLE <- data.frame(scale = numeric(), # make empty data frame to store tapply output
                         lower = numeric(), 
                         median = numeric(), 
                         upper = numeric(),
                         Species = character())
  for (i in 1:length(breed_sumLE)){ # add tapply output to dataframe
    df_breedLE[i,] <- c(
      as.numeric(names(breed_sumLE))[i], # store percent management applied
      breed_sumLE[[i]][1], # lower bound
      breed_sumLE[[i]][2], # median
      breed_sumLE[[i]][3], # upper bound
      "L. lesueurii") # species name
  }
  
  # Compute median quantiles for Bd zoospores for each point on the management scale
  breed_sumZ <-tapply(breed_out$Z, list(breed_out$scale_val), find_quant) # tapply
  df_breedZ <- data.frame(scale = numeric(), # make empty data frame to store tapply output
                           lower = numeric(), 
                           median = numeric(), 
                           upper = numeric(),
                          Species = character())
  for (i in 1:length(breed_sumZ)){ # add tapply output to dataframe
    df_breedZ[i,] <- c(
      as.numeric(names(breed_sumZ))[i], # store percent management applied
      breed_sumZ[[i]][1], # lower bound
      breed_sumZ[[i]][2], # median
      breed_sumZ[[i]][3], # upper bound
      "BD") # species name
  }
  
  # bind SP and LE medians and CIs into one dataframe
  df_plot <- rbind(df_breedSP, df_breedLE)
  
  # convert all columns to numeric
  for (i in 1:(ncol(df_plot)-1)){
    df_plot[,i] <- as.numeric(df_plot[,i])
  }
  
  # covert all columns of Bd df to numeric
  for (i in 1:(ncol(df_breedZ)-1)){
    df_breedZ[,i] <- as.numeric(df_breedZ[,i])
  }
  
  # plot SP and LE management outcomes in one plot
  frog_plot <- ggplot(df_plot) + geom_line(aes(x=scale, y=median, color = Species), lwd=1.5)+
    geom_ribbon(aes(x=scale, ymin=lower, ymax=upper, fill = Species), alpha=0.4)+
    ylab("Adult density") + xlab(expression(paste("% increase in ", italic("L. spenceri")," fecundity"))) +
    expand_limits(y=0) + ggtitle(elevation)+ theme_grey(base_size=20)
  
  # plot BD outcome in other plot
  Bd_plot<- ggplot(df_breedZ) + geom_line(aes(x=scale, y=median), lwd=1.5)+
    geom_ribbon(aes(x=scale, ymin=lower, ymax=upper), alpha=0.4)+
    ylab("Zoospore density") + xlab(expression(paste("% increase in ", italic("L. spenceri")," fecundity"))) +
    expand_limits(y=0)+ ggtitle(elevation) + theme_grey(base_size=20)
  
  
  # Rename based on which population dataset is currently loaded 
  if (elevation == 'High') { # High elevation model
    High_densities <- frog_plot
    High_Bd <- Bd_plot
    High_breed_summary <- list('SP' = df_breedSP, 'LE' = df_breedLE, 'BD' = df_breedZ)
  }
  
  if (elevation == 'Moderate') { # Moderate elevation model
    Moderate_densities <- frog_plot
    Moderate_Bd <- Bd_plot
    Moderate_breed_summary <- list('SP' = df_breedSP, 'LE' = df_breedLE, 'BD' = df_breedZ)
  }
  
  if (elevation == 'Low') { # Moderate elevation model
    Low_densities <- frog_plot
    Low_Bd <- Bd_plot
    Low_breed_summary <- list('SP' = df_breedSP, 'LE' = df_breedLE, 'BD' = df_breedZ)
  }
}


#####################
### Plotting code ###
#####################

# Calculate universal upper ploting bound for frog densities
FrogMax <- max(as.numeric(High_breed_summary$LE$upper), 
               as.numeric(High_breed_summary$SP$upper),
               as.numeric(Moderate_breed_summary$LE$upper), 
               as.numeric(Moderate_breed_summary$SP$upper),
               as.numeric(Low_breed_summary$LE$upper), 
               as.numeric(Low_breed_summary$SP$upper))
# Calculate universal upper ploting bound for BD densities
BdMax <- max(as.numeric(High_breed_summary$BD$upper),
             as.numeric(Moderate_breed_summary$BD$upper),
             as.numeric(Low_breed_summary$BD$upper))

# Add universal upper bound to frog density plots
High_densities <- High_densities + ylim(0, FrogMax)+ theme(legend.position="none") 
Moderate_densities <- Moderate_densities + ylim(0, FrogMax)+ theme(legend.position="none") 
Low_densities <- Low_densities + ylim(0, FrogMax)+ theme(legend.position="none") 

# Add universal upper bound to Bd density plots
High_Bd <- High_Bd + ylim(0, BdMax)+ theme(legend.position="none") 
Moderate_Bd <- Moderate_Bd + ylim(0, BdMax)+ theme(legend.position="none") 
Low_Bd <- Low_Bd + ylim(0, BdMax)+ theme(legend.position="none") 

# Save summary data object
save(High_breed_summary, Moderate_breed_summary, Low_breed_summary,
     High_densities, High_Bd, Moderate_densities, Moderate_Bd, Low_densities, Low_Bd,
     file = "breed_summary.RData")

# Map subplots to grid and output
n <-9 # scaling factor
png(filename = 'Management_breed.png', width=800*n, height=800*n, res=600)

layout <- rbind(c(1,1,2,2),
                c(3,3,4,4),
                c(5,5,6,6))

grid.arrange(grobs = list(
  High_densities, High_Bd,
  Moderate_densities, Moderate_Bd,
  Low_densities, Low_Bd),
  nrow=3, ncol=2, layout_matrix = layout)
dev.off()

