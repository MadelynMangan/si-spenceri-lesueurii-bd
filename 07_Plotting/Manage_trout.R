# File: Manage_trout.R
# Title: Aggregates management scenario results for trout removal and 
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

# define RData files to process
data_names <- c("High_trout.RData", "Moderate_trout.RData", "Low_trout.RData") 

### For loop to compile ggplots within/across datasets
for (j in 1:length(data_names)){
  population <- data_names[j] # set scenario datafile name
  load(population)            # load scenario quasi-eq output
  elevation <- strsplit(population, "[_]")[[1]][1] # extract scenario name from file name
  print(paste("Begin", elevation, "elevation compilation..."))
  # aggregate final year of data across simulation in single dataframe using agg_final()
  trout_out <- agg_final(All_Results_All,  m_slide)
  
  # Compute median and quantiles for L. spenceri for each point on the management scale
  trout_sumSP <-tapply(trout_out$A_SP_Total, list(trout_out$scale_val), find_quant) # tapply
  df_troutSP <- data.frame(scale = numeric(), # make empty data frame to store tapply output
                             lower = numeric(), 
                             median = numeric(), 
                             upper = numeric(),
                             Species = character())
  for (i in 1:length(trout_sumSP)){ # add tapply output to dataframe
    df_troutSP[i,] <- c(
      as.numeric(names(trout_sumSP))[i], # store percent management applied
      trout_sumSP[[i]][1], # lower bound
      trout_sumSP[[i]][2], # median
      trout_sumSP[[i]][3], # upper bound
      "L. spenceri") # species name
  }

  # Compute median quantiles for L. lesueurii for each point on the management scale
  trout_sumLE <-tapply(trout_out$A_LE_Total, list(trout_out$scale_val), find_quant) # tapply
  df_troutLE <- data.frame(scale = numeric(), # make empty data frame to store tapply output
                         lower = numeric(), 
                         median = numeric(), 
                         upper = numeric(),
                         Species = character())
  for (i in 1:length(trout_sumLE)){ # add tapply output to dataframe
    df_troutLE[i,] <- c(
      as.numeric(names(trout_sumLE))[i], # store percent management applied
      trout_sumLE[[i]][1], # lower bound
      trout_sumLE[[i]][2], # median
      trout_sumLE[[i]][3], # upper bound
      "L. lesueurii") # species name
  }
  
  # Compute median quantiles for Bd zoospores for each point on the management scale
  trout_sumZ <-tapply(trout_out$Z, list(trout_out$scale_val), find_quant) # tapply
  df_troutZ <- data.frame(scale = numeric(), # make empty data frame to store tapply output
                           lower = numeric(), 
                           median = numeric(), 
                           upper = numeric(),
                          Species = character())
  for (i in 1:length(trout_sumZ)){ # add tapply output to dataframe
    df_troutZ[i,] <- c(
      as.numeric(names(trout_sumZ))[i], # store percent management applied
      trout_sumZ[[i]][1], # lower bound
      trout_sumZ[[i]][2], # median
      trout_sumZ[[i]][3], # upper bound
      "BD") # species name
  }
  
  # bind SP and LE medians and CIs into one dataframe
  df_plot <- rbind(df_troutSP, df_troutLE)
  
  # convert all columns to numeric
  for (i in 1:(ncol(df_plot)-1)){
    df_plot[,i] <- as.numeric(df_plot[,i])
  }
  
  # covert all columns of Bd df to numeric
  for (i in 1:(ncol(df_troutZ)-1)){
    df_troutZ[,i] <- as.numeric(df_troutZ[,i])
  }
  
  # plot SP and LE management outcomes in one plot
  frog_plot <- ggplot(df_plot) + geom_line(aes(x=scale, y=median, color = Species), lwd=1.5)+
    geom_ribbon(aes(x=scale, ymin=lower, ymax=upper, fill = Species), alpha=0.4)+
    ylab("Adult density") + xlab("% Reduction in trout predation") +
    expand_limits(y=0) + ggtitle(elevation)+ theme_grey(base_size=20)
  
  # plote BD outcome in other plot
  Bd_plot<- ggplot(df_troutZ) + geom_line(aes(x=scale, y=median), lwd=1.5)+
    geom_ribbon(aes(x=scale, ymin=lower, ymax=upper), alpha=0.4)+
    ylab("Zoospore density") + xlab("% Reduction in trout predation") + 
    expand_limits(y=0)+ ggtitle(elevation) + theme_grey(base_size=20)
  
  
  # Rename based on which population dataset is currently loaded 
  if (elevation == 'High') { # High elevation model
    High_densities <- frog_plot
    High_Bd <- Bd_plot
    High_trout_summary <- list('SP' = df_troutSP, 'LE' = df_troutLE, 'BD' = df_troutZ)
  }
  
  if (elevation == 'Moderate') { # Moderate elevation model
    Moderate_densities <- frog_plot
    Moderate_Bd <- Bd_plot
    Moderate_trout_summary <- list('SP' = df_troutSP, 'LE' = df_troutLE, 'BD' = df_troutZ)
  }
  
  if (elevation == 'Low') { # Moderate elevation model
    Low_densities <- frog_plot
    Low_Bd <- Bd_plot
    Low_trout_summary <- list('SP' = df_troutSP, 'LE' = df_troutLE, 'BD' = df_troutZ)
  }
}


#####################
### Plotting code ###
#####################

# Calculate universal upper ploting bound for frog densities
FrogMax <- max(as.numeric(High_trout_summary$LE$upper), 
               as.numeric(High_trout_summary$SP$upper),
               as.numeric(Moderate_trout_summary$LE$upper), 
               as.numeric(Moderate_trout_summary$SP$upper),
               as.numeric(Low_trout_summary$LE$upper), 
               as.numeric(Low_trout_summary$SP$upper))
# Calculate universal upper ploting bound for BD densities
BdMax <- max(as.numeric(High_trout_summary$BD$upper),
             as.numeric(Moderate_trout_summary$BD$upper),
             as.numeric(Low_trout_summary$BD$upper))

# Add universal upper bound to frog density plots
High_densities <- High_densities + ylim(0, FrogMax)+ theme(legend.position="none") 
Moderate_densities <- Moderate_densities + ylim(0, FrogMax)+ theme(legend.position="none") 
Low_densities <- Low_densities + ylim(0, FrogMax)+ theme(legend.position="none") 

# Add universal upper bound to Bd density plots
High_Bd <- High_Bd + ylim(0, BdMax)+ theme(legend.position="none") 
Moderate_Bd <- Moderate_Bd + ylim(0, BdMax)+ theme(legend.position="none") 
Low_Bd <- Low_Bd + ylim(0, BdMax)+ theme(legend.position="none") 

# Save output to object
save(High_trout_summary, Moderate_trout_summary, Low_trout_summary,
     High_densities, High_Bd, Moderate_densities, Moderate_Bd, Low_densities, Low_Bd,
     file = "trout_summary.RData")

# Map subplots to grid and output
n <-9 # scaling factor
png(filename = 'Management_Trout.png', width=800*n, height=800*n, res=600)

layout <- rbind(c(1,1,2,2),
                c(3,3,4,4),
                c(5,5,6,6))

grid.arrange(grobs = list(
  High_densities, High_Bd,
  Moderate_densities, Moderate_Bd,
  Low_densities, Low_Bd),
  nrow=3, ncol=2, layout_matrix = layout)
dev.off()

