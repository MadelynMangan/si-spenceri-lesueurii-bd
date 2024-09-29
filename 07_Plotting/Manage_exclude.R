# File: Manage_exclude.R
# Title: Aggregates manaement scenario results for L. lesueurii exclusion and 
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
data_names <- c("High_exclude.RData", "Moderate_exclude.RData", "Low_exclude.RData") 

### For loop to compile ggplots within/across datasets
for (j in 1:length(data_names)){
  population <- data_names[j] # set scenario datafile name
  load(population)            # load scenario quasi-eq output
  elevation <- strsplit(population, "[_]")[[1]][1] # extract scenario name from file name
  print(paste("Begin", elevation, "elevation compilation..."))
  # aggregate final year of data across simulation in single dataframe using agg_final()
  exclude_out <- agg_final(All_Results_All,  mS1_slide)
  
  # Compute median and quantiles for L. spenceri for each point on the management scale
  exclude_sumSP <-tapply(exclude_out$A_SP_Total, list(exclude_out$scale_val), find_quant) # tapply
  df_excludeSP <- data.frame(scale = numeric(), # make empty data frame to store tapply output
                             lower = numeric(), 
                             median = numeric(), 
                             upper = numeric(),
                             Species = character())
  for (i in 1:length(exclude_sumSP)){ # add tapply output to dataframe
    df_excludeSP[i,] <- c(
      as.numeric(names(exclude_sumSP))[i], # store percent management applied
      exclude_sumSP[[i]][1], # lower bound
      exclude_sumSP[[i]][2], # median
      exclude_sumSP[[i]][3], # upper bound
      "L. spenceri") # species name
  }

  # Compute median quantiles for L. lesueurii for each point on the management scale
  exclude_sumLE <-tapply(exclude_out$A_LE_Total, list(exclude_out$scale_val), find_quant) # tapply
  df_excludeLE <- data.frame(scale = numeric(), # make empty data frame to store tapply output
                         lower = numeric(), 
                         median = numeric(), 
                         upper = numeric(),
                         Species = character())
  for (i in 1:length(exclude_sumLE)){ # add tapply output to dataframe
    df_excludeLE[i,] <- c(
      as.numeric(names(exclude_sumLE))[i], # store percent management applied
      exclude_sumLE[[i]][1], # lower bound
      exclude_sumLE[[i]][2], # median
      exclude_sumLE[[i]][3], # upper bound
      "L. lesueurii") # species name
  }
  
  # Compute median quantiles for Bd zoospores for each point on the management scale
  exclude_sumZ <-tapply(exclude_out$Z, list(exclude_out$scale_val), find_quant) # tapply
  df_excludeZ <- data.frame(scale = numeric(), # make empty data frame to store tapply output
                           lower = numeric(), 
                           median = numeric(), 
                           upper = numeric(),
                          Species = character())
  for (i in 1:length(exclude_sumZ)){ # add tapply output to dataframe
    df_excludeZ[i,] <- c(
      as.numeric(names(exclude_sumZ))[i], # store percent management applied
      exclude_sumZ[[i]][1], # lower bound
      exclude_sumZ[[i]][2], # median
      exclude_sumZ[[i]][3], # upper bound
      "BD") # species name
  }
  
  # bind SP and LE medians and CIs into one dataframe
  df_plot <- rbind(df_excludeSP, df_excludeLE)
  
  # convert all columns to numeric
  for (i in 1:(ncol(df_plot)-1)){
    df_plot[,i] <- as.numeric(df_plot[,i])
  }
  
  # covert all columns of Bd df to numeric
  for (i in 1:(ncol(df_excludeZ)-1)){
    df_excludeZ[,i] <- as.numeric(df_excludeZ[,i])
  }
  
  # plot SP and LE management outcomes in one plot
  frog_plot <- ggplot(df_plot) + geom_line(aes(x=scale, y=median, color = Species), lwd=1.5)+
    geom_ribbon(aes(x=scale, ymin=lower, ymax=upper, fill = Species), alpha=0.4)+
    ylab("Adult density") + xlab(expression(paste("% increase ", italic("L. lesueurii")," mortality"))) +
    expand_limits(y=0) + ggtitle(elevation)+ theme_grey(base_size=20)
  
  # plot BD outcome in other plot
  Bd_plot<- ggplot(df_excludeZ) + geom_line(aes(x=scale, y=median), lwd=1.5)+
    geom_ribbon(aes(x=scale, ymin=lower, ymax=upper), alpha=0.4)+
    ylab("Zoospore density") + xlab(expression(paste("% increase ", italic("L. lesueurii")," mortality"))) +
    expand_limits(y=0)+ ggtitle(elevation) + theme_grey(base_size=20)
  
  
  # Rename based on which population dataset is currently loaded 
  if (elevation == 'High') { # High elevation model
    High_densities <- frog_plot
    High_Bd <- Bd_plot
    High_exclude_summary <- list('SP' = df_excludeSP, 'LE' = df_excludeLE, 'BD' = df_excludeZ)
  }
  
  if (elevation == 'Moderate') { # Moderate elevation model
    Moderate_densities <- frog_plot
    Moderate_Bd <- Bd_plot
    Moderate_exclude_summary <- list('SP' = df_excludeSP, 'LE' = df_excludeLE, 'BD' = df_excludeZ)
  }
  
  if (elevation == 'Low') { # Moderate elevation model
    Low_densities <- frog_plot
    Low_Bd <- Bd_plot
    Low_exclude_summary <- list('SP' = df_excludeSP, 'LE' = df_excludeLE, 'BD' = df_excludeZ)
  }
}


#####################
### Plotting code ###
#####################

# Calculate universal upper ploting bound for frog densities
FrogMax <- max(as.numeric(High_exclude_summary$LE$upper), 
               as.numeric(High_exclude_summary$SP$upper),
               as.numeric(Moderate_exclude_summary$LE$upper), 
               as.numeric(Moderate_exclude_summary$SP$upper),
               as.numeric(Low_exclude_summary$LE$upper), 
               as.numeric(Low_exclude_summary$SP$upper))
# Calculate universal upper ploting bound for BD densities
BdMax <- max(as.numeric(High_exclude_summary$BD$upper),
             as.numeric(Moderate_exclude_summary$BD$upper),
             as.numeric(Low_exclude_summary$BD$upper))

# Add universal upper bound to frog density plots
High_densities <- High_densities + ylim(0, FrogMax)+ theme(legend.position="none") 
Moderate_densities <- Moderate_densities + ylim(0, FrogMax)+ theme(legend.position="none") 
Low_densities <- Low_densities + ylim(0, FrogMax)+ theme(legend.position="none") 

# Add universal upper bound to Bd density plots
High_Bd <- High_Bd + ylim(0, BdMax)+ theme(legend.position="none") 
Moderate_Bd <- Moderate_Bd + ylim(0, BdMax)+ theme(legend.position="none") 
Low_Bd <- Low_Bd + ylim(0, BdMax)+ theme(legend.position="none") 

# Save output to object
save(High_exclude_summary, Moderate_exclude_summary, Low_exclude_summary,
     High_densities, High_Bd, Moderate_densities, Moderate_Bd, Low_densities, Low_Bd,
     file = "exclude_summary.RData")

# Map subplots to grid and output
n<-9
png(filename = 'Management_exclude.png', width=800*n, height=800*n, res=600)

layout <- rbind(c(1,1,2,2),
                c(3,3,4,4),
                c(5,5,6,6))

grid.arrange(grobs = list(
  High_densities, High_Bd,
  Moderate_densities, Moderate_Bd,
  Low_densities, Low_Bd),
  nrow=3, ncol=2, layout_matrix = layout)
dev.off()

