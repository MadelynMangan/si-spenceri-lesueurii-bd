# File: Sensitivity.R
# Description: generates a sensitivity analysis using partial rank correlation
#              coefficients (PRCCSs) for all model parameters. Generates a full
#			   figure and a slim figure (with PRCCs averaged across life stages
#			   for a given parameter). 
# Author: Madelyn J. Mangan
# Date: 2024/09/27

library(dplyr)
library(epiR)
library(tidyr)
library(ggplot2)

# set working directory, change manually if not using Rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Function to compile input data frames for epi.prcc() in the epiR package

compile_PRCC <- function(eq_init, eq_parm){
  df <- data.frame("SP" = numeric(),
                   "LE" = numeric(),
                   "SPtoLE" = numeric(),
                   "SP_Prev"=numeric(),
                   "LE_Prev"=numeric(),
                   "BD" = numeric())
  for (i in 1:length(eq_init)){
    temp <- as.list(eq_init[[i]])
    sp <- with(temp, S_A1.1_SP + I_A1.1_SP + S_A2_SP + I_A2_SP)
    sp_prev <- with(temp, (I_A1.1_SP + I_A2_SP)/sp) 
    le <- with(temp, S_A1.1_LE + I_A1.1_LE + S_A2_LE + I_A2_LE)
    le_prev <- with(temp, (I_A1.1_LE + I_A2_LE)/le) 
    sp_le <- sp/(sp+le)
    z <- temp$Z
    df[i,] <- c(sp, le, sp_le, sp_prev, le_prev, z)
  }
  outList <- list("SP" = bind_cols(eq_parm, data.frame("SP" = df[,1])),
                  "LE" = bind_cols(eq_parm, data.frame("LE" = df[,2])),
                  "SP_Fit" = bind_cols(eq_parm, data.frame("SP_Fit" = df[,3])),
                  "SP_Prev"=bind_cols(eq_parm, data.frame("SP_Prev" = df[,4])),
                  "LE_Prev"=bind_cols(eq_parm, data.frame("LE_Prev" = df[,5])),
                  "BD" = bind_cols(eq_parm,data.frame("BD" = df[,6])))
  return(outList)
}

load('High_manage.RData')
High_input <- compile_PRCC(init_eq, parm_eq)
load('Moderate_manage.RData')
Moderate_input <- compile_PRCC(init_eq, parm_eq)
load('Low_manage.RData')
Low_input <- compile_PRCC(init_eq, parm_eq)

long_summary <- function(input, elevation){
  for (i in 1:length(input)){
    temp <- epi.prcc(input[[i]])
    ifelse(i==1, 
           sens <- data.frame('prcc' = temp$est, 
                              'measure' = rep(names(input)[i], length(temp$est)),
                              'parameter' = names(input[[i]])[1:(ncol(input[[i]])-1)],
                              'elevation' = rep(elevation, length(temp$est))),
           sens <- bind_rows(sens, data.frame(data.frame('prcc' = temp$est, 
                                                         'measure' = rep(names(input)[i], length(temp$est)),
                                                         'parameter' = names(input[[i]])[1:(ncol(input[[i]])-1)],
                                                         'elevation' = rep(elevation, length(temp$est))))))
  }
  return(sens)
}

High_out <- long_summary(High_input, "High")
Moderate_out <- long_summary(Moderate_input, "Mod")
Low_out <- long_summary(Low_input, "Low")

All_out <- bind_rows(High_out, Moderate_out, Low_out)
All_out$parameter <- factor(All_out$parameter,
                          levels = names(High_input$SP)[1:(length(High_input$SP-1))])
All_out$elevation <- factor(All_out$elevation,
                            levels = c("Low", "Mod", "High"))
All_out$label <- with(All_out, paste(measure, elevation, sep=' - ')) 

All_out$label <- factor(All_out$label,
                        levels = c("SP - Low", "SP - Mod", "SP - High",
                                   "LE - Low", "LE - Mod", "LE - High",
                                   "SP_Fit - Low", "SP_Fit - Mod", "SP_Fit - High",
                                   "SP_Prev - Low", "SP_Prev - Mod", "SP_Prev - High",
                                   "LE_Prev - Low", "LE_Prev - Mod", "LE_Prev - High",
                                   "BD - Low", "BD - Mod", "BD - High"))


sens <- ggplot(data=All_out) + geom_tile(aes(x=label, y = parameter, fill = prcc))+
  scale_fill_gradient2(low="red", mid="white", high="navy") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Population") + ylab("Parameter") +
  theme(panel.background = element_rect(fill = "white",
                                    colour = "white"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "light gray"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "light gray"))

png(filename = 'Sensitivity.png', width = 2400*1.25, height = 6000*1.25, res =600)
sens
dev.off()


#########################################
### Simplified Composite PRCC Figure #####
##########################################

SP_out <- subset(All_out, measure == 'SP')
LE_out <- subset(All_out, measure == 'LE')
SPfit_out <- subset(All_out, measure =='SP_Fit')

data_list <- list(SP_out, LE_out, SPfit_out)  
names <- c('SP adult density', 'LE adult density', 'SP competitive fitness')       
composite = tibble('PRCC_avg' = rep(NA, 13*3), "Parameter"= rep(NA, 13*3), 'Outcome'= rep(NA, 13*3))
index = 1 
for (i in 1:3){
  temp <- data_list[[i]]
  f_SP <- mean(temp$prcc[grep(pattern = glob2rx("f*SP"), temp$parameter)])
  beta_SP <- mean(temp$prcc[grep(pattern = glob2rx("beta*SP"), temp$parameter)])
  m_SP <- mean(temp$prcc[grep(pattern = glob2rx("m*SP"), temp$parameter)])
  mu_SP <- mean(temp$prcc[grep(pattern = glob2rx("mu*SP"), temp$parameter)])
  gamma_SP <- mean(temp$prcc[grep(pattern = glob2rx("gamma*SP"), temp$parameter)])
  lambda_SP <- mean(temp$prcc[grep(pattern = glob2rx("lambda*SP"), temp$parameter)])
  f_LE <- mean(temp$prcc[grep(pattern = glob2rx("f*LE"), temp$parameter)])
  beta_LE <- mean(temp$prcc[grep(pattern = glob2rx("beta*LE"), temp$parameter)])
  m_LE <- mean(temp$prcc[grep(pattern = glob2rx("m*LE"), temp$parameter)])
  mu_LE <- mean(temp$prcc[grep(pattern = glob2rx("mu*LE"), temp$parameter)])
  gamma_LE <- mean(temp$prcc[grep(pattern = glob2rx("gamma*LE"), temp$parameter)])
  lambda_LE <- mean(temp$prcc[grep(pattern = glob2rx("lambda*LE"), temp$parameter)])
  gamma_Z <- mean(temp$prcc[grep(pattern = glob2rx("gamma*Z"), temp$parameter)])
  composite[index:(index+12),] <- cbind(c(f_SP, beta_SP, m_SP, mu_SP, gamma_SP, lambda_SP, 
                              f_LE, beta_LE, m_LE, mu_LE, gamma_LE, lambda_LE,
                              gamma_Z),
                            c('SP fecundity (f)', 'SP transmission (\U03B2)', 
                              'SP baseline mortality (m)', 'SP Bd mortality (\U03BC)', 
                              'SP recovery (\U03B3)', 'SP shedding (\U03BB)', 
                              'LE fecundity (f)', 'LE transmission (\U03B2)', 
                              'LE baseline mortality (m)', 'LE Bd mortality (\U03BC)', 
                              'LE recovery (\U03B3)', 'LE shedding (\U03BB)',
                              'Zoospore decay (\U03B3z)'),
                            rep(names[i], 13))
  index <- index+13
}
 
composite$PRCC_avg <- as.numeric(composite$PRCC_avg)
composite$Parameter <- factor(composite$Parameter, levels = rev(c('SP fecundity (f)','LE fecundity (f)',
                                                              'SP baseline mortality (m)', 'LE baseline mortality (m)',
                                                              'SP Bd mortality (\U03BC)', 'LE Bd mortality (\U03BC)', 
                                                              'SP transmission (\U03B2)', 'LE transmission (\U03B2)', 
                                                              'SP recovery (\U03B3)', 'LE recovery (\U03B3)', 
                                                              'SP shedding (\U03BB)', 'LE shedding (\U03BB)',
                                                              'Zoospore decay (\U03B3z)')))
composite$Outcome <- factor(composite$Outcome, levels = c('SP competitive fitness','LE adult density', 'SP adult density' ) ) 

cbPalette <- c("#56B4E9","#E69F00","#999999")

v2 <- ggplot(data = composite) + geom_bar(aes(y = Parameter, x = PRCC_avg, fill = Outcome), position = 'dodge', stat = 'identity') +
  geom_hline(yintercept = c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5)) +
  geom_hline(yintercept = c(2.5, 4.5, 6.5, 8.5, 10.5,12.5), color = alpha('black', 0.45), linetype='dashed') +
  xlab('Composite PRCC') +
  theme_bw(base_size=20) +
  scale_fill_manual(labels = rev(c(expression(paste(italic('L. spenceri '), '(SP) adult density')),
                               expression(paste(italic('L. lesueurii '), '(LE) adult density')),
                               expression(paste(italic('L. spenceri '), 'competitive fitness')))),
                    values=rev(cbPalette)) + 
  theme(legend.position='top', legend.direction = 'vertical',legend.title.align=0,
        panel.border = element_rect(colour = "black", fill=NA), legend.justification = 'right',
        legend.box.just = 'right', legend.text.align = 0, legend.spacing.y = unit(.75, 'lines')) +
  guides(fill = guide_legend(byrow = TRUE, reverse=T))

png(filename = 'Sensitivity_slim.png', width = 4000, height = 8000, res =600)
v2
dev.off()

  



