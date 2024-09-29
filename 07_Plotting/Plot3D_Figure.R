# File: Plot3D_Figure.R
# Title: Aggregates management outcomes for combined trout removal and captive breeding, and
#        makes contour plot for both L. spenceri competitive fitness and Bd zoospore concentration
# Author: Madelyn J. Mangan
# Date: 2024/09/27

# load packages
library(ggplot2)
library(ggpubr)
library(metR)


# set working directory, change manually if not using Rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load Data
load("High_3D_out.RData")
data_High <- all_results
load("Moderate_3D_out.RData")
data_Moderate <- all_results
load("Low_3D_out.RData")
data_Low <- all_results

###########################
### Competitive fitness ###
###########################

lower <- 0 # set lower scale boundary (0% SP adult prevalence)
upper <- 1 # set upper scale boundary (100% SP adult prevalence)

# Generate High Elevation Plot
High_fit <- ggplot(data_High) + 
  geom_tile(aes(x=Trout_reduction, y=Captive_breeding, fill = Ratio_SP_LE))+
  scale_fill_gradient2(low="red", mid="white", high="navy", midpoint = .5,
                       limits = c(lower, upper), guide = "colorbar") +
  geom_contour(aes(x=Trout_reduction, y=Captive_breeding, z = Ratio_SP_LE), 
               colour=alpha(1, alpha=0.05),linewidth = 1, binwidth=0.05)+
  metR::geom_text_contour(aes(x=Trout_reduction, y=Captive_breeding, z = Ratio_SP_LE), 
                          label.placer = label_placer_fraction(frac = 0.5),
                          stroke = 0, rotate=F, colour=alpha(1, alpha=0.35)) +
  xlab("Reduction in trout predation (%)") + ylab(expression(paste("Increased ", italic("L. spenceri"), " fecundity")))+
  labs(fill=expression(paste(italic("L. spenceri"), " competitive fitness")))+ theme_classic(base_size = 20) +
  theme(legend.position="top", legend.direction = "horizontal", legend.title.align=0.5, legend.justification = c(.625,0)) +
  guides(fill = guide_colorbar(barwidth = 20, barheight =1, title.position = "top"))

# Generate Moderate Elevation Plot
Moderate_fit <- ggplot(data_Moderate) + geom_tile(aes(x=Trout_reduction, y=Captive_breeding, fill = Ratio_SP_LE))+
  scale_fill_gradient2(low="red", mid="white", high="navy", midpoint = .5,
                       limits = c(lower, upper)) +
  geom_contour(aes(x=Trout_reduction, y=Captive_breeding, z = Ratio_SP_LE), 
               colour=alpha(1, alpha=0.05),linewidth = 1, binwidth=0.05)+
  metR::geom_text_contour(aes(x=Trout_reduction, y=Captive_breeding, z = Ratio_SP_LE), 
                          label.placer = label_placer_fraction(frac = 0.5),
                          stroke = 0, rotate=F, colour=alpha(1, alpha=0.35)) +
  xlab("Reduction in trout predation (%)") + ylab(expression(paste("Increased ", italic("L. spenceri"), " fecundity")))+
  labs(fill='L. spenceri competitive fitness')+ theme_classic(base_size = 20) +
  theme(legend.position="none")

# Generate Low Elevation Plot
Low_fit <- ggplot(data_Low) + geom_tile(aes(x=Trout_reduction, y=Captive_breeding, fill = Ratio_SP_LE))+
  scale_fill_gradient2(low="red", mid="white", high="navy", midpoint = .5,
                       limits = c(lower, upper)) +
  geom_contour(aes(x=Trout_reduction, y=Captive_breeding, z = Ratio_SP_LE), 
               colour=alpha(1, alpha=0.05),linewidth = 1, binwidth=0.05)+
  metR::geom_text_contour(aes(x=Trout_reduction, y=Captive_breeding, z = Ratio_SP_LE), 
                          label.placer = label_placer_fraction(frac = 0.5), skip=0,
                          stroke = 0, rotate=F, colour=alpha(1, alpha=0.35), binwidth=0.05) +
  xlab("Reduction in trout predation (%)") + ylab(expression(paste("Increased ", italic("L. spenceri"), " fecundity")))+
  labs(fill='L. spenceri competitive fitness')+ theme_classic(base_size = 20) +
  theme(legend.position="none")


###########################
### Bd Zoospore Density ###
###########################

lower <- min(data_High$Zoospores,data_Moderate$Zoospores,data_Low$Zoospores) # set lower scale boundary (lowest Bd conc)
upper <- max(data_High$Zoospores,data_Moderate$Zoospores,data_Low$Zoospores) # set upper scale boundary (highest Bd conc)

# Generate High Elevation Plot
High_Z <- ggplot(data_High) + geom_tile(aes(x=Trout_reduction, y=Captive_breeding, fill = Zoospores))+
  scale_fill_gradient2(low="white", mid="orange", high="red", midpoint = lower + (upper-lower)/2,
                       limits = c(lower, upper)) +
  geom_contour(aes(x=Trout_reduction, y=Captive_breeding, z = Zoospores), 
               colour=alpha(1, alpha=0.05),linewidth = 1)+
  metR::geom_text_contour(aes(x=Trout_reduction, y=Captive_breeding, z = Zoospores), 
                          label.placer = label_placer_fraction(frac = 0.5),
                          stroke = 0, skip = 0, rotate=T, colour=alpha(1, alpha=0.35)) +
  xlab("Reduction in trout predation (%)") + ylab(expression(paste("Increased ", italic("L. spenceri"), " fecundity")))+
  labs(fill='Bd zoospore density')+ theme_classic(base_size = 20)+
  theme(legend.position="top", legend.direction = "horizontal", legend.title.align=0.5, legend.justification = c(.625,0)) +
  guides(fill = guide_colorbar(barwidth = 20, barheight =1, title.position = "top"))

# Generate Moderate Elevation Plot
Moderate_Z <- ggplot(data_Moderate) + geom_tile(aes(x=Trout_reduction, y=Captive_breeding, fill = Zoospores))+
  scale_fill_gradient2(low="white", mid="orange", high="red" , midpoint = lower + (upper-lower)/2,
                       limits = c(lower, upper)) +
  geom_contour(aes(x=Trout_reduction, y=Captive_breeding, z = Zoospores), 
               colour=alpha(1, alpha=0.05),linewidth = 1)+
  metR::geom_text_contour(aes(x=Trout_reduction, y=Captive_breeding, z = Zoospores), 
                          label.placer = label_placer_fraction(frac = 0.5),
                          stroke = 0, skip = 0, rotate=T, colour=alpha(1, alpha=0.35)) +
  xlab("Reduction in trout predation (%)") + ylab(expression(paste("Increased ", italic("L. spenceri"), " fecundity")))+
  labs(fill='Bd zoospore density')+ theme_classic(base_size = 20)

# Generate Low Elevation Plot
Low_Z<- ggplot(data_Low) + geom_tile(aes(x=Trout_reduction, y=Captive_breeding, fill = Zoospores))+
  scale_fill_gradient2(low="white", mid="orange", high="red" , midpoint = lower + (upper-lower)/2,
                       limits = c(lower, upper)) +
  geom_contour(aes(x=Trout_reduction, y=Captive_breeding, z = Zoospores), 
               colour=alpha(1, alpha=0.05),linewidth = 1)+
  metR::geom_text_contour(aes(x=Trout_reduction, y=Captive_breeding, z = Zoospores), 
                          label.placer = label_placer_fraction(frac = 0.5),
                          stroke = 0, skip = 0, rotate=T, colour=alpha(1, alpha=0.35)) +
  xlab("Reduction in trout predation (%)") + ylab(expression(paste("Increased ", italic("L. spenceri"), " fecundity")))+
  labs(fill='Bd zoospore density')+ theme_classic(base_size = 20)


################################
### Generate Figure 3 Panels ###
################################

# Competitive Fitness
png(filename = 'Contour_host.png', width=500*7, height=1450*7, res=600)
ggarrange(High_fit, Moderate_fit, Low_fit, ncol=1, nrow=3, common.legend = T)
dev.off()

# Bd Zoospore Density
png(filename = 'Contour_Bd.png', width=500*7, height=1450*7, res=600)
ggarrange(High_Z, Moderate_Z, Low_Z, ncol=1, nrow=3, common.legend = T)
dev.off()



