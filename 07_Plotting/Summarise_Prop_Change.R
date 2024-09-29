# File: Summarise_Prop_Change.R
# Title: Plots proportional change in adult density across elevations for each management scenario
# Author: Madelyn J. Mangan
# Date: 2024/04/28

# set working directory, change manually if not using Rstudio
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/export/home/s5209728/SI_final/07_Plotting") # set working directory


source(file = 'Manage_trout.R')   
source(file = 'Manage_breed.R') 
source(file = 'Manage_exclude.R') 

load("trout_summary.Rdata")
load("breed_summary.Rdata")
load("exclude_summary.Rdata")

library(ggpubr)

#### TROUT ROWS
# Initialize data frame and High-SP
df_all <- with(High_trout_summary$SP,
               data.frame("Scale" = as.numeric(scale),
                          "Median" = as.numeric(median)/as.numeric(median)[1],
                          "Elevation" = rep("High", length(median)),
                          "Species" = rep("L. spenceri", length(median)),
                          "Intervention" = rep("trout", length(median))))
# High-LE
df_all <- with(High_trout_summary$LE,
               rbind(df_all, 
                 data.frame("Scale" = as.numeric(scale),
                            "Median" = as.numeric(median)/as.numeric(median)[1],
                            "Elevation" = rep("High", length(median)),
                            "Species" = rep("L. lesueurii", length(median)),
                            "Intervention" = rep("trout", length(median)))))

# Moderate-SP
df_all <- with(Moderate_trout_summary$SP,
               rbind(df_all, 
                 data.frame("Scale" = as.numeric(scale),
                            "Median" = as.numeric(median)/as.numeric(median)[1],
                            "Elevation" = rep("Moderate", length(median)),
                            "Species" = rep("L. spenceri", length(median)),
                            "Intervention" = rep("trout", length(median)))))
# Moderate-LE
df_all <- with(Moderate_trout_summary$LE,
               rbind(df_all, 
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("Moderate", length(median)),
                                "Species" = rep("L. lesueurii", length(median)),
                                "Intervention" = rep("trout", length(median)))))

# Low-SP
df_all <- with(Low_trout_summary$SP,
               rbind(df_all, 
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("Low", length(median)),
                                "Species" = rep("L. spenceri", length(median)),
                                "Intervention" = rep("trout", length(median)))))
# Low-LE
df_all <- with(Low_trout_summary$LE,
               rbind(df_all, 
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("Low", length(median)),
                                "Species" = rep("L. lesueurii", length(median)),
                                "Intervention" = rep("trout", length(median)))))

#### BREED ROWS
# High-SP
df_all <- with(High_breed_summary$SP,
               rbind(df_all,
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("High", length(median)),
                                "Species" = rep("L. spenceri", length(median)),
                                "Intervention" = rep("breed", length(median)))))
# High-LE
df_all <- with(High_breed_summary$LE,
               rbind(df_all,
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("High", length(median)),
                                "Species" = rep("L. lesueurii", length(median)),
                                "Intervention" = rep("breed", length(median)))))

# Moderate-SP
df_all <- with(Moderate_breed_summary$SP,
               rbind(df_all,
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("Moderate", length(median)),
                                "Species" = rep("L. spenceri", length(median)),
                                "Intervention" = rep("breed", length(median)))))
# Moderate-LE
df_all <- with(Moderate_breed_summary$LE,
               rbind(df_all,
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("Moderate", length(median)),
                                "Species" = rep("L. lesueurii", length(median)),
                                "Intervention" = rep("breed", length(median)))))

# Low-SP
df_all <- with(Low_breed_summary$SP,
               rbind(df_all,
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("Low", length(median)),
                                "Species" = rep("L. spenceri", length(median)),
                                "Intervention" = rep("breed", length(median)))))
# Low-LE
df_all <- with(Low_breed_summary$LE,
               rbind(df_all,
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("Low", length(median)),
                                "Species" = rep("L. lesueurii", length(median)),
                                "Intervention" = rep("breed", length(median)))))

#### EXCLUDE ROWS
# High-SP
df_all <- with(High_exclude_summary$SP,
               rbind(df_all,
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("High", length(median)),
                                "Species" = rep("L. spenceri", length(median)),
                                "Intervention" = rep("exclude", length(median)))))
# High-LE
df_all <- with(High_exclude_summary$LE,
               rbind(df_all, 
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("High", length(median)),
                                "Species" = rep("L. lesueurii", length(median)),
                                "Intervention" = rep("exclude", length(median)))))

# Moderate-SP
df_all <- with(Moderate_exclude_summary$SP,
               rbind(df_all, 
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("Moderate", length(median)),
                                "Species" = rep("L. spenceri", length(median)),
                                "Intervention" = rep("exclude", length(median)))))
# Moderate-LE
df_all <- with(Moderate_exclude_summary$LE,
               rbind(df_all, 
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("Moderate", length(median)),
                                "Species" = rep("L. lesueurii", length(median)),
                                "Intervention" = rep("exclude", length(median)))))

# Low-SP
df_all <- with(Low_exclude_summary$SP,
               rbind(df_all, 
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("Low", length(median)),
                                "Species" = rep("L. spenceri", length(median)),
                                "Intervention" = rep("exclude", length(median)))))
# Low-LE
df_all <- with(Low_exclude_summary$LE,
               rbind(df_all, 
                     data.frame("Scale" = as.numeric(scale),
                                "Median" = as.numeric(median)/as.numeric(median)[1],
                                "Elevation" = rep("Low", length(median)),
                                "Species" = rep("L. lesueurii", length(median)),
                                "Intervention" = rep("exclude", length(median)))))



df_all$Elevation <- factor(df_all$Elevation, levels = c('High', 'Moderate', 'Low'))
df_all$Species <- factor(df_all$Species, levels = c('L. lesueurii', 'L. spenceri'))
                          
lower <- 0
upper <- max(df_all$Median)
df_trout <- subset(df_all, Intervention == 'trout')
df_breed <- subset(df_all, Intervention == 'breed')
df_exclude <- subset(df_all, Intervention == 'exclude')

gg_trout <- ggplot(data = df_trout) + 
  geom_line(aes(x = Scale, y = Median, color = Species, linetype = Elevation),
            lwd=1) + ylim(lower, upper) + 
  geom_abline(intercept=1, slope=0, linetype=1, alpha=0.15, lwd=1.1 ) +
  ylab("20-year prop. change in adult density") +
  xlab("Reduction in trout predation (%)") +
  theme_bw(base_size=17)

gg_exclude <- ggplot(data = df_exclude) + 
  geom_line(aes(x = Scale, y = Median, color = Species, linetype = Elevation),
            lwd=1) + ylim(lower, upper) + 
  geom_abline(intercept=1, slope=0, linetype=1, alpha=0.15, lwd=1.1 ) +
  ylab("20-year prop. change in adult density") +
  xlab(expression(paste("Removal of ", 
                        italic('L. lesueurii '), 'adults/subadults (%)')))+
  theme_bw(base_size=17) +
  scale_color_hue(labels=c("L. spenceri" = expression(italic('L. spenceri')),
                           "L. lesueurii" = expression(italic('L. lesueurii'))))+
  theme(legend.position="top", legend.direction = "horizontal", 
        legend.justification = c(.5,.5), legend.text=element_text(size=18),
        legend.title = element_text(size=20))
  

gg_breed <- ggplot(data = df_breed) + 
  geom_line(aes(x = Scale, y = Median, color = Species, linetype = Elevation),
            lwd=1.1) + ylim(lower, upper) + 
  theme_bw(base_size=17) +
  geom_abline(intercept=1, slope=0, linetype=1, alpha=0.15, lwd=1.1 ) +
  ylab("20-year prop. change in adult density") +
  xlab(expression(paste(italic("L. spenceri "), 'captive breeding (%)')))

  

png(filename = 'Proportional_change.png', width=9000, height=3000, res=550)
ggarrange(gg_exclude, gg_trout, gg_breed, ncol=3, common.legend = T, legend = "top")
dev.off()


