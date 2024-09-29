### README for 04_QuasiDiagnostics

Input files: (From 03_SameK_Quasi-Equilibration)
	- High_quasi.RData
	- Moderate_quasi.RData
	- Low_quasi.RData

Output files: 
	- High_manage.RData
	- Moderate_manage.RData
	- Low_manage.RData
	- High_manage3d.RData
	- Moderate_manage3D.RData
	- Low_manage3D.RData
	- Equilibrium_Densities.png
	- Equilibrium_Prevalence.png
	- LEtoSP_Ratio.png
	- Year_of_EQ.png
	- Sensitivity.png
	- Sensitivity_slim.png

Scripts to execute: 
	- quasi-eq_diagnostics
	- Sensitivity.R

# quasi-eq_diagnostics.R
The primary script for this folder is quas-eq_diagnostics.R. For it to run, all *.RData output from 03_QuasiEquilibration 
must be in the directory. It cycles through equilibrated systems which passed realism criteria, and calculates several
outcomes/statistics from those data sets. It also consolidates passing systems in to *_manage.RData files for use in 
05_ManagementScenarios, and then takes the saves median values of realistic systems as *manage3D.RData for use in 
06_RecruitmentContours.

The final outputs of this script are:
(1) LEtoSP_Ratio.png - A density plot displaying the L. lesueurii-to-L. spenceri ration at quasi-equilibrium aross all 
    passing parameter sets for each elevation. 
(2) Equilibrium_Prevalence.png - Density plots displaying the prevalence of infection in both summer (January 1st) and 
    winter (July 2nd) at quasi-equilibrium across all passing parameter sets for each elevation. 
(3) Year_of_EQ.png - A density plot displaying the year of quasi-equilibrium across passing parameters sets for each
	elevation
(4) Equilibrium_Densities.png - A boxplot with jittered points showing the final density for each host compartment 
	across all simulations and elevations. 
(5) *manage.RData objects for use in management simulations
(6) *manage3D.RData objects for use in generating contour plots for joint trout/captive breeding intervention

# Sensitivity.R 
This script conducts a sensitivity analysis using partial rank correlation coefficients (PRCCs) in the package 'epi.R'. 
It calculates PRCCS for L. spenceri density, L. lesueurii density, L. spenceri competitive fitness, L. spenceri
prevalence of infection, L. lesueurii prevalence of infection, and density of Bd zoospores across all model parameters
and elevation models. 
The final outputs of this script are:
(1) Sensitivity.png - A full figure displaying PRCCS for every outcome-by-parameter-by-elevation combination. 
(2) Sensitivity_slim.png - A consolidated figure displaying PRCCs only for L. spenceri density, L. lesueurii density, and 
	L. spenceri competitive fitness, averaged for each parameter type (across life stages) and across elevations. 

