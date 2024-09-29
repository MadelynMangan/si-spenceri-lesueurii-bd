### README for 07_Plotting

Input files: (From 05_ManagementScenarios and 06_RecruitmentContours)
	- High_breed.RData
	- High_trout.RData
	- High_exclude.RData
	- Moderate_breed.RData
	- Moderate_trout.RData
	- Moderate_exclude.RData
	- Low_breed.RData
	- Low_trout.RData
	- Low_exclude.RData
	- High_manage3D.RData
	- Moderate_manage3D.RData
	- Low_manage3D.RData

Output files: 
	- trout_summary.RData
	- breed_summary.RData
	- exclude_summary.RData
	- Management_trout.png
	- Management_breed.png
	- Management_exclude.png
	- Proportional_change.png
	- Contour_host.png
	- Contour_Bd.png
	
Scripts to execute: 
	- Summarise_Prop_Change.R
	- Plot3D_Figure.R

This directory requires the execution of two scripts:
(1) Summarise_Prop_Change.R - consolidates management scenarios and plots
	outcomes as proportional change vs degree of intervention
(2) Plot3D_Figure.R - Consolidates and plots the contour plot simulations
    for joint intervention using captive breeding and trout
	removal 

## Summarise_Prop_Change.R 
This script requires that all output *.RData files from 05_ManagementScenarios
has been transferred to the directory. This script first calls three other scripts,
manage_trout.R, manage_breed.R, and manage_exclude.R. These manage_*.R scripts sift through
management scenario output for each elevation and consolidate final year results from each 
simulation. They then output a plot of raw density vs percent intervention showing trajectories
of L. spenceri, L. lesueurii, and Bd as management effort increases (located in the appendix 
supplementary figures). These manage*R scripts also output a *summary.RData object with aggregated 
results for the given management scenario. The Summarise_Prop_Change.R script then loads the 
*summary.RData objects that were created using the manage_*.R scripts, and uses them to create the
Proportional_change.png plot which is featured in the main text of the paper. 
Outputs:
	- trout_summary.RData
	- breed_summary.RData
	- exclude_summary.RData
	- Management_trout.png
	- Management_breed.png
	- Management_exclude.png
	- Proportional_change.png

## Plot3D_Figure.R
This script uses the *3D_out.RData objects created in 06_RecruitmentContours to make final contour 
plots. Contour_host.png shows contour plots for L. spenceri competitive fitness across all three 
elevations. Contour_Bd.png shows contour plots of Bd zoospore density across all three elevations. 
These figures are combined in the main text of the paper. 
Outputs: 
	- Contour_host.png
	- Contour_Bd.png