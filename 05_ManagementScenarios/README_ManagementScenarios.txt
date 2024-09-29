### README for 05_ManagementScenarios

Input files: (From 04_QuasiDiagnostics)
	- High_manage.RData
	- Moderate_manage.RData
	- Low_manage.RData

Output files: 
	- High_breed.RData
	- High_trout.RData
	- High_exclude.RData
	- Moderate_breed.RData
	- Moderate_trout.RData
	- Moderate_exclude.RData
	- Low_breed.RData
	- Low_trout.RData
	- Low_exclude.RData

Scripts to execute: 
	- High_breed_set.R
	- High_trout_set.R
	- High_exclude_set.R
	- Moderate_breed_set.R
	- Moderate_trout_set.R
	- Moderate_exclude_set.R
	- Low_breed_set.R
	- Low_trout_set.R
	- Low_exclude_set.R

There are nine scripts in total to be run:
(1) three scripts *breed_set.R, one for each elevation - these scripts run management simulations across degrees
	of intervention for captive breeding. These scripts accept *manage.RData files as input, and then cycle through
	the *manage.R script associated with the appropriate elevation to run ODEs. The *manage.R script is called once
	for every quasi-equilibrated parameter set which passed realism criteria at a given elevation. 
(2) three scripts *trout_set.R, one for each elevation - these scripts run management simulations across degrees
	of intervention for trout removal. These scripts accept *manage.RData files as input, and then cycle through
	the *manage.R script associated with the appropriate elevation to run ODEs. The *manage.R script is called once
	for every quasi-equilibrated parameter set which passed realism criteria at a given elevation.  
(3) three scripts *exclude_set.R, one for each elevation - these scripts run management simulations across degrees
	of intervention for L. lesueurii removal. These scripts accept *manage.RData files as input, and then cycle through
	the *manage.R script associated with the appropriate elevation to run ODEs. The *manage.R script is called once
	for every quasi-equilibrated parameter set which passed realism criteria at a given elevation. 

These scripts output *breed.RData, *exclude.RData, and *trout.RData for each elevation. These RData objects should be
transferred to 07_Plotting for generation of final figures. 
	
	