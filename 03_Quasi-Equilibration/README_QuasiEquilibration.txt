### README for 03_Quasi-Equilibration

Input files: (From 02_LatinHyperCubeSampling)
	- LHS_high.RData
	- LHS_moderate.RData
	- LHS_low.RData

Output files: 
	- High_quasi.RData
	- Moderate_quasi.RData
	- Low_quasi.RData

Scripts to execute: 
	- High_quasi_set.R
	- Moderate_quasi_set.R
	- Low_quasi_set.R

This folder runs all Latin Hypercube Sampled parameter sets to quasi-equilibrium, and then filters equilibrated 
systems based on realism criteria. Before this step, transfer all output files from 02_LationHyperCubeSampling 
into the working directory. This should comprise three files, LHS_high.RData, LHS_moderate.RData, and LHS_low.RData. 

To run the code, run scripts ending in *set.R, one for each elevation. These scripts call the *quasi.R scripts (containing 
ODE solvers) once for each LHS parameter set, and then evaluate the output against realism criteria. Each *set.R file should 
output an associated *quasi.RData with quasi-equilibrated states and realism filtering information for each parameter set.
These .RData files are used in the subsequent step 04_QuasiDiagnostics. 