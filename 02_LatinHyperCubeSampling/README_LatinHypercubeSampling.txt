# README for 02_LatinHyperCubeSampling

Input files: 
	- HighLHS.csv
	- ModerateLHS.csv
	- LowLHS.csv

Output files: 
	- LHS_high.RData
	- LHS_moderate.RData
	- LHS_low.RData

Scripts to execute: 
	- fastLHS.R

The fastLHS.R script generates a Latin Hypercube Sample across parameter space, 
resulting in N total parameters sets. (N is specified at the top of this script). 
This script calls a .csv input file which should contain the following columns:
	- 'elevation': the elevation of the model in which the parameter will be used
	- 'species': the species in which the parameter will be used
	- 'symbol': shorthand name of the parameter
	- 'dist': the probability distribution of the parameter. Choices are 'normal' or 'uniform'
	- 'mean': the mean (the estimated value) of the parameter (unnecessary if dist = uniform)
	- 'sd': the standard deviation of the parameter (unnecessary if dist = uniform)
	- 'lower': the lower bound of the parameter distribution (unnecessary for dist = normal)
	
The .csv input files for each elevation contain all best estimate parameter values for 
each respective elevation model. The output is given in .RData files which should 
subsequently be used in the model quasi-equilibration step (folder 03) 
