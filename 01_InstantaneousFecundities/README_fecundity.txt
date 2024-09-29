### README for 01_InstantaneousFecundities

Input files: None

Output files: None (results displayed in terminal)

Scripts to execute: 
	- fecundity_high.R
	- fecundity_moderate.R
	- fecundity_low.R

These three scripts runs SI models to find the instantaneous daily fecundity for Litoria lesueurii 
and Litoria spenceri in the absence of (1) Bd, (2) carrying capacities, and 
(3) seasonality in larval hatching. Using the bisection method with a tolerance 
of 0.00001 and arbitrary initial age structure, this script converges on the 
instantaneous daily fecundity that matches Bd-free discrete annual growth rates 
displayed in West et al. (2020). This is performed separately for all three elevation models.
The accompanying InstantaneousFecundities.csv file shows the results obtained from each script, 
which are included in the parameter .csv files necessary for 02_LatinHyperCubeSampling scripts. 

Notes:

1) These files can be run in isolation without connection to other data sources or files. 

2) Warnings appear because the growth rate of the log(population densities) 
is essentially a perfect fit in the linear model. This is expected and can be disregarded. 

3) Instantaneous daily fecundities are multiplied by the probability of breeding (0.5 in 
adults year one and 1 in adults year two) and the sex ratio (0.5) as a part of the bisection
function. The output of this script is the instantaneous fecundity, not the instantaneous daily
hatching rate. 


