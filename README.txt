### README for MadelynMangan/si-bd-spenceri-lesueurii

# Author: Madelyn Mangan
# Date: 2024/09/27

This repository contains R code for the manuscript "Differential recruitment drives pathogen-mediated competition between species in an amphibian chytridiomycosis system". This pipeline uses susceptible-infected compartmental modelling to investigate pathogen-mediated competitive dynamics in a stage-structured, two-host amphibian system. The two hosts, in this specific case, are Litoria spenceri and Litoria lesueurii, while the pathogen is the chytrid fungus Batrachochytrium dendrobatidis, which causes the fungal disease amphibian chytridiomycosis. Models are run in three elevation scenarios: low, moderate, and high. 

This pipeline is comprised of seven stages, which are visually summarised in the file pipeline.png.

(1) 01_InstantaneousFecundities - For each elevation, runs SI models in the absence of Bd, carrying capacities, and seasonality in births. The bisection method is used to iteratively converge on a per capita instantaneous fecundity which replicates Bd-free finite population growth rates intrinsic to the population matrix models described by West et al. (2020).
(2) 02_LatinHyperCubeSampling - generates a Latin hypercube sample of 10,000 prospective parameter sets for each elevation model. 
(3) 03_Quasi-Equilibration - runs each parameter set past transient dynamics until reaching a quasi-equilibrated state, evaluating each system for realism. 
(4) 04_QuasiDiagnostics - Calculates diagnostics across quasi-equilibrated systems which passed realism criteria, and outputs figures to summarise these diagnostics.  
(5) 05_ManagementScenarios - Runs all realistic quasi-equilibrated systems through three 20-year management scenarios as different degrees of management effort. Interventions are (1) exclusion of postmetamorphic L. lesueurii, (2) invasive trout removal, and (3) captive breeding and release of L. spenceri larvae. 
(6) 06_RecruitmentContours - Using the median parameter and compartment values across all realistic quasi-equilibrated systems, run joint intervention of both captive breeding and trout removal in a 100x100 grid of varying management effort until each simulation reaches new quasi-equilibrium. This is later used to create a contour plot of 10,000 simulations. 
(&) 07_plotting - aggregate, summarise and plot final results of management interventions (05_ManagementScenarios) and joint-interventions (06_RecruitmentContours).  

Directories are numbered from 01-07, reflecting the order in which the pipeline should be run. Inside each directory, there is a README explaining scripts to execute, necessary input files, and resulting outputs. Outputs from one directory must be manually transferred to be used as inputs in a subsequent directory, as this was not coded automatically into the pipeline. Directions on which files to transfer are included in READMEs. (Note: for reference, there is an additional directory containing all figures generated in this pipeline.)

Some notes:
- Scripts require a substantial amount of computing power and runtime, such that use of a high-performance computing cluster (HPC) is advised - especially for 03_Quasi-Equilibration, 05_ManagementScenarios, and 06_RecruitmentContours. Code is NOT parallelized. 
- The scripts are currently set such that the working directory is automatically set to the directory of the active script, but this function only works in Rstudio. If running on an HPC or if reorganising script locations, this command needs to be changed. 
- Necessary packages are loaded at the top of each script using library(). Ensure all packages are installed before running each script. Packages used in the pipeline are as follows:
	-deSolve
	-dplyr
	-epiR
	-tidyr
	-ggplot2
	-ggpubr
	-metR
	-gridExtra
	-grid
	-dplyr
	-scales
	-lhs


### References
West, M., C. R. Todd, G. R. Gillespie, and M. McCarthy. 2020. Recruitment is key to understanding amphibian's different population-level responses to chytrid fungus infection. Biological Conservation 241.
