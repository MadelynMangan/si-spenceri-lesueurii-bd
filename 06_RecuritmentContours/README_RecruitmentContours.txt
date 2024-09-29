### README for 06_RecruitmentContours

Input files: (From 04_QuasiDiagnostics)
	- High_manage3D.RData
	- Moderate_manage3D.RData
	- Low_manage3D.RData

Output files: 
	- High_3D_out.RData
	- Moderate_3D_out.RData
	- Low_3D_out.RData

Scripts to execute: 
	- r3D_set.R

The only script to be run is r3D_set.R. This uses the *manage3D.RData objects 
from 04_QuasiDiagnostics as input. It then cycles through each *3D_quasi.R script (one 
for each elevation) over 10,0000 simulations, moving through different levels of 
captive breeding and trout removal interventions (a 100-by-100 grid). The output 
file for each elevation is *3D_out.RData. These files should be moved to the 07_Plotting 
directory for generation of figures. 