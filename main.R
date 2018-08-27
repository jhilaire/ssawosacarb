##############################################################################################################
## This is the main script to run the entire analysis underlying the paper:
## "Lions in the dragon’s shoes? On carbonization patterns in Sub-Sahara Africa"
## 
## Author: Jerome Hilaire
## Institution: Mercator Research Institute on Global Commons and Climate Change (MCC), Berlin, Germany
## Email: hilaire@mcc-berlin.net
###############################################################################################################

#== USER SECTION =======================
source("scripts/user_section.R")

#== INITIALIZE =========================
source("scripts/init.R")

#== DATA ====================
source("scripts/process_IEA2017data.R")
source("scripts/read_n_transform_IEA2017.R")
source("scripts/get_data.R")

#== ANALYSIS ================
#-- Historical data ---------
# Figures 1 and 2
source("scripts/plot_kayadecomposition_africa_ssa.R")

#-- Projection data ---------
# Figures 3, S2 and S3
source("scripts/projection_sensitivity_analysis.R")

#-- Comparison to SSP data ---------
#Figure 4
source("scripts/compare_SSPs.R")

#-- Power plant capacities ------
# Figure S1
source("scripts/plot_powerplant_capacities.R")

#== CHECKS ==================
source("scripts/check_consistency_iea2017_platts2017_BB2018.R")