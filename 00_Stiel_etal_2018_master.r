#================================================================================================
#		  Article: Do Private Utilities Outperform Local Government-Owned Utilities? Evidence
# 					from German Retail Electricity 
#							
# published in: German Economic Review, 2018, 19(4), 401-425.
# authors: Caroline Stiel, Astrid CulLmann, Maria Nieswand
# affiliations: DIW Berlin, Technische Universitaet Berlin
#				
#================================================================================================
#
#
#  						MASTER FILE
#
#
# 					calls the different programs
#
#================================================================================================


#================================================================================================
# 1) Set up R-environment
#================================================================================================


# clean memory 
#--------------
rm(list = ls())



# Use fixed notation instead of exponential notation
#---------------------------------------------------
options(scipen = 999)



#================================================================================================
# 2) Define working environment
#================================================================================================


# Variable 'RDC' defines the working environment
#-----------------------------------------------
# 0: own computer
# 1: remote access to original data set at research data centre (RDC)
 
RDC <- 1  

# Working environment 0: own computer
# -----------------------------------
if (RDC == 0)  {
	# Path1: directory where working data is saved
	Path1   <- "" 
	# Path2: directory where original data is stored
	Path2   <- "" 
	# path3: directory where all the programs are saved
	Path3   <- ""
	# Path 4: directory where output and logfiles are saved
	Path4   <- ""
	.libPaths("")


# Working environment 1: remote access to original data set at RDC
# ----------------------------------------------------------------
if (RDC == 1)  {
	Path1   <- "" 
	Path2   <- "" 
	Path3   <- "" 
	Path4	<- ""
	.libPaths("")

	
#================================================================================================
# 3) Run programs
#================================================================================================

# start the log file
#---------------------
sink(paste(Path4, "/Electricity_retail_ownership.log", sep = ""), append = FALSE, type = c("output", "message"), split = TRUE)

# Step 1: source functions
#----------------------------
source(paste(Path3, "/01_Stiel_etal_2018_functions.r", sep = ""), echo = TRUE, max.deparse.length = 99999)


# Step 2: sample construction and summary statistics
#---------------------------------------------------
source(paste(Path3, "/02_Stiel_etal_2018_sample_construction_descriptives.r", sep = ""), echo = TRUE, max.deparse.length = 99999)


# Step 3: TFP estimation
#-------------------------
source(paste(Path3, "/03_Stiel_etal_2018_TFP_estimation.r", sep = ""), echo = TRUE, max.deparse.length = 99999)


# Step 4: Robustness checks
#---------------------------
source(paste(Path3, "/04_Stiel_etal_2018_robustness_checks.r", sep = ""), echo = TRUE, max.deparse.length = 99999)


# close the log file
#-------------------
sink()
  

#================================================================================================
# date ()
#===============================End of file======================================================