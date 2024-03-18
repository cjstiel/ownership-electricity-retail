#================================================================================================
#		  Article: Do Private Utilities Outperform Local Government-Owned Utilities? Evidence
# 					from German Retail Electricity 
#							
# published in: German Economic Review, 2018, 19(4), 401-425.
# authors: Caroline Stiel, Astrid CulLmann, Maria Nieswand
# affiliations: DIW Berlin, Technische Universitaet Berlin
#				
#================================================================================================
#													                 ´
#	
#					ROBUSTNESS CHECKS - DEMAND, TECHNOLOGY, AND SCALE
#
#
#	CONTENT: Performs robustness checks on production technology
#
#	OUTLINE: PART 1: Robustness check model I: German electricity demand
#			 PART 2: Robustness check model II: Technology
#			 PART 3: Robustness check model III: Scale
#					
# ----------------------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
# version: 18-Aug-2015 (v87)
# ----------------------------------------------------------------------------------------------					 
#
# input: data_sr.dta 
# output: -
#	        
#
#================================================================================================

#================================================================================================
#				0) Preparation                                              
#================================================================================================


date()


#================================================================================================
#  0.1 Packages	     	      
#================================================================================================


# load packages
# -------------
library(reshape)
library(foreign)
library(data.table)
library(gdata)
library(plm)
library(optimx)
library(boot)
library(car)
library(ggplot2)



#================================================================================================
# 0.2 Load data
#================================================================================================

# load data set
# -------------
data_sr_U <- read.dta13(file.path(Path1,"/data_sr.dta"))  

class(data_sr_U)
dim(data_sr_U)




#================================================================================================
# 							START
#================================================================================================


# recode data as panel data: i = utility's id, t = year
# -----------------------------------------------------
data_p <- pdata.frame(data_sr_U, index=c("unr","Jahr"),row.names=FALSE)
pdim(data_p)


#=================================================================================================
# 1) Robustness check model I: German electricity demand   
#=================================================================================================

# First stage of structural production function estimation.

#----------------------------
# Table A1: Robustness checks
# ---------------------------


first_stage_s1 <- lm(ln_firm_total_output_s ~
                      # production function inputs
                      ln_labour_s + ln_fremdeDL_s + I(0.5*ln_labour_s^2) + I(0.5*ln_fremdeDL_s^2) 
                    + I(ln_labour_s*ln_fremdeDL_s) + ShareTK + ShareWV
                    # + German electricity demand
                    + demand_original
                    # + proxy function h_t for productivity
                    + ln_labour_s*ln_wage*ln_fremdeDL_s
                    + I(ln_labour_s^2)*I(ln_fremdeDL_s^2)*I(ln_wage^2)
                    + ln_labour_s*I(ln_fremdeDL_s^2)*I(ln_wage^2)
                    + ln_labour_s*I(ln_fremdeDL_s^2)*ln_wage
                    + ln_labour_s*ln_fremdeDL_s*I(ln_wage^2)
                    + ln_fremdeDL_s*I(ln_labour_s^2)*I(ln_wage^2)
                    + ln_fremdeDL_s*I(ln_labour_s^2)*ln_wage
                    + ln_wage*I(ln_labour_s^2)*I(ln_fremdeDL_s^2)
                    ,data_p)

summary(first_stage_s1)




#=================================================================================================
# 2) Robustness check model II: Technology  
#=================================================================================================

# First stage of structural production function estimation.

#----------------------------
# Table A1: Robustness checks
# ---------------------------

first_stage_s2 <- lm(ln_firm_total_output_s ~
                       # production function inputs
                       ln_labour_s + ln_fremdeDL_s + I(0.5*ln_labour_s^2) + I(0.5*ln_fremdeDL_s^2) 
                     + I(ln_labour_s*ln_fremdeDL_s) + ShareTK + ShareWV
                     # interaction terms with ownership
                     + own + ln_labour_s:own + ln_fremdeDL_s:own + ShareTK:own + ShareWV:own
                     # + proxy function h_t for productivity
                     + ln_labour_s*ln_wage*ln_fremdeDL_s
                     + I(ln_labour_s^2)*I(ln_fremdeDL_s^2)*I(ln_wage^2)
                     + ln_labour_s*I(ln_fremdeDL_s^2)*I(ln_wage^2)
                     + ln_labour_s*I(ln_fremdeDL_s^2)*ln_wage
                     + ln_labour_s*ln_fremdeDL_s*I(ln_wage^2)
                     + ln_fremdeDL_s*I(ln_labour_s^2)*I(ln_wage^2)
                     + ln_fremdeDL_s*I(ln_labour_s^2)*ln_wage
                     + ln_wage*I(ln_labour_s^2)*I(ln_fremdeDL_s^2)
                     ,data_p)

summary(first_stage_s2)



#=================================================================================================
# 3) Robustness check model III: Scale 
#=================================================================================================

#=================================================================================================
# 3.1 Select subsample
#=================================================================================================

# How many observations with an output > 20 TWh?
# ----------------------------------------------
nrow(subset(data_sr_U, firm_total_output_s>=20*10^6))

# number of unique (large) firms
# ------------------------------
length(unique(subset(data_sr_U,firm_total_output_s>=20*10^6)$unr))
length(unique(data_sr_U$unr[data_sr_U$firm_total_output_s>=20*10^6]))

# drop large firms with more than 20 TWh electricity supply
# --------------------------------------------------------
data_sr_U <- subset(data_sr_U, firm_total_output_s<20*10^6)


# recode data as panel data: i = utility's id, t = year
# -----------------------------------------------------
data_p <- pdata.frame(data_sr_U, index=c("unr","Jahr"),row.names=FALSE)
pdim(data_p)




#=================================================================================================
# 3.2 Plain OLS model                                                
#=================================================================================================


# run plain OLS model to obtain starting values for GMM model
# -----------------------------------------------------------
starting_values <- lm(ln_firm_total_output_s ~ ln_labour_s + ln_fremdeDL_s + I(0.5*ln_labour_s^2) 
                      + I(0.5*ln_fremdeDL_s^2) + I(ln_labour_s*ln_fremdeDL_s) + ShareTK + ShareWV
                      ,data_p)

summary(starting_values)

# save coefficients to use them as starting values in GMM
# -------------------------------------------------------
betas1b <- as.vector(starting_values$coefficients[1:8])



#=================================================================================================
# 3.3 Structural production function estimation                                             
#=================================================================================================

#=================================================================================================
# 3.3.1 First-stage estimation (OLS)                         
#=================================================================================================


# Production function 
# -------------------
first_stage_s <- lm(ln_firm_total_output_s ~
                      # production function inputs
                      ln_labour_s + ln_fremdeDL_s + I(0.5*ln_labour_s^2) + I(0.5*ln_fremdeDL_s^2) 
                    + I(ln_labour_s*ln_fremdeDL_s) 
					# control for customer structure
					+ ShareTK + ShareWV
                    # proxy terms
                    + ln_labour_s*ln_wage*ln_fremdeDL_s
                    + I(ln_labour_s^2)*I(ln_fremdeDL_s^2)*I(ln_wage^2)
                    + ln_labour_s*I(ln_fremdeDL_s^2)*I(ln_wage^2)
                    + ln_labour_s*I(ln_fremdeDL_s^2)*ln_wage
                    + ln_labour_s*ln_fremdeDL_s*I(ln_wage^2)
                    + ln_fremdeDL_s*I(ln_labour_s^2)*I(ln_wage^2)
                    + ln_fremdeDL_s*I(ln_labour_s^2)*ln_wage
                    + ln_wage*I(ln_labour_s^2)*I(ln_fremdeDL_s^2)
                    ,data_p)

summary(first_stage_s)


# store coefficients from the first stage
# ---------------------------------------
betas1 <- as.vector(first_stage_s$coefficients[1:8])

# predict Phi
# ------------
data_p$Phi <- first_stage_s$fitted.values
length(data_p$Phi)
length(which(is.na(data_p$Phi)==FALSE))

# construct Lag-Phi
# -----------------
data_p$lag_Phi <- lag(data_p$Phi)
length(data_p$lag_Phi)
length(which(is.na(data_p$lag_Phi)==FALSE))

# store residuals for later use
# ------------------------------
data_p$exp_u_it <- exp(first_stage_s$residuals)




#=================================================================================================
# 3.3.2 Second-stage estimation: Preparing the lags
#=================================================================================================


# Combine all inputs in a matrix (full first-stage sample)
# --------------------------------------------------------
# Note: The order must be identical to that of the OLS estimation's coefficients. 
Inputs <- as.matrix(cbind(rep(1,length(data_p$ln_labour_s)),data_p$ln_labour_s,data_p$ln_fremdeDL_s
                          ,0.5*(data_p$ln_labour_s)^2,0.5*(data_p$ln_fremdeDL_s)^2
                          ,data_p$ln_labour_s*data_p$ln_fremdeDL_s,data_p$ShareTK,data_p$ShareWV))
dim(Inputs)


# Drop all observations with lag(phi) == missing
# ----------------------------------------------
data_gmm <- subset(data_p,is.na(lag_Phi)==FALSE)
pdim(data_gmm)



# Generate input matrix based on second-stage sample (at time t)
# ---------------------------------------------------------------
Inputs_gmm <- cbind(rep(1,length(data_gmm$ln_labour_s)),"ln_labour_s"=data_gmm$ln_labour_s
                    ,"ln_fremdeDL_s"=data_gmm$ln_fremdeDL_s
                    ,"ln_labour2_s"=0.5*data_gmm$ln_labour_s^2
                    ,"ln_fremdeDL2_s"=0.5*data_gmm$ln_fremdeDL_s^2
                    ,"ln_labour_s_ln_fremdeDL_s"=data_gmm$ln_labour_s*data_gmm$ln_fremdeDL_s
                    ,"ShareTK"=data_gmm$ShareTK,"ShareWV"=data_gmm$ShareWV) 




# Generate input matrix based on second-stage sample (at time t-1)
# -----------------------------------------------------------------
lag_Inputs <- as.matrix(cbind(rep(1,length(data_p$ln_labour_s)),lag(data_p$ln_labour_s)
                              ,lag(data_p$ln_fremdeDL_s),0.5*lag(data_p$ln_labour_s)^2
                              ,0.5*lag(data_p$ln_fremdeDL_s)^2
                              ,lag(data_p$ln_labour_s)*lag(data_p$ln_fremdeDL_s),lag(data_p$ShareTK)
                              ,lag(data_p$ShareWV)))
							  
lag_Inputs_gmm <- na.omit(lag_Inputs)
dim(lag_Inputs)



# Choose instruments for moment function
# --------------------------------------
instr <- cbind("const"=rep(1,length(data_p$ln_labour_s)),"ln_labour_s"=data_p$ln_labour_s
               ,"lag_fremdeDL_s"=lag(data_p$ln_fremdeDL_s),"ln_labour2_s"=data_p$ln_labour_s^2
               ,"lag_fremdeDL2_s"=lag(data_p$ln_fremdeDL_s)^2
               ,"ln_labour_s_ln_fremdeDL2_s"=data_p$ln_labour_s*lag(data_p$ln_fremdeDL_s)
               ,"ShareTK"=data_p$ShareTK,"ShareWV"=data_p$ShareWV)
instr_gmm <- na.omit(instr)

# lag ownership t-1
# -----------------
data_p$lag_own <- lag(data_p$own)

# number of observations
# ----------------------
n <- dim(data_gmm)[1]
n


 
#=================================================================================================
# 3.3.3  Second-stage estimation: GMM optimisation     
#=================================================================================================


# Choose starting values
# ----------------------
initial_betas <- betas1b
length(initial_betas)


# Run GMM optimisation routine
# ----------------------------
optimization1 <- optimx(par=initial_betas,fn=gmm_moment_condition, method=c("Nelder-Mead","BFGS"
                                                                            ,"nlminb","nlm"))
print(optimization1)

# Choose best optimisation routine (lowest value for objective function)
# ----------------------------------------------------------------------
j <- which.min(optimization1$value)


# Store coefficients from the second stage (betas)
# ------------------------------------------------
betas21 <- rbind(optimization1$p1[j],optimization1$p2[j],optimization1$p3[j],optimization1$p4[j]
                   ,optimization1$p5[j],optimization1$p6[j],optimization1$p7[j],optimization1$p8[j])




#=================================================================================================
# 3.3.4 Bootstrapping the SE              
#=================================================================================================

# Boostrap the SE
# ---------------
date()
clusterBootSE(data_sr_U,output.type="total",method="BFGS",B=2000)
date()



#=================================================================================================
date()
#========================================== End of file ==========================================