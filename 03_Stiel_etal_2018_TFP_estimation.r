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
#					TFP ESTIMATION
#
#
#	CONTENT: Estimates production function of electricity retailers
#
#	OUTLINE: PART 1: Plain OLS model
#			 PART 2: Structural production function estimation
# 					2.1 First-stage estimation (OLS)
#					2.2 Second stage estimation: preparing the lags
# 					2.3 Second-stage estimation: GMM optimisation
# 					2.4 Bootstrapping the SE
# 			 PART 3: Results
#					3.1 Productivity growth
#					3.2 Ownership and productivity
#
#					
# ----------------------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
# version: 08-Jun-2015 (v75)
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
# 1) Plain OLS model                                                
#=================================================================================================

# -----------------------------------------
# Table 6: Production function coefficients
# -----------------------------------------


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
# 2) Structural production function estimation                                             
#=================================================================================================

#=================================================================================================
# 2.1 First-stage estimation (OLS)                         
#=================================================================================================

# First stage OLS estimation in ACF (2005). Eliminates error u_it.

# Model production function as a translog function:

# Output: total electricity supplied
# Inputs: labour, external services

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
# 2.2  Second-stage estimation: Preparing the lags
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
# 2.3  Second-stage estimation: GMM optimisation     
#=================================================================================================


# The GMM's objective function is the moment condition E[(Z'v)'*(Z'v)]=0.

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
# 2.4 Bootstrapping the SE              
#=================================================================================================


# -----------------------------------------
# Table 6: Production function coefficients
# -----------------------------------------

# In this step, we bootstrap the SE for the coefficients from the second stage.
# We use the function 'ClusterBootSE' (see beginning of script) that draws the B bootstrap 
# subsamples from the full data set and the function 'boot.acf' that summarises the ACF algorithm.

# The function 'clusterBootSE' requires 4 inputs:
# (1) data: The data set from which the B bootstrap samples are to be drawn. It should be the
#     main data set without panel structure as available at the end of section (1).
# (2) method: Choose the numerical approach for the optimisation routine in the ACF algorithm.
#     It should be identical to the one used in section (2.3).
# (3) B: Number of replications
# (4) output.type: output measure 

# Boostrap the SE
# ---------------
date()
clusterBootSE(data_sr_U,output.type="total",method="BFGS",B=2000)
date()




#=================================================================================================
# 3) Results			                               
#=================================================================================================

#=================================================================================================
# 3.1 Productivity growth			                               
#=================================================================================================

# calculate TFP
# -------------
data_p$omega21 <- data_p$Phi - Inputs%*%betas21

# summary statistics
# -------------------
dstat(data_p$omega21,d=3)


# --------------------------------------
# Figure 3: Productivity growth, 2003-12
# --------------------------------------

pty_mean_median_growth <- cbind(c(seq(2003,2012))
,"Median"=c(dstat(data_p$omega21,d=3)[,4]-dstat(data_p$omega21,d=3)[1,4])
,"Mean"=c(dstat(data_p$omega21,d=3)[,5]-dstat(data_p$omega21,d=3)[1,5]))

# growth rates (2003=100)
# ----------------------
pdf("TFP_ts_growth.pdf")
par(pin=c(5,1.5))
plot(pty_mean_median_growth[,c(1,2)],type="l",bty="l",lty=1,lwd=2,ylab=NA,xlab=NA,ylim=c(-0.5,1.5))
lines(pty_mean_median_growth[,c(1,3)],type="l",lwd=2,lty=2,xlab=NULL,ylab=NULL)
legend("bottomright", c("median productivity","mean productivity"),lty=c(1,2),bty="n",lwd=2,cex=0.8)
loc <- par("usr")
text(loc[1], loc[4]+0.2, "productivity\ngrowth", pos=3, xpd = T,cex=1)
text(loc[2], loc[3], "Year", pos=4, xpd = T,cex=1)
text(loc[1]+0.9, loc[3]+0.15, "(2003=0)", pos=3, xpd = T,cex=0.8)
dev.off() 



#=================================================================================================
# 3.2 Ownership and productivity		                               
#=================================================================================================

# calculate lag-omega (omega_t-1) for the LOM
# -------------------------------------------
data_gmm$omega2 <- data_gmm$omega21
data_gmm$lag_omega2 <- data_gmm$lag_Phi - lag_Inputs_gmm%*%betas21
data_gmm <- pdata.frame(data.frame(data_gmm),index=c("unr","Jahr"),row.names=FALSE)


# -----------------------------------------
# Table 6: Production function coefficients
# -----------------------------------------

# Markov process (LOM)
# --------------------
AR1_expost <- plm(omega2 ~ lag_omega2 + I(lag_omega2^2) + I(lag_omega2^3) + lag_own,data=data_gmm
                   ,model="pooling",effect="time")
summary(AR1_expost)


# ----------------------------
# Section 7.3: Group mean test
# ----------------------------

# group mean test
# ---------------
boot.results.own <- boot.ownership(data_boot2=data_p,B=2000)
boot.results.own


# ----------------------------------------------
# Table 7: Governance structure and productivity
# ----------------------------------------------

# regress productivity on governance structure
# --------------------------------------------
explain_pty <- plm(omega21~ GmbH1 + AG0 + AG1 + Genossenschaft0 + GmbHCoKG0 + GmbHCoKG1 
                    + Eigenbetrieb1 + Einzelfirma0 + KG0,data=data_p
                    ,model="pooling",effect="time")
summary(explain_pty)



#=================================================================================================
date()
#========================================== End of file ==========================================