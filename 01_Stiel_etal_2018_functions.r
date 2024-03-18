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
#					FUNCTIONS
#
#
#	CONTENT: User-written functions for the analysis
#
#	OUTLINE: function 'dstat'
#			 function 'gmm_moment_condition'
#			 function 'boot.acf'
#			 function 'clusterBootSE'
#			 function 'boot.ownership'
#					
# ----------------------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
#================================================================================================



#================================================================================================
# Define functions to be used in the analysis              	        
#================================================================================================


#================================================================================================
# Function 'dstat'                                                                                   
# ---------------                                                                                 
# Calculates descriptive statistics: Provides q1, q5 q25 q50, q75, q95, q99 quantiles, 
# ratio q75/q25, ratio 95/q5, mean, variance, standard deviation, sum, number of zeroes, 
# number of '.', NA's, max, second-hightes value.
#                                                                                                 
# Inputs: X - data frame with the variables for which descriptives statistics should be calculated
# Inputs: d - number of digits to which R shall round (default=0)                      
#================================================================================================

dstat <- function(X,d){
  X <- as.matrix(X)
  mat <- matrix(NA, ncol=17, nrow=ncol(X))
  colnames(mat) <- c("q1","q5","q25","med","mean","q75","q95","q99","var", "sd", "sum","nonNAs"
                     , "zeroes", "number '.'","NAs","max","secondhighest")
  rownames(mat) <- colnames(X)
  mat[,1] <- round(apply(X, 2, quantile, probs=0.01, na.rm=T),digits=d)
  mat[,2] <- round(apply(X, 2, quantile, probs=0.05, na.rm=T),digits=d)
  mat[,3] <- round(apply(X, 2, quantile, probs=0.25, na.rm=T),digits=d)
  mat[,4] <- round(apply(X, 2, median, na.rm=T),digits=d)
  mat[,5] <- round(apply(X, 2, mean, na.rm=T),digits=d)
  mat[,6] <- round(apply(X, 2, quantile, probs=0.75, na.rm=T),digits=d)
  mat[,7] <- round(apply(X, 2, quantile, probs=0.95, na.rm=T),digits=d)
  mat[,8] <- round(apply(X, 2, quantile, probs=0.99, na.rm=T),digits=d)
  mat[,9] <- round(apply(X, 2, var,na.rm=T),digits=d)
  mat[,10] <- round(apply(X, 2, sd, na.rm=T), digits=d)
  mat[,11] <- round(apply(X, 2, sum, na.rm=T), digits=d)
  mat[,12] <- round(apply(X, 2, nobs),digits=d)
  for (i in 1:ncol(X)) {mat[i,13] <- length(which(X[,i]==0))[1]}
  for (i in 1:ncol(X)) {mat[i,14] <- length(which(X[,i]=="."))[1]}
  for (i in 1:ncol(X)) {mat[i,15] <- length(which(is.na(X[,i])))[1]}
  mat[,16] <- round(apply(X, 2, max, na.rm=T), digits=d)
  for (i in 1:ncol(X)) {mat[i,17] <- round(sort(X[,i],decreasing=T)[2],digits=d)}
  return(mat)
}






#================================================================================================
# Function 'gmm_moment_condition'                                                                    
# ----------------------------- 
# This function, describing the optimisation routine, builds the core of the ACF algorithm. It
# minimises the objective function (the moment conditions), while the equation from the Markov process
# and those calculating TFP are constraints in the optimisation process. 
#
# In total, there are 5 constraints:
#                                                           
# (1) omega_it = Phi_it - Inputs_it * betas                                                        
# (2) omega_it-1 = Phi_it-1 - Inputs_it-1 * betas                                                  
# (3) omega_pol = 1 + omega_it-1 + (omega_it-1)^2 + (omega_it-1)^3 + controls            
# (4) g_p = inv(omega_pol'*omega_pol)*omega_pol'*omega                                             
# (5) innovation_it = omega_it - omega_pol*g_p                                                     
#                                                                                                  
# gmm_moment condition: The innovation in productivity (v_it), which forms the moment
# condition together with the betas, is calculated in constraint (5). After that, the 
# objective function of the optimisation process, the sample moment condition, is 
# calculated in 'moment_condition'. The output is a scalar that the optimisation
# routine seeks to minimise.
#================================================================================================                                                          


gmm_moment_condition <- function(betas){
  omega <<- data_gmm$Phi - Inputs_gmm%*%betas
  lag_omega <<- data_gmm$lag_Phi-lag_Inputs_gmm%*%betas
  omega_pol <<- cbind(rep(1,n),lag_omega,lag_omega^2,lag_omega^3,data_gmm$lag_own)
  AR1 <<- lm(omega ~ lag_omega+ I(lag_omega^2) + I(lag_omega^3) + data_gmm$lag_own)
  g_b <<- as.vector(AR1$coefficients)
  innovation <<- omega - omega_pol%*%g_b
  moment_condition <- t(t(instr_gmm)%*%innovation)%*%(t(instr_gmm)%*%innovation)
  return(as.vector(moment_condition))
}






#================================================================================================
# Function boot.acf										                                                             
# -----------------
# Part I of the bootstrapping procedure for estimating the standard errors of the production 
# function's parameters.										                                                                
# It contains the full ACF algorithm (first + second stage) and returns the coefficients of the
# second stage.    		   
#================================================================================================


boot.acf <- function(data,indices,output.type,method){
  data_boot <- data[indices,]
  data_p <- pdata.frame(data_boot, index=c("unr","Jahr"))
  first_stage_s <- lm(switch(output.type,
                             total = ln_firm_total_output_s,
                             retail = ln_firm_retail_output_s) ~
                        ln_labour_s + ln_fremdeDL_s + I(0.5*ln_labour_s^2) + I(0.5*ln_fremdeDL_s^2) 
                      + I(ln_labour_s*ln_fremdeDL_s) + ShareTK + ShareWV
                      + ln_labour_s*ln_wage*ln_fremdeDL_s
                      + I(ln_labour_s^2)*I(ln_fremdeDL_s^2)*I(ln_wage^2) 
                      + ln_labour_s*I(ln_fremdeDL_s^2)*I(ln_wage^2) 
                      + ln_labour_s*I(ln_fremdeDL_s^2)*ln_wage
                      + ln_labour_s*ln_fremdeDL_s*I(ln_wage^2)
                      + ln_fremdeDL_s*I(ln_labour_s^2)*I(ln_wage^2)
                      + ln_fremdeDL_s*I(ln_labour_s^2)*ln_wage
                      + ln_wage*I(ln_labour_s^2)*I(ln_fremdeDL_s^2),data_p)
  betas1 <- as.vector(first_stage_s$coefficients[1:8])
  data_p$Phi <- first_stage_s$fitted.values
  data_p$lag_Phi <- lag(data_p$Phi)
  lag_Inputs <- as.matrix(cbind(rep(1,length(data_p$ln_labour_s)),lag(data_p$ln_labour_s)
                                ,lag(data_p$ln_fremdeDL_s),0.5*lag(data_p$ln_labour_s)^2
                                ,0.5*lag(data_p$ln_fremdeDL_s)^2
                                ,lag(data_p$ln_labour_s)*lag(data_p$ln_fremdeDL_s)
                                ,lag(data_p$ShareTK),lag(data_p$ShareWV)))
  instr <- cbind("const"=rep(1,length(data_p$ln_labour_s)),"ln_labour_s"=data_p$ln_labour_s
                 ,"lag_fremdeDL_s"=lag(data_p$ln_fremdeDL_s),"ln_labour2_s"=data_p$ln_labour_s^2
                 ,"lag_fremdeDL2_s"=lag(data_p$ln_fremdeDL_s)^2
                 ,"ln_labour_s_ln_fremdeDL2_s"=data_p$ln_labour_s*lag(data_p$ln_fremdeDL_s)
                 ,"ShareTK"=data_p$ShareTK,"ShareWV"=data_p$ShareWV)
  instr_gmm <- na.omit(instr)
  data_p$lag_own <- lag(data_p$own)
  data_gmm <- subset(data_p,is.na(lag_Phi)==FALSE)
  Inputs_gmm <- cbind(rep(1,length(data_gmm$ln_labour_s)),"ln_labour_s"=data_gmm$ln_labour_s
                      ,"ln_fremdeDL_s"=data_gmm$ln_fremdeDL_s
                      ,"ln_labour2_s"=0.5*data_gmm$ln_labour_s^2
                      ,"ln_fremdeDL2_s"=0.5*data_gmm$ln_fremdeDL_s^2
                      ,"ln_labour_s_ln_fremdeDL_s"=data_gmm$ln_labour_s*data_gmm$ln_fremdeDL_s
                      ,"ShareTK"=data_gmm$ShareTK,"ShareWV"=data_gmm$ShareWV)
  lag_Inputs_gmm <- na.omit(lag_Inputs)
  n <- dim(data_gmm)[1]
  starting_values <- lm(switch(output.type,
                               total = ln_firm_total_output_s,
                               retail = ln_firm_retail_output_s)
                        ~ ln_labour_s + ln_fremdeDL_s + I(0.5*ln_labour_s^2) 
                        + I(0.5*ln_fremdeDL_s^2) + I(ln_labour_s*ln_fremdeDL_s) + ShareTK + ShareWV
                        , data_p)
  betas1b <- as.vector(starting_values$coefficients[1:8])
  initial_betas <- betas1b  
  optimization <- optimx(par=initial_betas,fn=gmm_moment_condition, method=method)
  betas21_boot <- rbind(optimization$p1[1],optimization$p2[1],optimization$p3[1],optimization$p4[1]
                        ,optimization$p5[1],optimization$p6[1],optimization$p7[1]
                        ,optimization$p8[1])
  return(betas21_boot)
}



#=================================================================================================
# Funktion clusterBootSE									                                                         
# ----------------------
# Part II of the bootstrapping procedure for estimating the standard errors of the production 
# function's parameters. 		
# The function draws the observations for the bootstrap sample. It applies clustering, i.e. the
# function does not draw single observations but all observations of a firm. 							                                                         
#
# source: https://diffuseprior.wordpress.com/2013/01/12/the-cluster-bootstrap		        
#================================================================================================


clusterBootSE<-function(data,output.type,method,B){
  # Define index variable
  clusters<-unique(data[,"unr"])
   # Generate empty matric for storing the ACF coefficients
  sterrs <- matrix(NA, nrow=B, ncol=8)
    # Start sampling
	for(i in 1:B){
        # Sample from firm IDs
		units<-sample(clusters,size=length(clusters),replace=T)
        # In the main sample, identify all observations t of the firm IDs that are part of the
		# current subsample b
		df.bs<-sapply(units,function(x)which(data[,"unr"]==x))
        # Draw these observations from the main sample and store them in a new data frame
		df.bs<-data[unlist(df.bs),]
		# Apply ACF algorithm to subsample b and store coefficients in row 'i'. The function uses
		# All observations from subsample b (1:dim(df.bs)[1]) to calculate the ACF coefficients 
		sterrs[i,]<-boot.acf(data=df.bs,output.type=output.type,method=method,1:dim(df.bs)[1])
      # Tells us about the progress of the job
	}
  # Table with the ACF coefficients from the main sample (first column), SE calculated from 
  # the variance of all subsamples 1...B (second column), and t-values (third column)
  val <- cbind(boot.acf(data=data,output.type=output.type,method=method,indices=1:dim(data_sr_U)[1])
               ,apply(sterrs,2,sd))
  colnames(val) <- c("Estimate","Std. Error")
  cat("\n")
  return(val)
}



#=================================================================================================
# Function boot.ownership                                                                          
# -----------------------                                                                      
# Bootstrap hypothesis testing following Efron/Tibshirani (1993), Ch. 16, p. 224.     
#                                                                                            
# H_0: E[mean(pty_private)]=E[mean(pty_public)]                                               
# H_1: E[mean(pty_private]>E[mean(pty_public)]                                                
#=================================================================================================

boot.ownership <- function(data_boot2,B){
  compare <- c(rep(NA,B))
  # create two subsamples: public and private
  data_private <- subset(data_boot2, own==0)
  data_public <- subset(data_boot2, own==1)
  n1 <- nrow(data_private)
  n2 <- nrow(data_public)
  # recode data set as data frame since panel structure irrelevant
  data_private <- data.frame(data_private)
  data_public <- data.frame(data_public)
  # calculate mean productivity within both groups and for whole sample
  mean_pty_private <- mean(data_private$omega21,na.rm=TRUE); mean_pty_private
  mean_pty_public <- mean(data_public$omega21, na.rm=TRUE); mean_pty_public
  mean_pty_all <- dstat(data.frame(data_boot2$omega21),d=3)[5];mean_pty_all
  # transform data to construct test statistics under null hypothesis (identical mean) 
  data_private$omega_new <- data_private$omega21 - mean_pty_private + mean_pty_all
  data_public$omega_new <- data_public$omega21 - mean_pty_public + mean_pty_all
  # test statistics according to equation (16.5) p. 223.
  teststatistik <- (mean_pty_private-mean_pty_public)/sqrt(var(data_private$omega21)/n1
                                                           +var(data_public$omega21)/n2)
  # bootstrap
  for(i in 1:B){
    boot.data_private <- data_private[sample(nrow(data_private),nrow(data_private),replace=TRUE),]
    boot.data_public <- data_public[sample(nrow(data_public),n2,replace=TRUE),]
    boot.mean_pty_private <- mean(boot.data_private$omega_new, na.rm=TRUE)
    boot.mean_pty_public <- mean(boot.data_public$omega_new, na.rm=TRUE)
    boot.teststatistik <- (boot.mean_pty_private-boot.mean_pty_public)/sqrt(
      var(boot.data_private$omega_new)/n1+var(boot.data_public$omega_new)/n2)
    compare[i] <- ifelse(boot.teststatistik>teststatistik,1,0)
  }
  count <- sum(compare)/B
  return(data.frame(cbind("p-Wert"=count,"Mean.Public"=mean_pty_public
                          ,"Mean.Private"=mean_pty_private,"Mean.all"=mean_pty_all)))
}



#=================================================================================================
date()
#========================================== End of file ==========================================