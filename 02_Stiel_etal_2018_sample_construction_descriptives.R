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
#			SAMPLE CONSTRUCTION AND DESCRIPTIVE STATISTICS
#
#
#	CONTENT: Prepares the retailer data set for TFP estimation
#
#	OUTLINE: PART 1: Select sample of retailers
#			 PART 2: Ownership variable
# 			 PART 3: Aggregate plant-level data to firm level
# 			 PART 4: Drop missing values
#			 PART 5: Input and output variables
#					5.1 labour
# 					5.2 wages
#					5.3 external services
# 					5.4 output
#			 PART 6: Covariates
#			 PART 7: Regulator's data (BNetzA data)
#					
# ----------------------------------------------------------------------------------------------
# code author: Caroline Stiel (DIW Berlin)
# version: 08-Jun-2015 (v75)
# ----------------------------------------------------------------------------------------------					 
#
# input: <original data set name>.dta 
# output: data_sr.dta
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
data <- read.dta13(file.path(Path2,"/<original data set name>.dta"))  

class(data)
dim(data)

# convert format
# --------------
if(class(data$unr) != "numeric") data$unr <- as.numeric(data$unr)



#================================================================================================
# 							START
#================================================================================================

#================================================================================================
# 1) Select sample of retailers
#================================================================================================


# exclude firms with missings in balance sheet data, firm ID, year, ownership status
# ----------------------------------------------------------------------------------
data1 <- subset(data, TMEVU_u=="ja" & is.na(data$Jahr)==FALSE & is.na(data$unr)==FALSE 
                & is.na(data$jab)==FALSE)
				
				
# reduce sample to electricity retailers and electricity and gas retailers
# ------------------------------------------------------------------------
# Pure retailers, i.e., no generation or distribution activities.

data_sr <- subset(data1, UI_Code11_7==0 & UI_Code11_6==0 &UI_Code11_5==0 
                               &UI_Code11_4==0 &UI_Code11_2==0 & U_ABS_EF1061>0 
                               & (TM066N_u=="nein" & TM070_u=="nein") & TM066K_b=="nein" 
                               & TM064_b=="nein"
                               & (UI_Code3201==0 | is.na(UI_Code3201)==TRUE) 
                               & (UI_Code3001==0 | is.na(UI_Code3001)==TRUE)
                               & (UK_Code8701==0 | is.na(UK_Code8701)==TRUE)
                               & (TM065_b=="ja" 
                                  | (art_u=="Einbetriebsunternehmen"
                                     & rowSums(cbind(data1$UI_Code11_1,data1$UI_Code11_2,data1$UI_Code11_3
                                                     ,data1$UI_Code11_4,data1$UI_Code11_5,data1$UI_Code11_6
                                                     ,data1$UI_Code11_7))==1)))



#================================================================================================
# 2) Generate ownership variable
#================================================================================================


# define public ownership
# -----------------------
# criterion 1: match with official statistics on public firms
# criterion 2: legal form exclusive to public ownership
data_sr$own <- ifelse((data_sr$jab==1 | (is.na(data_sr$Rechtsform)==FALSE 
                       & data_sr$Rechtsform=="Eigenbetrieb")),1,0)



#================================================================================================
# 3) Aggregate plant-level data to firm level
#================================================================================================


# recode legal form as numeric variable to facilitate aggregation
# ----------------------------------------------------------------
data_sr$Rechtsform_Zahl <- ifelse(data_sr$Rechtsform=="keine Angabe",0
                                  ,ifelse(data_sr$Rechtsform=="Einzelfirma",1
                                          ,ifelse(data_sr$Rechtsform=="ohg",2
                                                  ,ifelse(data_sr$Rechtsform=="kg",3
                                                          ,ifelse(data_sr$Rechtsform=="GmbH & Co KG",4
                                                                  ,ifelse(data_sr$Rechtsform=="GmbH",5
                                                                          ,ifelse(data_sr$Rechtsform=="AG bzw. KGaA",6
                                                                                  ,ifelse(data_sr$Rechtsform=="Genossenschaft",7
                                                                                          ,ifelse(data_sr$Rechtsform=="Eigenbetrieb",8
                                                                                                  ,ifelse(data_sr$Rechtsform=="Verband",9
                                                                                                          ,ifelse(data_sr$Rechtsform=="sonstiges",10,NA)))))))))))





# aggregate observations from plant to firm level (sum aggregation)
# ------------------------------------------------------------------
sum_aggregation <- aggregate(cbind("elec_workers_MBE"=data_sr$B_MBE_EF11_mean
                                   , "gas_workers_MBE"=data_sr$B_MBE_EF13_mean)
                             ,by=list("unr"=data_sr$unr,"Jahr"=data_sr$Jahr), sum, na.rm=TRUE)

# aggregate observations from plant to firm level (mean aggregation)
# ------------------------------------------------------------------
mean_aggregation <- aggregate(cbind("UI_Code11_1"=data_sr$UI_Code11_1
                                    ,"UI_Code11_3"=data_sr$UI_Code11_3
                                    ,"UK_Code1501"=data_sr$UK_Code1501
                                    ,"UK_Code1601"=data_sr$UK_Code1601
                                    ,"UK_Code2001"=data_sr$UK_Code2001
                                    ,"UK_Code2501"=data_sr$UK_Code2001
                                    ,"UK_Code4501"=data_sr$UK_Code4501
                                    ,"UK_Code5001"=data_sr$UK_Code5001
                                    ,"UK_Code5201"=data_sr$UK_Code5201
                                    ,"UK_Code5501"=data_sr$UK_Code5501
                                    ,"UK_Code6301"=data_sr$UK_Code6301
                                    ,"UK_Code7301"=data_sr$UK_Code7301
                                    ,"U_ABS_EF1061"=data_sr$U_ABS_EF1061
                                    ,"U_ABS_EF1062"=data_sr$U_ABS_EF1062*1000
                                    ,"U_ABS_EF1011_sum"=data_sr$U_ABS_EF1011_sum
                                    ,"U_ABS_EF1012_sum"=data_sr$U_ABS_EF1012_sum*1000
                                    ,"U_ABS_EF1041_sum"=data_sr$U_ABS_EF1041_sum
                                    ,"U_ABS_EF1051_sum"=data_sr$U_ABS_EF1051_sum
                                    ,"U_ABS_EF1071"=data_sr$U_ABS_EF1071
                                    ,"U_ABS_EF1072"=data_sr$U_ABS_EF1072*1000
                                    ,"U_ABS_EF1081"=data_sr$U_ABS_EF1081
                                    ,"U_ABS_EF1082"=data_sr$U_ABS_EF1082*1000
                                    ,"U_ABS_EF1091"=data_sr$U_ABS_EF1091
                                    ,"U_ABS_EF1092"=data_sr$U_ABS_EF1092*1000
                                    ,"own"=data_sr$own
                                    ,"Rechtsform"=data_sr$Rechtsform_Zahl)
                              ,by=list("unr"=data_sr$unr,"Jahr"=data_sr$Jahr), mean, na.rm=TRUE)



# recode legal form to string variable
# ------------------------------------
# if the value is non-integer (inconsistent reporting at plant level) set to NA
mean_aggregation$Rechtsform <- ifelse(mean_aggregation$Rechtsform==0,"keine Angabe"
                                 ,ifelse(mean_aggregation$Rechtsform==1,"Einzelfirma"
                                         ,ifelse(mean_aggregation$Rechtsform==2,"ohg"
                                                 ,ifelse(mean_aggregation$Rechtsform==3,"kg"
                                                         ,ifelse(mean_aggregation$Rechtsform==4,"GmbH & Co KG"
                                                                 ,ifelse(mean_aggregation$Rechtsform==5,"GmbH"
                                                                         ,ifelse(mean_aggregation$Rechtsform==6,"AG bzw. KGaA"
                                                                                 ,ifelse(mean_aggregation$Rechtsform==7,"Genossenschaft"
                                                                                         ,ifelse(mean_aggregation$Rechtsform==8,"Eigenbetrieb"
                                                                                                 ,ifelse(mean_aggregation$Rechtsform==9,"Verband"
                                                                                                         ,ifelse(mean_aggregation$Rechtsform==10,"sonstiges"
                                                                                                                 ,ifelse(is.na(mean_aggregation$Rechtsform),"NA","Datenfehler"))))))))))))


# merge both aggregated datasets
# -------------------------------
data_sr_U_aux <- merge(mean_aggregation,sum_aggregation)

# number of observations
# -----------------------
addmargins(table(data_sr_U_aux$Jahr))


# define utility as public if at least one of its plants fulfills public criteria
# -------------------------------------------------------------------------------
# reason: private firms cannot own public plants, parent utility must be public
data_sr_U$own <- ifelse(data_sr_U$own>0,1,0)



#================================================================================================
# 4) Drop missing values
#================================================================================================

# drop utilities with missing values in inputs and output
# -------------------------------------------------------
data_sr_U <- subset(data_sr_U_aux, UK_Code1501>0 & UK_Code5001>0 
                    & UK_Code5501>0
                    & is.na(data_sr_U_aux$U_ABS_EF1051_sum)==FALSE
                    & is.na(data_sr_U_aux$UK_Code1601)==FALSE
                    & U_ABS_EF1061>0)


# -------------------------------
# Table 2: number of observations
# -------------------------------
addmargins(table(data_sr_U$own,data_sr_U$Jahr,useNA="ifany"))


#================================================================================================
# 5) Generate input and output variables
#================================================================================================

#================================================================================================
# 5.1 labour
#================================================================================================

# distribution of inputs between electricity and gas sector
# ---------------------------------------------------------
# calculates ratio between electricity and gas workers for firms active in both sectors
# sets ratio to 1 for single product firms
data_sr_U$ratio_sg <- ifelse(data_sr_U$UI_Code11_1==1&data_sr_U$UI_Code11_3==1
                             &data_sr_U$gas_workers_MBE>0&data_sr_U$elec_workers_MBE>0
                             & is.na(data_sr_U$gas_workers_MBE)==FALSE 
                             & is.na(data_sr_U$elec_workers_MBE)==FALSE
                             ,(data_sr_U$elec_workers_MBE/(data_sr_U$gas_workers_MBE+data_sr_U$elec_workers_MBE)),1)
dstat(data_sr_U$ratio_sg,d=2)

# how many firms are multiproduct firms?
# --------------------------------------
length(data_sr_U$ratio_sg[data_sr_U$ratio_sg!=1])


# number of employees
# -------------------
# number of employees in electricity sector
data_sr_U$labour_s <- (data_sr_U$ratio_sg)*(data_sr_U$UK_Code1501)
# median-correction
data_sr_U$labour_s_mb <- (data_sr_U$labour_s)/median(data_sr_U$labour_s,na.rm=TRUE)
# log
data_sr_U$ln_labour_s <- log(data_sr_U$labour_s_mb)


# -----------------------------------------
# Table 3: Summary statistics
# -----------------------------------------
dstat(data_sr_U$labour_s[data_sr_U$own==0],d=0)
dstat(data_sr_U$labour_s[data_sr_U$own==1],d=0)


#================================================================================================
# 5.2 wages (input prices)
#================================================================================================


# labour costs per firm
# ---------------------
data_sr_U$bruttolohn1 <- data_sr_U$UK_Code5001 + data_sr_U$UK_Code5201
# deflation
data_sr_U$bruttolohn <- data_sr_U$bruttolohn1/data_sr_U$lc_deflation_index


# -----------------------------------------
# Table 3: Summary statistics
# -----------------------------------------
dstat(data_sr_U$bruttolohn[data_sr_U$own==0],d=2)
dstat(data_sr_U$bruttolohn[data_sr_U$own==1],d=2)


# hourly wage
# -----------
data_sr_U$hourly_wage1 <- ifelse((data_sr_U$bruttolohn>0&data_sr_U$UK_Code1601>0)
                                 ,data_sr_U$bruttolohn/data_sr_U$UK_Code1601,NA)
								 
# set to average wage if firm-specific wage is unavailable for a firm
data_sr_U$hourly_wage <- ifelse(is.na(data_sr_U$hourly_wage1)==TRUE,mean(data_sr_U$hourly_wage1
                                                                         ,na.rm=TRUE),
                                data_sr_U$hourly_wage1)
# median correction
data_sr_U$hourly_wage_mb <- data_sr_U$hourly_wage/median(data_sr_U$hourly_wage,na.rm=TRUE)
# log
data_sr_U$ln_wage <- log(data_sr_U$hourly_wage_mb)



# -----------------------------------------
# Table 3: Summary statistics
# -----------------------------------------
dstat(data_sr_U$hourly_wage[data_sr_U$own==0],d=2)
dstat(data_sr_U$hourly_wage[data_sr_U$own==1],d=2)


#================================================================================================
# 5.3 external services
#================================================================================================

# measure the amount of external services by expenditure for external services
# -----------------------------------------------------------------------------
data_sr_U$fremdeDL1 <- data_sr_U$UK_Code5501
# deflation
data_sr_U$fremdeDL <- data_sr_U$fremdeDL1/data_sr_U$fdl_deflation_index

# distribute external services between electricity and gas for multiproduct firms
# -------------------------------------------------------------------------------
# assume that the distribution od external services is proportional to the distribution of employees
data_sr_U$fremdeDL_s <- data_sr_U$ratio_sg * data_sr_U$fremdeDL
# median correction
data_sr_U$fremdeDL_s_mb <- data_sr_U$fremdeDL_s/median(data_sr_U$fremdeDL_s,na.rm=TRUE)
# log
data_sr_U$ln_fremdeDL_s <- log(data_sr_U$fremdeDL_s_mb)


# -----------------------------------------
# Table 3: Summary statistics
# -----------------------------------------
dstat(data_sr_U$fremdeDL_s[data_sr_U$own==0],d=2)
dstat(data_sr_U$fremdeDL_s[data_sr_U$own==1],d=2)


#================================================================================================
# 5.4 output
#================================================================================================

# electricity supplied to end-consumers [MWh]
# -------------------------------------------
data_sr_U$firm_retail_output_s <- data_sr_U$U_ABS_EF1061
# median correction
data_sr_U$firm_retail_output_s_mb <- data_sr_U$firm_retail_output_s/median(data_sr_U$firm_retail_output_s,na.rm=TRUE)
# log
data_sr_U$ln_firm_retail_output_s<- log(data_sr_U$firm_retail_output_s_mb)


# total electricity supplied [MWh]
# --------------------------------
# reason: inputs are used to produce full output incl. wholesale
data_sr_U$firm_total_output_s <- data_sr_U$U_ABS_EF1061+ifelse(is.na(data_sr_U$U_ABS_EF1011_sum)==TRUE
                                                               ,0,data_sr_U$U_ABS_EF1011_sum)
# median-correction
data_sr_U$firm_total_output_s_mb <- data_sr_U$firm_total_output_s/median(data_sr_U$firm_total_output_s,na.rm=TRUE)
# log
data_sr_U$ln_firm_total_output_s<- log(data_sr_U$firm_total_output_s_mb)

# ------------------------------------------------------
# Table 4: Summary statistics for total electricity sold
# ------------------------------------------------------
dstat(data_sr_U$firm_total_output_s[data_sr_U$own==0],d=2)
dstat(data_sr_U$firm_total_output_s[data_sr_U$own==1],d=2)



#================================================================================================
# 6) Generate covariates
#================================================================================================

# customer structure: share supply to residential customers in total end-consumer supply
# --------------------------------------------------------------------------------------
data_sr_U$ShareTK<-ifelse(data_sr_U$U_ABS_EF1051_sum>0
                          ,data_sr_U$U_ABS_EF1051_sum/data_sr_U$U_ABS_EF1061,0)
						  



# customer structure: share supply to other retailers in total supply
# -------------------------------------------------------------------
data_sr_U$ShareWV<-ifelse(is.na(data_sr_U$U_ABS_EF1011_sum)==FALSE
                           ,data_sr_U$U_ABS_EF1011_sum/data_sr_U$firm_total_output_s,0)
dstat(data_sr_U$ShareWV,d=2)



# ------------------------------------------------------
# Table 5: Summary statistics for customer structure
# ------------------------------------------------------
dstat(data_sr_U$ShareTK[data_sr_U$own==0],d=2)
dstat(data_sr_U$ShareTK[data_sr_U$own==1],d=2)

dstat(data_sr_U$ShareWV[data_sr_U$own==0],d=2)
dstat(data_sr_U$ShareWV[data_sr_U$own==1],d=2)



# legal form fixed effects
# ------------------------
data_sr_U$AG1 <- ifelse(data_sr_U$Rechtsform=="AG bzw. KGaA" & data_sr_U$own==1,1,0)
data_sr_U$AG0 <- ifelse(data_sr_U$Rechtsform=="AG bzw. KGaA" & data_sr_U$own==0,1,0)
data_sr_U$GmbH0 <- ifelse(data_sr_U$Rechtsform=="GmbH" & data_sr_U$own==0,1,0)
data_sr_U$GmbH1 <- ifelse(data_sr_U$Rechtsform=="GmbH" & data_sr_U$own==1,1,0)
data_sr_U$Eigenbetrieb1 <- ifelse( data_sr_U$Rechtsform=="Eigenbetrieb" & data_sr_U$own==1,1,0)
data_sr_U$GmbHCoKG0 <- ifelse(data_sr_U$Rechtsform=="GmbH & Co KG" & data_sr_U$own==0,1,0)
data_sr_U$GmbHCoKG1 <- ifelse(data_sr_U$Rechtsform=="GmbH & Co KG" & data_sr_U$own==1,1,0)
data_sr_U$Genossenschaft0 <- ifelse(data_sr_U$Rechtsform=="Genossenschaft" & data_sr_U$own==0,1,0)
data_sr_U$Einzelfirma0 <- ifelse(data_sr_U$Rechtsform=="Einzelfirma" & data_sr_U$own==0,1,0)
data_sr_U$KG0 <- ifelse(data_sr_U$Rechtsform=="kg" & data_sr_U$own==0,1,0)




#================================================================================================
# 7) Regulator's data (BNetzA data)
#================================================================================================

# -----------------------------------------------
# Figure 1: Consumer switching rates (%), 2006-14
# ------------------------------------------------

# source: BNetzA (2006-2014) - Monitoringberichte 2006-2014.

# consumer switching rates in %
# -----------------------------
switching_rates1 <- cbind(c(seq(2005,2013)),"less10MWh"=c(2.2,2.6,4.2,5.5,5.7,6.8,9.2,7.8,7.8)
                          ,"more10MWh"=c(7.6,12.5,12,10.4,11.1,10.4,12,11.3,12.1))

pdf("Consumer_switching_rates.pdf")
par(pin=c(5,2),xpd=TRUE)
plot(switching_rates1[,c(1,2)],type="l",pch=16,lty=1,lwd=2,ylab=NA,xlab=NA,ylim=c(0,17),cex.axis=0.9
     ,cex.lab=0.9,bty="l")
lines(switching_rates1[,c(1,3)],type="l",pch=16,lty=2,lwd=2,xlab=NULL,ylab=NULL)
par(pin=c(5,1.5),xpd=TRUE)
legend("bottomright",c("consumers with less than 10 MWh/a","consumers with more than 10 MWh/a")
       ,lty=c(1,2),lwd=2,cex=0.8,bty="n")
title(sub = "Source: Bundesnetzagentur (BNetzA): Monitoringberichte 2006-2014", cex.sub = 0.8
      , adj =1, font.sub=3)
loc <- par("usr")
text(loc[1], loc[4]+0.2, "switching rates\n%", pos=3, xpd = T,cex=1)
text(loc[2], loc[3], "Year", pos=4, xpd = T,cex=1)
dev.off()


# ------------------------------------------------------------
# Figure 2: Competition intensity across supply areas, 2007-13
# ------------------------------------------------------------

# source: BNetzA (2014) - Monitoringbericht 2014.

netzgebiete <- cbind("Jahr"=seq(2007,2013),"monopolist"=c(1.9,0.6,1.3,1.5,1.3,1.2,0.5)
                 ,"2-50 competitors"=c(75,65.8,48.4,35.9,25.1,22.4,19.7)
                 ,">50 competitors"=c(23.1,33.6,50.4,62.6,73.6,76.4,79.7))

pdf("competition_intenstity.pdf")
par(pin=c(5,3),xpd=TRUE)
barplot(netzgebiete[,2:4],bty='l',beside=TRUE,ylim=c(0,100),ylab="Share of Supply Areas (%)"
        ,density=c(10,50,100,10,50,100,10)
#,col=c("darkblue","red","black")
)
legend(-1,-15,seq(2007,2013),density=c(8,40,65,8,40,65,8),horiz=TRUE,bty="n")
title(sub = "Source: Bundesnetzagentur (BNetzA): Monitoringbericht 2014", cex.sub = 0.75, adj =1
      , font.sub=3)
dev.off()


#================================================================================================
# Clean and save
#================================================================================================

# save data set
# -------------
write.dta(data_sr_U,paste(Path1,"data_sr.dta",sep=""),version=10)


#================================================================================================
date()
#=====================================End of file================================================
