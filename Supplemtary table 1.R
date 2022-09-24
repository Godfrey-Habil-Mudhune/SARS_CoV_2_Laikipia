#ARNOLD LAMBISIA
##Cc 2022


#This is a scri[t used for calculating the odds ratios forSupplementary table 1
  ##A 2 by 2 table was created after populating the numbers by tabyl from the data and the fisher.test function was used to caluctae the odds ratio. An option would be to use 
      #the oddsratio function from epitools and specify the method as fisher (example in line 29)
#load libraries
library(epitools)
library(tidyverse)
library(rio)

##import data
LKP_dta <- import("Laikipia_Metadata_05082022_Mudhune.xlsx") %>% 
  mutate(date_collected = as.Date(date_collected, "%d/%m/%Y"))

#####OR_supplementary table 1
#calculate odds ratio per VOC relative to other VOCs
#### For the Alpha VOC relative to Delta + Omicron

#Cough
tab_cough <- tabyl(LKP_dta, VOC_lineage, cough_7) #can be used to tabulate the counts per symptom per VOC
tab_cough


OR_Alpha_cough_table<-matrix(c(2,6,72,17),nrow = 2, ncol = 2)
OR_Alpha_cough_table  
fisher.test(OR_Alpha_cough_table) #calculate odds ratio
oddsratio(OR_Alpha_cough_table, method = "fisher") ###This can also be used instead for all the other symptoms

#chest pain
OR_Alpha_cp_table<-matrix(c(0,8,17,72),nrow = 2, ncol = 2)
OR_Alpha_cp_table  
fisher.test(OR_Alpha_cp_table) #calculate odds ratio
    ##No cases for alpha - OR 0

#shortness of breath
OR_Alpha_sob_table<-matrix(c(1,7,20,69),nrow = 2, ncol = 2)
OR_Alpha_sob_table  
fisher.test(OR_Alpha_sob_table) #calculate odds ratio

#fever
OR_Alpha_fever_table<-matrix(c(1,7,18,71),nrow = 2, ncol = 2)
OR_Alpha_fever_table  
fisher.test(OR_Alpha_fever_table) #calculate odds ratio

#irritation
OR_Alpha_irritation_table<-matrix(c(1,7,1,88),nrow = 2, ncol = 2)
OR_Alpha_irritation_table  
fisher.test(OR_Alpha_irritation_table) #calculate odds ratio



######## For the Delta VOC relative to Alpha + Omicron
#cough
OR_Delta_cough_table<-matrix(c(39,13,35,10),nrow = 2, ncol = 2)
OR_Delta_cough_table  
fisher.test(OR_Delta_cough_table) #calculate odds ratio

#chest pain
OR_Delta_cp_table<-matrix(c(17,35,0,45),nrow = 2, ncol = 2)
OR_Delta_cp_table  
fisher.test(OR_Delta_cp_table) #calculate odds ratio

#sore throat
OR_Delta_STR_table<-matrix(c(7,45,28,17),nrow = 2, ncol = 2)
OR_Delta_STR_table  
fisher.test(OR_Delta_STR_table) #calculate odds ratio

#shortness of breath
OR_Delta_sob_table<-matrix(c(20,32,1,44),nrow = 2, ncol = 2)
OR_Delta_sob_table  
fisher.test(OR_Delta_sob_table) #calculate odds ratio

#muscular pain
OR_Delta_MP_table<-matrix(c(12,40,0,45),nrow = 2, ncol = 2)
OR_Delta_MP_table  
fisher.test(OR_Delta_MP_table) #calculate odds ratio 
    ####No cases in other VOCs, hence estimated as infinity
#joint pain
OR_Delta_JP_table<-matrix(c(2,50,2,43),nrow = 2, ncol = 2)
OR_Delta_JP_table  
fisher.test(OR_Delta_JP_table) #calculate odds ratio

#General weakness
OR_Delta_GW_table<-matrix(c(17,35,1,44),nrow = 2, ncol = 2)
OR_Delta_GW_table  
fisher.test(OR_Delta_GW_table) #calculate odds ratio

#fever
OR_Delta_fever_table<-matrix(c(16,36,3,42),nrow = 2, ncol = 2)
OR_Delta_fever_table  
fisher.test(OR_Delta_fever_table) #calculate odds ratio

#headache
OR_Delta_headache_table<-matrix(c(13,39,11,34),nrow = 2, ncol = 2)
OR_Delta_headache_table  
fisher.test(OR_Delta_headache_table) #calculate odds ratio

#irritation
OR_Delta_irritation_table<-matrix(c(1,51,1,44),nrow = 2, ncol = 2)
OR_Delta_irritation_table  
fisher.test(OR_Delta_irritation_table)#calculate odds ratio
 
############ For the Omicron VOC relative to Alpha + Delta
#Cough
OR_Omicron_cough_table<-matrix(c(33,4,41,19),nrow = 2, ncol = 2)
OR_Omicron_cough_table  
fisher.test(OR_Omicron_cough_table) #calculate odds ratio

#chest pain
OR_Omicron_cp_table<-matrix(c(0,37,17,43),nrow = 2, ncol = 2)
OR_Omicron_cp_table  
fisher.test(OR_Omicron_cp_table) #calculate odds ratio

#sore throat
OR_Omicron_STR_table<-matrix(c(28,9,7,53),nrow = 2, ncol = 2)
OR_Omicron_STR_table  
fisher.test(OR_Omicron_STR_table) #calculate odds ratio

#joint pain
OR_Omicron_JP_table<-matrix(c(2,35,2,58),nrow = 2, ncol = 2)
OR_Omicron_JP_table  
fisher.test(OR_Omicron_JP_table) #calculate odds ratio

#abdominal pain
OR_Omicron_AP_table<-matrix(c(1,36,0,60),nrow = 2, ncol = 2)
OR_Omicron_AP_table  
fisher.test(OR_Omicron_AP_table) #calculate odds ratio

#general weakness
OR_Omicron_GW_table<-matrix(c(1,36,17,43),nrow = 2, ncol = 2)
OR_Omicron_GW_table  
fisher.test(OR_Omicron_GW_table) #calculate odds ratio

#fever
OR_Omicron_fever_table<-matrix(c(2,35,16,44),nrow = 2, ncol = 2)
OR_Omicron_fever_table  
fisher.test(OR_Omicron_fever_table) #calculate odds ratio

#cheadache
OR_Omicron_headache_table<-matrix(c(11,26,13,47),nrow = 2, ncol = 2)
OR_Omicron_headache_table  
fisher.test(OR_Omicron_headache_table) #calculate odds ratio

#irritation
OR_Omicron_irritation_table<-matrix(c(1,51,1,44),nrow = 2, ncol = 2)
OR_Omicron_irritation_table  
fisher.test(OR_Omicron_irritation_table) #calculate odds ratio




