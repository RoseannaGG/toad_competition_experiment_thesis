##########


      # frog response variable


#########

rm(list=ls())

library(ggplot2)
library(ggthemes)
library(reshape2)
library(gridExtra)
library(plyr)
library(dplyr)
library(visreg)
library(lme4)
library(nlme)
library(MuMIn)
library(lmerTest)
library(sjPlot)
library(sjmisc)
library(effects)
library(car)
library(ggbeeswarm)
library(optimx)
library(arules)
library(rstanarm)
library(tidyr)
library(dataRetrieval)
library(cowplot)
library(forcats)
library(performance) # colinearity
library(ggpubr)

setwd("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20")

##########################################
############## SHORTCUT ###########
########################################

data_frogs_long_enviro_sumDD_01_05_20<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/data_frogs_long_enviro_sumDD_01_05_20.csv",header=T,row.names=NULL,sep=",") 

dim(data_frogs_long_enviro_sumDD_01_05_20) #360  45

data_frogs_long_enviro_sumDD_01_05_20$Block<-as.factor(data_frogs_long_enviro_sumDD_01_05_20$Block)

#data_frogs_long_enviro_sumDD_01_05_20<-data_frogs_long_enviro_sumDD_01_05_20

data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass[data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass == 0] <- NA


data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass_mg<-data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass*1000

data_frogs_long_enviro_sumDD_01_05_20$log10_Av_frog_juv_biomass_mg<-
  log10(data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass_mg)


data_frogs_long_enviro_sumDD_01_05_20$GR_final_days_mgDay<-data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass_mg/data_frogs_long_enviro_sumDD_01_05_20$Days

data_frogs_long_enviro_sumDD_01_05_20$log10_GR_final_days_mgDay<-
  log10(data_frogs_long_enviro_sumDD_01_05_20$GR_final_days_mgDay)






## final - initial / just degree days in experiment 
data_frogs_long_enviro_sumDD_01_05_20$GR_frog_finalmin_intial_degreedays_mgDD<-((data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass-data_frogs_long_enviro_sumDD_01_05_20$FROG_25.5.18_experiment_start_tadpole_av_individual_weight)/data_frogs_long_enviro_sumDD_01_05_20$SumDD)*1000






##########################
#### short dataset
##################
subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs

subset_dataRGRdd_frogs2$Tank<-subset_dataRGRdd_frogs2$ï..Tank


#order levels of factor
subset_dataRGRdd_frogs2$Treatment_nameC<- factor(subset_dataRGRdd_frogs2$Treatment_nameC, levels = c("HGF_HGT","HGF_MLT","MLF_MLT","MLF_HGT"))
subset_dataRGRdd_frogs2 <- droplevels(subset_dataRGRdd_frogs2)


#NB two metamorph numbers for 23.8.18 - some were weighed and then realized that some were left out at the end
#X23.8.18_endexperiment_number_metamorphs <- ones that were found when we emptied the tanks - not weighed
#X23.8.18_number_of_metamorphs - number that were weighed


subset_dataRGRdd_frogs2$Frogmortality<-subset_dataRGRdd_frogs2$Total_number_individuals_start-subset_dataRGRdd_frogs2$Survived_counts_weighedandkilled

subset_dataRGRdd_frogs2$Frogmortality_percapita<-subset_dataRGRdd_frogs2$Frogmortality/36

subset_dataRGRdd_frogs2$Frogmortality_noperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_yesperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$Perished_total_onsideoftank+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_tadsadded_yesperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$Perished_total_onsideoftank+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

#remove negative value
subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished[subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished < 0] <- NA



subset_dataRGRdd_frogs2$Frogmortality_tadsadded_yesperished_percapita<-subset_dataRGRdd_frogs2$Frogmortality_tadsadded_yesperished/(subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th)



####################################
##### join with morality data
subset_dataRGRdd_frogs2_sub<-subset_dataRGRdd_frogs2[c(233:239)]

data_frogs_long_enviro_sumDD_01_05_20_2<-left_join(data_frogs_long_enviro_sumDD_01_05_20,subset_dataRGRdd_frogs2_sub,by="Tank")




###### scaled ########
################

#scale Ctadpole top up
Xtemp9 <-data_frogs_long_enviro_sumDD_01_05_20_2$Tadpoles_added_as_top.up_june7thand8th_percapita

Xscaled9 <- (Xtemp9 - mean(Xtemp9))/sd(Xtemp9)
data_frogs_long_enviro_sumDD_01_05_20_2$scaled_Tadpoles_added_as_top.up_june7thand8th_percapita  <- Xscaled9

#starting weight
Xtemp <- data_frogs_long_enviro_sumDD_01_05_20_2$FROG_25.5.18_experiment_start_tadpole_av_individual_weight
Xtemp

Xscaled <- (Xtemp - mean(Xtemp))/sd(Xtemp)
data_frogs_long_enviro_sumDD_01_05_20_2$scaled_FROG_25.5.18_experiment_start_tadpole_av_individual_weight  <- Xscaled



######### Mean_averageTemp
Xtemp8 <-data_frogs_long_enviro_sumDD_01_05_20_2$Mean_averageTemp

Xscaled8 <- (Xtemp8 - mean(Xtemp8))/sd(Xtemp8)
data_frogs_long_enviro_sumDD_01_05_20_2$scaled_Mean_averageTemp  <- Xscaled8




#################
### END SHORTCUT #######
##########################






######################################
########## see how did it (MADE SHORT DATASET) ... TURN DATASET LONG #############
################################
subset_dataRGRdd_frogs2_long_enviro<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs2_long_enviro.csv",header=T,row.names=NULL,sep=",") 


subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs


subset_dataRGRdd_frogs2$Tank<-subset_dataRGRdd_frogs2$?..Tank


names(subset_dataRGRdd_frogs2)[names(subset_dataRGRdd_frogs2) == "?..Tank"] <- "Tank"

names(subset_dataRGRdd_frogs2)
#SUBSET TO JUST DATASET i WANT
myvars<- c("Tank",
           "X19.7.18_._average_weight_of_adults"                                                 
           , "X31.7.18_._average_weight_of_adults"                                                 
           , "X3.8.18_._average_weight_of_adults"                                                  
           , "X7.8.18_._average_weight_of_adults"                                                  
           , "X10.8.18_._average_weight_of_adults"                                                 
           , "X13.8.18_._average_weight_of_adults"                                                 
           , "X16.8.18_._average_weight_of_adults"                                                 
           , "X20.8.18_._average_weight_of_adults"                                                 
           , "X23.8.18_._average_weight_of_adults" 
     )      



subset_dataRGRdd_frogs2_juvweights<-subset_dataRGRdd_frogs2[myvars]
head(subset_dataRGRdd_frogs2_juvweights)

subset_dataRGRdd_frogs2_juvweightslong<-gather(subset_dataRGRdd_frogs2_juvweights, "Days", "Av_frog_juv_biomass", 2:10)



subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X19.7.18_._average_weight_of_adults"] <- "55"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X31.7.18_._average_weight_of_adults"] <- "67"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X3.8.18_._average_weight_of_adults"] <- "70"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X7.8.18_._average_weight_of_adults"] <- "74"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X10.8.18_._average_weight_of_adults"] <- "77"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X13.8.18_._average_weight_of_adults"] <- "80"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X16.8.18_._average_weight_of_adults"] <- "83"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X20.8.18_._average_weight_of_adults"] <- "87"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X23.8.18_._average_weight_of_adults"] <- "90"

subset_dataRGRdd_frogs2_juvweightslong$Days<-as.numeric(subset_dataRGRdd_frogs2_juvweightslong$Days)


#write.csv(subset_dataRGRdd_frogs2_juvweightslong,"subset_dataRGRdd_frogs2_juvweightslong.csv")


##########################
######## add counts in
#################################
subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs


subset_dataRGRdd_frogs2$Tank<-subset_dataRGRdd_frogs2$?..Tank


names(subset_dataRGRdd_frogs2)[names(subset_dataRGRdd_frogs2) == "?..Tank"] <- "Tank"

names(subset_dataRGRdd_frogs2)
#SUBSET TO JUST DATASET i WANT
myvars<- c("Tank",
           "Frog_popsource", "Population" ,"Region","Block"
           ,"Tadpoles_added_as_top.up_june7thand8th"
           ,"Tadpoles_added_as_top.up_june7thand8th_percapita"
           ,"X19.7.18_._number_adults"                                                  
           ,"X27.7.18_._NUMBER_ADULTS._WERE_NOT_WEIGHED"                                
           ,"X31.7.18_._number_adults"                                                  
           ,"X3.8.18_._number_adults"                                                   
          ,"X7.8.18_._number_adults"                                                   
          , "X10.8.18_._number_adults"                                                  
          , "X13.8.18_._number_adults"                                                  
           , "X16.8.18_._number_adults"                                                  
           ,"X20.8.18_._number_adults"                                                  
           , "X23.8.18_._number_adults"  
           ,"May_MaxwaterTemp"                                                          
           ,"May_MinwaterTemp"                                                          
           ,"May_MeanwaterTemp"                                                         
           ,"June_MaxwaterTemp"                                                         
           ,"June_MinwaterTemp"                                                         
           ,"June_MeanwaterTemp"                                                        
           ,"July_MaxwaterTemp"                                                         
           ,"July_MinwaterTemp"                                                         
           ,"July_MeanwaterTemp"                                                        
           ,"August_MaxwaterTemp"                                                       
           ,"August_MinwaterTemp"                                                       
           ,"August_MeanwaterTemp"                                                      
           ,"Max_averageTemp"                                                           
           ,"Min_averageTemp"                                                           
           ,"Mean_averageTemp" 
           ,"Total_chla_conc_8.6.18_ug.l"                                               
           , "Total_chla_conc_26.7.18_ug.l" 
           ,"FROG_25.5.18_experiment_start_tadpole_av_individual_weight"
           ,"Average_weight_of_tadpole_replacements_june7thand8th"
           ,"Total_number_individuals_start" 
          ,"Survived_counts_weighedandkilled"                                          
, "Perished_total_onsideoftank"                                               
, "Survived_and_perished"                                                     
,"DIED_unexplaineddeath"                                                     
,"DIED_no_extras"                                                            
,"DIED_no_extras_divided_bystartingnumber"                                   
,"DIED_all"                                                                  
,"DIED_all_percapita"                                                        
,"Extra_number_at_end"  
,"Running_total_of_adults_emerged"
,"Daysbeforestartedexpmt"
,"Daysincaptivitybeforeexpmtstart"
,"Daysbeforeminusdayscap")      



subset_dataRGRdd_frogs2_juvcounts<-subset_dataRGRdd_frogs2[myvars]
head(subset_dataRGRdd_frogs2_juvcounts)

subset_dataRGRdd_frogs2_juvcountslong<-gather(subset_dataRGRdd_frogs2_juvcounts, "Days", "Count_juvfrog_emerged", 8:17)



subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X19.7.18_._number_adults"] <- "55"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X27.7.18_._NUMBER_ADULTS._WERE_NOT_WEIGHED"] <- "63"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X31.7.18_._number_adults"] <- "67"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X3.8.18_._number_adults"] <- "70"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X7.8.18_._number_adults"] <- "74"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X10.8.18_._number_adults"] <- "77"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X13.8.18_._number_adults"] <- "80"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X16.8.18_._number_adults"] <- "83"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X20.8.18_._number_adults"] <- "87"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X23.8.18_._number_adults"] <- "90"

subset_dataRGRdd_frogs2_juvcountslong$Days<-as.numeric(subset_dataRGRdd_frogs2_juvcountslong$Days)


head(subset_dataRGRdd_frogs2_juvcountslong)


#write.csv(subset_dataRGRdd_frogs2_juvcountslong,"subset_dataRGRdd_frogs2_juvcountslong.csv")




############## top down join between subset_dataRGRdd_frogs2_juvcountslong and subset_dataRGRdd_frogs2_juvweightslong
data_frogs_long_enviro_01_05_20<-left_join(subset_dataRGRdd_frogs2_juvcountslong,subset_dataRGRdd_frogs2_juvweightslong)



########################################
################ add in SumDD #############
###################################
subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs


subset_dataRGRdd_frogs2$Tank<-subset_dataRGRdd_frogs2$?..Tank


names(subset_dataRGRdd_frogs2)[names(subset_dataRGRdd_frogs2) == "?..Tank"] <- "Tank"

names(subset_dataRGRdd_frogs2)
#SUBSET TO JUST DATASET i WANT
myvars<- c("Tank"
          , "SumDD_23_8"                                                                ,"SumDD_20_8"                                                                
           ,"SumDD_16_8"                                                                
           ,"SumDD_13_8"                                                                
           ,"SumDD_10_8"                                                                
           , "SumDD_7_8"                                                                 
          ,"SumDD_3_8"                                                                 
          ,"SumDD_31_7"                                                                
         , "SumDD_27_7"                                                                
         , "SumDD_19_7")                                                            
  



subset_dataRGRdd_frogs2_sumDD<-subset_dataRGRdd_frogs2[myvars]
head(subset_dataRGRdd_frogs2_sumDD)

subset_dataRGRdd_frogs2_sumDDlong<-gather(subset_dataRGRdd_frogs2_sumDD, "Days", "SumDD", 2:11)



subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_19_7"] <- "55"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_27_7"] <- "63"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_31_7"] <- "67"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_3_8"] <- "70"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_7_8"] <- "74"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_10_8"] <- "77"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_13_8"] <- "80"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_16_8"] <- "83"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_20_8"] <- "87"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_23_8"] <- "90"

subset_dataRGRdd_frogs2_sumDDlong$Days<-as.numeric(subset_dataRGRdd_frogs2_sumDDlong$Days)


head(subset_dataRGRdd_frogs2_sumDDlong)


##########join

data_frogs_long_enviro_sumDD_01_05_20<-left_join(data_frogs_long_enviro_01_05_20,subset_dataRGRdd_frogs2_sumDDlong)


#write.csv(data_frogs_long_enviro_sumDD_01_05_20,"data_frogs_long_enviro_sumDD_01_05_20.csv")




############## #############
####### 1. WEIGHT at metamorph ###########
###################################
ggbetweenstats(data_frogs_long_enviro_sumDD_01_05_20,
               Frog_popsource, Av_frog_juv_biomass_mg, outlier.tagging = TRUE)








#################
####### mean sd #########
########################


data_frogs_long_enviro_sumDD_01_05_20_2_noNA<-data_frogs_long_enviro_sumDD_01_05_20_2[!(data_frogs_long_enviro_sumDD_01_05_20_2$Av_frog_juv_biomass_mg=="NA"),]

dim(data_frogs_long_enviro_sumDD_01_05_20_2_noNA)

Av_juv_biomass_mg_360 <- data_frogs_long_enviro_sumDD_01_05_20_2_noNA %>%
  group_by(Frog_popsource,Region)

Av_juv_biomass_mg_360<-Av_juv_biomass_mg_360 %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), Av_frog_juv_biomass_mg)

Av_juv_biomass_mg_360<-as.data.frame(Av_juv_biomass_mg_360)
Av_juv_biomass_mg_360

write.csv(Av_juv_biomass_mg_360,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/Mean_sd_se_FROG_Av_juv_biomass_mg_36.csv")



########## HG frog ####
466.500-395.9881

((466.500-395.9881)/395.9881)*100


############## 
####### SCALING ###########
###############


############## 
####### ANOVA ###########
###############

### very omprotnaty
data_frogs_long_enviro_sumDD_01_05_20_2$Frog_popsource<- factor(data_frogs_long_enviro_sumDD_01_05_20_2$Frog_popsource, levels = c("ML","HG"))
data_frogs_long_enviro_sumDD_01_05_20_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_2)


data_frogs_long_enviro_sumDD_01_05_20_2$Toad_Cooccurrence_History<- factor(data_frogs_long_enviro_sumDD_01_05_20_2$Toad_Cooccurrence_History, levels = c("Long","Short"))
data_frogs_long_enviro_sumDD_01_05_20_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_2)


data_frogs_long_enviro_sumDD_01_05_20_2$Region<- factor(data_frogs_long_enviro_sumDD_01_05_20_2$Region, levels = c("ML","HG"))
data_frogs_long_enviro_sumDD_01_05_20_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_2)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/hist_log10_Av_frog_juv_biomass_mg.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(data_frogs_long_enviro_sumDD_01_05_20$log10_Av_frog_juv_biomass_mg)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/hist_Av_frog_juv_biomass_mg.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass_mg)
dev.off()


model_FAW1<-lmer(
  log10_Av_frog_juv_biomass_mg~
    Region*Frog_popsource+ 
    Tadpoles_added_as_top.up_june7thand8th_percapita+ 
    FROG_25.5.18_experiment_start_tadpole_av_individual_weight+ 
    Frogmortality_percapita+
    Mean_averageTemp+
    Days+ #scaled
    Block+
    (1|Population)+ 
    (1+Days|Tank), #random effects
  data=data_frogs_long_enviro_sumDD_01_05_20_2,na.action=na.exclude,REML=FALSE,control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'),check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

Anova(model_FAW1)
AIC(model_FAW1) #-58.09773
summary(model_FAW1)


#360
capture.output(anova(model_FAW1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/anova_avjuvweight_model_FAW1_360.txt")
capture.output(Anova(model_FAW1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/Anova_chisq_avjuvweight_model_FAW1_360.txt")
write.csv(anova(model_FAW1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/anova_avjuvweight_model_FAW1_360.csv")
write.csv(Anova(model_FAW1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/Anova_chisq_avjuvweight_model_FAW1_360.csv")
capture.output(summary(model_FAW1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/summary_avjuvweight_model_FAW1_360.txt")


capture.output(summary(model_FAW1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/summary_avjuvweight_model_FAW1_360.csv")

tab_model(model_FAW1, file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/tab_model_avjuvweight_model_FAW1_360.doc")

tab_model(model_FAW1,show.df = TRUE,p.val = "kr",
 file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/tab_model_avjuvweight_model_FAW1_360_df_pkr.doc")


### test residuals - mostly ok
qqnorm(resid(model_FAW1))
hist(resid(model_FAW1))
plot(model_FAW1)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/Residualsplot_model_FAW1_360.png", width = 12, height = 6.5, units = 'in', res = 300)
plot(model_FAW1)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/Residualshist_model_FAW1_360.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(resid(model_FAW1))
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/QQnorm_model_FAW1_360.png", width = 12, height = 6.5, units = 'in', res = 300)
qqnorm(resid(model_FAW1))
dev.off()



(model_FAW1.means <- interactionMeans(model_FAW1))
plot(model_FAW1.means)

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/interactionMeans_model_FAW1_360.png", width = 12, height = 6.5, units = 'in', res = 300)
(model_FAW1.means <- interactionMeans(model_FAW1))
plot(model_FAW1.means)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/interactionMeans_model_FAW1_360.svg", width = 12, height = 6.5)
(model_FAW1.means <- interactionMeans(model_FAW1))
plot(model_FAW1.means)
dev.off()


par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FAW1)



png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/visreg_model_FAW1_360.png", width = 8, height = 6.5, units = 'in', res = 300)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FAW1)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/visreg_model_FAW1_360.svg", width = 8, height = 6.5)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FAW1)
dev.off()




L.S_model_FAW1 <- pairs(lsmeans(model_FAW1, ~ Region | Frog_popsource))
test(L.S_model_FAW1, adjust = "tukey")

capture.output(test(L.S_model_FAW1, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/L.S_model_FAW1_360.txt")
write.csv(test(L.S_model_FAW1, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/L.S_model_FAW1_360.csv")

L.S_model_FAW1_2 <- pairs(lsmeans(model_FAW1, ~  Frog_popsource | Region))
test(L.S_model_FAW1_2, adjust = "tukey")

capture.output(test(L.S_model_FAW1_2, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/L.S_model_FAW1_2_360.txt")
write.csv(test(L.S_model_FAW1_2, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/L.S_model_FAW1_2_360.csv")




#############################
## best 01.05.20 ######
##########################
model_FROG_avjuvbiomass<-lmer(
  Av_frog_juv_biomass~
    Region*Frog_popsource+ 
    Tadpoles_added_as_top.up_june7thand8th+ 
    #FROG_25.5.18_experiment_start_tadpole_av_individual_weight+ raises AIC
    Mean_averageTemp+
    Days+ #scaled
    (1|Population)+ 
    (1|Block)+
    (1+Days|Tank), #random effects
  data=data_frogs_long_enviro_sumDD_01_05_20,na.action=na.exclude,REML=FALSE,control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'),check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

anova(model_FROG_avjuvbiomass)
visreg(model_FROG_avjuvbiomass)
AIC(model_FROG_avjuvbiomass) # -67.96858

#interaction
model_FROG_avjuvbiomass_interaction<- effect('Region*Frog_popsource', model_FROG_avjuvbiomass,
                                                               se=TRUE)
(model_FROG_avjuvbiomass_interactionplot<-plot(model_FROG_avjuvbiomass_interaction, multiline = TRUE) )

capture.output(model_FROG_avjuvbiomass,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/model_FROG_avjuvbiomass.txt")
capture.output(anova(model_FROG_avjuvbiomass),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/anova_model_FROG_avjuvbiomass.txt")


###########################
############# PLOTS #############
###############################
labels <- c(HG = "Haida Gwaii Frogs", ML = "Mainland Frogs")

#turn zero to NA
#data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass[data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass == 0] <- NA

#ylim1 = boxplot.stats(data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass)$stats[c(1, 5)]


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplotfrog_Av_frog_juv_biomass_nofacet_360.png", width = 200, height = 150, units = 'mm', res = 600)
(frogs_meta_weightplot_nofacet_29_01_22<-
    data_frogs_long_enviro_sumDD_01_05_20_2%>%
    ggplot(aes(x=Region, y=Av_frog_juv_biomass_mg, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    labs(y="Average frog weight at metamorphosis (mg)")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                    labels=c("Haida Gwaii","Mainland" ))+
   # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                #          labels=c("Haida Gwaii", "Mainland"),
            #              guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 15)))
dev.off()

ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplotfrog_Av_frog_juv_biomass_nofacet_360.svg", width = 200, height = 150,units="mm")
(frogs_meta_weightplot_nofacet_29_01_22<-
    data_frogs_long_enviro_sumDD_01_05_20_2%>%
    ggplot(aes(x=Region, y=Av_frog_juv_biomass_mg, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    labs(y="Average frog weight at metamorphosis (mg)")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #          labels=c("Haida Gwaii", "Mainland"),
    #              guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 15)))
dev.off()


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplotfrog_Av_frog_juv_biomass_logscale_nofacet_360.png", width = 200, height = 150, units = 'mm', res = 600)
(frogs_meta_weightplot_logscale_nofacet_29_01_22<-
    data_frogs_long_enviro_sumDD_01_05_20_2%>%
    ggplot(aes(x=Region, y=Av_frog_juv_biomass_mg, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    scale_y_continuous(trans = 'log10')+
    annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    labs(y="Average frog weight at metamorphosis (mg)")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #          labels=c("Haida Gwaii", "Mainland"),
    #              guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 15)))
dev.off()

ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplotfrog_Av_frog_juv_biomass_logscale_nofacet_360.svg", width = 200, height = 150, units = 'mm')
(frogs_meta_weightplot_logscale_nofacet_29_01_22<-
    data_frogs_long_enviro_sumDD_01_05_20_2%>%
    ggplot(aes(x=Region, y=Av_frog_juv_biomass_mg, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    scale_y_continuous(trans = 'log10')+
    annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    labs(y="Average frog weight at metamorphosis (mg)")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #          labels=c("Haida Gwaii", "Mainland"),
    #              guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 15)))
dev.off()


###########################################################################
################################## CUSTOM CONTRASTS ########################
############################################################################

#L.S <- pairs(lsmeans(model_FROG_avjuvbiomass, ~ Region*Frog_popsource))
#test(L.S, adjust = "tukey")


rm(emm3_model_FROG_avjuvbiomass)

#model_FROG_avjuvbiomass
emm3_model_FROG_avjuvbiomass=emmeans(model_FAW1,specs=~Region*Frog_popsource)


capture.output(emm3_model_FROG_avjuvbiomass,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/emm3_model_FROG_avjuvbiomass_model_FAW1.txt")

#make into dataframe
emm3_model_FROG_avjuvbiomassdf<-as.data.frame(emm3_model_FROG_avjuvbiomass)

#make new column
emm3_model_FROG_avjuvbiomassdf<-cbind(Model = "model_FROG_avjuvbiomass", emm3_model_FROG_avjuvbiomassdf)


#region HG, comp level low control - single out estimate for each level of region/competition to insect into contrast table later to calculate percent
toadHG_frogHG_model_FROG_avjuvbiomassdf<-emm3_model_FROG_avjuvbiomassdf[1,4]
toadML_frogHG_model_FROG_avjuvbiomassdf<-emm3_model_FROG_avjuvbiomassdf[2,4]
toadHG_frogML_model_FROG_avjuvbiomassdf<-emm3_model_FROG_avjuvbiomassdf[3,4]
toadML_frogML_model_FROG_avjuvbiomassdf<-emm3_model_FROG_avjuvbiomassdf[4,4]


#create custom contrasts
toadHG_frogHG<-rep(c(1,0), times = c(1,3))
toadML_frogHG<-rep(c(0,1,0), times = c(1,1,2))
toadHG_frogML<-rep(c(0,1,0), times = c(2,1,1))
toadML_frogML<-rep(c(0,1), times = c(3,1))




(contrast_model_FROG_avjuvbiomass_bonferroni2<-contrast(emm3_model_FROG_avjuvbiomass,adjust="bonferroni",method=list(
  
  "toadHG_frogHG - toadML_frogHG" = toadHG_frogHG - toadML_frogHG, #not different
  "toadHG_frogML - toadML_frogML" = toadHG_frogML - toadML_frogML
  #not different
  
)))



(contrast_model_FROG_avjuvbiomass_bonferroni2<-contrast(emm3_model_FROG_avjuvbiomass,adjust="bonferroni",method=list(
 
  "toadHG_frogHG - toadML_frogHG" = toadHG_frogHG - toadML_frogHG, #not different
  "toadHG_frogML - toadML_frogML" = toadHG_frogML - toadML_frogML
  #not different

  )))

(contrast_model_FROG_avjuvbiomass_bonferroni_EXTRA<-contrast(emm3_model_FROG_avjuvbiomass,adjust="bonferroni",method=list(
  
  "toadHG_frogHG - toadML_frogHG" = toadHG_frogHG - toadML_frogHG, #not different
  "toadHG_frogML - toadML_frogML" = toadHG_frogML - toadML_frogML,
  #not different
  "toadHG_frogHG - toadML_frogHG - toadHG_frogML - toadML_frogML" = (toadHG_frogHG - toadML_frogHG) - (toadHG_frogML - toadML_frogML)
)))


(contrast_model_FROG_avjuvbiomassdf<-as.data.frame(contrast_model_FROG_avjuvbiomass_bonferroni2))

#make new column
contrast_model_FROG_avjuvbiomassdf<-cbind(Model = "model_FROG_avjuvbiomass", contrast_model_FROG_avjuvbiomassdf)

#write.csv(contrast_model_FROG_avjuvbiomassdf,"contrast_model_FROG_avjuvbiomassdf.csv")


#######################
################## PERCENTS ##################
####################

#make into dataframe
#contrast_model_FROG_avjuvbiomassdf<-as.data.frame(contrast_model_FROG_avjuvbiomass_bonferronilarge)


(contrast_model_FROG_avjuvbiomassdf<-as.data.frame(contrast_model_FROG_avjuvbiomass_bonferroni2))


#make new column
contrast_model_FROG_avjuvbiomassdf<-cbind(Model = "model_FROG_avjuvbiomass", contrast_model_FROG_avjuvbiomassdf)

#HGLow_controlHG
(contrast_model_FROG_avjuvbiomassdf$emmean_HGLow_controlHG<-rep(HGLow_controlHG_model_FROG_avjuvbiomassdf,length(contrast_model_FROG_avjuvbiomassdf$p.value)))
#percent
(contrast_model_FROG_avjuvbiomassdf$percent_HGLow_controlHG<-(contrast_model_FROG_avjuvbiomassdf$estimate/contrast_model_FROG_avjuvbiomassdf$emmean_HGLow_controlHG)*100)
#value
((HGLow_controlHG_model_FROG_avjuvbiomassdf_value<-contrast_model_FROG_avjuvbiomassdf[c(3,7),c(1:9)]))



#MLLow_controlML
(contrast_model_FROG_avjuvbiomassdf$emmean_MLLow_controlML<-rep(MLLow_controlML_model_FROG_avjuvbiomassdf,length(contrast_model_FROG_avjuvbiomassdf$p.value)))
#percent
(contrast_model_FROG_avjuvbiomassdf$percent_MLLow_controlML<-(contrast_model_FROG_avjuvbiomassdf$estimate/contrast_model_FROG_avjuvbiomassdf$emmean_MLLow_controlML)*100)
#value
(MLLow_controlML_model_FROG_avjuvbiomassdf_value<-contrast_model_FROG_avjuvbiomassdf[c(4,8),c(1:7,10:11)])

#HGFrogoverallHG
(contrast_model_FROG_avjuvbiomassdf$emmean_HGFrog_overallHG<-rep(HGFrog_overallHG_model_FROG_avjuvbiomassdf,length(contrast_model_FROG_avjuvbiomassdf$p.value)))
#percent
(contrast_model_FROG_avjuvbiomassdf$percent_HGFrog_overallHG<-(contrast_model_FROG_avjuvbiomassdf$estimate/contrast_model_FROG_avjuvbiomassdf$emmean_HGFrog_overallHG)*100)
#value
((HGFrog_overallHG_model_FROG_avjuvbiomassdf_value<-contrast_model_FROG_avjuvbiomassdf[c(5),c(1:7,12:13)]))

#MLFrogoverallML
contrast_model_FROG_avjuvbiomassdf$emmean_MLFrog_overallML<-rep(MLFrog_overallML_model_FROG_avjuvbiomassdf,length(contrast_model_FROG_avjuvbiomassdf$p.value))
#percent
contrast_model_FROG_avjuvbiomassdf$percent_MLFrog_overallML<-(contrast_model_FROG_avjuvbiomassdf$estimate/contrast_model_FROG_avjuvbiomassdf$emmean_MLFrog_overallML)*100
#value
(MLFrog_overallML_model_FROG_avjuvbiomassdf_value<-contrast_model_FROG_avjuvbiomassdf[c(6),c(1:7,14:15)])

#write.csv(contrast_model_FROG_avjuvbiomassdf,"contrast_model_GR_toads_FinalmInitial_degreeDays.Frank.csv")

#bind together
HGLow_controlHG_model_FROG_avjuvbiomassdf_value
MLLow_controlML_model_FROG_avjuvbiomassdf_value
HGFrog_overallHG_model_FROG_avjuvbiomassdf_value
MLFrog_overallML_model_FROG_avjuvbiomassdf_value

(contrast_model_FROG_avjuvbiomassdf_selected<-bind_rows(
  HGLow_controlHG_model_FROG_avjuvbiomassdf_value,
  MLLow_controlML_model_FROG_avjuvbiomassdf_value,
  HGFrog_overallHG_model_FROG_avjuvbiomassdf_value,
  MLFrog_overallML_model_FROG_avjuvbiomassdf_value))



#   model_avjuvbiomass_toads_Frank
#write.csv(contrast_model_FROG_avjuvbiomassdf_selected,"contrast_selected_bonferroni2_model_avjuvbiomass_toads_Frank.csv")




###########################
############# 2.GROWTH RATE ###############
#############################



#################
####### mean sd #########
########################


data_frogs_long_enviro_sumDD_01_05_20_2_noNA<-data_frogs_long_enviro_sumDD_01_05_20_2[!(data_frogs_long_enviro_sumDD_01_05_20_2$Av_frog_juv_biomass_mg=="NA"),]

dim(data_frogs_long_enviro_sumDD_01_05_20_2_noNA)

GR_final_days_mgDay_360 <- data_frogs_long_enviro_sumDD_01_05_20_2_noNA %>%
  group_by(Frog_popsource,Region)

GR_final_days_mgDay_360<-GR_final_days_mgDay_360 %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), GR_final_days_mgDay)

GR_final_days_mgDay_360<-as.data.frame(GR_final_days_mgDay_360)
GR_final_days_mgDay_360

write.csv(GR_final_days_mgDay_360,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/Mean_sd_se_FROG_GR_final_days_mgDay_36.csv")



########## HG frog ####
6.653252 - 4.952009
((6.653252 - 4.952009)/4.952009)*100

############# 
####### ANOVA ###########
###############

### very omprotnaty
data_frogs_long_enviro_sumDD_01_05_20_2$Frog_popsource<- factor(data_frogs_long_enviro_sumDD_01_05_20_2$Frog_popsource, levels = c("ML","HG"))
data_frogs_long_enviro_sumDD_01_05_20_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_2)


data_frogs_long_enviro_sumDD_01_05_20_2$Toad_Cooccurrence_History<- factor(data_frogs_long_enviro_sumDD_01_05_20_2$Toad_Cooccurrence_History, levels = c("Long","Short"))
data_frogs_long_enviro_sumDD_01_05_20_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_2)


data_frogs_long_enviro_sumDD_01_05_20_2$Region<- factor(data_frogs_long_enviro_sumDD_01_05_20_2$Region, levels = c("ML","HG"))
data_frogs_long_enviro_sumDD_01_05_20_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_2)




png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/hist_log10_GR_final_days_mgDay.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(data_frogs_long_enviro_sumDD_01_05_20$log10_GR_final_days_mgDay)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/hist_GR_final_days_mgDay.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(data_frogs_long_enviro_sumDD_01_05_20$GR_final_days_mgDay)
dev.off()




### best #########
model_FGR1<-lmer(
  log10_GR_final_days_mgDay~
    Region*Frog_popsource+ 
    scaled_Tadpoles_added_as_top.up_june7thand8th_percapita+ 
    scaled_FROG_25.5.18_experiment_start_tadpole_av_individual_weight+ 
   Frogmortality_percapita+
    scaled_Mean_averageTemp+
    Days+ #scaled
    Block+
    (1|Population)+ 
    (1+Days|Tank), #random effects
  data=data_frogs_long_enviro_sumDD_01_05_20_2,na.action=na.exclude,REML=FALSE,control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'),check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))


Anova(model_FGR1)
AIC(model_FGR1) # -55.85908
summary(model_FGR1)


#360
capture.output(anova(model_FGR1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/anova_growthrate_model_FGR1_360.txt")
capture.output(Anova(model_FGR1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/Anova_chisq_growthrate_model_FGR1_360.txt")
write.csv(anova(model_FGR1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/anova_growthrate_model_FGR1_360.csv")
write.csv(Anova(model_FGR1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/Anova_chisq_growthrate_model_FGR1_360.csv")
capture.output(summary(model_FGR1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/summary_growthrate_model_FGR1_360.txt")

capture.output(summary(model_FGR1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/summary_growthrate_model_FGR1_360.csv")


tab_model(model_FGR1, file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/tab_model_growthrate_model_FGR1_360.doc")


tab_model(model_FGR1,show.df = TRUE,p.val = "kr", file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/tab_model_growthrate_model_FGR1_360_df_pkr.doc")


### test residuals - mostly ok
qqnorm(resid(model_FGR1))
hist(resid(model_FGR1))
plot(model_FGR1)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/Residualsplot_model_FGR1_360.png", width = 12, height = 6.5, units = 'in', res = 300)
plot(model_FGR1)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/Residualshist_model_FGR1_360.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(resid(model_FGR1))
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/QQnorm_model_FGR1_360.png", width = 12, height = 6.5, units = 'in', res = 300)
qqnorm(resid(model_FGR1))
dev.off()



(model_FGR1.means <- interactionMeans(model_FGR1))
plot(model_FGR1.means)

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/interactionMeans_model_FGR1_360.png", width = 12, height = 6.5, units = 'in', res = 300)
(model_FGR1.means <- interactionMeans(model_FGR1))
plot(model_FGR1.means)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/interactionMeans_model_FGR1_360.svg", width = 12, height = 6.5)
(model_FGR1.means <- interactionMeans(model_FGR1))
plot(model_FGR1.means)
dev.off()


par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FGR1)



png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/visreg_model_FGR1_360.png", width = 8, height = 6.5, units = 'in', res = 300)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FGR1)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/visreg_model_FGR1_360.svg", width = 8, height = 6.5)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FGR1)
dev.off()




L.S_model_FGR1 <- pairs(lsmeans(model_FGR1, ~ Region | Frog_popsource))
test(L.S_model_FGR1, adjust = "tukey")

capture.output(test(L.S_model_FGR1, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/L.S_model_FGR1_360.txt")
write.csv(test(L.S_model_FGR1, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/L.S_model_FGR1_360.csv")

L.S_model_FGR1_2 <- pairs(lsmeans(model_FGR1, ~  Frog_popsource | Region))
test(L.S_model_FGR1_2, adjust = "tukey")

capture.output(test(L.S_model_FGR1_2, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/L.S_model_FGR1_2_360.txt")
write.csv(test(L.S_model_FGR1_2, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/L.S_model_FGR1_2_360.csv")











####################
##  ######
#############
model_FROG_growthrate<-lmer(
  GR_frog_finalmin_intial_degreedays_mgDD~
    Region*Frog_popsource+ 
    Tadpoles_added_as_top.up_june7thand8th+ 
    Days+ #scaled
    (1|Population)+ 
    (1|Block)+
    (1+Days|Tank), #random effects
  data=data_frogs_long_enviro_sumDD_01_05_20,na.action=na.exclude,REML=FALSE,control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'),check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))#aic 133.7337 region compelvel sig

anova(model_FROG_growthrate)
visreg(model_FROG_growthrate)
AIC(model_FROG_growthrate) #-188.6779

#interaction
model_FROG_growthrate_interaction<- effect('Region*Frog_popsource', model_FROG_growthrate,
                                             se=TRUE)
(model_FROG_growthrate_interactionplot<-plot(model_FROG_growthrate_interaction, multiline = TRUE) )






#######################################################################
############################################################################
################## CUSTOM CONTRASTS TOAD GROWTH RATE ####################
############################################################################
############################################################################

# http://www2.uaem.mx/r-mirror/web/packages/phia/vignettes/phia.pdf
## https://stats.stackexchange.com/questions/376441/interpreting-contrasts-for-non-significant-interaction-in-a-linear-mixed-model


testInteractions(model_GR3_noA13,1, pairwise="Region", across="Competition_level")



(model_GR_toads_model_GR3.means <- interactionMeans(model_GR3))

# adjusted mean plot
plot(model_GR_toads_model_GR3.means)

#model_GR1
(model_GR3.means <- interactionMeans(model_GR3))

# adjusted mean plot
plot(model_GR3.means)




########


emm3_model_growthrate=emmeans(model_GR3_noA13,specs=~ Competition_level*Region)


#718
capture.output(emm3_model_growthrate,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/contrast_model_growthrate_model_GR3_CI_718_19_01_22.txt")
write.csv(emm3_model_growthrate,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/contrast_model_growthrate_model_GR3_CI_718_19_01_22.csv")




#make into dataframe
emm3_model_growthratedf<-as.data.frame(emm3_model_growthrate)

#make new column
emm3_model_growthratedf<-cbind(Model = "model_GR_toads_model_GR3", emm3_model_growthratedf)

##### VERY IMPORTANT #######
#### NB ORDER MATTERS!!!!!!!!
#### must order the Rawdata file that ran the lmmr has HG first, then ML!!!!

#create custom contrasts
Low_controlHG<-rep(c(1,0), times = c(1,7))
FrogMLHG<-rep(c(0,1,0), times = c(1,1,6))
FrogHGHG<-rep(c(0,1,0), times = c(2,1,5))
High_controlHG<-rep(c(0,1,0), times = c(3,1,4))
Low_controlML<-rep(c(0,1,0), times = c(4,1,3))
FrogMLML<-rep(c(0,1,0), times = c(5,1,2))
FrogHGML<-rep(c(0,1,0), times = c(6,1,1))
High_controlML<-rep(c(0,1), times = c(7,1))

#create joined frog response variable
Frog_overallHG=(FrogMLHG+FrogHGHG)/2
Frog_overallML=(FrogMLML+FrogHGML)/2


### test frog region doesn't have effect for both toad regions
(contrast_emm3_model_growthrate_frogregion_mvt_19_01_22<-contrast(emm3_model_growthrate,adjust="mvt",method=list(
  "FrogMLHG - FrogHGHG" = FrogMLHG - FrogHGHG,
  "FrogMLML - FrogHGML" = FrogMLML-FrogHGML
  
)))


capture.output(contrast_emm3_model_growthrate_frogregion_mvt_19_01_22,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRatecontrast_emm3_model_growthrate_frogregion_mvt_19_01_22_model_GR3_718.txt")




### include both frog region
(contrast_emm3_model_growthrate_6tests_includefrogregion_bonf_values_19_01_22<-contrast(emm3_model_growthrate,adjust="bonferroni",method=list(
  "FrogHGHG - FrogHGML" = FrogHGHG - FrogHGML,
  "FrogMLHG - FrogMLML" = FrogMLHG - FrogMLML,
  "FrogHGHG - High_controlHG" = FrogHGHG - High_controlHG,
  "FrogMLHG - High_controlHG" = FrogMLHG - High_controlHG,
  "FrogHGML - High_controlML" = FrogHGML - High_controlML,
  "FrogMLML - High_controlML" = FrogMLML - High_controlML
  
)))


#718
capture.output(contrast_emm3_model_growthrate_6tests_includefrogregion_bonf_values_19_01_22,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/contrast_emm3_model_growthrate_6tests_includefrogregion_bonf_values_19_01_22_model_GR3_718.txt")
write.csv(contrast_emm3_model_growthrate_6tests_includefrogregion_bonf_values_19_01_22,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/contrast_emm3_model_growthrate_6tests_includefrogregion_bonf_values_19_01_22_model_GR3_718.csv")



### include both frog region
(contrast_emm3_model_growthrate_6tests_includefrogregion_mvt_values_19_01_22<-contrast(emm3_model_growthrate,adjust="mvt",method=list(
  "FrogHGHG - FrogHGML" = FrogHGHG - FrogHGML,
  "FrogMLHG - FrogMLML" = FrogMLHG - FrogMLML,
  "FrogHGHG - High_controlHG" = FrogHGHG - High_controlHG,
  "FrogMLHG - High_controlHG" = FrogMLHG - High_controlHG,
  "FrogHGML - High_controlML" = FrogHGML - High_controlML,
  "FrogMLML - High_controlML" = FrogMLML - High_controlML
  
)))


#718
capture.output(contrast_emm3_model_growthrate_6tests_includefrogregion_mvt_values_19_01_22,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/contrast_emm3_model_growthrate_6tests_includefrogregion_mvt_values_19_01_22_model_GR3_718.txt")
write.csv(contrast_emm3_model_growthrate_6tests_includefrogregion_mvt_values_19_01_22,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/contrast_emm3_model_growthrate_6tests_includefrogregion_mvt_values_19_01_22_model_GR3_718.csv")





### include both frog region
(contrast_emm3_model_growthrate_8tests_includefrogregion_anbtwnfrog_mvt_values_19_01_22<-contrast(emm3_model_growthrate,adjust="mvt",method=list(
  "FrogHGHG - FrogHGML" = FrogHGHG - FrogHGML,
  "FrogMLHG - FrogMLML" = FrogMLHG - FrogMLML,
  "FrogHGHG - High_controlHG" = FrogHGHG - High_controlHG,
  "FrogMLHG - High_controlHG" = FrogMLHG - High_controlHG,
  "FrogHGML - High_controlML" = FrogHGML - High_controlML,
  "FrogMLML - High_controlML" = FrogMLML - High_controlML,
  "FrogMLHG - FrogHGHG" = FrogMLHG - FrogHGHG,
  "FrogMLML - FrogHGML" = FrogMLML-FrogHGML
  
)))


#718
#capture.output(contrast_emm3_model_growthrate_8tests_includefrogregion_anbtwnfrog_mvt_values_19_01_22,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/contrast_emm3_model_growthrate_8tests_includefrogregion_anbtwnfrog_mvt_values_19_01_22_model_GR3_718.txt")
write.csv(contrast_emm3_model_growthrate_8tests_includefrogregion_anbtwnfrog_mvt_values_19_01_22,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/contrast_emm3_model_growthrate_8tests_includefrogregion_anbtwnfrog_mvt_values_19_01_22_model_GR3_718.csv")























###########################
############# PLOTS #############
###############################
labels <- c(HG = "Haida Gwaii Frogs", ML = "Mainland Frogs")

#data_frogs_long_enviro_sumDD_01_05_20$GR_frog_finalmin_intial_degreedays_mgDD[data_frogs_long_enviro_sumDD_01_05_20$GR_frog_finalmin_intial_degreedays_mgDD == 0] <- NA

#ylim1 = boxplot.stats(data_frogs_long_enviro_sumDD_01_05_20$GR_frog_finalmin_intial_degreedays_mgDD)$stats[c(1, 5)]



png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplotfrog_GR_final_days_mgDay_nofacet_360.png", width = 200, height = 150,units="mm",res=600)
(frogs_GR_final_days_mgDay_nofacet_29_01_22<-
    data_frogs_long_enviro_sumDD_01_05_20_2%>%
    ggplot(aes(x=Region, y=GR_final_days_mgDay, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    labs(y="Frog growth rate (mg/day)")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #          labels=c("Haida Gwaii", "Mainland"),
    #              guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 15)))
dev.off()

ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplotfrog_GR_final_days_mgDay_nofacet_360.svg", width = 200, height = 150,units="mm")
(frogs_GR_final_days_mgDay_nofacet_29_01_22<-
    data_frogs_long_enviro_sumDD_01_05_20_2%>%
    ggplot(aes(x=Region, y=GR_final_days_mgDay, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    labs(y="Frog growth rate (mg/day)")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #          labels=c("Haida Gwaii", "Mainland"),
    #              guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 15)))
dev.off()


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplotfrog_GR_final_days_mgDay_logscale_nofacet_360.png", width = 200, height = 150, units = 'mm', res = 600)
(frogs_GR_final_days_mgDay_logscale_nofacet_29_01_22<-
    data_frogs_long_enviro_sumDD_01_05_20_2%>%
    ggplot(aes(x=Region, y=GR_final_days_mgDay, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    scale_y_continuous(trans = 'log10')+
    annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    labs(y="Frog growth rate (mg/day)")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #          labels=c("Haida Gwaii", "Mainland"),
    #              guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 15)))
dev.off()

ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplotfrog_GR_final_days_mgDay_logscale_nofacet_360.svg", width = 200, height = 150, units = 'mm')
(frogs_GR_final_days_mgDay_logscale_nofacet_29_01_22<-
    data_frogs_long_enviro_sumDD_01_05_20_2%>%
    ggplot(aes(x=Region, y=GR_final_days_mgDay, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    scale_y_continuous(trans = 'log10')+
    annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    labs(y="Frog growth rate (mg/day)")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #          labels=c("Haida Gwaii", "Mainland"),
    #              guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 15)))
dev.off()












(frogs_growtrate_plot_01_05_20<- ggplot(data_frogs_long_enviro_sumDD_01_05_20, aes(x=Region, y=GR_frog_finalmin_intial_degreedays_mgDD)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    coord_cartesian(ylim = ylim1*1.005)+
    labs(x="Toad Region")+
    labs(y="Frog growth rate (mg/degree day)")+
    facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_fill_manual("grey")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()))


#################
###3. MEDIAN TIME to meta #####
################


######### shortcut #####
##################

data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large.csv",header=T,row.names=NULL,sep=",")

data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large$Tadpoles_added_as_top.up_june7thand8th_percapita<-data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large$Tadpoles_added_as_top.up_june7thand8th



############
#### join with mortality #######
#############################



##########################
#### short dataset
##################
subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs

subset_dataRGRdd_frogs2$Tank<-subset_dataRGRdd_frogs2$ï..Tank


#order levels of factor
subset_dataRGRdd_frogs2$Treatment_nameC<- factor(subset_dataRGRdd_frogs2$Treatment_nameC, levels = c("HGF_HGT","HGF_MLT","MLF_MLT","MLF_HGT"))
subset_dataRGRdd_frogs2 <- droplevels(subset_dataRGRdd_frogs2)


#NB two metamorph numbers for 23.8.18 - some were weighed and then realized that some were left out at the end
#X23.8.18_endexperiment_number_metamorphs <- ones that were found when we emptied the tanks - not weighed
#X23.8.18_number_of_metamorphs - number that were weighed


subset_dataRGRdd_frogs2$Frogmortality<-subset_dataRGRdd_frogs2$Total_number_individuals_start-subset_dataRGRdd_frogs2$Survived_counts_weighedandkilled

subset_dataRGRdd_frogs2$Frogmortality_percapita<-subset_dataRGRdd_frogs2$Frogmortality/36

subset_dataRGRdd_frogs2$Frogmortality_noperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_yesperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$Perished_total_onsideoftank+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_tadsadded_yesperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$Perished_total_onsideoftank+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

#remove negative value
subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished[subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished < 0] <- NA






####################################
##### join with morality data
subset_dataRGRdd_frogs2_sub<-subset_dataRGRdd_frogs2[c(233:239)]

data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2<-left_join(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large,subset_dataRGRdd_frogs2_sub,by="Tank")






########################
####### this is how i calcualted median days #########
#############

subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs

#subset_dataRGRdd_frogs2$Tank<-subset_dataRGRdd_frogs2$?..Tank


names(subset_dataRGRdd_frogs2)[names(subset_dataRGRdd_frogs2) == "?..Tank"] <- "Tank"



data_frogs_long_enviro_sumDD_01_05_20<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/data_frogs_long_enviro_sumDD_01_05_20.csv",header=T,row.names=NULL,sep=",") 

#make each toad have its own row and then run the code below
data_frogs_long_enviro_sumDD_01_05_20_indivrows2 <- as.data.frame(lapply(data_frogs_long_enviro_sumDD_01_05_20, rep, data_frogs_long_enviro_sumDD_01_05_20$Count_juvfrog_emerged))


#calc median
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2<-setDT(data_frogs_long_enviro_sumDD_01_05_20_indivrows2)[,list(MeanDays=mean(Days), MaxDays=max(Days), MinDays=min(Days), MedianDays=as.numeric(median(Days)), StdDays=sd(Days)), by=Tank]

#ry chaning tank to chracter to see if that fixes error
subset_dataRGRdd_frogs2$Tank<-as.character(subset_dataRGRdd_frogs2$Tank)
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2$Tank<-as.character(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2$Tank)


myvars<- c("Tank", "Population" ,"Region","Block","Frog_popsource","Total_number_individuals_start","Tadpoles_added_as_top.up_june7thand8th"          
           ,"Average_weight_of_tadpole_replacements_june7thand8th"                      
           , "Total_chla_conc_8.6.18_ug.l"                                               
           , "Total_chla_conc_26.7.18_ug.l"   
           ,"Daysbeforestartedexpmt"                                                    
           , "Daysincaptivitybeforeexpmtstart"                                           
           , "Daysbeforeminusdayscap"                                                    
           , "Daysbeforedividedayscap"
           ,"May_MaxwaterTemp"                                                          
           ,"May_MinwaterTemp"                                                          
           ,"May_MeanwaterTemp"                                                         
           ,"June_MaxwaterTemp"                                                         
           ,"June_MinwaterTemp"                                                         
           ,"June_MeanwaterTemp"                                                        
           ,"July_MaxwaterTemp"                                                         
           ,"July_MinwaterTemp"                                                         
           ,"July_MeanwaterTemp"                                                        
           ,"August_MaxwaterTemp"                                                       
           ,"August_MinwaterTemp"                                                       
           ,"August_MeanwaterTemp"                                                      
           ,"Max_averageTemp"                                                           
           ,"Min_averageTemp"                                                           
           ,"Mean_averageTemp"
           
           ,"Perished_total_onsideoftank"
           ,"Survived_counts_weighedandkilled"                                
           
           ,"Survived_and_perished"                                                     
           , "DIED_unexplaineddeath"                                                     
           , "DIED_no_extras"                                                            
           , "DIED_no_extras_divided_bystartingnumber"                                   
           , "DIED_all"                                                                  
           , "Extra_number_at_end"
           ,"X23.8.18_endexperiment_number_tadpoles_metas_excluding_adults_weighedmetas"
           ,"X23.8.18_endexperiment_number_metamorphs"                                  
           ,"X23.8.18_endexperiment_number_tadpoles"    
           , "X25.5.18_experiment_start_tapole_av_individual_weight"                     
           , "FROG_25.5.18_experiment_start_tadpole_av_individual_weight"  
           ,"Running_total_of_adults_emerged" ) 

subset_dataRGRdd_frogs2_sub<-subset_dataRGRdd_frogs2[myvars]
head(subset_dataRGRdd_frogs2_sub)


#join to large dataset - use sub to deal with error and use for plotting only 
#data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large<-left_join(subset_dataRGRdd_frogs2_sub,data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2,by="Tank")



#write.csv(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large,"data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large.csv")


############## 
####### SCALING ###########
###############


#################
########## mean sd #########
########################


data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2_noNA<-data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2[!(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$MedianDays=="NA"),]

dim(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2_noNA)

MedianDays <- data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2_noNA %>%
  group_by(Frog_popsource,Region)

MedianDays<-MedianDays %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), MedianDays)

MedianDays<-as.data.frame(MedianDays)
MedianDays

write.csv(MedianDays,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/Mean_sd_se_FROG_MedianDays.csv")



########## HG frog ####
71.12500-82.92857

((71.12500-82.92857)/82.92857)*100

############## 
####### ANOVA ###########
###############

### very omprotnaty
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Frog_popsource<- factor(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Frog_popsource, levels = c("ML","HG"))
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2)


data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Toad_Cooccurrence_History<- factor(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Toad_Cooccurrence_History, levels = c("Long","Short"))
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2)


data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Region<- factor(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Region, levels = c("ML","HG"))
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2)

data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Block<-as.factor(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Block)


model_FMD1<-lmer(
  MedianDays~
    Region*Frog_popsource+ 
    Tadpoles_added_as_top.up_june7thand8th_percapita+ 
    #FROG_25.5.18_experiment_start_tadpole_av_individual_weight+ raises AIC
    Frogmortality_percapita+
    Mean_averageTemp+
    Block+
    (1|Population),
  data=data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2,na.action=na.exclude,REML=FALSE,control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'),check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

Anova(model_FMD1)
AIC(model_FMD1) # 193.7084
summary(model_FMD1)


#360
capture.output(anova(model_FMD1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/anova_mediandaysmeta_model_FMD1_360.txt")
capture.output(Anova(model_FMD1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/Anova_chisq_mediandaysmeta_model_FMD1_360.txt")
write.csv(anova(model_FMD1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/anova_mediandaysmeta_model_FMD1_360.csv")
write.csv(Anova(model_FMD1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/Anova_chisq_mediandaysmeta_model_FMD1_360.csv")
capture.output(summary(model_FMD1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/summary_mediandaysmeta_model_FMD1_360.txt")

capture.output(summary(model_FMD1),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/summary_mediandaysmeta_model_FMD1_360.csv")


tab_model(model_FMD1, file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/tab_model_mediandaysmeta_model_FMD1_360.doc")


tab_model(model_FMD1,show.df = TRUE,p.val = "kr", file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/tab_model_mediandaysmeta_model_FMD1_360_df_pkr.doc")





### test residuals - mostly ok
qqnorm(resid(model_FMD1))
hist(resid(model_FMD1))
plot(model_FMD1)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/Residualsplot_model_FMD1_360.png", width = 12, height = 6.5, units = 'in', res = 300)
plot(model_FMD1)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/Residualshist_model_FMD1_360.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(resid(model_FMD1))
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/QQnorm_model_FMD1_360.png", width = 12, height = 6.5, units = 'in', res = 300)
qqnorm(resid(model_FMD1))
dev.off()



(model_FMD1.means <- interactionMeans(model_FMD1))
plot(model_FMD1.means)

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/interactionMeans_model_FMD1_360.png", width = 12, height = 6.5, units = 'in', res = 300)
(model_FMD1.means <- interactionMeans(model_FMD1))
plot(model_FMD1.means)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/interactionMeans_model_FMD1_360.svg", width = 12, height = 6.5)
(model_FMD1.means <- interactionMeans(model_FMD1))
plot(model_FMD1.means)
dev.off()


par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FMD1)



png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/visreg_model_FMD1_360.png", width = 8, height = 6.5, units = 'in', res = 300)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FMD1)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/visreg_model_FMD1_360.svg", width = 8, height = 6.5)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FMD1)
dev.off()




L.S_model_FMD1 <- pairs(lsmeans(model_FMD1, ~ Region | Frog_popsource))
test(L.S_model_FMD1, adjust = "tukey")

capture.output(test(L.S_model_FMD1, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/L.S_model_FMD1_360.txt")
write.csv(test(L.S_model_FMD1, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/L.S_model_FMD1_360.csv")

L.S_model_FMD1_2 <- pairs(lsmeans(model_FMD1, ~  Frog_popsource | Region))
test(L.S_model_FMD1_2, adjust = "tukey")

capture.output(test(L.S_model_FMD1_2, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/L.S_model_FMD1_2_360.txt")
write.csv(test(L.S_model_FMD1_2, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/L.S_model_FMD1_2_360.csv")
















#############################
## best 04.05.20 ######
##########################
model_FROG_mediandays<-lmer(
  MedianDays~
    Region*Frog_popsource+ 
    Tadpoles_added_as_top.up_june7thand8th+ 
    #FROG_25.5.18_experiment_start_tadpole_av_individual_weight+ raises AIC
    Mean_averageTemp+
    (1|Population)+ 
    (1|Block),
  data=data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large,na.action=na.exclude,REML=FALSE,control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'),check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

anova(model_FROG_mediandays)
visreg(model_FROG_mediandays)
AIC(model_FROG_mediandays) # -67.96858

#interaction
model_FROG_mediandays_interaction<- effect('Region*Frog_popsource', model_FROG_mediandays,
                                           se=TRUE)
(model_FROG_mediandays_interactionplot<-plot(model_FROG_mediandays_interaction, multiline = TRUE) )



###########################
############# PLOTS #############
###############################

labels <- c(HG = "Haida Gwaii Frogs", ML = "Mainland Frogs")


#ylim1 = boxplot.stats(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large$MedianDays)$stats[c(1, 5)]

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplot_frogs_mediandays_nofacet_29_01_22_36.png", width = 200, height = 150, units = 'mm', res = 600)
(boxplot_frogs_mediandays_nofacet_29_01_22<- 
    ggplot(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2, aes(x=Region, y=MedianDays, fill= Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    
    labs(x="Toad origin")+
    labs(y="Median time to frog metamorphosis (days)")+
 #   scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad origin",breaks=c("HG", "ML"),
    #                  labels=c("Haida Gwaii", "Mainland"))+
   # facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                                      labels=c("Haida Gwaii","Mainland" ))+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",
                      breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    #scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                      #    labels=c("Haida Gwaii", "Mainland"),
                    #      guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 15)))
dev.off()



ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplot_frogs_mediandays_nofacet_29_01_22_36.svg", width = 200, height = 150, units = 'mm')
(boxplot_frogs_mediandays_nofacet_29_01_22<- 
    ggplot(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2, aes(x=Region, y=MedianDays, fill= Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    
    labs(x="Toad origin")+
    labs(y="Median time to frog metamorphosis (days)")+
   # scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad origin",breaks=c("HG", "ML"),
     #                 labels=c("Haida Gwaii", "Mainland"))+
    # facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                               labels=c("Haida Gwaii","Mainland" ))+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",
                      breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    #scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #    labels=c("Haida Gwaii", "Mainland"),
    #      guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 15)))
dev.off()








(frogs_mediandays_plot_03_05_20<- ggplot(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2, aes(x=Region, y=MedianDays)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    labs(x="Toad Region")+
    labs(y="Median time to frog metamorphosis (days)")+
    facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_fill_manual("grey")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()))





(frogs_meanndays_plot_03_05_20<- ggplot(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2, aes(x=Region, y=MeanDays)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    labs(x="Toad Region")+
    labs(y="Mean time to frog metamorphosis (days)")+
    facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_fill_manual("grey")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()))

###########################################
######################### OLD ########################
#################################################

###########################################
######################### OLD ########################
#################################################

#shortcut
subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs



###############   below to see how I did it

dataRGRdd<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/megdata_RGRdd_12_11_19_treatment4.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs
#joinedlong_MSGR<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/joinedlong_MSGR.csv",header=T,row.names=NULL,sep=",") 


dataRGRdd$Perished_total_onsideoftankpercapita<-dataRGRdd$Perished_total_onsideoftank/dataRGRdd$Total_number_individuals_start

#days during experiment - 25.5.18-23.8.18
dataRGRdd$Days19.7justexperiment<-(as.numeric(dataRGRdd$Days19.7justexperiment<-"55"))
dataRGRdd$Days27.7justexperiment<-(as.numeric(dataRGRdd$Days27.7justexperiment<-"63")) #just killed didn't weigh
dataRGRdd$Days31.7justexperiment<-(as.numeric(dataRGRdd$Days31.7justexperiment<-"67"))
dataRGRdd$Days3.8justexperiment<-(as.numeric(dataRGRdd$Days3.8justexperiment<-"70"))
dataRGRdd$Days7.8justexperiment<-(as.numeric(dataRGRdd$Days7.8justexperiment<-"74"))
dataRGRdd$Days10.8justexperiment<-(as.numeric(dataRGRdd$Days10.8justexperiment<-"77"))
dataRGRdd$Days13.8justexperiment<-(as.numeric(dataRGRdd$Days13.8justexperiment<-"80"))
dataRGRdd$Days16.8justexperiment<-(as.numeric(dataRGRdd$Days16.8justexperiment<-"83"))
dataRGRdd$Days20.8justexperiment<-(as.numeric(dataRGRdd$Days20.8justexperiment<-"87"))
dataRGRdd$Days23.8justexperiment<-(as.numeric(dataRGRdd$Days23.8justexperiment<-"90"))

#mass specific growth rate
dataRGRdd$finalminusinitial_19_7_dividedays<-dataRGRdd$finalminusintial_19_7 / dataRGRdd$Days19.7justexperiment   
dataRGRdd$finalminusinitial_31_7_dividedays<-dataRGRdd$finalminusintial_31_7 / dataRGRdd$Days31.7justexperiment  
dataRGRdd$finalminusinitial_3_8_dividedays<-dataRGRdd$finalminusintial_3_8 / dataRGRdd$Days3.8justexperiment   
dataRGRdd$finalminusinitial_7_8_dividedays<-dataRGRdd$finalminusintial_7_8 / dataRGRdd$Days7.8justexperiment 
dataRGRdd$finalminusinitial_10_8_dividedays<-dataRGRdd$finalminusintial_10_8 / dataRGRdd$Days10.8justexperiment 
dataRGRdd$finalminusinitial_13_8_dividedays<-dataRGRdd$finalminusintial_13_8 / dataRGRdd$Days13.8justexperiment 
dataRGRdd$finalminusinitial_16_8_dividedays<-dataRGRdd$finalminusintial_16_8 / dataRGRdd$Days16.8justexperiment 
dataRGRdd$finalminusinitial_20_8_dividedays<-dataRGRdd$finalminusintial_20_8 / dataRGRdd$Days20.8justexperiment 
dataRGRdd$finalminusinitial_23_8_dividedays<-dataRGRdd$finalminusintial_23_8 / dataRGRdd$Days23.8justexperiment 

#average mass specific growth rate per tank
dataRGRdd$averagemassspecificgrowthrate<-(sum(dataRGRdd$finalminusinitial_19_7_dividedays, dataRGRdd$finalminusinitial_31_7_dividedays                                        ,dataRGRdd$finalminusinitial_3_8_dividedays                                      
                                              , dataRGRdd$finalminusinitial_7_8_dividedays                                           
                                              , dataRGRdd$finalminusinitial_10_8_dividedays                                          
                                              ,dataRGRdd$finalminusinitial_13_8_dividedays                                          
                                              ,dataRGRdd$finalminusinitial_16_8_dividedays                                          
                                              , dataRGRdd$finalminusinitial_20_8_dividedays                                          
                                              ,dataRGRdd$finalminusinitial_23_8_dividedays )/dataRGRdd$Count_nonzero_juvenileweights)

dataRGRdd$averagemassspecificgrowthrate[ dataRGRdd$averagemassspecificgrowthrate == "Inf" ] <- NA


#mass specific growth rate DEGREEdays
dataRGRdd$finalminusinitial_19_7_divideDEGREEdays<-dataRGRdd$finalminusintial_19_7 / dataRGRdd$SumDD_19_7  
dataRGRdd$finalminusinitial_31_7_divideDEGREEdays<-dataRGRdd$finalminusintial_31_7 / dataRGRdd$SumDD_31_7  
dataRGRdd$finalminusinitial_3_8_divideDEGREEdays<-dataRGRdd$finalminusintial_3_8 / dataRGRdd$SumDD_3_8   
dataRGRdd$finalminusinitial_7_8_divideDEGREEdays<-dataRGRdd$finalminusintial_7_8 / dataRGRdd$SumDD_7_8
dataRGRdd$finalminusinitial_10_8_divideDEGREEdays<-dataRGRdd$finalminusintial_10_8 / dataRGRdd$SumDD_10_8 
dataRGRdd$finalminusinitial_13_8_divideDEGREEdays<-dataRGRdd$finalminusintial_13_8 / dataRGRdd$SumDD_13_8
dataRGRdd$finalminusinitial_16_8_divideDEGREEdays<-dataRGRdd$finalminusintial_16_8 / dataRGRdd$SumDD_16_8
dataRGRdd$finalminusinitial_20_8_divideDEGREEdays<-dataRGRdd$finalminusintial_20_8 / dataRGRdd$SumDD_20_8 
dataRGRdd$finalminusinitial_23_8_divideDEGREEdays<-dataRGRdd$finalminusintial_23_8 / dataRGRdd$SumDD_23_8

#average mass specific growth rate per tank DEGREEdays
dataRGRdd$averagemassspecificgrowthrateDEGREEdays<-(sum(dataRGRdd$finalminusinitial_19_7_divideDEGREEdays, dataRGRdd$finalminusinitial_31_7_divideDEGREEdays                                        ,dataRGRdd$finalminusinitial_3_8_divideDEGREEdays                                      
                                                        , dataRGRdd$finalminusinitial_7_8_divideDEGREEdays                                           
                                                        , dataRGRdd$finalminusinitial_10_8_divideDEGREEdays                                          
                                                        ,dataRGRdd$finalminusinitial_13_8_divideDEGREEdays                                          
                                                        ,dataRGRdd$finalminusinitial_16_8_divideDEGREEdays                                          
                                                        , dataRGRdd$finalminusinitial_20_8_divideDEGREEdays                                          
                                                        ,dataRGRdd$finalminusinitial_23_8_divideDEGREEdays )/dataRGRdd$Count_nonzero_juvenileweights)

dataRGRdd$averagemassspecificgrowthrateDEGREEdays[ dataRGRdd$averagemassspecificgrowthrateDEGREEdays == "Inf" ] <- NA

dataRGRdd$Proportionemergedfrom_nondead19.7<-dataRGRdd$X19.7.18_._number_adults/dataRGRdd$Survived_counts_weighedandkilled
dataRGRdd$Proportionemergedfrom_nondead27.7<-dataRGRdd$X27.7.18_._NUMBER_ADULTS._WERE_NOT_WEIGHED/dataRGRdd$Survived_counts_weighedandkilled
dataRGRdd$Proportionemergedfrom_nondead31.7<-dataRGRdd$X31.7.18_._number_adults/dataRGRdd$Survived_counts_weighedandkilled
dataRGRdd$Proportionemergedfrom_nondead3.8<-dataRGRdd$X3.8.18_._number_adults/dataRGRdd$Survived_counts_weighedandkilled
dataRGRdd$Proportionemergedfrom_nondead7.8<-dataRGRdd$X7.8.18_._number_adults/dataRGRdd$Survived_counts_weighedandkilled
dataRGRdd$Proportionemergedfrom_nondead10.8<-dataRGRdd$X10.8.18_._number_adults/dataRGRdd$Survived_counts_weighedandkilled
dataRGRdd$Proportionemergedfrom_nondead13.8<-dataRGRdd$X13.8.18_._number_adults/dataRGRdd$Survived_counts_weighedandkilled
dataRGRdd$Proportionemergedfrom_nondead16.8<-dataRGRdd$X16.8.18_._number_adults/dataRGRdd$Survived_counts_weighedandkilled
dataRGRdd$Proportionemergedfrom_nondead20.8<-dataRGRdd$X20.8.18_._number_adults/dataRGRdd$Survived_counts_weighedandkilled
dataRGRdd$Proportionemergedfrom_nondead23.8<-dataRGRdd$X23.8.18_._number_adults/dataRGRdd$Survived_counts_weighedandkilled


head(dataRGRdd)
str(dataRGRdd)


totalalgaeconc<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/totalalgaeconc.csv",header=T,row.names=NULL,sep=",")

dataRGRdd<-left_join(dataRGRdd,totalalgaeconc)

dataRGRdd$Treatment_nameC<- factor(dataRGRdd$Treatment_nameC, levels = c("MLT","MLT_Highcontrol","MLF_MLT","HGF_MLT","HGT","HGT_Highcontrol","HGF_HGT","MLF_HGT"))
dataRGRdd <- droplevels(dataRGRdd)

dataRGRdd$Treatment_name<- factor(dataRGRdd$Treatment_name, levels = c("MLT","MLF_MLT","HGF_MLT","HGT","HGF_HGT","MLF_HGT"))
dataRGRdd <- droplevels(dataRGRdd)

dataRGRdd$Sitename<- factor(dataRGRdd$Sitename, levels = c("Lucille","Alice","Lost","Mayer","Evac","Chickundal","Port","Squamish"))
dataRGRdd <- droplevels(dataRGRdd)

dataRGRdd$Treatment_three<- factor(dataRGRdd$Treatment_three, levels = c("Low_control","Frog","High_control"))
dataRGRdd <- droplevels(dataRGRdd)


dataRGRdd$Treatment<- factor(dataRGRdd$Treatment, levels = c("Mayer","Mayer_80control","Mayer_160control",    
                                                             "Evac","Evac_80control","Evac_160control",                           "Chickundal","Chickundal_80control","Chickundal_160control", 
                                                             "Lucille","Lucille_80control","Lucille_160control",
                                                             "Alice","Alice_80control","Alice_160control",                       "Lost","Lost_80control","Lost_160control",                           
                                                             "Port","Squamish"))
dataRGRdd <- droplevels(dataRGRdd)

dataRGRdd$Popsource_control<- factor(dataRGRdd$Popsource_control, levels = c("HG","HG_control","HG_control_160","ML","ML_control","ML_control_160","Port","Squamish"))
dataRGRdd <- droplevels(dataRGRdd)


#remove toads
subset_dataRGRdd_frogs<-subset(dataRGRdd, Toad_or_frog=="Frog", drop=TRUE)



# don't use b/c popsource is wrong!!!
#write.csv(subset_dataRGRdd_frogs,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs.csv")




################################################################################
################################## ORDER LEVELS#############################
############################################################################

#order levels of factor
subset_dataRGRdd_frogs2$Treatment_nameC<- factor(subset_dataRGRdd_frogs2$Treatment_nameC, levels = c("HGF_HGT","HGF_MLT","MLF_MLT","MLF_HGT"))
subset_dataRGRdd_frogs2 <- droplevels(subset_dataRGRdd_frogs2)

#subset_dataRGRdd_frogs2$Treatment_three<- factor(subset_dataRGRdd_frogs2$Treatment_three, levels = c("Low_control","Frog","High_control"))
#subset_dataRGRdd_frogs2 <- droplevels(subset_dataRGRdd_frogs2)
#View(subset_dataRGRdd_frogs2)




####


#plots




###***
#more frogs died with ML T than HG T, suggesting that ML T is a better competitor!
plot(subset_dataRGRdd_frogs2$Treatment_nameC,subset_dataRGRdd_frogs2$DIED_all)



### ***
#more frogs emerged when with HG T than with ML T, suggesting that ML T is a better competitor!!
plot(subset_dataRGRdd_frogs2$Treatment_nameC,subset_dataRGRdd_frogs2$Survived_counts_weighedandkilled)



#slightly more frogs from HG died
plot(subset_dataRGRdd_frogs2$Frog_popsource,subset_dataRGRdd_frogs2$Survived_counts_weighedandkilled)


#appers as though frogs weighed less with HG T than with ML toad.... seems odd
plot(subset_dataRGRdd_frogs2$Treatment_nameC,subset_dataRGRdd_frogs2$Manual_running_total_average_weight_adults)


plot(subset_dataRGRdd_frogs2$Region,subset_dataRGRdd_frogs2$Manual_running_total_average_weight_adults)



### counts


#################################
################# 1. MORTALITY ###############
#################################################
rm(list=ls())

subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs

#order levels of factor
subset_dataRGRdd_frogs2$Treatment_nameC<- factor(subset_dataRGRdd_frogs2$Treatment_nameC, levels = c("HGF_HGT","HGF_MLT","MLF_MLT","MLF_HGT"))
subset_dataRGRdd_frogs2 <- droplevels(subset_dataRGRdd_frogs2)


#NB two metamorph numbers for 23.8.18 - some were weighed and then realized that some were left out at the end
#X23.8.18_endexperiment_number_metamorphs <- ones that were found when we emptied the tanks - not weighed
#X23.8.18_number_of_metamorphs - number that were weighed


subset_dataRGRdd_frogs2$Frogmortality<-subset_dataRGRdd_frogs2$Total_number_individuals_start-subset_dataRGRdd_frogs2$Survived_counts_weighedandkilled

subset_dataRGRdd_frogs2$Frogmortality_percapita<-subset_dataRGRdd_frogs2$Frogmortality/36

subset_dataRGRdd_frogs2$Frogmortality_noperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_yesperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$Perished_total_onsideoftank+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_tadsadded_yesperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$Perished_total_onsideoftank+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

#remove negative value
subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished[subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished < 0] <- NA


####################################################################
################################## ANOVA #############################
########################################################################

#Frogmortality_tadsadded_noperished

lmmer_model_frog_mortality_full_08_05_20<-lmer((Frogmortality_tadsadded_noperished)~
           Frog_popsource*Region+
        Tadpoles_added_as_top.up_june7thand8th+
                                            Mean_averageTemp+
                                            (1|Block)+
                                            (1|Population), 
 data=subset_dataRGRdd_frogs2,na.action=na.exclude,control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

anova(lmmer_model_frog_mortality_full_08_05_20) #significant region of toad - ML high deaths
summary(lmmer_model_frog_mortality_full_08_05_20)


#capture.output(lmmer_model_frog_mortality_03_05_20,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/lmmer_model_frog_mortality_03_05_20.txt")
#capture.output(anova(lmmer_model_frog_mortality_03_05_20),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/anova_lmmer_model_frog_mortality_03_05_20.txt")




#turn negative to NA
subset_dataRGRdd_frogs2$Frogmortality[subset_dataRGRdd_frogs2$Frogmortality < 0] <- NA


################# linear models

# region frog * region toad
#no warnings
lmmer_model_frog_mortality_03_05_20<-lmer((Frogmortality)~
                                      Frog_popsource*Region+
             Tadpoles_added_as_top.up_june7thand8th+
                                      Mean_averageTemp+
                                    (1|Block)+
                                  (1|Population), 
 data=subset_dataRGRdd_frogs2,na.action=na.exclude,control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

anova(lmmer_model_frog_mortality_03_05_20) #significant region of toad - ML high deaths
summary(lmmer_model_frog_mortality_03_05_20)


#capture.output(lmmer_model_frog_mortality_03_05_20,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/lmmer_model_frog_mortality_03_05_20.txt")
#capture.output(anova(lmmer_model_frog_mortality_03_05_20),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/anova_lmmer_model_frog_mortality_03_05_20.txt")

#linear model versiomn
lmmodel_frog_mortality_03_05_20<-lm((Frogmortality)~
                                    Frog_popsource*Region+
                                    Block+
                                    Population+
                                    Mean_averageTemp, 
    data=subset_dataRGRdd_frogs2,na.action=na.exclude)

anova(lmmodel_frog_mortality_03_05_20) #significant region of toad - ML high deaths
plot(lmmodel_frog_mortality_03_05_20)
visreg(lmmodel_frog_mortality_03_05_20)
summary(lmmodel_frog_mortality_03_05_20)
AIC(lmmodel_frog_mortality_03_05_20) # 192.5491
fit3.4<-fitted(lmmodel_frog_mortality_03_05_20)
resid3.4<-residuals(lmmodel_frog_mortality_03_05_20)
plot(fit3.4,resid3.4) 
residmodfitmod<-lm(resid3.4~fit3.4)
anova(residmodfitmod)
summary(residmodfitmod)
(slope <-  coef(residmodfitmod)[2] ) #don't transform


summary(lmmodel_frog_mortality_03_05_20)


####### died all
modelfrog<-lm((DIED_all)~Frog_popsource*Region+Block+Population+Mean_averageTemp, data=subset_dataRGRdd_frogs2,na.action=na.exclude)
anova(modelfrog) #significant region of toad - ML high deaths
plot(modelfrog)
visreg(modelfrog)
summary(modelfrog)
AIC(modelfrog) # 192.5491
fit3.4<-fitted(modelfrog)
resid3.4<-residuals(modelfrog)
plot(fit3.4,resid3.4) 
residmodfitmod<-lm(resid3.4~fit3.4)
anova(residmodfitmod)
summary(residmodfitmod)
(slope <-  coef(residmodfitmod)[2] ) #don't transform


L.S <- pairs(lsmeans(modelfrog, ~ Frog_popsource*Region))
test(L.S, adjust = "tukey")

################################################################################
################################## CUSTOM CONTRASTS ############################
############################################################################
#use modelfrog because other model with (1|pop)*interaction doesn't work

rm(emm3_modelfrog)
emm3_modelfrog=emmeans(lmmer_model_frog_mortality_full_08_05_20,specs=~ Frog_popsource*Region)


#emm3_modelfrog=emmeans(modelfrog,specs=~ Frog_popsource*Region)

#capture.output(emm3_modelfrog,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/emm3_modelfrog.txt")

#make into dataframe
emm3_modelfrogdf<-as.data.frame(emm3_modelfrog)

#make new column
emm3_modelfrogdf<-cbind(Model = "lmmer_model_frog_mortality_full_08_05_20", emm3_modelfrogdf)




#region HG, comp level low control - single out estimate for each level of region/competition to insect into contrast table later to calculate percent
frogHG_toadHG_modelfrogdf<-emm3_modelfrogdf[1,4]
frogML_toadHG_modelfrogdf<-emm3_modelfrogdf[2,4]
frogHG_toadML_modelfrogdf<-emm3_modelfrogdf[3,4]
frogML_toadML_modelfrogdf<-emm3_modelfrogdf[4,4]


#contrast(emm3_modelfrog, "consec",simple="each",combine=TRUE,adjust="mvt")#these do not give me the contrasts I want

#create custom contrasts
frogHG_toadHG<-rep(c(1,0), times = c(1,3))
frogML_toadHG<-rep(c(0,1,0), times = c(1,1,2))
frogHG_toadML<-rep(c(0,1,0), times = c(2,1,1))
frogML_toadML<-rep(c(0,1), times = c(3,1))

#create joined frog response variable
#frogML=(frogML_toadHG+frogML_toadML)/2
#frogHG=(frogHG_toadML+frogHG_toadHG)/2
toadML=(frogHG_toadML+frogML_toadML)/2
toadHG=(frogHG_toadHG+frogML_toadHG)/2

#works!
# adjust = "mvt" adds multiple tests adjustment, could also use "bonferroni" but its too conservative https://cran.r-project.org/web/packages/emmeans/vignettes/confidence-intervals.html


(contrast_modelfrog_bonferroni<-contrast(emm3_modelfrog,adjust="bonferroni",method=list(
  "frogHG_toadHG - frogHG_toadML" = frogHG_toadHG - frogHG_toadML,
  "frogML_toadML - frogML_toadHG" = frogML_toadHG - frogML_toadML,
  "toadML - toadHG" = toadML - toadHG
  
))) #nothing significant

(contrast_modelfrog2<-contrast(emm3_modelfrog,method=list(
  "toadML - toadHG" = toadML - toadHG
))) #nothing significant

#(contrast_modelfrog<-contrast(emm3_modelfrog,adjust="mvt",method=list(
# "frogML - frogHG" = frogML - frogHG, #not different
#))) #nothing significant

#######################
##################################################### PERCENTS #############
####################

#make into dataframe
contrast_modelfrogdf<-as.data.frame(contrast_modelfrog_bonferroni)

#make new column
contrast_modelfrogdf<-cbind(Model = "lmmer_model_frog_mortality_full_08_05_20", contrast_modelfrogdf)

#capture.output(contrast_modelfrogdf,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/contrast_modelfrogdf_bonferroni.txt")

#HGLow_controlHG
contrast_modelfrogdf$emmean_HGLow_controlHG<-rep(HGLow_controlHG_modelfrogdf,length(contrast_modelfrogdf$p.value))
#percent
contrast_modelfrogdf$percent_HGLow_controlHG<-(contrast_modelfrogdf$estimate/contrast_modelfrogdf$emmean_HGLow_controlHG)*100
#value
(HGLow_controlHG_modelfrogdf_value<-contrast_modelfrogdf[c(3,7),c(1:9)])



#MLLow_controlML
contrast_modelfrogdf$emmean_MLLow_controlML<-rep(MLLow_controlML_modelfrogdf,length(contrast_modelfrogdf$p.value))
#percent
contrast_modelfrogdf$percent_MLLow_controlML<-(contrast_modelfrogdf$estimate/contrast_modelfrogdf$emmean_MLLow_controlML)*100
#value
(MLLow_controlML_modelfrogdf_value<-contrast_modelfrogdf[c(4,7),c(1:7,10:11)])

#HGFrogoverallHG
contrast_modelfrogdf$emmean_HGFrog_overallHG<-rep(HGFrog_overallHG_modelfrogdf,length(contrast_modelfrogdf$p.value))
#percent
contrast_modelfrogdf$percent_HGFrog_overallHG<-(contrast_modelfrogdf$estimate/contrast_modelfrogdf$emmean_HGFrog_overallHG)*100
#value
(HGFrog_overallHG_modelfrogdf_value<-contrast_modelfrogdf[c(5,8),c(1:7,12:13)])

#MLFrogoverallML
contrast_modelfrogdf$emmean_MLFrog_overallML<-rep(MLFrog_overallML_modelfrogdf,length(contrast_modelfrogdf$p.value))
#percent
contrast_modelfrogdf$percent_MLFrog_overallML<-(contrast_modelfrogdf$estimate/contrast_modelfrogdf$emmean_MLFrog_overallML)*100
#value
(MLFrog_overallML_modelfrogdf_value<-contrast_modelfrogdf[c(6,8),c(1:7,14:15)])

write.csv(contrast_modelfrogdf,"contrast_modelfrogdf.csv")

#bind together
HGLow_controlHG_modelfrogdf_value
MLLow_controlML_modelfrogdf_value
HGFrog_overallHG_modelfrogdf_value
MLFrog_overallML_modelfrogdf_value

(contrast_modelfrogdf_selected<-bind_rows(
  HGLow_controlHG_modelfrogdf_value,
  MLLow_controlML_modelfrogdf_value,
  HGFrog_overallHG_modelfrogdf_value,
  MLFrog_overallML_modelfrogdf_value))

write.csv(contrast_modelfrogdf_selected,"contrast_modelfrogdf_selected.csv")


###########################
############# PLOTS #############
###############################
labels <- c(HG = "Haida Gwaii Frogs", ML = "Mainland Frogs")

#turn negative to NA
subset_dataRGRdd_frogs2$Frogmortality[subset_dataRGRdd_frogs2$Frogmortality < 0] <- NA

subset_dataRGRdd_frogs2$Frogmortality_percapita[subset_dataRGRdd_frogs2$Frogmortality_percapita < 0] <- NA

ylim1 = boxplot.stats(subset_dataRGRdd_frogs2$Frogmortality)$stats[c(1, 5)]


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplot_Frogmortality_percapita_nofacet_29_01_22_36.png", width = 200, height = 150, units = 'mm', res = 600)
(boxplot_Frogmortality_percapita_nofacet_29_01_22<- 
    ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Frogmortality_percapita, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    labs(x="Toad origin")+
    labs(y="Frog mortality per capita")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                                  labels=c("Haida Gwaii", "Mainland"))+
       labs(x="Toad origin")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
   # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
     #                     labels=c("Haida Gwaii", "Mainland"),
      #                    guide=FALSE)+    
    #scale_fill_manual("grey")+
    ylim(0,0.5)+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))
dev.off()

ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplot_Frogmortality_percapita_nofacet_29_01_22_36.svg", width = 200, height = 150, units = 'mm')
(boxplot_Frogmortality_percapita_nofacet_29_01_22<- 
    ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Frogmortality_percapita, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    labs(x="Toad origin")+
    labs(y="Frog mortality per capita")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    ylim(0,0.5)+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #                     labels=c("Haida Gwaii", "Mainland"),
    #                    guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))
dev.off()



#Frogmortality_tadsadded_yesperished_percapita
png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplot_Frogmortality_tadsadded_yesperished_percapita_nofacet_29_01_22_36.png", width = 200, height = 150, units = 'mm', res = 600)
(boxplot_Frogmortality_tadsadded_yesperished_percapita_nofacet_29_01_22<- 
    ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Frogmortality_tadsadded_yesperished_percapita, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    labs(x="Toad origin")+
    labs(y="Frogmortality_tadsadded_yesperished_percapita")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #                     labels=c("Haida Gwaii", "Mainland"),
    #                    guide=FALSE)+    
    #scale_fill_manual("grey")+
    ylim(0,0.5)+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))
dev.off()



(frogs_mortality_01_05_20<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Frogmortality)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    labs(x="Toad Region")+
    labs(y="Frog mortality")+
    facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_fill_manual("grey")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()))

#Frogmortality_tadsadded_noperished
(frogs_mortality_06_05_20<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Frogmortality_tadsadded_noperished)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    labs(x="Toad Region")+
    labs(y="Frog mortality")+
    facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_fill_manual("grey")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()))

################################################################################
################################## 2. SURVIVAL COUNTS MODEL ####################
############################################################################

####################################################################
################################## ANOVA #############################
########################################################################

#Survived_counts_weighedandkilled
#no warning



modelfrog_countsemergerd<-lm((Survived_counts_weighedandkilled)~Frog_popsource*Region +Population +Block +Mean_averageTemp, data=subset_dataRGRdd_frogs2,na.action=na.exclude)
anova(modelfrog_countsemergerd) #significant region of toad - HG higher emergerd
plot(modelfrog_countsemergerd)
visreg(modelfrog_countsemergerd)
summary(modelfrog_countsemergerd)
AIC(modelfrog_countsemergerd) # 197.9177
fit3.4<-fitted(modelfrog_countsemergerd)
resid3.4<-residuals(modelfrog_countsemergerd)
plot(fit3.4,resid3.4) 
residmodfitmod<-lm(resid3.4~fit3.4)
anova(residmodfitmod)
summary(residmodfitmod)
(slope <-  coef(residmodfitmod)[2] ) #don't transform




### frog weight
#turn zeros to NA

subset_dataRGRdd_frogs2$Manual_running_total_average_weight_adults[subset_dataRGRdd_frogs2$Manual_running_total_average_weight_adults == 0] <- NA

#not significant
modelfrog_weights<-lm((Manual_running_total_average_weight_adults)~Frog_popsource*Region+FROG_25.5.18_experiment_start_tadpole_av_individual_weight+Max_averageTemp, data=subset_dataRGRdd_frogs2,na.action=na.exclude)
anova(modelfrog_weights) #no significant
plot(modelfrog_countsemergerd)
visreg(modelfrog_weights)
summary(modelfrog_weights)
AIC(modelfrog_weights) # -20.38992
fit3.4<-fitted(modelfrog_weights)
resid3.4<-residuals(modelfrog_weights)
plot(fit3.4,resid3.4) 
residmodfitmod<-lm(resid3.4~fit3.4)
anova(residmodfitmod)
summary(residmodfitmod)
(slope <-  coef(residmodfitmod)[2] ) #don't transform



modelfrog2<-lm(DIED_all~Treatment_nameC, data=subset_dataRGRdd_frogs2,na.action=na.exclude)
anova(modelfrog2)
plot(modelfrog2)
visreg(modelfrog2)

# slopes are the same but the intercepts are different
modelfrog_interaction<- effect('Frog_popsource*Region', modelfrog,
                               se=TRUE)
(modelfrog_interactionplot<-plot(modelfrog_interaction, multiline = TRUE) )




##############################

#### number metamorphosed ############
####
###########################

modelfrogmeta<-lm((Running_total_of_adults_emerged)~Frog_popsource*Region+Block+Population+Mean_averageTemp, data=subset_dataRGRdd_frogs2,na.action=na.exclude)
anova(modelfrogmeta) #significant region of toad - ML high deaths
plot(modelfrogmeta)
visreg(modelfrogmeta)
summary(modelfrog)
AIC(modelfrog) # 192.5491
fit3.4<-fitted(modelfrog)
resid3.4<-residuals(modelfrog)
plot(fit3.4,resid3.4) 
residmodfitmod<-lm(resid3.4~fit3.4)
anova(residmodfitmod)
summary(residmodfitmod)
(slope <-  coef(residmodfitmod)[2] ) #don't transform

modelfrogmeta

#################################################################################
################################## PLOTS #############################
############################################################################


##################### plots

labels <- c(HG = "Haida Gwaii Frogs", ML = "Mainland Frogs")


############## survivorship

ylim1 = boxplot.stats(subset_dataRGRdd_frogs2$Survived_counts_weighedandkilled)$stats[c(1, 5)]

(frogsemerged_nofrogregion_greyscale_facet_small<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Survived_counts_weighedandkilled)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    coord_cartesian(ylim = ylim1*1.05)+
    labs(x="Toad Region")+
    labs(y="Number of frogs metamorphosed by end of experiment")+
    facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_fill_manual("grey")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()))

(frogsemerged_nofrogregion_greyscale<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Survived_counts_weighedandkilled)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    labs(x="Toad Region")+
    labs(y="Number of frogs metamorphosed by end of experiment")+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_fill_manual("grey")+
    theme_bw())



### plot deaths

ylim1 = boxplot.stats(subset_dataRGRdd_frogs2$DIED_all)$stats[c(1, 5)]

(frogdeaths_notoadregion_greyscale_facet_small<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=DIED_all)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    labs(x="Toad Region")+
    labs(y="Frog mortality")+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_fill_manual("grey")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()))

(frogdeaths_notoadregion_greyscale- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=DIED_all)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    labs(x="Toad Region")+
    labs(y="Frog mortality")+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_fill_manual("grey")+
    theme_bw())


(frogdeaths<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=DIED_all,colour=Region)) +
    geom_boxplot() +
    geom_beeswarm()+
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    facet_wrap(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+    
    labs(x="Toad Region")+
    labs(y="Frog mortality")+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_colour_manual(values=c("#009E73", "#56B4E9"),guide=FALSE)+
    theme_bw())




(frogdeaths_notoadregion<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=DIED_all,colour=Region)) +
    geom_boxplot() +
    geom_beeswarm()+
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    labs(x="Toad Region")+
    labs(y="Frog mortality")+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_colour_manual(values=c("#009E73", "#56B4E9"),guide=FALSE)+
    theme_bw())

subset_dataRGRdd_frogs2$DIED_all<-as.numeric(subset_dataRGRdd_frogs2$DIED_all)

(frogdeaths_notoadregion_bar<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=DIED_all,fill=Region)) +
    geom_bar(stat="identity") +
    #geom_beeswarm()+
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    labs(x="Toad Region")+
    labs(y="Frog mortality")+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_fill_manual(values=c("#009E73", "#56B4E9"),guide=FALSE)+
    theme_bw())






(frogsemerged_nofrogregion<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Survived_counts_weighedandkilled,colour=Region)) +
    geom_boxplot() +
    geom_beeswarm()+
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    labs(x="Toad Region")+
    labs(y="Number of frogs emerged")+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_colour_manual(values=c("#009E73", "#56B4E9"),guide=FALSE)+
    theme_bw())




#################
#  Running_total_of_adults_emerged
#####################
ylim1 = boxplot.stats(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged)$stats[c(1, 5)]

(frogs_meta_nofrogregion_greyscale_facet_small<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Running_total_of_adults_emerged)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    coord_cartesian(ylim = ylim1*1.11)+
    labs(x="Toad Region")+
    labs(y="Number of frogs metamorphosed by end of experiment")+
    facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_fill_manual("grey")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()))


################################################
############## 3. FROG WEIGHT ##################
##############################################


#turn zero to NA


subset_dataRGRdd_frogs2$Manual_running_total_average_weight_adults[subset_dataRGRdd_frogs2$Manual_running_total_average_weight_adults== 0] <-NA


subset_dataRGRdd_frogs2$Manual_running_total_average_weight_adults2[subset_dataRGRdd_frogs2$Manual_running_total_average_weight_adults2== 0] <-NA

hist(subset_dataRGRdd_frogs2$Manual_running_total_average_weight_adults)

####################################################################
################################## ANOVA #############################
########################################################################






####################################################################
################################## PLOTS #############################
########################################################################



(frogweight_notoadregion<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Manual_running_total_average_weight_adults,colour=Region)) +
   geom_boxplot() +
   geom_point()+
   #geom_beeswarm()+
   #scale_colour_hue(l=50) +
   #coord_cartesian(ylim = ylim1*1.05)+
   labs(x="Toad Region")+
   labs(y="Juvenile frog weight (g)")+
   scale_x_discrete(breaks=c("HG",  "ML"),
                    labels=c("Haida Gwaii","Mainland" ))+
   scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                         labels=c("Haida Gwaii", "Mainland"),
                         guide=FALSE)+    
   scale_colour_manual(values=c("#009E73", "#56B4E9"),guide=FALSE)+
   theme_bw())

(frogweight<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Manual_running_total_average_weight_adults,colour=Region)) +
    geom_boxplot() +
    #geom_beeswarm()+
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    facet_wrap(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+    
    labs(x="Toad Region")+
    labs(y="Juvenile frog weight at emergence")+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                          labels=c("Haida Gwaii", "Mainland"),
                          guide=FALSE)+    
    scale_colour_manual(values=c("#009E73", "#56B4E9"),guide=FALSE)+
    theme_bw())


subset_dataRGRdd_frogs2$Running_total_of_adults_emerged[subset_dataRGRdd_frogs2$Running_total_of_adults_emerged== 0] <-NA


####################################################################
################################## 4. COUNTS #############################
########################################################################





####################################################################
################################## ANOVA #############################
########################################################################



####################################################################
################################## PLOTS #############################
########################################################################

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplot_frogcounts_nozeros_nofacet_29_01_22_36.png", width = 200, height = 150, units = 'mm', res = 600)
(boxplot_frogcounts_nozeros_nofacet_29_01_22_36<- 
   ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Running_total_of_adults_emerged,fill=Frog_popsource)) +
   geom_boxplot() +
   #geom_point()+
   #geom_beeswarm()+
   #scale_colour_hue(l=50) +
   #coord_cartesian(ylim = ylim1*1.05)+
   labs(x="Toad Region")+
   labs(y="Juvenile frogs emerged")+
   scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",
                                          breaks=c("HG", "ML"),
                                           labels=c("Haida Gwaii", "Mainland"))+
   scale_x_discrete(breaks=c("HG",  "ML"),
                    labels=c("Haida Gwaii","Mainland" ))+
   #scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #                     labels=c("Haida Gwaii", "Mainland"),
     #                    guide=FALSE)+    
   theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 15)))
dev.off()

ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplot_frogcounts_nozeros_nofacet_29_01_22_36.svg", width = 200, height = 150, units = 'mm')
(boxplot_frogcounts_nozeros_nofacet_29_01_22_36<- 
    ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Running_total_of_adults_emerged,fill=Frog_popsource)) +
    geom_boxplot() +
    #geom_point()+
    #geom_beeswarm()+
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    labs(x="Toad Region")+
    labs(y="Juvenile frogs emerged")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",
                      breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    #scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #                     labels=c("Haida Gwaii", "Mainland"),
    #                    guide=FALSE)+    
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 15)))
dev.off()



###  counts
(frogcounts_notoadregion_nozeros<- ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Running_total_of_adults_emerged,colour=Region)) +
   geom_boxplot() +
   geom_point()+
   #geom_beeswarm()+
   #scale_colour_hue(l=50) +
   #coord_cartesian(ylim = ylim1*1.05)+
   labs(x="Toad Region")+
   labs(y="Juvenile frogs emerged")+
   scale_x_discrete(breaks=c("HG",  "ML"),
                    labels=c("Haida Gwaii","Mainland" ))+
   scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
                         labels=c("Haida Gwaii", "Mainland"),
                         guide=FALSE)+    
   scale_colour_manual(values=c("#009E73", "#56B4E9"),guide=FALSE)+
   theme_bw())


####################################################################
################################## mixed effects models ##############
########################################################################


mmefrog<-lmer(DIED_all~Region+Treatment_four + (1|Population),data=subset_dataRGRdd_frogs2,na.action=na.exclude,REML=FALSE)
anova(mmefrog) 


# region frog * region toad
#singular fit
# still region significant

mmee_modelfrog<-lmer((DIED_all)~Frog_popsource*Region + (1|Population), data=subset_dataRGRdd_frogs2,na.action=na.exclude)
anova(mmee_modelfrog) #significant region of toad - ML high deaths
plot(modelfrog)
visreg(mmee_modelfrog)
summary(modelfrog)
AIC(mmee_modelfrog) # 186.8238
fit3.4<-fitted(modelfrog)
resid3.4<-residuals(modelfrog)
plot(fit3.4,resid3.4) 
residmodfitmod<-lm(resid3.4~fit3.4)
anova(residmodfitmod)
summary(residmodfitmod)
(slope <-  coef(residmodfitmod)[2] ) #don't transform



#Survived_counts_weighedandkilled
#no warning

modelfrog_countsemergerd<-lm((Survived_counts_weighedandkilled)~Frog_popsource*Region, data=subset_dataRGRdd_frogs2,na.action=na.exclude)
anova(modelfrog_countsemergerd) #significant region of toad - HG higher emergerd
plot(modelfrog_countsemergerd)
visreg(modelfrog_countsemergerd)
summary(modelfrog_countsemergerd)
AIC(modelfrog_countsemergerd) # 197.9177
fit3.4<-fitted(modelfrog_countsemergerd)
resid3.4<-residuals(modelfrog_countsemergerd)
plot(fit3.4,resid3.4) 
residmodfitmod<-lm(resid3.4~fit3.4)
anova(residmodfitmod)
summary(residmodfitmod)
(slope <-  coef(residmodfitmod)[2] ) #don't transform



######################
######## EXTRA PLOTS #######
#####################



png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplot_Tadpoles_added_as_top.up_june7thand8th_percapita_nofacet_29_01_22_36.png", width = 200, height = 150, units = 'mm', res = 600)
(boxplot_Tadpoles_added_as_top.up_june7thand8th_percapita_nofacet_29_01_22<- 
    ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Tadpoles_added_as_top.up_june7thand8th_percapita, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    labs(x="Toad origin")+
    labs(y="Frog tadpoles added at two weeks (per capita)")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #                     labels=c("Haida Gwaii", "Mainland"),
    #                    guide=FALSE)+    
    #scale_fill_manual("grey")+
    ylim(0,0.5)+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))
dev.off()

ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/boxplot_Tadpoles_added_as_top.up_june7thand8th_percapita_nofacet_29_01_22_36.svg", width = 200, height = 150, units = 'mm')
(boxplot_Tadpoles_added_as_top.up_june7thand8th_percapita_nofacet_29_01_22<- 
    ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Tadpoles_added_as_top.up_june7thand8th_percapita, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    labs(x="Toad origin")+
    labs(y="Frog tadpoles added at two weeks (per capita)")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #                     labels=c("Haida Gwaii", "Mainland"),
    #                    guide=FALSE)+    
    #scale_fill_manual("grey")+
    ylim(0,0.5)+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))
dev.off()




############
####### ggarrange ####
########################

library(ggpubr)

###################
##################
###### join plots together ###########
###################
########################

### 1
(frogs_GR_final_days_mgDay_logscale_nofacet_29_01_22<-
   data_frogs_long_enviro_sumDD_01_05_20 %>%
   ggplot(aes(x=Region, y=GR_final_days_mgDay, fill=Frog_popsource)) +
   geom_boxplot(lwd=0.6) +
   #scale_colour_hue(l=50) +
   #coord_cartesian(ylim = ylim1*1.15)+
   scale_y_continuous(trans = 'log10')+
   annotation_logticks(sides="l")+
   scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog population",breaks=c("HG", "ML"),
                     labels=c("Haida Gwaii", "Mainland"))+
   labs(x="Toad population")+
   labs(y="Frog growth rate (mg/day)")+
   #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
   scale_x_discrete(breaks=c("HG",  "ML"),
                    labels=c("Haida Gwaii","Mainland" ))+
   # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
   #          labels=c("Haida Gwaii", "Mainland"),
   #              guide=FALSE)+    
   #scale_fill_manual("grey")+
   theme_bw()+
   theme(axis.title.x = element_text(margin = margin(t = 10)))+
   theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))


##### 2
(frogs_meta_weightplot_logscale_nofacet_29_01_22<-
    data_frogs_long_enviro_sumDD_01_05_20 %>%
    ggplot(aes(x=Region, y=Av_frog_juv_biomass_mg, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    scale_y_continuous(trans = 'log10')+
    annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog population",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad population")+
    labs(y="Average frog weight\nat metamorphosis (mg)")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #          labels=c("Haida Gwaii", "Mainland"),
    #              guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))


### 3
(boxplot_frogs_mediandays_nofacet_29_01_22<- 
   ggplot(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large, aes(x=Region, y=MedianDays, fill= Frog_popsource)) +
   geom_boxplot(lwd=0.6) +
   #scale_colour_hue(l=50) +
   #coord_cartesian(ylim = ylim1*1.05)+
   
   labs(x="Toad population")+
   labs(y="Median time to frog\nmetamorphosis (days)")+
   #   scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad population",breaks=c("HG", "ML"),
   #                  labels=c("Haida Gwaii", "Mainland"))+
   # facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
   scale_x_discrete(breaks=c("HG",  "ML"),
                    labels=c("Haida Gwaii","Mainland" ))+
   scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog population",
                     breaks=c("HG", "ML"),
                     labels=c("Haida Gwaii", "Mainland"))+
   #scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
   #    labels=c("Haida Gwaii", "Mainland"),
   #      guide=FALSE)+    
   #scale_fill_manual("grey")+
   theme_bw()+
   theme(axis.title.x = element_text(margin = margin(t = 10)))+
   theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))


### legend
(boxplot_frogs_mediandays_nofacet_29_01_22_LEGEND<- 
    ggplot(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large, aes(x=Region, y=MedianDays, fill= Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.05)+
    
    labs(x="Toad population")+
    labs(y="Median time to frog metamorphosis (days)")+
    #   scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad population",breaks=c("HG", "ML"),
    #                  labels=c("Haida Gwaii", "Mainland"))+
    # facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog population",
                      breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    #scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #    labels=c("Haida Gwaii", "Mainland"),
    #      guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(legend.key.size = unit(2, 'cm'),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))





#### GET LEGEND ON SECOND PLOT
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend_frog<-g_legend(boxplot_frogs_mediandays_nofacet_29_01_22_LEGEND)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/Frog_growthrate_avweightatmeta_mediantime_2x2_onelegend.png", width = 300, height = 300, units = 'mm', res = 600)
ggarrange(frogs_GR_final_days_mgDay_logscale_nofacet_29_01_22  + theme(legend.position="none"), frogs_meta_weightplot_logscale_nofacet_29_01_22 + theme(legend.position="none"), boxplot_frogs_mediandays_nofacet_29_01_22 + theme(legend.position="none"),
          labels = c("a)", "b)", "c)"), font.label = list(size = 17),hjust =-0.25,
          ncol = 2,  mylegend_frog,nrow = 2) 
dev.off()



ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/Frog_growthrate_avweightatmeta_mediantime_2x2_onelegend.svg", width = 300, height = 300, units = 'mm')
ggarrange(frogs_GR_final_days_mgDay_logscale_nofacet_29_01_22  + theme(legend.position="none"), frogs_meta_weightplot_logscale_nofacet_29_01_22 + theme(legend.position="none"), boxplot_frogs_mediandays_nofacet_29_01_22 + theme(legend.position="none"),
          labels = c("a)", "b)", "c)"), font.label = list(size = 17),hjust =-0.25,
          ncol = 2,  mylegend_frog,nrow = 2) 
dev.off()



###################
### other way around #######

labels <- c(ML = "Mainland Toads", HG = "Haida Gwaii Toads")

### 1
(frogs_GR_final_days_mgDay_logscale_nofacet_29_01_22_swaptoadfrog<-
   data_frogs_long_enviro_sumDD_01_05_20 %>%
   ggplot(aes(x=Frog_popsource, y=GR_final_days_mgDay, fill=Region)) +
   geom_boxplot(lwd=0.6) +
   #scale_colour_hue(l=50) +
   #coord_cartesian(ylim = ylim1*1.15)+
   scale_y_continuous(trans = 'log10')+
   annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
   labs(x="Interspecific Competition Treatment")+
   labs(y="Frog growth rate (mg/day)")+
   #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
   scale_x_discrete(breaks=c("HG",  "ML"),
                    labels=c("Toad +\nHaida Gwaii NRLF","Toad +\nMainland NRLF" ))+
   # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
   #          labels=c("Haida Gwaii", "Mainland"),
   #              guide=FALSE)+    
   #scale_fill_manual("grey")+
   theme_bw()+
   theme(axis.title.x = element_text(margin = margin(t = 10)))+
   theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))


##### 2
(frogs_meta_weightplot_logscale_nofacet_29_01_22_swaptoadfrog<-
    data_frogs_long_enviro_sumDD_01_05_20 %>%
    ggplot(aes(x=Frog_popsource, y=GR_final_days_mgDay, fill=Region)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    scale_y_continuous(trans = 'log10')+
    annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Interspecific Competition Treatment")+
    labs(y="Average frog weight\nat metamorphosis (mg)")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Toad +\nHaida Gwaii NRLF","Toad +\nMainland NRLF" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #          labels=c("Haida Gwaii", "Mainland"),
    #              guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))


### 3
(boxplot_frogs_mediandays_nofacet_29_01_22_swaptoadfrog<- 
    data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2%>% ggplot(aes(x=Frog_popsource, y=MedianDays, fill=Region)) +    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    #scale_y_continuous(trans = 'log10')+
    #annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Interspecific Competition Treatment")+
    labs(y="Median time to frog\nmetamorphosis (days)")+
    #   scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad population",breaks=c("HG", "ML"),
    #                  labels=c("Haida Gwaii", "Mainland"))+
    # facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Toad +\nHaida Gwaii NRLF","Toad +\nMainland NRLF" ))+
    #scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog population",
                     # breaks=c("HG", "ML"),
                      #labels=c("Haida Gwaii", "Mainland"))+
    #scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #    labels=c("Haida Gwaii", "Mainland"),
    #      guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))


### legend
(boxplot_frogs_mediandays_nofacet_29_01_22_swaptoadfrog_LEGEND<- 
    data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2%>% ggplot(aes(x=Frog_popsource, y=MedianDays, fill=Region)) +    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    #scale_y_continuous(trans = 'log10')+
    #annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad Co-occurrence History\nwith the NRLF",breaks=c("HG", "ML"),
                      labels=c("Short", "Long"))+
    labs(x="Frog population")+
    labs(y="Median time to frog\nmetamorphosis (days)")+
    #   scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad population",breaks=c("HG", "ML"),
    #                  labels=c("Haida Gwaii", "Mainland"))+
    # facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    #scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog population",
    # breaks=c("HG", "ML"),
    #labels=c("Haida Gwaii", "Mainland"))+
    #scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #    labels=c("Haida Gwaii", "Mainland"),
    #      guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(legend.key.size = unit(3, 'cm'),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))




#### GET LEGEND ON SECOND PLOT
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend_frog2<-g_legend(boxplot_frogs_mediandays_nofacet_29_01_22_swaptoadfrog_LEGEND)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/Frog_growthrate_avweightatmeta_mediantime_2x2_onelegend_swaptoadfrog_cooccurencehist.png", width = 300, height = 300, units = 'mm', res = 600)
ggarrange(frogs_GR_final_days_mgDay_logscale_nofacet_29_01_22_swaptoadfrog  + theme(legend.position="none"), frogs_meta_weightplot_logscale_nofacet_29_01_22_swaptoadfrog + theme(legend.position="none"), boxplot_frogs_mediandays_nofacet_29_01_22_swaptoadfrog + theme(legend.position="none"),
          labels = c("a)", "b)", "c)"), font.label = list(size = 17),hjust =-0.25,
          ncol = 2,  mylegend_frog2,nrow = 2) 
dev.off()



ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/Frog_growthrate_avweightatmeta_mediantime_2x2_onelegend_swaptoadfrog_cooccurencehist.svg", width = 300, height = 300, units = 'mm')
ggarrange(frogs_GR_final_days_mgDay_logscale_nofacet_29_01_22_swaptoadfrog  + theme(legend.position="none"), frogs_meta_weightplot_logscale_nofacet_29_01_22_swaptoadfrog + theme(legend.position="none"), boxplot_frogs_mediandays_nofacet_29_01_22_swaptoadfrog + theme(legend.position="none"),
          labels = c("a)", "b)", "c)"), font.label = list(size = 17),hjust =-0.25,
          ncol = 2,  mylegend_frog2,nrow = 2) 
dev.off()


############
####### initial weight frogs ######
#################



### get means and standard errors
subset_dataRGRdd_frogs2_noNA<-subset_dataRGRdd_frogs2[!(subset_dataRGRdd_frogs2$FROG_25.5.18_experiment_start_tadpole_av_individual_weight=="NA"),]

dim(subset_dataRGRdd_frogs2_noNA)

avweight_36 <- subset_dataRGRdd_frogs2_noNA %>%
  group_by(Frog_popsource)

avweight_36<-avweight_36 %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), FROG_25.5.18_experiment_start_tadpole_av_individual_weight)

avweight_36<-as.data.frame(avweight_36)
avweight_36

write.csv(avweight_36,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/Mean_sd_se_STARTING_WEIGHT_FROGS_avjuvweight_539.csv")



0.09290278-0.10357986


(0.09290278-0.10357986/0.10357986)*100


########
##################
######## ggarrange ########
######################

boxplot_Tadpoles_added_as_top.up_june7thand8th_percapita_nofacet_29_01_22

boxplot_Frogmortality_percapita_nofacet_29_01_22

(boxplot_Frogmortality_percapita_nofacet_29_01_22_small<- 
    ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Frogmortality_percapita, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    labs(x="Toad origin")+
    labs(y="Frog mortality\nper capita")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #                     labels=c("Haida Gwaii", "Mainland"),
    #                    guide=FALSE)+    
    #scale_fill_manual("grey")+
    ylim(0,0.5)+
    theme(plot.margin = unit(c(0.5,1,0.5,0.5), "cm"),axis.title.x = element_text(margin = margin(t = 10)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))


(boxplot_Tadpoles_added_as_top.up_june7thand8th_percapita_nofacet_29_01_22_small<- 
    ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Tadpoles_added_as_top.up_june7thand8th_percapita, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    labs(x="Toad origin")+
    labs(y="Frog tadpoles added at\ntwo weeks (per capita)")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #                     labels=c("Haida Gwaii", "Mainland"),
    #                    guide=FALSE)+    
    #scale_fill_manual("grey")+
    ylim(0,0.5)+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))


(boxplot_Tadpoles_added_as_top.up_june7thand8th_percapita_nofacet_29_01_22_small_LEGEND<- 
    ggplot(subset_dataRGRdd_frogs2, aes(x=Region, y=Tadpoles_added_as_top.up_june7thand8th_percapita, fill=Frog_popsource)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    labs(x="Toad origin")+
    labs(y="Frog tadpoles added at\ntwo weeks (per capita)")+
    scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog origin",breaks=c("HG", "ML"),
                      labels=c("Haida Gwaii", "Mainland"))+
    labs(x="Toad origin")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Haida Gwaii","Mainland" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #                     labels=c("Haida Gwaii", "Mainland"),
    #                    guide=FALSE)+    
    #scale_fill_manual("grey")+
    ylim(0,0.5)+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(legend.key.size = unit(2, 'cm'),legend.direction = "horizontal", panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))




#### GET LEGEND ON SECOND PLOT
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegendFROG_MORT<-g_legend(boxplot_Tadpoles_added_as_top.up_june7thand8th_percapita_nofacet_29_01_22_small_LEGEND)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/ggarrange_Frogmortality_percapita_Tadpoles_added_as_top.png", width = 200, height = 300, units = 'mm', res = 600)
ggarrange(boxplot_Frogmortality_percapita_nofacet_29_01_22_small+ theme(legend.position="none"),boxplot_Tadpoles_added_as_top.up_june7thand8th_percapita_nofacet_29_01_22_small + theme(legend.position="none"),  
          labels = c("a)", "b)"), font.label = list(size = 17),hjust =-0.25,
          
          ncol = 1, mylegendFROG_MORT, nrow = 3,heights = c(2,2,1)
) 
dev.off()

ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/ggarrange_Frogmortality_percapita_Tadpoles_added_as_top.svg", width = 200, height = 300, units = 'mm')
ggarrange(boxplot_Frogmortality_percapita_nofacet_29_01_22_small+ theme(legend.position="none"),boxplot_Tadpoles_added_as_top.up_june7thand8th_percapita_nofacet_29_01_22_small + theme(legend.position="none"),  
          labels = c("a)", "b)"), font.label = list(size = 17),hjust =-0.25,
          
          ncol = 1, mylegendFROG_MORT, nrow = 3,heights = c(2,2,1)
) 
dev.off()
