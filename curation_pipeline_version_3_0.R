# UNITE-COVID data curation notebook (R-Kernel)

#This describes the curation process. We use the finalised dataset "UNITE_COVID_19_global_export-1Dec2020 CSV.csv" 
#as the source data / master dataframe. The working dataframe is 'working.df' and is updated by the curation script.

#"UNITE_curation_dictionary.xlsx" contains curation/explanatory notes.

#ESICM UNITE-COVID curation team
#- Ari Ercole (University of Cambridge: Cambridge, GB) ORCID: 0000-0001-8350-8093
#- Paul W Elbers (Amsterdam UMC: Amsterdam, NL) ORCID: 0000-0003-0447-6893
#- Harm-Jan de Grooth (VU University Medical Center: Amsterdam, NL) ORCID: 0000-0002-7499-076X
#- Thomas De Corte (Universiteit Gent: Gent, BE) ORCID: 0000-0001-5011-6640
#- Massimiliano Greco (Humanitas University, Rozzano, IT) ORCID: 0000-0003-1003-4637
#- Andrea Lavinio (Cambridge University Hospitals NHS Foundation Trust: Cambridge, GB)


#Version 3.1 13/02/2022

library(tidyverse)

get_data <- function() {

working.df <- read.csv(file = "./data/UNITE_COVID_19_global_export-1Dec2020.csv", sep=';')


## First deal with patient identifier: split from 'country/site/patient format and cast to factors
working.df <- separate(data = working.df, col=Patient, into=c('NEW_COUNTRY_ID', 'NEW_SITE_ID', 'NEW_SUBJECT_ID'), sep='/')
working.df$NEW_SITE_ID <- as.factor(working.df$NEW_SITE_ID)
working.df$NEW_SUBJECT_ID <- as.factor(working.df$NEW_SUBJECT_ID)
working.df$NEW_COUNTRY_ID <- as.factor(gsub("'", "", as.character(working.df$NEW_COUNTRY_ID) ))    # remove leading ' character



# List of variables for curation
#Patient
#Iteration
## Don't know what this is

#INC_CRIT1_YN
## Inclusion criterion- whether patient>18
## All ones
working.df$INC_CRIT1_YN <- as.factor(with(working.df, 
                                            ifelse(INC_CRIT1_YN==0, FALSE, 
                                                   ifelse(INC_CRIT1_YN==1, TRUE, NA))))

#INC_CRIT2_YN
## Inclusion criterion- whether patients in critical care or looked after by critical care team
working.df$INC_CRIT2_YN <- as.factor(with(working.df, 
                                            ifelse(INC_CRIT2_YN==0, FALSE, 
                                                   ifelse(INC_CRIT2_YN==1, TRUE, NA))))

#INC_CRIT3_YN
## Inclusion criterion- whether patients have confirmed COVID-19 via PCR or equivalent
working.df$INC_CRIT3_YN <- as.factor(with(working.df, 
                                            ifelse(INC_CRIT3_YN==0, FALSE, 
                                                   ifelse(INC_CRIT3_YN==1, TRUE, NA))))

#INC_CRIT4_YN
## Inclusion criterion- SARS-CoV-2 positive but not COVID
working.df$INC_CRIT4_YN <- as.factor(with(working.df, 
                                            ifelse(INC_CRIT4_YN==0, FALSE, 
                                                   ifelse(INC_CRIT4_YN==1, TRUE, NA))))

#INC_SEX_RAD
## Issue- original data dictionary codes '3' as not specified but these appear as 'ND' in data
## Fix- replace 'ND' with NA and finally type cast as factor
working.df$INC_SEX_RAD <- as.character(working.df$INC_SEX_RAD)
working.df$INC_SEX_RAD <- with(working.df, ifelse(INC_SEX_RAD == 1, 
                                                         "Male", INC_SEX_RAD))
working.df$INC_SEX_RAD <- with(working.df, ifelse(INC_SEX_RAD == 2, 
                                                         "Female", INC_SEX_RAD))
working.df$INC_SEX_RAD <- with(working.df, ifelse(INC_SEX_RAD == 3, NA, INC_SEX_RAD))
working.df$INC_SEX_RAD <- with(working.df, ifelse(INC_SEX_RAD == 'ND', NA, INC_SEX_RAD)) 
working.df$INC_SEX_RAD <- with(working.df, ifelse(INC_SEX_RAD == '', NA, INC_SEX_RAD))     
working.df$INC_SEX_RAD <- as.factor(working.df$INC_SEX_RAD)

    
    
#INC_AGE_INT
## Set ND and missing to NA, remove patients > 150 years old.
working.df$INC_AGE_INT <- ifelse(working.df$INC_AGE_INT=='ND', NA, working.df$INC_AGE_INT)
working.df$INC_AGE_INT <- as.integer(working.df$INC_AGE_INT)
working.df$INC_AGE_INT <- ifelse(working.df$INC_AGE_INT >150, NA, working.df$INC_AGE_INT)

#INC_HEIGHT_INT
## There are implausibly small heights present- not recoded.
working.df$INC_HEIGHT_INT <- ifelse(working.df$INC_HEIGHT_INT=='ND', NA, working.df$INC_HEIGHT_INT)
working.df$INC_HEIGHT_INT <- ifelse(working.df$INC_HEIGHT_INT=='', NA, working.df$INC_HEIGHT_INT)
working.df$INC_HEIGHT_INT <- as.integer(working.df$INC_HEIGHT_INT)


#INC_WEIGHT_INT
working.df$INC_WEIGHT_INT <- ifelse(working.df$INC_WEIGHT_INT=='ND', NA, working.df$INC_WEIGHT_INT)
working.df$INC_WEIGHT_INT <- ifelse(working.df$INC_WEIGHT_INT=='', NA, working.df$INC_WEIGHT_INT)
working.df$INC_WEIGHT_INT <- as.integer(working.df$INC_WEIGHT_INT)

#INC_HELTH_WORK_YN
working.df$INC_HELTH_WORK_YN <- as.factor(with(working.df, 
                                            ifelse(INC_HELTH_WORK_YN==0, FALSE, 
                                                   ifelse(INC_HELTH_WORK_YN==1, TRUE, NA))))
#INC_PREGNANT_YN
working.df$INC_PREGNANT_YN <- as.factor(with(working.df, 
                                            ifelse(INC_PREGNANT_YN==0, FALSE, 
                                                   ifelse(INC_PREGNANT_YN==1, TRUE, 0))))


#INC_LOS_PRIOR_ADM_INT
working.df$INC_LOS_PRIOR_ADM_INT <- as.character(working.df$INC_LOS_PRIOR_ADM_INT)
working.df$INC_LOS_PRIOR_ADM_INT <- as.numeric(with(working.df, 
                                                            ifelse(INC_LOS_PRIOR_ADM_INT %in% c("ND", ""), NA, 
                                                                   INC_LOS_PRIOR_ADM_INT)))
#INC_LOS_PRIOR_ADM_UNK
working.df$INC_LOS_PRIOR_ADM_UNK <- as.factor(with(working.df, 
                                            ifelse(INC_LOS_PRIOR_ADM_UNK==0, FALSE, 
                                                   ifelse(INC_LOS_PRIOR_ADM_UNK==1, TRUE, FALSE))))

#INC_INTERVAL_HOSPIT_INT
working.df$INC_INTERVAL_HOSPIT_INT <- as.character(working.df$INC_INTERVAL_HOSPIT_INT)
working.df$INC_INTERVAL_HOSPIT_INT <- as.numeric(with(working.df, 
                                                            ifelse(INC_INTERVAL_HOSPIT_INT %in% c("ND", ""), NA, 
                                                                   INC_INTERVAL_HOSPIT_INT)))
working.df$INC_INTERVAL_HOSPIT_INT <- abs(working.df$INC_INTERVAL_HOSPIT_INT) # There is one (only) patient with a -7 day LOS- impute as positive as likely misunderstanding in data entry

#INC_INTERVAL_HOSPIT_UNK
working.df$INC_INTERVAL_HOSPIT_UNK <- as.factor(with(working.df, 
                                            ifelse(INC_INTERVAL_HOSPIT_UNK==0, FALSE, 
                                                   ifelse(INC_INTERVAL_HOSPIT_UNK==1, TRUE, FALSE))))

#INC_ADM_SURGE_BED_YN
working.df$INC_ADM_SURGE_BED_YN <- as.factor(with(working.df, 
                                            ifelse(INC_ADM_SURGE_BED_YN==0, FALSE, 
                                                   ifelse(INC_ADM_SURGE_BED_YN==1, TRUE, NA))))

#INC_CARDIAC_DISEASE_YN
working.df$INC_CARDIAC_DISEASE_YN <- as.factor(with(working.df, 
                                            ifelse(INC_CARDIAC_DISEASE_YN==0, FALSE, 
                                                   ifelse(INC_CARDIAC_DISEASE_YN==1, TRUE, NA))))

#INC_LIVER_DISEASE_YN
working.df$INC_LIVER_DISEASE_YN <- as.factor(with(working.df, 
                                            ifelse(INC_LIVER_DISEASE_YN==0, FALSE, 
                                                   ifelse(INC_LIVER_DISEASE_YN==1, TRUE, NA))))

#INC_HBP_YN
working.df$INC_HBP_YN <- as.factor(with(working.df, 
                                            ifelse(INC_HBP_YN==0, FALSE, 
                                                   ifelse(INC_HBP_YN==1, TRUE, NA))))

#INC_NEURO_YN
working.df$INC_NEURO_YN <- as.factor(with(working.df, 
                                            ifelse(INC_NEURO_YN==0, FALSE, 
                                                   ifelse(INC_NEURO_YN==1, TRUE, NA))))

#INC_PULMO_DISEASE_YN
working.df$INC_PULMO_DISEASE_YN <- as.factor(with(working.df, 
                                            ifelse(INC_PULMO_DISEASE_YN==0, FALSE, 
                                                   ifelse(INC_PULMO_DISEASE_YN==1, TRUE, NA))))

#INC_DIABETES_YN
working.df$INC_DIABETES_YN <- as.factor(with(working.df, 
                                            ifelse(INC_DIABETES_YN==0, FALSE, 
                                                   ifelse(INC_DIABETES_YN==1, TRUE, NA))))

#INC_ASTHMA_YN
working.df$INC_ASTHMA_YN <- as.factor(with(working.df, 
                                            ifelse(INC_ASTHMA_YN==0, FALSE, 
                                                   ifelse(INC_ASTHMA_YN==1, TRUE, NA))))

#INC_NEOPLASM_YN
working.df$INC_NEOPLASM_YN <- as.factor(with(working.df, 
                                            ifelse(INC_NEOPLASM_YN==0, FALSE, 
                                                   ifelse(INC_NEOPLASM_YN==1, TRUE, NA))))

#INC_KIDNEY_DISEASE_YN
working.df$INC_KIDNEY_DISEASE_YN <- as.factor(with(working.df, 
                                            ifelse(INC_KIDNEY_DISEASE_YN==0, FALSE, 
                                                   ifelse(INC_KIDNEY_DISEASE_YN==1, TRUE, NA))))

#INC_IMMUNOSUPPR_YN
working.df$INC_IMMUNOSUPPR_YN <- as.factor(with(working.df, 
                                            ifelse(INC_IMMUNOSUPPR_YN==0, FALSE, 
                                                   ifelse(INC_IMMUNOSUPPR_YN==1, TRUE, NA))))

#INC_HIV_YN
working.df$INC_HIV_YN <- as.factor(with(working.df, 
                                            ifelse(INC_HIV_YN==0, FALSE, 
                                                   ifelse(INC_HIV_YN==1, TRUE, NA))))

#INC_ACE_INHIB_YN
working.df$INC_ACE_INHIB_YN <- as.factor(with(working.df, 
                                            ifelse(INC_ACE_INHIB_YN==0, FALSE, 
                                                   ifelse(INC_ACE_INHIB_YN==1, TRUE, NA))))

#INC_ANTOCOAG_YN
working.df$INC_ANTOCOAG_YN <- as.factor(with(working.df, 
                                            ifelse(INC_ANTOCOAG_YN==0, FALSE, 
                                                   ifelse(INC_ANTOCOAG_YN==1, TRUE, NA))))

#INC_ANGIO_II_YN
working.df$INC_ANGIO_II_YN <- as.factor(with(working.df, 
                                            ifelse(INC_ANGIO_II_YN==0, FALSE, 
                                                   ifelse(INC_ANGIO_II_YN==1, TRUE, NA))))

#INC_ANTIPLAT_YN
working.df$INC_ANTIPLAT_YN <- as.factor(with(working.df, 
                                            ifelse(INC_ANTIPLAT_YN==0, FALSE, 
                                                   ifelse(INC_ANTIPLAT_YN==1, TRUE, NA))))



                                                   # master.df contains the master dump. Do not modify
#master.df <- read.csv(file = "/UNITE_COVID_19_global_export-1Dec2020 CSV.csv", 
#            sep=';')
# working.df contains the final dataset with any transformations or additional 
# variables. It should be updated by the curation process.
#working.df <- master.df

# List of variables for curation

#ICU_ADM_DIAG_RAD
## Respiratory failure due to COVID-19	1
## Other complication of COVID-19	2
## Other diagnosis	3
## Referral from another ICU	4
working.df$ICU_ADM_DIAG_RAD <- as.character(working.df$ICU_ADM_DIAG_RAD)
working.df$ICU_ADM_DIAG_RAD <- with(working.df, ifelse(ICU_ADM_DIAG_RAD == 1, 
                                                         "Respiratory failure due to COVID-19", ICU_ADM_DIAG_RAD))
working.df$ICU_ADM_DIAG_RAD <- with(working.df, ifelse(ICU_ADM_DIAG_RAD == 2, 
                                                         "Other complication of COVID-19", ICU_ADM_DIAG_RAD))
working.df$ICU_ADM_DIAG_RAD <- with(working.df, ifelse(ICU_ADM_DIAG_RAD == 3, 
                                                         "Other diagnosis", ICU_ADM_DIAG_RAD))
working.df$ICU_ADM_DIAG_RAD <- with(working.df, ifelse(ICU_ADM_DIAG_RAD == 4, 
                                                         "Referral from another ICU", ICU_ADM_DIAG_RAD))
working.df$ICU_ADM_DIAG_RAD <- as.factor(working.df$ICU_ADM_DIAG_RAD)    
    


#ICU_ADM_DIAG_YN
## Unformatted text

#ICU_THROMBO_DVT_CB
# Thromboembolic complications at admission - DVT
working.df$ICU_THROMBO_DVT_CB <- ifelse(working.df$ICU_THROMBO_DVT_CB == '1', TRUE, working.df$ICU_THROMBO_DVT_CB)
working.df$ICU_THROMBO_DVT_CB <- ifelse(is.na(working.df$ICU_THROMBO_DVT_CB), FALSE, working.df$ICU_THROMBO_DVT_CB)

#ICU_THROMBO_PE_CB
# Thromboembolic complications at admission - PE
working.df$ICU_THROMBO_PE_CB <- ifelse(working.df$ICU_THROMBO_PE_CB == '1', TRUE, working.df$ICU_THROMBO_PE_CB)
working.df$ICU_THROMBO_PE_CB <- ifelse(is.na(working.df$ICU_THROMBO_PE_CB), FALSE, working.df$ICU_THROMBO_PE_CB)

#ICU_THROMBO_OTHER_CB
# Thromboembolic complications at admission - Other
working.df$ICU_THROMBO_OTHER_CB <- ifelse(working.df$ICU_THROMBO_OTHER_CB == '1', TRUE, working.df$ICU_THROMBO_OTHER_CB)
working.df$ICU_THROMBO_OTHER_CB <- ifelse(is.na(working.df$ICU_THROMBO_OTHER_CB), FALSE, working.df$ICU_THROMBO_OTHER_CB)

#ICU_THROMBO_NONE_CB
# Thromboembolic complications at admission - none
working.df$ICU_THROMBO_NONE_CB <- ifelse(working.df$ICU_THROMBO_NONE_CB == '1', TRUE, working.df$ICU_THROMBO_NONE_CB)
working.df$ICU_THROMBO_NONE_CB <- ifelse(is.na(working.df$ICU_THROMBO_NONE_CB), FALSE, working.df$ICU_THROMBO_NONE_CB)

#ICU_RESP_SUPPORT_YN
#Did the patient receive respiratory support before ICU admission? 2"=unknown recoded as NA since same meaning. n=316
working.df$ICU_RESP_SUPPORT_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_RESP_SUPPORT_YN==0, FALSE, 
                                                       ifelse(ICU_RESP_SUPPORT_YN==1, TRUE, NA)))) 
    

#ICU_SUPP_TYPE_RAD
# If YES, which type of support?
# Select highest level of support used before ICU admission : NIV > CPAP > HFNO > standard oxygen
# Standard oxygen	1
# HFNC	2
# CPAP	3
# NIV	4
working.df$ICU_SUPP_TYPE_RAD <- as.character(working.df$ICU_SUPP_TYPE_RAD)
working.df$ICU_SUPP_TYPE_RAD <- with(working.df, ifelse(ICU_SUPP_TYPE_RAD == 1, 
                                                         "Standard oxygen", ICU_SUPP_TYPE_RAD))
working.df$ICU_SUPP_TYPE_RAD <- with(working.df, ifelse(ICU_SUPP_TYPE_RAD == 2, 
                                                         "HFNO", ICU_SUPP_TYPE_RAD))
working.df$ICU_SUPP_TYPE_RAD <- with(working.df, ifelse(ICU_SUPP_TYPE_RAD == 3, 
                                                         "CPAP", ICU_SUPP_TYPE_RAD))
working.df$ICU_SUPP_TYPE_RAD <- with(working.df, ifelse(ICU_SUPP_TYPE_RAD == 4, 
                                                         "NIV", ICU_SUPP_TYPE_RAD))
working.df$ICU_SUPP_TYPE_RAD <- as.factor(working.df$ICU_SUPP_TYPE_RAD)    


#ICU_SUPP_DURATION_INT
# Total duration of support before admission
# Only if HFNC, CPAP or NIV
# In hours
# Integer
working.df$ICU_SUPP_DURATION_INT <- as.character(working.df$ICU_SUPP_DURATION_INT)
working.df$ICU_SUPP_DURATION_INT <- ifelse(working.df$ICU_SUPP_DURATION_INT == 'ND', NA, working.df$ICU_SUPP_DURATION_INT)
working.df$ICU_SUPP_DURATION_INT <- as.numeric(working.df$ICU_SUPP_DURATION_INT)

#ICU_WHITE_CELL_INT
working.df$ICU_WHITE_CELL_INT <- as.character(working.df$ICU_WHITE_CELL_INT)
working.df$ICU_WHITE_CELL_INT <- ifelse(working.df$ICU_WHITE_CELL_INT == '', NA, working.df$ICU_WHITE_CELL_INT)
working.df$ICU_WHITE_CELL_INT <- ifelse(working.df$ICU_WHITE_CELL_INT == 'ND', NA, working.df$ICU_WHITE_CELL_INT)
working.df$ICU_WHITE_CELL_INT <- ifelse(as.numeric(working.df$ICU_WHITE_CELL_INT) >100, as.numeric(working.df$ICU_WHITE_CELL_INT)/1000, working.df$ICU_WHITE_CELL_INT)
working.df$ICU_WHITE_CELL_INT <- as.numeric(working.df$ICU_WHITE_CELL_INT)

#ICU_TEMPERATURE_DEC
working.df$ICU_TEMPERATURE_DEC <- as.character(working.df$ICU_TEMPERATURE_DEC)
working.df$ICU_TEMPERATURE_DEC <- ifelse(working.df$ICU_TEMPERATURE_DEC == '', NA, working.df$ICU_TEMPERATURE_DEC)
working.df$ICU_TEMPERATURE_DEC <- ifelse(working.df$ICU_TEMPERATURE_DEC == 'ND', NA, working.df$ICU_TEMPERATURE_DEC)
working.df$ICU_TEMPERATURE_DEC <- ifelse(as.numeric(working.df$ICU_TEMPERATURE_DEC) <30, NA, working.df$ICU_TEMPERATURE_DEC)
working.df$ICU_TEMPERATURE_DEC <- ifelse(as.numeric(working.df$ICU_TEMPERATURE_DEC) >50, NA, working.df$ICU_TEMPERATURE_DEC)
working.df$ICU_TEMPERATURE_DEC <- as.numeric(working.df$ICU_TEMPERATURE_DEC)

#ICU_NEUTRO_INT
working.df$ICU_NEUTRO_INT <- as.character(working.df$ICU_NEUTRO_INT)
working.df$ICU_NEUTRO_INT <- ifelse(working.df$ICU_NEUTRO_INT == '', NA, working.df$ICU_NEUTRO_INT)
working.df$ICU_NEUTRO_INT <- ifelse(working.df$ICU_NEUTRO_INT == 'ND', NA, working.df$ICU_NEUTRO_INT)
working.df$ICU_NEUTRO_INT <- ifelse(as.numeric(working.df$ICU_NEUTRO_INT) >100, as.numeric(working.df$ICU_NEUTRO_INT)/1000, working.df$ICU_NEUTRO_INT)
working.df$ICU_NEUTRO_INT <-as.numeric(working.df$ICU_NEUTRO_INT)

#ICU_CRP_INT
working.df$ICU_CRP_INT <- as.character(working.df$ICU_CRP_INT)
working.df$ICU_CRP_INT <- ifelse(working.df$ICU_CRP_INT == '', NA, working.df$ICU_CRP_INT)
working.df$ICU_CRP_INT <- ifelse(working.df$ICU_CRP_INT == 'ND', NA, working.df$ICU_CRP_INT)
working.df$ICU_CRP_INT <- ifelse(as.numeric(working.df$ICU_CRP_INT) >800, as.numeric(working.df$ICU_CRP_INT)/1000, working.df$ICU_CRP_INT)
working.df$ICU_CRP_INT <- as.numeric(working.df$ICU_CRP_INT)    

#ICU_LYMPH_DEC
working.df$ICU_LYMPH_DEC <- as.character(working.df$ICU_LYMPH_DEC)
working.df$ICU_LYMPH_DEC <- ifelse(working.df$ICU_LYMPH_DEC == '', NA, working.df$ICU_LYMPH_DEC)
working.df$ICU_LYMPH_DEC <- ifelse(working.df$ICU_LYMPH_DEC == 'ND', NA, working.df$ICU_LYMPH_DEC)
working.df$ICU_LYMPH_DEC <- ifelse(as.numeric(working.df$ICU_LYMPH_DEC) >50, as.numeric(working.df$ICU_LYMPH_DEC)/1000, working.df$ICU_LYMPH_DEC)
working.df$ICU_LYMPH_DEC <- as.numeric(working.df$ICU_LYMPH_DEC)    

#ICU_PRO_CALCIT_DEC
working.df$ICU_PRO_CALCIT_DEC <- as.character(working.df$ICU_PRO_CALCIT_DEC)
working.df$ICU_PRO_CALCIT_DEC <- ifelse(working.df$ICU_PRO_CALCIT_DEC == '', NA, working.df$ICU_PRO_CALCIT_DEC)
working.df$ICU_PRO_CALCIT_DEC <- ifelse(working.df$ICU_PRO_CALCIT_DEC == 'ND', NA, working.df$ICU_PRO_CALCIT_DEC)
working.df$ICU_PRO_CALCIT_DEC <- ifelse(as.numeric(working.df$ICU_PRO_CALCIT_DEC) >20, NA, working.df$ICU_PRO_CALCIT_DEC)
working.df$ICU_PRO_CALCIT_DEC <- as.numeric(working.df$ICU_PRO_CALCIT_DEC)
    
    
#ICU_FERRITINE_INT
# I am unsure about the window
working.df$ICU_FERRITINE_INT <- as.character(working.df$ICU_FERRITINE_INT)
working.df$ICU_FERRITINE_INT <- ifelse(working.df$ICU_FERRITINE_INT == '', NA, working.df$ICU_FERRITINE_INT)
#working.df$ICU_FERRITINE_INT <- ifelse(working.df$ICU_FERRITINE_INT == 0.0, NA, working.df$ICU_FERRITINE_INT)
working.df$ICU_FERRITINE_INT <- ifelse(working.df$ICU_FERRITINE_INT == 'ND', NA, working.df$ICU_FERRITINE_INT)
working.df$ICU_FERRITINE_INT <- as.numeric(working.df$ICU_FERRITINE_INT)
    
#ICU_HS_TROPONIN_DEC
working.df$ICU_HS_TROPONIN_DEC <- as.character(working.df$ICU_HS_TROPONIN_DEC)
working.df$ICU_HS_TROPONIN_DEC <- ifelse(working.df$ICU_HS_TROPONIN_DEC == '', NA, working.df$ICU_HS_TROPONIN_DEC)
working.df$ICU_HS_TROPONIN_DEC <- ifelse(working.df$ICU_HS_TROPONIN_DEC == 'ND', NA, working.df$ICU_HS_TROPONIN_DEC)
working.df$ICU_HS_TROPONIN_DEC <- ifelse(as.numeric(working.df$ICU_HS_TROPONIN_DEC) >100, NA, working.df$ICU_HS_TROPONIN_DEC)
working.df$ICU_HS_TROPONIN_DEC <- as.numeric(working.df$ICU_HS_TROPONIN_DEC)    

#ICU_FIBRIN_DEC
#fibrinogen (!)
working.df$ICU_FIBRIN_DEC <- as.character(working.df$ICU_FIBRIN_DEC)
working.df$ICU_FIBRIN_DEC <- ifelse(working.df$ICU_FIBRIN_DEC == '', NA, working.df$ICU_FIBRIN_DEC)
working.df$ICU_FIBRIN_DEC <- ifelse(working.df$ICU_FIBRIN_DEC == 'ND', NA, working.df$ICU_FIBRIN_DEC)
working.df$ICU_FIBRIN_DEC <- ifelse(as.numeric(working.df$ICU_FIBRIN_DEC) >15, NA, working.df$ICU_FIBRIN_DEC)
working.df$ICU_FIBRIN_DEC <- as.numeric(working.df$ICU_FIBRIN_DEC)




#ICU_APTT_INT
working.df$ICU_APTT_INT <- as.character(working.df$ICU_APTT_INT)
working.df$ICU_APTT_INT <- ifelse(working.df$ICU_APTT_INT == '', NA, working.df$ICU_APTT_INT)
working.df$ICU_APTT_INT <- ifelse(working.df$ICU_APTT_INT == 'ND', NA, working.df$ICU_APTT_INT)
working.df$ICU_APTT_INT <- ifelse(as.numeric(working.df$ICU_APTT_INT) >570, NA, working.df$ICU_APTT_INT)
working.df$ICU_APTT_INT <- as.numeric(working.df$ICU_APTT_INT)

#ICU_DIMERS_INT
# I am unsure about the window
working.df$ICU_DIMERS_INT <- as.character(working.df$ICU_DIMERS_INT)
working.df$ICU_DIMERS_INT <- ifelse(working.df$ICU_DIMERS_INT == '', NA, working.df$ICU_DIMERS_INT)
working.df$ICU_DIMERS_INT <- ifelse(working.df$ICU_DIMERS_INT == 'ND', NA, working.df$ICU_DIMERS_INT)
working.df$ICU_DIMERS_INT <- ifelse(as.numeric(working.df$ICU_DIMERS_INT) >100000, NA, working.df$ICU_DIMERS_INT)
working.df$ICU_DIMERS_INT <- as.numeric(working.df$ICU_DIMERS_INT)    

#ICU_PLATELETS_INT
working.df$ICU_PLATELETS_INT <- as.character(working.df$ICU_PLATELETS_INT)
working.df$ICU_PLATELETS_INT <- ifelse(working.df$ICU_PLATELETS_INT == '', NA, working.df$ICU_PLATELETS_INT)
working.df$ICU_PLATELETS_INT <- ifelse(working.df$ICU_PLATELETS_INT == 'ND', NA, working.df$ICU_PLATELETS_INT)
working.df$ICU_PLATELETS_INT <- ifelse(as.numeric(working.df$ICU_PLATELETS_INT) >2000, NA, working.df$ICU_PLATELETS_INT)
working.df$ICU_PLATELETS_INT <- as.numeric(working.df$ICU_PLATELETS_INT)    

#ICU_PROTHROMB_INT
working.df$ICU_PROTHROMB_INT <- as.character(working.df$ICU_PROTHROMB_INT)
working.df$ICU_PROTHROMB_INT <- ifelse(working.df$ICU_PROTHROMB_INT == '', NA, working.df$ICU_PROTHROMB_INT)
working.df$ICU_PROTHROMB_INT <- ifelse(working.df$ICU_PROTHROMB_INT == 'ND', NA, working.df$ICU_PROTHROMB_INT)
working.df$ICU_PROTHROMB_INT <- ifelse(as.numeric(working.df$ICU_PROTHROMB_INT) >370, NA, working.df$ICU_PROTHROMB_INT)
working.df$ICU_PROTHROMB_INT <- as.numeric(working.df$ICU_PROTHROMB_INT)    

#ICU_CARDIAC_THERAPY_YN 2"=unknown recoded as NA since same meaning. n=316
working.df$ICU_CARDIAC_THERAPY_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_CARDIAC_THERAPY_YN==0, FALSE, 
                                                       ifelse(ICU_CARDIAC_THERAPY_YN==1, TRUE, NA)))) 

#ICU_SEPSIS_YN 2"=unknown recoded as NA since same meaning. n=319
working.df$ICU_SEPSIS_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_SEPSIS_YN==0, FALSE, 
                                                       ifelse(ICU_SEPSIS_YN==1, TRUE, NA)))) 

#ICU_STRESS_MYOC_YN 2"=unknown recoded as NA since same meaning. n=331
working.df$ICU_STRESS_MYOC_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_STRESS_MYOC_YN==0, FALSE, 
                                                       ifelse(ICU_STRESS_MYOC_YN==1, TRUE, NA)))) 


#ICU_MYOCARDITIS_YN 2"=unknown recoded as NA since same meaning. n=286
working.df$ICU_MYOCARDITIS_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_MYOCARDITIS_YN==0, FALSE, 
                                                       ifelse(ICU_MYOCARDITIS_YN==1, TRUE, NA)))) 

#ICU_PERICARD_YN 2"=unknown recoded as NA since same meaning. n=195
working.df$ICU_PERICARD_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_PERICARD_YN==0, FALSE, 
                                                       ifelse(ICU_PERICARD_YN==1, TRUE, NA)))) 


#ICU_PNEUMOTHORAX_YN 2"=unknown recoded as NA since same meaning. n=34
working.df$ICU_PNEUMOTHORAX_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_PNEUMOTHORAX_YN==0, FALSE, 
                                                       ifelse(ICU_PNEUMOTHORAX_YN==1, TRUE, NA)))) 

#ICU_ATELECTASIS_YN 2"=unknown recoded as NA since same meaning. n=34
working.df$ICU_ATELECTASIS_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_ATELECTASIS_YN==0, FALSE, 
                                                       ifelse(ICU_ATELECTASIS_YN==1, TRUE, NA)))) 

#ICU_DELIRIUM_YN 2"=unknown recoded as NA since same meaning. n=123
working.df$ICU_DELIRIUM_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_DELIRIUM_YN==0, FALSE, 
                                                       ifelse(ICU_DELIRIUM_YN==1, TRUE, NA)))) 

#ICU_SEIZURE_YN 2"=unknown recoded as NA since same meaning. n=50
working.df$ICU_SEIZURE_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_SEIZURE_YN==0, FALSE, 
                                                       ifelse(ICU_SEIZURE_YN==1, TRUE, NA)))) 

#ICU_PRESSURE_FAC_YN 2"=unknown recoded as NA since same meaning. n=170
working.df$ICU_PRESSURE_FAC_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_PRESSURE_FAC_YN==0, FALSE, 
                                                       ifelse(ICU_PRESSURE_FAC_YN==1, TRUE, NA)))) 



# List of variables for curation

#ICU_PRESSURE_OTH_YN
## not clear what response=2 means
working.df$ICU_PRESSURE_OTH_YN <- as.factor(with(working.df, 
                                            ifelse(ICU_PRESSURE_OTH_YN==0, FALSE, 
                                                   ifelse(ICU_PRESSURE_OTH_YN==1, TRUE, NA))))

#ICU_KIDNEY_INJ_YN
working.df$ICU_KIDNEY_INJ_YN <- as.factor(with(working.df, 
                                                 ifelse(ICU_KIDNEY_INJ_YN==0, FALSE, 
                                                        ifelse(ICU_KIDNEY_INJ_YN==1, TRUE, NA))))

#ICU_OBSTRUCTION_YN
working.df$ICU_OBSTRUCTION_YN <- as.factor(with(working.df, 
                                               ifelse(ICU_OBSTRUCTION_YN==0, FALSE, 
                                                      ifelse(ICU_OBSTRUCTION_YN==1, TRUE, NA))))

#ICU_EXTUB_YN
working.df$ICU_EXTUB_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_EXTUB_YN==0, FALSE, 
                                                       ifelse(ICU_EXTUB_YN==1, TRUE, NA))))

#ICU_ANTIVIRALS_YN
working.df$ICU_ANTIVIRALS_YN <- as.factor(with(working.df, 
                                          ifelse(ICU_ANTIVIRALS_YN==0, FALSE, 
                                                 ifelse(ICU_ANTIVIRALS_YN==1, TRUE, NA))))

#ICU_ANTIVIRALS_RAD
working.df$ICU_ANTIVIRALS_RAD <- as.character(working.df$ICU_ANTIVIRALS_RAD)
working.df$ICU_ANTIVIRALS_RAD <- with(working.df, ifelse(ICU_ANTIVIRALS_RAD == 1, 
                                                         "Ribavirin", ICU_ANTIVIRALS_RAD))
working.df$ICU_ANTIVIRALS_RAD <- with(working.df, ifelse(ICU_ANTIVIRALS_RAD == 2, 
                                                         "Lopinavir/Ritonavir", ICU_ANTIVIRALS_RAD))
working.df$ICU_ANTIVIRALS_RAD <- with(working.df, ifelse(ICU_ANTIVIRALS_RAD == 3, 
                                                         "Neuramidase inhibitor", ICU_ANTIVIRALS_RAD))
working.df$ICU_ANTIVIRALS_RAD <- with(working.df, ifelse(ICU_ANTIVIRALS_RAD == 4, 
                                                         "Remdesivir", ICU_ANTIVIRALS_RAD))
working.df$ICU_ANTIVIRALS_RAD <- with(working.df, ifelse(ICU_ANTIVIRALS_RAD %in% c("5", "6"), 
                                                         NA, ICU_ANTIVIRALS_RAD))
working.df$ICU_ANTIVIRALS_RAD <- as.factor(working.df$ICU_ANTIVIRALS_RAD)

#ICU_OTHER_ANTIVIRALS_RAD
working.df$ICU_OTHER_ANTIVIRALS_RAD <- as.character(working.df$ICU_OTHER_ANTIVIRALS_RAD)
working.df$ICU_OTHER_ANTIVIRALS_RAD <- with(working.df, ifelse(ICU_OTHER_ANTIVIRALS_RAD == 1, 
                                                         "Interferon alpha", ICU_OTHER_ANTIVIRALS_RAD))
working.df$ICU_OTHER_ANTIVIRALS_RAD <- with(working.df, ifelse(ICU_OTHER_ANTIVIRALS_RAD == 2, 
                                                         "Interferon beta", ICU_OTHER_ANTIVIRALS_RAD))
working.df$ICU_OTHER_ANTIVIRALS_RAD <- with(working.df, ifelse(ICU_OTHER_ANTIVIRALS_RAD == 3, 
                                                         "Tocilizumab", ICU_OTHER_ANTIVIRALS_RAD))
working.df$ICU_OTHER_ANTIVIRALS_RAD <- with(working.df, ifelse(ICU_OTHER_ANTIVIRALS_RAD == 4, 
                                                         "Anakinra", ICU_OTHER_ANTIVIRALS_RAD))
working.df$ICU_OTHER_ANTIVIRALS_RAD <- with(working.df, ifelse(ICU_OTHER_ANTIVIRALS_RAD == 5, 
                                                          "Convalescent plasma", ICU_OTHER_ANTIVIRALS_RAD))
working.df$ICU_OTHER_ANTIVIRALS_RAD <- as.factor(working.df$ICU_OTHER_ANTIVIRALS_RAD)

#ICU_CORTICO_YN
working.df$ICU_CORTICO_YN <- as.factor(with(working.df, 
                                               ifelse(ICU_CORTICO_YN==0, FALSE, 
                                                      ifelse(ICU_CORTICO_YN==1, TRUE, NA))))

#ICU_CORTICO_DURATION_INT
working.df$ICU_CORTICO_DURATION_INT <- as.character(working.df$ICU_CORTICO_DURATION_INT)
working.df$ICU_CORTICO_DURATION_INT <- as.numeric(with(working.df, 
                                                            ifelse(ICU_CORTICO_DURATION_INT %in% c("ND", ""), NA, 
                                                                   ICU_CORTICO_DURATION_INT)))
working.df$ICU_CORTICO_DURATION_INT <- with(working.df, 
                                            ifelse(ICU_CORTICO_YN==FALSE | is.na(ICU_CORTICO_YN), NA, 
                                                   ICU_CORTICO_DURATION_INT))

#ICU_CORTICO_INTERV_INT
working.df$ICU_CORTICO_INTERV_INT <- as.character(working.df$ICU_CORTICO_INTERV_INT)
working.df$ICU_CORTICO_INTERV_INT <- as.numeric(with(working.df, 
                                            ifelse(ICU_CORTICO_INTERV_INT %in% c("ND", ""), NA, 
                                                   ICU_CORTICO_INTERV_INT)))
working.df$ICU_CORTICO_INTERV_INT <- as.numeric(with(working.df, 
                                            ifelse(ICU_CORTICO_YN==FALSE | is.na(ICU_CORTICO_YN), NA, 
                                                   ICU_CORTICO_INTERV_INT)))

#ICU_CORTICO_INDICATION_RAD
working.df$ICU_CORTICO_INDICATION_RAD <- as.character(working.df$ICU_CORTICO_INDICATION_RAD)
working.df$ICU_CORTICO_INDICATION_RAD <- with(working.df, ifelse(ICU_CORTICO_YN != TRUE | is.na(ICU_CORTICO_YN), 
                                                                 NA, ICU_CORTICO_INDICATION_RAD))
working.df$ICU_CORTICO_INDICATION_RAD <- with(working.df, ifelse(ICU_CORTICO_INDICATION_RAD == 1, 
                                                               "Shock", ICU_CORTICO_INDICATION_RAD))
working.df$ICU_CORTICO_INDICATION_RAD <- with(working.df, ifelse(ICU_CORTICO_INDICATION_RAD == 2, 
                                                               "Hyper-inflammation", ICU_CORTICO_INDICATION_RAD))
working.df$ICU_CORTICO_INDICATION_RAD <- with(working.df, ifelse(ICU_CORTICO_INDICATION_RAD == 3, 
                                                               "Pneumonitis", ICU_CORTICO_INDICATION_RAD))
working.df$ICU_CORTICO_INDICATION_RAD <- with(working.df, ifelse(ICU_CORTICO_INDICATION_RAD == 4, 
                                                               "Pre-existing condition", ICU_CORTICO_INDICATION_RAD))
working.df$ICU_CORTICO_INDICATION_RAD <- with(working.df, ifelse(ICU_CORTICO_INDICATION_RAD == 5, 
                                                               "Other", ICU_CORTICO_INDICATION_RAD))
working.df$ICU_CORTICO_INDICATION_RAD <- as.factor(working.df$ICU_CORTICO_INDICATION_RAD)

#ICU_ANTIMALARIAL_YN
working.df$ICU_ANTIMALARIAL_YN <- as.factor(with(working.df, 
                                            ifelse(ICU_ANTIMALARIAL_YN==3, FALSE, 
                                                   ifelse(ICU_ANTIMALARIAL_YN %in% c("1", "2"), TRUE, NA))))

#ICU_ANTIMALARIAL_DURATION_INT
working.df$ICU_ANTIMALARIAL_DURATION_INT <- as.character(working.df$ICU_ANTIMALARIAL_DURATION_INT)
working.df$ICU_ANTIMALARIAL_DURATION_INT <- as.numeric(with(working.df, 
                                          ifelse(ICU_ANTIMALARIAL_DURATION_INT %in% c("ND", ""), NA, 
                                                 ICU_ANTIMALARIAL_DURATION_INT)))

#ICU_CLIN_TRIAL_YN
working.df$ICU_CLIN_TRIAL_YN <- as.factor(with(working.df, 
                                                 ifelse(ICU_CLIN_TRIAL_YN==0, FALSE, 
                                                        ifelse(ICU_CLIN_TRIAL_YN %in% c("1", "2"), TRUE, NA))))

#ICU_SEDATION_YN
working.df$ICU_SEDATION_YN <- as.factor(with(working.df, 
                                               ifelse(ICU_SEDATION_YN==0, FALSE, 
                                                      ifelse(ICU_SEDATION_YN %in% c("1", "2"), TRUE, NA))))

#ICU_SEDAT_DURATION_INT
working.df$ICU_SEDAT_DURATION_INT <- as.character(working.df$ICU_SEDAT_DURATION_INT)
working.df$ICU_SEDAT_DURATION_INT <- as.numeric(with(working.df, 
                                                            ifelse(ICU_SEDAT_DURATION_INT %in% c("ND", ""), NA, 
                                                                   ICU_SEDAT_DURATION_INT)))
working.df$ICU_SEDAT_DURATION_INT <- as.numeric(with(working.df, 
                                                     ifelse(ICU_SEDATION_YN==FALSE | is.na(ICU_SEDATION_YN), NA, 
                                                            ICU_SEDAT_DURATION_INT)))

#ICU_RRT_DIAL_YN
working.df$ICU_RRT_DIAL_YN <- as.factor(with(working.df, 
                                             ifelse(ICU_RRT_DIAL_YN==0, FALSE, 
                                                    ifelse(ICU_RRT_DIAL_YN %in% c("1", "2"), TRUE, NA))))

#ICU_RRT_DURATION_INT
working.df$ICU_RRT_DURATION_INT <- as.character(working.df$ICU_RRT_DURATION_INT)
working.df$ICU_RRT_DURATION_INT <- as.numeric(with(working.df, 
                                                     ifelse(ICU_RRT_DURATION_INT %in% c("ND", ""), NA, 
                                                            ICU_RRT_DURATION_INT)))
working.df$ICU_RRT_DURATION_INT <- as.numeric(with(working.df, 
                                                   ifelse(ICU_RRT_DIAL_YN==FALSE | is.na(ICU_RRT_DURATION_INT), NA,
                                                          ICU_RRT_DURATION_INT)))

#ICU_RRT_METHOD_RAD
working.df$ICU_RRT_METHOD_RAD <- as.character(working.df$ICU_RRT_METHOD_RAD)
working.df$ICU_RRT_METHOD_RAD <- with(working.df, ifelse(ICU_RRT_DIAL_YN != TRUE | is.na(ICU_RRT_DIAL_YN), 
                                                                 NA, ICU_RRT_METHOD_RAD))
working.df$ICU_RRT_METHOD_RAD <- with(working.df, ifelse(ICU_RRT_METHOD_RAD == 1, 
                                                                 "CRRT", ICU_RRT_METHOD_RAD))
working.df$ICU_RRT_METHOD_RAD <- with(working.df, ifelse(ICU_RRT_METHOD_RAD == 2, 
                                                                 "Intermittent", ICU_RRT_METHOD_RAD))
working.df$ICU_RRT_METHOD_RAD <- with(working.df, ifelse(ICU_RRT_METHOD_RAD == 3, 
                                                                 "Peritoneal dialysis", ICU_RRT_METHOD_RAD))
working.df$ICU_RRT_METHOD_RAD <- with(working.df, ifelse(ICU_RRT_METHOD_RAD == 4, 
                                                                 "Mixture", ICU_RRT_METHOD_RAD))
working.df$ICU_RRT_METHOD_RAD <- as.factor(working.df$ICU_RRT_METHOD_RAD)

#ICU_RRT_UNUSUAL_PRACT_YN
working.df$ICU_RRT_UNUSUAL_PRACT_YN <- as.factor(with(working.df, 
                                             ifelse(ICU_RRT_UNUSUAL_PRACT_YN==0, FALSE, 
                                                    ifelse(ICU_RRT_UNUSUAL_PRACT_YN %in% c("1"), TRUE, NA))))

#ICU_BLOOD_PURIF_YN
working.df$ICU_BLOOD_PURIF_YN <- as.factor(with(working.df, 
                                                      ifelse(ICU_BLOOD_PURIF_YN==0, FALSE, 
                                                             ifelse(ICU_BLOOD_PURIF_YN %in% c("1", "2"), TRUE, NA))))

#ICU_BLOOD_PURIF_RAD
working.df$ICU_BLOOD_PURIF_RAD <- as.character(working.df$ICU_BLOOD_PURIF_RAD)
working.df$ICU_BLOOD_PURIF_RAD <- with(working.df, ifelse(ICU_BLOOD_PURIF_YN != TRUE | is.na(ICU_BLOOD_PURIF_YN), 
                                                         NA, ICU_BLOOD_PURIF_RAD))
working.df$ICU_BLOOD_PURIF_RAD <- with(working.df, ifelse(ICU_BLOOD_PURIF_RAD == 1, 
                                                         "Hemoperfusion", ICU_BLOOD_PURIF_RAD))
working.df$ICU_BLOOD_PURIF_RAD <- with(working.df, ifelse(ICU_BLOOD_PURIF_RAD == 2, 
                                                         "Hemoadsorption", ICU_BLOOD_PURIF_RAD))
working.df$ICU_BLOOD_PURIF_RAD <- as.factor(working.df$ICU_BLOOD_PURIF_RAD)

#ICU_INOTROPES_YN
working.df$ICU_INOTROPES_YN <- as.factor(with(working.df, 
                                                ifelse(ICU_INOTROPES_YN==0, FALSE, 
                                                       ifelse(ICU_INOTROPES_YN %in% c("1", "2"), TRUE, NA))))

#ICU_INOTROPE_DURATION_INT
working.df$ICU_INOTROPE_DURATION_INT <- as.character(working.df$ICU_INOTROPE_DURATION_INT)
working.df$ICU_INOTROPE_DURATION_INT <- as.numeric(with(working.df, 
                                                   ifelse(ICU_INOTROPE_DURATION_INT %in% c("ND", ""), NA, 
                                                          ICU_INOTROPE_DURATION_INT)))
working.df$ICU_INOTROPE_DURATION_INT <- as.numeric(with(working.df, 
                                                   ifelse(ICU_INOTROPES_YN==FALSE | is.na(ICU_INOTROPES_YN), NA, 
                                                          ICU_INOTROPE_DURATION_INT)))

#ICU_TRACHEOS_YN
working.df$ICU_TRACHEOS_YN <- as.factor(with(working.df, 
                                              ifelse(ICU_TRACHEOS_YN==0, FALSE, 
                                                     ifelse(ICU_TRACHEOS_YN %in% c("1", "2"), TRUE, NA))))

#ICU_TRACHEO_DAY_INT
working.df$ICU_TRACHEO_DAY_INT <- as.character(working.df$ICU_TRACHEO_DAY_INT)
working.df$ICU_TRACHEO_DAY_INT <- as.numeric(with(working.df, 
                                                        ifelse(ICU_TRACHEO_DAY_INT %in% c("ND", ""), NA, 
                                                               ICU_TRACHEO_DAY_INT)))
working.df$ICU_TRACHEO_DAY_INT <- as.numeric(with(working.df, 
                                                   ifelse(ICU_TRACHEOS_YN==FALSE | is.na(ICU_TRACHEOS_YN), NA, 
                                                          ICU_TRACHEO_DAY_INT)))

#ICU_TRACHEO_METHOD_RAD
working.df$ICU_TRACHEO_METHOD_RAD <- as.character(working.df$ICU_TRACHEO_METHOD_RAD)
working.df$ICU_TRACHEO_METHOD_RAD <- with(working.df, ifelse(ICU_TRACHEOS_YN != TRUE | is.na(ICU_TRACHEOS_YN), 
                                                          NA, ICU_TRACHEO_METHOD_RAD))
working.df$ICU_TRACHEO_METHOD_RAD <- with(working.df, ifelse(ICU_TRACHEO_METHOD_RAD == 1, 
                                                          "Surgical", ICU_TRACHEO_METHOD_RAD))
working.df$ICU_TRACHEO_METHOD_RAD <- with(working.df, ifelse(ICU_TRACHEO_METHOD_RAD == 2, 
                                                          "Dilatative/percutaneous", ICU_TRACHEO_METHOD_RAD))
working.df$ICU_TRACHEO_METHOD_RAD <- with(working.df, ifelse(ICU_TRACHEO_METHOD_RAD == 3, 
                                                             NA, ICU_TRACHEO_METHOD_RAD))
working.df$ICU_TRACHEO_METHOD_RAD <- as.factor(working.df$ICU_TRACHEO_METHOD_RAD)

#ICU_SUP_TEMP_DEC
working.df$ICU_SUP_TEMP_DEC <- as.character(working.df$ICU_SUP_TEMP_DEC)
working.df$ICU_SUP_TEMP_DEC <- as.numeric(with(working.df, 
                                               ifelse(ICU_SUP_TEMP_DEC %in% c("ND", ""), NA, 
                                                      ICU_SUP_TEMP_DEC)))
working.df$ICU_SUP_TEMP_DEC <- as.numeric(with(working.df, 
                                               ifelse(ICU_SUP_TEMP_DEC > 77 & ICU_SUP_TEMP_DEC < 113, (ICU_SUP_TEMP_DEC - 32) * 5/9, 
                                                      ICU_SUP_TEMP_DEC)))
working.df$ICU_SUP_TEMP_DEC <- as.numeric(with(working.df, 
                                               ifelse(ICU_SUP_TEMP_DEC < 25 | ICU_SUP_TEMP_DEC > 45 , NA, 
                                                      ICU_SUP_TEMP_DEC)))


#ICU_SUP_WHITE_CELL_INT
working.df$ICU_SUP_WHITE_CELL_INT <- as.character(working.df$ICU_SUP_WHITE_CELL_INT)
working.df$ICU_SUP_WHITE_CELL_INT <- as.numeric(with(working.df, 
                                                  ifelse(ICU_SUP_WHITE_CELL_INT %in% c("ND", ""), NA, 
                                                         ICU_SUP_WHITE_CELL_INT)))
working.df$ICU_SUP_WHITE_CELL_INT <- as.numeric(ifelse(working.df$ICU_SUP_WHITE_CELL_INT >200, 
                                                as.numeric(working.df$ICU_SUP_WHITE_CELL_INT)/1000, 
                                                working.df$ICU_SUP_WHITE_CELL_INT))

#ICU_SUP_NEUTRO_INT
working.df$ICU_SUP_NEUTRO_INT <- as.character(working.df$ICU_SUP_NEUTRO_INT)
working.df$ICU_SUP_NEUTRO_INT <- as.numeric(with(working.df, 
                                                     ifelse(ICU_SUP_NEUTRO_INT %in% c("ND", ""), NA, 
                                                            ICU_SUP_NEUTRO_INT)))
working.df$ICU_SUP_NEUTRO_INT <- as.numeric(ifelse(working.df$ICU_SUP_NEUTRO_INT >200, 
                                                as.numeric(working.df$ICU_SUP_NEUTRO_INT)/1000, 
                                                working.df$ICU_SUP_NEUTRO_INT))

#ICU_SUP_LYMPH
working.df$ICU_SUP_LYMPH <- as.character(working.df$ICU_SUP_LYMPH)
working.df$ICU_SUP_LYMPH <- as.numeric(with(working.df, 
                                                 ifelse(ICU_SUP_LYMPH %in% c("ND", ""), NA, 
                                                        ICU_SUP_LYMPH)))
working.df$ICU_SUP_LYMPH <- as.numeric(ifelse(working.df$ICU_SUP_LYMPH >50, 
                                                   as.numeric(working.df$ICU_SUP_LYMPH)/1000, 
                                                   working.df$ICU_SUP_LYMPH))

#ICU_SUP_FERRITINE_DEC
working.df$ICU_SUP_FERRITINE_DEC <- as.character(working.df$ICU_SUP_FERRITINE_DEC)
working.df$ICU_SUP_FERRITINE_DEC <- as.numeric(with(working.df, 
                                            ifelse(ICU_SUP_FERRITINE_DEC %in% c("ND", ""), NA, 
                                                   ICU_SUP_FERRITINE_DEC)))
working.df$ICU_SUP_FERRITINE_DEC <- as.numeric(ifelse(working.df$ICU_SUP_FERRITINE_DEC >500, 
                                              as.numeric(working.df$ICU_SUP_FERRITINE_DEC)/1000, 
                                              working.df$ICU_SUP_FERRITINE_DEC))


# List of variables for curation
#ICU_SUPP_TROPT_DEC
## Problem: Some extreme outliers? Was the same unit used in all hospitals?
## Fiw --> to be discussed
working.df$ICU_SUPP_TROPT_DEC <- ifelse(working.df$ICU_SUPP_TROPT_DEC == 'ND', NA, working.df$ICU_SUPP_TROPT_DEC)
working.df$ICU_SUPP_TROPT_DEC <- ifelse(working.df$ICU_SUPP_TROPT_DEC == '', NA, working.df$ICU_SUPP_TROPT_DEC)
working.df$ICU_SUPP_TROPT_DEC <- as.numeric(as.character(working.df$ICU_SUPP_TROPT_DEC))

#RESP_INTUBATED_YN
working.df$RESP_INTUBATED_YN <- as.factor(with(working.df, 
                                                ifelse(RESP_INTUBATED_YN==0, FALSE, 
                                                       ifelse(RESP_INTUBATED_YN==1, TRUE, NA))))

    
    
#RESP_INTUBATED_ICU_STAY_YN
working.df$RESP_INTUBATED_ICU_STAY_YN <- as.factor(with(working.df, 
                                                ifelse(RESP_INTUBATED_ICU_STAY_YN==0, FALSE, 
                                                       ifelse(RESP_INTUBATED_ICU_STAY_YN==1, TRUE, NA))))    
    

#RESP_INTUB_DAYS_AFT_ADM_INT
working.df$RESP_INTUB_DAYS_AFT_ADM_INT <- ifelse(working.df$RESP_INTUB_DAYS_AFT_ADM_INT == "", NA, working.df$RESP_INTUB_DAYS_AFT_ADM_INT)
working.df$RESP_INTUB_DAYS_AFT_ADM_INT <- ifelse(working.df$RESP_INTUB_DAYS_AFT_ADM_INT == "ND", NA, working.df$RESP_INTUB_DAYS_AFT_ADM_INT)
working.df$RESP_INTUB_DAYS_AFT_ADM_INT <- as.numeric(working.df$RESP_INTUB_DAYS_AFT_ADM_INT)

## Inconsistency curation RESP_INTUBATED_YN & RESP_INTUBATED_ICU_YN & RESP_INTUBATED_DAYS_AFT_ADM_INT
working.df$RESP_INTUBATED_YN[working.df$RESP_INTUBATED_YN == TRUE & working.df$RESP_INTUBATED_ICU_STAY_YN == TRUE & is.na(working.df$RESP_INTUB_DAYS_AFT_ADM_INT)] <- TRUE
working.df$RESP_INTUBATED_ICU_STAY_YN[working.df$RESP_INTUBATED_YN == TRUE & working.df$RESP_INTUBATED_ICU_STAY_YN == TRUE & is.na(working.df$RESP_INTUB_DAYS_AFT_ADM_INT)] <- FALSE
working.df$RESP_INTUBATED_YN[working.df$RESP_INTUBATED_YN == TRUE & working.df$RESP_INTUBATED_ICU_STAY_YN == TRUE & working.df$RESP_INTUB_DAYS_AFT_ADM_INT >= 0] <- FALSE
working.df$RESP_INTUBATED_ICU_STAY_YN[working.df$RESP_INTUBATED_YN == TRUE & working.df$RESP_INTUBATED_ICU_STAY_YN == TRUE & working.df$RESP_INTUB_DAYS_AFT_ADM_INT >= 0] <- TRUE
working.df$RESP_INTUBATED_YN[working.df$RESP_INTUBATED_YN == TRUE & working.df$RESP_INTUBATED_ICU_STAY_YN == FALSE & working.df$RESP_INTUB_DAYS_AFT_ADM_INT >=0] <- TRUE
working.df$RESP_INTUB_DAYS_AFT_ADM_INT[working.df$RESP_INTUBATED_YN == TRUE & working.df$RESP_INTUBATED_ICU_STAY_YN == FALSE & working.df$RESP_INTUB_DAYS_AFT_ADM_INT >=0] <- NA
    
    
    
    

#RESP_NI_VENT_YN "2"=unknown recoded as NA since same meaning.
working.df$RESP_NI_VENT_YN <- as.factor(with(working.df, 
                                                ifelse(RESP_NI_VENT_YN==0, FALSE, 
                                                       ifelse(RESP_NI_VENT_YN==1, TRUE, NA))))  
    

#RESP_NIV_DUR_BEF_INTUB_INT
#Range [0,40]
working.df$RESP_NIV_DUR_BEF_INTUB_INT <- ifelse(working.df$RESP_NIV_DUR_BEF_INTUB_INT == "", NA, working.df$RESP_NIV_DUR_BEF_INTUB_INT)
working.df$RESP_NIV_DUR_BEF_INTUB_INT <- ifelse(working.df$RESP_NIV_DUR_BEF_INTUB_INT == "ND", NA, working.df$RESP_NIV_DUR_BEF_INTUB_INT)
working.df$RESP_NIV_DUR_BEF_INTUB_INT <- as.numeric(working.df$RESP_NIV_DUR_BEF_INTUB_INT)

    
    
#RESP_HFNC_YN "2"=unknown recoded as NA since same meaning.
working.df$RESP_HFNC_YN <- as.factor(with(working.df, 
                                                ifelse(RESP_HFNC_YN==0, FALSE, 
                                                       ifelse(RESP_HFNC_YN==1, TRUE, NA))))  
    

#RESP_HFNC_DUR_BEF_INTUB_INT
# Range [0,32]
# NA: 4148
## Number of patients that should have no number of days, since they did not receive HFNC: 2
## one 0 days, other 5 days
working.df$RESP_HFNC_DUR_BEF_INTUB_INT <- ifelse(working.df$RESP_HFNC_DUR_BEF_INTUB_INT == "", NA, working.df$RESP_HFNC_DUR_BEF_INTUB_INT)
working.df$RESP_HFNC_DUR_BEF_INTUB_INT <- ifelse(working.df$RESP_HFNC_DUR_BEF_INTUB_INT == "ND", NA, working.df$RESP_HFNC_DUR_BEF_INTUB_INT)
working.df$RESP_HFNC_DUR_BEF_INTUB_INT <- as.numeric(working.df$RESP_HFNC_DUR_BEF_INTUB_INT)

#RESP_INV_VENT_YN    "2"=unknown recoded as NA since same meaning.
working.df$RESP_INV_VENT_YN <- as.factor(with(working.df, 
                                                ifelse(RESP_INV_VENT_YN==0, FALSE, 
                                                       ifelse(RESP_INV_VENT_YN==1, TRUE, NA))))  
    
#RESP_DURATION_INV_VENT_INT
working.df$RESP_DURATION_INV_VENT_INT <- ifelse(working.df$RESP_DURATION_INV_VENT_INT == "", NA, working.df$RESP_DURATION_INV_VENT_INT)
working.df$RESP_DURATION_INV_VENT_INT <- ifelse(working.df$RESP_DURATION_INV_VENT_INT == "ND", NA, working.df$RESP_DURATION_INV_VENT_INT)
working.df$RESP_DURATION_INV_VENT_INT <- ifelse(working.df$RESP_DURATION_INV_VENT_INT == "NA", NA, working.df$RESP_DURATION_INV_VENT_INT)
working.df$RESP_DURATION_INV_VENT_INT <- as.numeric(working.df$RESP_DURATION_INV_VENT_INT)


## Inconcistency curation RESP_INTUBATED_YN & RESP_INTUBATED_ICU_YN & RESP_INTUBATED_DAYS_AFT_ADM_INT
working.df$RESP_INTUBATED_YN[working.df$RESP_INTUBATED_YN == FALSE & working.df$RESP_INTUBATED_ICU_STAY_YN == FALSE & is.na(working.df$RESP_INTUB_DAYS_AFT_ADM_INT) & working.df$RESP_INV_VENT_INT == TRUE] <- TRUE


#RESP_ECMO_YN "2"=unknown recoded as NA since same meaning. n=16
working.df$RESP_ECMO_YN <- as.factor(with(working.df, 
                                                ifelse(RESP_ECMO_YN==0, FALSE, 
                                                       ifelse(RESP_ECMO_YN==1, TRUE, NA))))  

#RESP_DURATION_ECMO_INT
working.df$RESP_DURATION_ECMO_INT <- ifelse(working.df$RESP_DURATION_ECMO_INT == "", NA, working.df$RESP_DURATION_ECMO_INT)
working.df$RESP_DURATION_ECMO_INT <- ifelse(working.df$RESP_DURATION_ECMO_INT == "ND", NA, working.df$RESP_DURATION_ECMO_INT)
working.df$RESP_DURATION_ECMO_INT <- ifelse(working.df$RESP_DURATION_ECMO_INT == "NA", NA, working.df$RESP_DURATION_ECMO_INT)
working.df$RESP_DURATION_ECMO_INT <- as.numeric(working.df$RESP_DURATION_ECMO_INT)


#RESP_PRONE_YN "2"=unknown recoded as NA since same meaning n=93.
## n patients PRONE_YN = YES and n days of pronation = 0:  6
## n patients PRONE_YN = No and n days of pronation >= 0: 1 
working.df$RESP_PRONE_YN <- as.factor(with(working.df, 
                                                ifelse(RESP_PRONE_YN==0, FALSE, 
                                                       ifelse(RESP_PRONE_YN==1, TRUE, NA))))  

#RESP_PRONE_INTUB_INT
working.df$RESP_PRONE_INTUB_INT <- ifelse(working.df$RESP_PRONE_INTUB_INT == "", NA, working.df$RESP_PRONE_INTUB_INT)
working.df$RESP_PRONE_INTUB_INT <- ifelse(working.df$RESP_PRONE_INTUB_INT == "ND", NA, working.df$RESP_PRONE_INTUB_INT)
working.df$RESP_PRONE_INTUB_INT <- ifelse(working.df$RESP_PRONE_INTUB_INT == "NA", NA, working.df$RESP_PRONE_INTUB_INT)
working.df$RESP_PRONE_INTUB_INT <- as.numeric(working.df$RESP_PRONE_INTUB_INT)    

#RESP_PRONE_NOTINTUB_INT
working.df$RESP_PRONE_NOTINTUB_INT <- ifelse(working.df$RESP_PRONE_NOTINTUB_INT == "", NA, working.df$RESP_PRONE_NOTINTUB_INT)
working.df$RESP_PRONE_NOTINTUB_INT <- ifelse(working.df$RESP_PRONE_NOTINTUB_INT == "ND", NA, working.df$RESP_PRONE_NOTINTUB_INT)
working.df$RESP_PRONE_NOTINTUB_INT <- ifelse(working.df$RESP_PRONE_NOTINTUB_INT == "NA", NA, working.df$RESP_PRONE_NOTINTUB_INT)
working.df$RESP_PRONE_NOTINTUB_INT <- as.numeric(working.df$RESP_PRONE_NOTINTUB_INT)
  

#RESP_NEUROM_BLOC_YN "2"=unknown recoded as NA since same meaning n=47.
#working.df$RESP_NEUROM_BLOC_YN <- ifelse(working.df$RESP_NEUROM_BLOC_YN == "", NA, working.df$RESP_NEUROM_BLOC_YN)
#working.df$RESP_NEUROM_BLOC_YN <- ifelse(working.df$RESP_NEUROM_BLOC_YN == "NA", NA, working.df$RESP_NEUROM_BLOC_YN)
#working.df$RESP_NEUROM_BLOC_YN <- ifelse(working.df$RESP_NEUROM_BLOC_YN == "0", "No", working.df$RESP_NEUROM_BLOC_YN)
#working.df$RESP_NEUROM_BLOC_YN <- ifelse(working.df$RESP_NEUROM_BLOC_YN == "1", "Yes", working.df$RESP_NEUROM_BLOC_YN)
#working.df$RESP_NEUROM_BLOC_YN <- ifelse(working.df$RESP_NEUROM_BLOC_YN == "2", "Unknown", working.df$RESP_NEUROM_BLOC_YN)
#working.df$RESP_NEUROM_BLOC_YN <- as.factor(working.df$RESP_NEUROM_BLOC_YN)
working.df$RESP_NEUROM_BLOC_YN <- as.factor(with(working.df, 
                                                ifelse(RESP_NEUROM_BLOC_YN==0, FALSE, 
                                                       ifelse(RESP_NEUROM_BLOC_YN==1, TRUE, NA))))  

#RESP_NEUROM_DURATION_INT
working.df$RESP_NEUROM_DURATION_INT <- ifelse(working.df$RESP_NEUROM_DURATION_INT == "", NA, working.df$RESP_NEUROM_DURATION_INT)
working.df$RESP_NEUROM_DURATION_INT <- ifelse(working.df$RESP_NEUROM_DURATION_INT == "ND", NA, working.df$RESP_NEUROM_DURATION_INT)
working.df$RESP_NEUROM_DURATION_INT <- as.numeric(working.df$RESP_NEUROM_DURATION_INT)


#RESP_EXTUB_SUPP1_CB Standard oxygen used after extubation?
working.df$RESP_EXTUB_SUPP1_CB <- as.factor(with(working.df, 
                                                ifelse(RESP_EXTUB_SUPP1_CB==0, FALSE, 
                                                       ifelse(RESP_EXTUB_SUPP1_CB==1, TRUE, NA))))

#RESP_EXTUB_SUPP2_CB HFNC used after extubation?
working.df$RESP_EXTUB_SUPP2_CB <- as.factor(with(working.df, 
                                                ifelse(RESP_EXTUB_SUPP2_CB==0, FALSE, 
                                                       ifelse(RESP_EXTUB_SUPP2_CB==1, TRUE, NA))))

#RESP_EXTUB_SUPP3_CB CPAP used after extubation?
working.df$RESP_EXTUB_SUPP3_CB <- as.factor(with(working.df, 
                                                ifelse(RESP_EXTUB_SUPP3_CB==0, FALSE, 
                                                       ifelse(RESP_EXTUB_SUPP3_CB==1, TRUE, NA))))

#RESP_EXTUB_SUPP4_CB NIV used after extubation?
working.df$RESP_EXTUB_SUPP4_CB <- as.factor(with(working.df, 
                                                ifelse(RESP_EXTUB_SUPP4_CB==0, FALSE, 
                                                       ifelse(RESP_EXTUB_SUPP4_CB==1, TRUE, NA))))

#RESP_MODE_RAD
working.df$RESP_MODE_RAD <- ifelse(working.df$RESP_MODE_RAD == "", NA, working.df$RESP_MODE_RAD)
working.df$RESP_MODE_RAD <- ifelse(working.df$RESP_MODE_RAD == "NA", NA, working.df$RESP_MODE_RAD)
working.df$RESP_MODE_RAD <- ifelse(working.df$RESP_MODE_RAD == "1", "VCV", working.df$RESP_MODE_RAD)
working.df$RESP_MODE_RAD <- ifelse(working.df$RESP_MODE_RAD == "2", "PCV", working.df$RESP_MODE_RAD)
working.df$RESP_MODE_RAD <- ifelse(working.df$RESP_MODE_RAD == "3", "BIPAP", working.df$RESP_MODE_RAD)
working.df$RESP_MODE_RAD <- ifelse(working.df$RESP_MODE_RAD == "4", "APRV", working.df$RESP_MODE_RAD)
working.df$RESP_MODE_RAD <- ifelse(working.df$RESP_MODE_RAD == "5", "PSV", working.df$RESP_MODE_RAD)
working.df$RESP_MODE_RAD <- as.factor(working.df$RESP_MODE_RAD)


#RESP_TIDAL_INT
## n < 50 ml: 114
## n > 1500 ml: 3 --> NA
working.df$RESP_TIDAL_INT <- ifelse(working.df$RESP_TIDAL_INT == "", NA, working.df$RESP_TIDAL_INT)
working.df$RESP_TIDAL_INT <- ifelse(working.df$RESP_TIDAL_INT == "ND", NA, working.df$RESP_TIDAL_INT)
working.df$RESP_TIDAL_INT <- ifelse(working.df$RESP_TIDAL_INT == "NA", NA, working.df$RESP_TIDAL_INT)
working.df$RESP_TIDAL_INT <- ifelse(working.df$RESP_TIDAL_INT > 1500, NA, working.df$RESP_TIDAL_INT)
working.df$RESP_TIDAL_INT <- as.numeric(working.df$RESP_TIDAL_INT)


#RESP_PEEP_INT
##  n PEEP < 5: 54
## n PEEP > 20: 29
working.df$RESP_PEEP_INT <- ifelse(working.df$RESP_PEEP_INT == '', NA, working.df$RESP_PEEP_INT)
working.df$RESP_PEEP_INT <- ifelse(working.df$RESP_PEEP_INT == 'NA', NA, working.df$RESP_PEEP_INT)
working.df$RESP_PEEP_INT <- ifelse(working.df$RESP_PEEP_INT == 'ND', NA, working.df$RESP_PEEP_INT)
working.df$RESP_PEEP_INT <- as.numeric(working.df$RESP_PEEP_INT)


#RESP_FIO2_INT
## n Fio2 < 21 = 56
working.df$RESP_FIO2_INT <- ifelse(working.df$RESP_FIO2_INT == '', NA, working.df$RESP_FIO2_INT)
working.df$RESP_FIO2_INT <- ifelse(working.df$RESP_FIO2_INT == 'NA', NA, working.df$RESP_FIO2_INT)
working.df$RESP_FIO2_INT <- ifelse(working.df$RESP_FIO2_INT =='NA', NA, working.df$RESP_FIO2_INT)
working.df$RESP_FIO2_INT <- as.numeric(working.df$RESP_FIO2_INT)
working.df$RESP_FIO2_INT <- ifelse(working.df$RESP_FIO2_INT <1, working.df$RESP_FIO2_INT *10, working.df$RESP_FIO2_INT)
working.df$RESP_FIO2_INT <- ifelse(working.df$RESP_FIO2_INT <21, NA, working.df$RESP_FIO2_INT)  
    

#RESP_PF_RATIO_INT
working.df$RESP_PF_RATIO_INT <- ifelse (working.df$RESP_PF_RATIO_INT == '', NA, working.df$RESP_PF_RATIO_INT)
working.df$RESP_PF_RATIO_INT <- ifelse (working.df$RESP_PF_RATIO_INT == 'NA', NA, working.df$RESP_PF_RATIO_INT)
working.df$RESP_PF_RATIO_INT <- ifelse (working.df$RESP_PF_RATIO_INT == 'ND', NA, working.df$RESP_PF_RATIO_INT)
working.df$RESP_PF_RATIO_INT <- as.numeric(working.df$RESP_PF_RATIO_INT)


#RESP_PACO2_INT
## n PaCO2 > 200 = 6
## n PaCO2 < 5 = 53, < 15 = 473
working.df$RESP_PACO2_INT <- ifelse(working.df$RESP_PACO2_INT == '', NA, working.df$RESP_PACO2_INT)
working.df$RESP_PACO2_INT <- ifelse(working.df$RESP_PACO2_INT == 'NA', NA, working.df$RESP_PACO2_INT)
working.df$RESP_PACO2_INT <- ifelse(working.df$RESP_PACO2_INT == 'ND', NA, working.df$RESP_PACO2_INT)
working.df$RESP_PACO2_INT <- as.numeric(working.df$RESP_PACO2_INT)


#RESP_DRIV_PRESS_INT
## n Driving Pressure > 50 = 12 (max 1512)
## n Driving Pressure < 1 = 32
working.df$RESP_DRIV_PRESS_INT <- ifelse(working.df$RESP_DRIV_PRESS_INT == '', NA, working.df$RESP_DRIV_PRESS_INT)
working.df$RESP_DRIV_PRESS_INT <- ifelse(working.df$RESP_DRIV_PRESS_INT == 'NA', NA, working.df$RESP_DRIV_PRESS_INT)
working.df$RESP_DRIV_PRESS_INT <- ifelse(working.df$RESP_DRIV_PRESS_INT == 'ND', NA, working.df$RESP_DRIV_PRESS_INT)
working.df$RESP_DRIV_PRESS_INT <- as.numeric(working.df$RESP_DRIV_PRESS_INT)


#RESP_VENT_ROUTINE_YN
working.df$RESP_VENT_ROUTINE_YN <- as.factor(with(working.df, 
                                                ifelse(RESP_VENT_ROUTINE_YN==0, FALSE, 
                                                       ifelse(RESP_VENT_ROUTINE_YN==1, TRUE, NA))))  

#RESP_WEANING_RAD
working.df$RESP_WEANING_RAD <- ifelse(working.df$RESP_WEANING_RAD == '', NA, working.df$RESP_WEANING_RAD)
working.df$RESP_WEANING_RAD <- ifelse(working.df$RESP_WEANING_RAD == 'NA', NA, working.df$RESP_WEANING_RAD)
working.df$RESP_WEANING_RAD <- ifelse(working.df$RESP_WEANING_RAD == 'ND', NA, working.df$RESP_WEANING_RAD)
working.df$RESP_WEANING_RAD <- ifelse(working.df$RESP_WEANING_RAD == 1, 'Normal', working.df$RESP_WEANING_RAD)
working.df$RESP_WEANING_RAD <- ifelse(working.df$RESP_WEANING_RAD == '2', 'Difficult', working.df$RESP_WEANING_RAD)
working.df$RESP_WEANING_RAD <- ifelse(working.df$RESP_WEANING_RAD == '3', 'Prolonged', working.df$RESP_WEANING_RAD)
working.df$RESP_WEANING_RAD <- as.factor(working.df$RESP_WEANING_RAD)

#RESP_REINTUBATED_YN     "2"=unknown recoded as NA since same meaning n=215.
working.df$RESP_REINTUBATED_YN <- as.factor(with(working.df, 
                                                ifelse(RESP_REINTUBATED_YN==0, FALSE, 
                                                       ifelse(RESP_REINTUBATED_YN==1, TRUE, NA))))  
    

#COAG_FIBRIN_DEC
## n Fibrinogen > 2500 = 4
## n Fibrinogen < 25 = 2723
working.df$COAG_FIBRIN_DEC <- ifelse(working.df$COAG_FIBRIN_DEC == '', NA, working.df$COAG_FIBRIN_DEC)
working.df$COAG_FIBRIN_DEC <- ifelse(working.df$COAG_FIBRIN_DEC == 'NA', NA, working.df$COAG_FIBRIN_DEC)
working.df$COAG_FIBRIN_DEC <- ifelse(working.df$COAG_FIBRIN_DEC == 'ND', NA, working.df$COAG_FIBRIN_DEC)
working.df$COAG_FIBRIN_DEC <- as.numeric(working.df$COAG_FIBRIN_DEC)


# List of variables for curation
#COAG_DIMERS_INT
working.df$COAG_DIMERS_INT <- as.character(working.df$COAG_DIMERS_INT)
working.df$COAG_DIMERS_INT <- as.numeric(with(working.df, 
                                                            ifelse(COAG_DIMERS_INT %in% c("ND", ""), NA, 
                                                                   COAG_DIMERS_INT)))

#COAG_PLATELETS_INT
working.df$COAG_PLATELETS_INT <- as.character(working.df$COAG_PLATELETS_INT)
working.df$COAG_PLATELETS_INT <- as.numeric(with(working.df, 
                                                            ifelse(COAG_PLATELETS_INT %in% c("ND", ""), NA, 
                                                                   COAG_PLATELETS_INT)))

#COAG_PLATELET_HIGH_INT
working.df$COAG_PLATELET_HIGH_INT <- as.character(working.df$COAG_PLATELET_HIGH_INT)
working.df$COAG_PLATELET_HIGH_INT <- as.numeric(with(working.df, 
                                                            ifelse(COAG_PLATELET_HIGH_INT %in% c("ND", ""), NA, 
                                                                   COAG_PLATELET_HIGH_INT)))

#COAG_PROTHROMB_INT
working.df$COAG_PROTHROMB_INT <- as.character(working.df$COAG_PROTHROMB_INT)
working.df$COAG_PROTHROMB_INT <- as.numeric(with(working.df, 
                                                            ifelse(COAG_PROTHROMB_INT %in% c("ND", ""), NA, 
                                                                   COAG_PROTHROMB_INT)))

#COAG_APTT_INT
working.df$COAG_APTT_INT <- as.character(working.df$COAG_PROTHROMB_INT)
working.df$COAG_APTT_INT <- as.numeric(with(working.df, 
                                                            ifelse(COAG_APTT_INT %in% c("ND", ""), NA, 
                                                                   COAG_APTT_INT)))

#COAG_FERRITIN_HIGH_DEC
working.df$COAG_FERRITIN_HIGH_DEC <- as.character(working.df$COAG_FERRITIN_HIGH_DEC)
working.df$COAG_FERRITIN_HIGH_DEC <- as.numeric(with(working.df, 
                                                            ifelse(COAG_FERRITIN_HIGH_DEC %in% c("ND", ""), NA, 
                                                                   COAG_FERRITIN_HIGH_DEC)))

#COAG_DVT_PROPHY_RAD
working.df$COAG_DVT_PROPHY_RAD <- as.character(working.df$COAG_DVT_PROPHY_RAD)
working.df$COAG_DVT_PROPHY_RAD <- with(working.df, ifelse(COAG_DVT_PROPHY_RAD == 1, 
                                                         "UFH/calciparine", COAG_DVT_PROPHY_RAD))
working.df$COAG_DVT_PROPHY_RAD <- with(working.df, ifelse(COAG_DVT_PROPHY_RAD == 2, 
                                                         "fondaparinux", COAG_DVT_PROPHY_RAD))
working.df$COAG_DVT_PROPHY_RAD <- with(working.df, ifelse(COAG_DVT_PROPHY_RAD == 3, 
                                                         "enoxaparin", COAG_DVT_PROPHY_RAD))
working.df$COAG_DVT_PROPHY_RAD <- with(working.df, ifelse(COAG_DVT_PROPHY_RAD == 4, 
                                                         "dalteparin", COAG_DVT_PROPHY_RAD))
working.df$COAG_DVT_PROPHY_RAD <- with(working.df, ifelse(COAG_DVT_PROPHY_RAD == 5, 
                                                         "tinzaparin", COAG_DVT_PROPHY_RAD))
working.df$COAG_DVT_PROPHY_RAD <- with(working.df, ifelse(COAG_DVT_PROPHY_RAD == 6, 
                                                         "nadroparin", COAG_DVT_PROPHY_RAD))
working.df$COAG_DVT_PROPHY_RAD <- with(working.df, ifelse(COAG_DVT_PROPHY_RAD == 7, 
                                                         "other", COAG_DVT_PROPHY_RAD))
working.df$COAG_DVT_PROPHY_RAD <- with(working.df, ifelse(COAG_DVT_PROPHY_RAD == 8, 
                                                         "none", COAG_DVT_PROPHY_RAD))
working.df$COAG_DVT_PROPHY_RAD <- as.factor(working.df$COAG_DVT_PROPHY_RAD)



#COAG_DVT_DOSE_TXT: THIS IS NOT POSSIBLE TO CURATE- wide range of free text / unformatted

#COAG_ANTIPLAT_RAD
working.df$COAG_ANTIPLAT_RAD <- as.character(working.df$COAG_ANTIPLAT_RAD)
working.df$COAG_ANTIPLAT_RAD <- with(working.df, ifelse(COAG_ANTIPLAT_RAD == 1, 
                                                         "aspirin", COAG_ANTIPLAT_RAD))
working.df$COAG_ANTIPLAT_RAD <- with(working.df, ifelse(COAG_ANTIPLAT_RAD == 2, 
                                                         "clopidogrel", COAG_ANTIPLAT_RAD))
working.df$COAG_ANTIPLAT_RAD <- with(working.df, ifelse(COAG_ANTIPLAT_RAD == 3, 
                                                         "ticlodipine", COAG_ANTIPLAT_RAD))
working.df$COAG_ANTIPLAT_RAD <- with(working.df, ifelse(COAG_ANTIPLAT_RAD == 4, 
                                                         "ticagrelor", COAG_ANTIPLAT_RAD))
working.df$COAG_ANTIPLAT_RAD <- with(working.df, ifelse(COAG_ANTIPLAT_RAD == 5, 
                                                         "triflusal", COAG_ANTIPLAT_RAD))
working.df$COAG_ANTIPLAT_RAD <- with(working.df, ifelse(COAG_ANTIPLAT_RAD == 6, 
                                                         "other", COAG_ANTIPLAT_RAD))
working.df$COAG_ANTIPLAT_RAD <- with(working.df, ifelse(COAG_ANTIPLAT_RAD == 7, 
                                                         "none", COAG_ANTIPLAT_RAD))
working.df$COAG_ANTIPLAT_RAD <- as.factor(working.df$COAG_ANTIPLAT_RAD)

#COAG_ANTIPLAT_DOSE_TXT: THIS IS NOT POSSIBLE TO CURATE- wide range of free text / unformatted



#COAG_LIFETHREAT_HEMO_YN
working.df$COAG_LIFETHREAT_HEMO_YN <- as.factor(with(working.df, 
                                                ifelse(COAG_LIFETHREAT_HEMO_YN==0, FALSE, 
                                                       ifelse(COAG_LIFETHREAT_HEMO_YN==1, TRUE, NA))))    
    
#COAG_BLEED_SOURCE1_CB (lines)
working.df$COAG_BLEED_SOURCE1_CB[is.na(working.df$COAG_BLEED_SOURCE1_CB)] <- FALSE
working.df$COAG_BLEED_SOURCE1_CB <- as.factor(with(working.df, 
                                            ifelse(COAG_BLEED_SOURCE1_CB==1, TRUE, FALSE)))

#COAG_BLEED_SOURCE2_CB (GI tract)
working.df$COAG_BLEED_SOURCE2_CB[is.na(working.df$COAG_BLEED_SOURCE2_CB)] <- FALSE
working.df$COAG_BLEED_SOURCE2_CB <- as.factor(with(working.df, 
                                            ifelse(COAG_BLEED_SOURCE2_CB==1, TRUE, FALSE)))

#COAG_BLEED_SOURCE3_CB (Respiratory tract)
working.df$COAG_BLEED_SOURCE3_CB[is.na(working.df$COAG_BLEED_SOURCE3_CB)] <- FALSE
working.df$COAG_BLEED_SOURCE3_CB <- as.factor(with(working.df, 
                                            ifelse(COAG_BLEED_SOURCE3_CB==1, TRUE, FALSE)))

#COAG_BLEED_SOURCE4_CB (CNS)
working.df$COAG_BLEED_SOURCE4_CB[is.na(working.df$COAG_BLEED_SOURCE4_CB)] <- FALSE
working.df$COAG_BLEED_SOURCE4_CB <- as.factor(with(working.df, 
                                            ifelse(COAG_BLEED_SOURCE4_CB==1, TRUE, FALSE)))

#COAG_BLEED_SOURCE5_CB (Other location)
working.df$COAG_BLEED_SOURCE5_CB[is.na(working.df$COAG_BLEED_SOURCE5_CB)] <- FALSE
working.df$COAG_BLEED_SOURCE5_CB <- as.factor(with(working.df, 
                                            ifelse(COAG_BLEED_SOURCE5_CB==1, TRUE, FALSE)))

#COAG_BLEED_SOURCE_NONE_CB (no bleeding)
working.df$COAG_BLEED_SOURCE_NONE_CB[is.na(working.df$COAG_BLEED_SOURCE_NONE_CB)] <- FALSE
working.df$COAG_BLEED_SOURCE_NONE_CB <- as.factor(with(working.df, 
                                            ifelse(COAG_BLEED_SOURCE_NONE_CB==1, TRUE, FALSE)))


#COAG_TRANSF_NB_INT
working.df$COAG_TRANSF_NB_INT <- as.character(working.df$COAG_TRANSF_NB_INT)
working.df$COAG_TRANSF_NB_INT <- as.numeric(with(working.df, 
                                                            ifelse(COAG_TRANSF_NB_INT %in% c("ND", ""), NA, 
                                                                   COAG_TRANSF_NB_INT)))

#COAG_THROMBO1_CB (DVT)
working.df$COAG_THROMBO1_CB[is.na(working.df$COAG_THROMBO1_CB)] <- FALSE
working.df$COAG_THROMBO1_CB <- as.factor(with(working.df, 
                                            ifelse(COAG_THROMBO1_CB==1, TRUE, FALSE)))

#COAG_THROMBO2_CB (PE)
working.df$COAG_THROMBO2_CB[is.na(working.df$COAG_THROMBO2_CB)] <- FALSE
working.df$COAG_THROMBO2_CB <- as.factor(with(working.df, 
                                            ifelse(COAG_THROMBO2_CB==1, TRUE, FALSE)))

#COAG_THROMBO3_CB (MI)
working.df$COAG_THROMBO3_CB[is.na(working.df$COAG_THROMBO3_CB)] <- FALSE
working.df$COAG_THROMBO3_CB <- as.factor(with(working.df, 
                                            ifelse(COAG_THROMBO3_CB==1, TRUE, FALSE)))

#COAG_THROMBO4_CB (Limb ischaemia)
working.df$COAG_THROMBO4_CB[is.na(working.df$COAG_THROMBO4_CB)] <- FALSE
working.df$COAG_THROMBO4_CB <- as.factor(with(working.df, 
                                            ifelse(COAG_THROMBO4_CB==1, TRUE, FALSE)))

#COAG_THROMBO5_CB (stroke)
working.df$COAG_THROMBO5_CB[is.na(working.df$COAG_THROMBO5_CB)] <- FALSE
working.df$COAG_THROMBO5_CB <- as.factor(with(working.df, 
                                            ifelse(COAG_THROMBO5_CB==1, TRUE, FALSE)))

#COAG_THROMBO_NONE_CB (none)
working.df$COAG_THROMBO_NONE_CB[is.na(working.df$COAG_THROMBO_NONE_CB)] <- FALSE
working.df$COAG_THROMBO_NONE_CB <- as.factor(with(working.df, 
                                            ifelse(COAG_THROMBO_NONE_CB==1, TRUE, FALSE)))


#COAG_THERAP_ANTICOAG_YN
working.df$COAG_THERAP_ANTICOAG_YN <- as.factor(with(working.df, 
                                            ifelse(COAG_THERAP_ANTICOAG_YN==0, FALSE, 
                                                   ifelse(COAG_THERAP_ANTICOAG_YN==1, TRUE, NA))))

#COAG_ANTICOAG_TYPE_RAD
working.df$COAG_ANTICOAG_TYPE_RAD <- as.character(working.df$COAG_ANTICOAG_TYPE_RAD)
working.df$COAG_ANTICOAG_TYPE_RAD <- with(working.df, ifelse(COAG_ANTICOAG_TYPE_RAD == 1, 
                                                         "UFH", COAG_ANTICOAG_TYPE_RAD))
working.df$COAG_ANTICOAG_TYPE_RAD <- with(working.df, ifelse(COAG_ANTICOAG_TYPE_RAD == 2, 
                                                         "LMWH", COAG_ANTICOAG_TYPE_RAD))
working.df$COAG_ANTICOAG_TYPE_RAD <- with(working.df, ifelse(COAG_ANTICOAG_TYPE_RAD == 3, 
                                                         "other", COAG_ANTICOAG_TYPE_RAD))
working.df$COAG_ANTICOAG_TYPE_RAD <- with(working.df, ifelse(COAG_ANTICOAG_TYPE_RAD == 'ND', 
                                                         NA, COAG_ANTICOAG_TYPE_RAD)) 
working.df$COAG_ANTICOAG_TYPE_RAD <- as.factor(working.df$COAG_ANTICOAG_TYPE_RAD)


#COAG_ANTICOAG_INTERVAL_INT
working.df$COAG_ANTICOAG_INTERVAL_INT <- as.character(working.df$COAG_ANTICOAG_INTERVAL_INT)
working.df$COAG_ANTICOAG_INTERVAL_INT <- as.numeric(with(working.df, 
                                                            ifelse(COAG_ANTICOAG_INTERVAL_INT %in% c("ND", ""), NA, 
                                                                   COAG_ANTICOAG_INTERVAL_INT)))


#COAG_ANTICOAG_INDIC_LD
working.df$COAG_ANTICOAG_INDIC_LD <- as.character(working.df$COAG_ANTICOAG_INDIC_LD)
working.df$COAG_ANTICOAG_INDIC_LD <- with(working.df, ifelse(COAG_ANTICOAG_INDIC_LD == 1, 
                                                         "DVT", COAG_ANTICOAG_INDIC_LD))
working.df$COAG_ANTICOAG_INDIC_LD <- with(working.df, ifelse(COAG_ANTICOAG_INDIC_LD == 2, 
                                                         "PE", COAG_ANTICOAG_INDIC_LD))
working.df$COAG_ANTICOAG_INDIC_LD <- with(working.df, ifelse(COAG_ANTICOAG_INDIC_LD == 3, 
                                                         "MI", COAG_ANTICOAG_INDIC_LD))
working.df$COAG_ANTICOAG_INDIC_LD <- with(working.df, ifelse(COAG_ANTICOAG_INDIC_LD == 4, 
                                                         "limb ischaemia", COAG_ANTICOAG_INDIC_LD))
working.df$COAG_ANTICOAG_INDIC_LD <- with(working.df, ifelse(COAG_ANTICOAG_INDIC_LD == 5, 
                                                         "line or filter clot", COAG_ANTICOAG_INDIC_LD))
working.df$COAG_ANTICOAG_INDIC_LD <- with(working.df, ifelse(COAG_ANTICOAG_INDIC_LD == 6, 
                                                         'prophylaxis', COAG_ANTICOAG_INDIC_LD))
working.df$COAG_ANTICOAG_INDIC_LD <- with(working.df, ifelse(COAG_ANTICOAG_INDIC_LD == 7, 
                                                         'previous condition', COAG_ANTICOAG_INDIC_LD))
working.df$COAG_ANTICOAG_INDIC_LD <- with(working.df, ifelse(COAG_ANTICOAG_INDIC_LD == 'ND', 
                                                         NA, COAG_ANTICOAG_INDIC_LD)) 
working.df$COAG_ANTICOAG_INDIC_LD <- as.factor(working.df$COAG_ANTICOAG_INDIC_LD)

#INF_ANTIBIO_YN
working.df$INF_ANTIBIO_YN <- as.factor(with(working.df, 
                                            ifelse(INF_ANTIBIO_YN==0, FALSE, 
                                                   ifelse(INF_ANTIBIO_YN==1, TRUE, NA))))

validAbx <- c(11, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25, 26, 31, 32, 33, 34, 35, 41, 42, 43, 51, 52, 53, 54, 61, 62, 63, 71, 72, 73, 81, 82, 83, 91, 92, 93, 94, 95, 96)
    
validAntifungal <- c(101, 102, 103, 104, 105, 106)     
    
#INF_ANTIBIO1_INT
## don't get what codes 0, 1, 3, 10, 45,56,67,69 are from the pdf on antibiotics
## there are several "other", one per class of antibiotics (i.e. i named "other quinolones", "other Aminoglycosides", "other2").     
working.df$INF_ANTIBIO1_INT <- ifelse(as.numeric(working.df$INF_ANTIBIO1_INT) %in% validAbx, working.df$INF_ANTIBIO1_INT, NA)
   
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == '', NA, working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 11, "Cefazolin", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 12, "Cefuroxime ", working.df$INF_ANTIBIO1_INT)    
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 13, "Ceftazidime", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 14, "Ceftriaxone", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 15, "Cefepime/cefpirome", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 16, "Other cephalosporin", working.df$INF_ANTIBIO1_INT)

working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 21, "Benzylpenicillin", working.df$INF_ANTIBIO1_INT)    
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 22, "Ampicillin", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 23, "Coamoxiclav", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 24, "Piperacillin-tazobactam", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 25, "Oxa/cloxa/flucloxacillin", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 26, "Other penicillin", working.df$INF_ANTIBIO1_INT)

working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 31, "Imipenam", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 32, "Meropenem", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 33, "Ertapenem", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 34, "Doripenem", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 35, "Other carbapenem", working.df$INF_ANTIBIO1_INT) 

working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 41, "Temocillin", working.df$INF_ANTIBIO1_INT)    
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 42, "Aztreonam", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 43, "Other beta lactams", working.df$INF_ANTIBIO1_INT)

working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 51, "Amikacin", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 52, "Tobramycin", working.df$INF_ANTIBIO1_INT)    
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 53, "Gentamicin", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 54, "Other Aminoglycosides", working.df$INF_ANTIBIO1_INT)

working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 61, "Ciprofloxacin", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 62, "Levofloxacin", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 63, "Other quinolone", working.df$INF_ANTIBIO1_INT)

working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 71, "Vancomycin", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 72, "Teicoplanin", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 73, "Other glycopeptide", working.df$INF_ANTIBIO1_INT)

working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 81, "Erythromycin", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 82, "clarithromycin", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 83, "azithromycin", working.df$INF_ANTIBIO1_INT)

working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 91, "Metronidazole", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 92, "Cotrimoxazole", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 93, "Linezolid", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 94, "Daptomycin", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 95, "Tigecycline", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- ifelse(working.df$INF_ANTIBIO1_INT == 96, "Other other", working.df$INF_ANTIBIO1_INT)
working.df$INF_ANTIBIO1_INT <- as.factor(working.df$INF_ANTIBIO1_INT)


#INF_ANTIBIO2_INT
working.df$INF_ANTIBIO2_INT <- ifelse(as.numeric(working.df$INF_ANTIBIO2_INT) %in% validAbx, working.df$INF_ANTIBIO2_INT, NA)
   
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == '', NA, working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 11, "Cefazolin", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 12, "Cefuroxime ", working.df$INF_ANTIBIO2_INT)    
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 13, "Ceftazidime", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 14, "Ceftriaxone", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 15, "Cefepime/cefpirome", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 16, "Other cephalosporin", working.df$INF_ANTIBIO2_INT)

working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 21, "Benzylpenicillin", working.df$INF_ANTIBIO2_INT)    
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 22, "Ampicillin", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 23, "Coamoxiclav", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 24, "Piperacillin-tazobactam", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 25, "Oxa/cloxa/flucloxacillin", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 26, "Other penicillin", working.df$INF_ANTIBIO2_INT)

working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 31, "Imipenam", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 32, "Meropenem", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 33, "Ertapenem", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 34, "Doripenem", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 35, "Other carbapenem", working.df$INF_ANTIBIO2_INT) 

working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 41, "Temocillin", working.df$INF_ANTIBIO2_INT)    
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 42, "Aztreonam", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 43, "Other beta lactams", working.df$INF_ANTIBIO2_INT)

working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 51, "Amikacin", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 52, "Tobramycin", working.df$INF_ANTIBIO2_INT)    
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 53, "Gentamicin", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 54, "Other Aminoglycosides", working.df$INF_ANTIBIO2_INT)

working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 61, "Ciprofloxacin", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 62, "Levofloxacin", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 63, "Other quinolone", working.df$INF_ANTIBIO2_INT)

working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 71, "Vancomycin", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 72, "Teicoplanin", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 73, "Other glycopeptide", working.df$INF_ANTIBIO2_INT)

working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 81, "Erythromycin", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 82, "clarithromycin", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 83, "azithromycin", working.df$INF_ANTIBIO2_INT)

working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 91, "Metronidazole", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 92, "Cotrimoxazole", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 93, "Linezolid", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 94, "Daptomycin", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 95, "Tigecycline", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- ifelse(working.df$INF_ANTIBIO2_INT == 96, "Other other", working.df$INF_ANTIBIO2_INT)
working.df$INF_ANTIBIO2_INT <- as.factor(working.df$INF_ANTIBIO2_INT)



#INF_ANTIBIO3_INT
working.df$INF_ANTIBIO3_INT <- ifelse(as.numeric(working.df$INF_ANTIBIO3_INT) %in% validAbx, working.df$INF_ANTIBIO3_INT, NA)
   
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == '', NA, working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 11, "Cefazolin", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 12, "Cefuroxime ", working.df$INF_ANTIBIO3_INT)    
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 13, "Ceftazidime", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 14, "Ceftriaxone", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 15, "Cefepime/cefpirome", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 16, "Other cephalosporin", working.df$INF_ANTIBIO3_INT)

working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 21, "Benzylpenicillin", working.df$INF_ANTIBIO3_INT)    
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 22, "Ampicillin", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 23, "Coamoxiclav", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 24, "Piperacillin-tazobactam", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 25, "Oxa/cloxa/flucloxacillin", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 26, "Other penicillin", working.df$INF_ANTIBIO3_INT)

working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 31, "Imipenam", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 32, "Meropenem", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 33, "Ertapenem", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 34, "Doripenem", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 35, "Other carbapenem", working.df$INF_ANTIBIO3_INT) 

working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 41, "Temocillin", working.df$INF_ANTIBIO3_INT)    
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 42, "Aztreonam", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 43, "Other beta lactams", working.df$INF_ANTIBIO3_INT)

working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 51, "Amikacin", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 52, "Tobramycin", working.df$INF_ANTIBIO3_INT)    
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 53, "Gentamicin", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 54, "Other Aminoglycosides", working.df$INF_ANTIBIO3_INT)

working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 61, "Ciprofloxacin", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 62, "Levofloxacin", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 63, "Other quinolone", working.df$INF_ANTIBIO3_INT)

working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 71, "Vancomycin", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 72, "Teicoplanin", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 73, "Other glycopeptide", working.df$INF_ANTIBIO3_INT)

working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 81, "Erythromycin", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 82, "clarithromycin", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 83, "azithromycin", working.df$INF_ANTIBIO3_INT)

working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 91, "Metronidazole", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 92, "Cotrimoxazole", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 93, "Linezolid", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 94, "Daptomycin", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 95, "Tigecycline", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- ifelse(working.df$INF_ANTIBIO3_INT == 96, "Other other", working.df$INF_ANTIBIO3_INT)
working.df$INF_ANTIBIO3_INT <- as.factor(working.df$INF_ANTIBIO3_INT)





#INF_ANTIBIO4_INT: Infection: antibio code 1
working.df$INF_ANTIBIO4_INT <- ifelse(as.numeric(working.df$INF_ANTIBIO4_INT) %in% validAbx, working.df$INF_ANTIBIO4_INT, NA)
   
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == '', NA, working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 11, "Cefazolin", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 12, "Cefuroxime ", working.df$INF_ANTIBIO4_INT)    
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 13, "Ceftazidime", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 14, "Ceftriaxone", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 15, "Cefepime/cefpirome", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 16, "Other cephalosporin", working.df$INF_ANTIBIO4_INT)

working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 21, "Benzylpenicillin", working.df$INF_ANTIBIO4_INT)    
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 22, "Ampicillin", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 23, "Coamoxiclav", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 24, "Piperacillin-tazobactam", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 25, "Oxa/cloxa/flucloxacillin", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 26, "Other penicillin", working.df$INF_ANTIBIO4_INT)

working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 31, "Imipenam", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 32, "Meropenem", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 33, "Ertapenem", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 34, "Doripenem", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 35, "Other carbapenem", working.df$INF_ANTIBIO4_INT) 

working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 41, "Temocillin", working.df$INF_ANTIBIO4_INT)    
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 42, "Aztreonam", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 43, "Other beta lactams", working.df$INF_ANTIBIO4_INT)

working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 51, "Amikacin", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 52, "Tobramycin", working.df$INF_ANTIBIO4_INT)    
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 53, "Gentamicin", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 54, "Other Aminoglycosides", working.df$INF_ANTIBIO4_INT)

working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 61, "Ciprofloxacin", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 62, "Levofloxacin", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 63, "Other quinolone", working.df$INF_ANTIBIO4_INT)

working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 71, "Vancomycin", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 72, "Teicoplanin", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 73, "Other glycopeptide", working.df$INF_ANTIBIO4_INT)

working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 81, "Erythromycin", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 82, "clarithromycin", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 83, "azithromycin", working.df$INF_ANTIBIO4_INT)

working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 91, "Metronidazole", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 92, "Cotrimoxazole", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 93, "Linezolid", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 94, "Daptomycin", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 95, "Tigecycline", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- ifelse(working.df$INF_ANTIBIO4_INT == 96, "Other other", working.df$INF_ANTIBIO4_INT)
working.df$INF_ANTIBIO4_INT <- as.factor(working.df$INF_ANTIBIO4_INT)





#INF_ANTIFUNG_YN
#Infection: antifungal
working.df$INF_ANTIFUNG_YN <- ifelse(working.df$INF_ANTIFUNG_YN == 1, "TRUE", working.df$INF_ANTIFUNG_YN)
working.df$INF_ANTIFUNG_YN <- ifelse(working.df$INF_ANTIFUNG_YN == 0, "FALSE", working.df$INF_ANTIFUNG_YN)
working.df$INF_ANTIFUNG_YN <- as.factor(working.df$INF_ANTIFUNG_YN)

#INF_ANTIFUNG1_INT
working.df$INF_ANTIFUNG1_INT <- ifelse(as.numeric(working.df$INF_ANTIFUNG1_INT) %in% validAntifungal, 
                                       working.df$INF_ANTIFUNG1_INT, NA)    
working.df$INF_ANTIFUNG1_INT <- ifelse(working.df$INF_ANTIFUNG1_INT == 101, "Fluconazole", working.df$INF_ANTIFUNG1_INT)
working.df$INF_ANTIFUNG1_INT <- ifelse(working.df$INF_ANTIFUNG1_INT == 102, "Amphotericin B", working.df$INF_ANTIFUNG1_INT)
working.df$INF_ANTIFUNG1_INT <- ifelse(working.df$INF_ANTIFUNG1_INT == 103, "Ampho lipid formulation", working.df$INF_ANTIFUNG1_INT)
working.df$INF_ANTIFUNG1_INT <- ifelse(working.df$INF_ANTIFUNG1_INT == 104, "Echinocandin", working.df$INF_ANTIFUNG1_INT)
working.df$INF_ANTIFUNG1_INT <- ifelse(working.df$INF_ANTIFUNG1_INT == 105, "Voriconazole", working.df$INF_ANTIFUNG1_INT)
working.df$INF_ANTIFUNG1_INT <- ifelse(working.df$INF_ANTIFUNG1_INT == 106, "Other antifungal", working.df$INF_ANTIFUNG1_INT)
working.df$INF_ANTIFUNG1_INT <- as.factor(working.df$INF_ANTIFUNG1_INT)
    
    
#INF_ANTIFUNG2_INT
working.df$INF_ANTIFUNG2_INT <- ifelse(as.numeric(working.df$INF_ANTIFUNG2_INT) %in% validAntifungal, 
                                       working.df$INF_ANTIFUNG2_INT, NA)    
working.df$INF_ANTIFUNG2_INT <- ifelse(working.df$INF_ANTIFUNG2_INT == 101, "Fluconazole", working.df$INF_ANTIFUNG2_INT)
working.df$INF_ANTIFUNG2_INT <- ifelse(working.df$INF_ANTIFUNG2_INT == 102, "Amphotericin B", working.df$INF_ANTIFUNG2_INT)
working.df$INF_ANTIFUNG2_INT <- ifelse(working.df$INF_ANTIFUNG2_INT == 103, "Ampho lipid formulation", working.df$INF_ANTIFUNG2_INT)
working.df$INF_ANTIFUNG2_INT <- ifelse(working.df$INF_ANTIFUNG2_INT == 104, "Echinocandin", working.df$INF_ANTIFUNG2_INT)
working.df$INF_ANTIFUNG2_INT <- ifelse(working.df$INF_ANTIFUNG2_INT == 105, "Voriconazole", working.df$INF_ANTIFUNG2_INT)
working.df$INF_ANTIFUNG2_INT <- ifelse(working.df$INF_ANTIFUNG2_INT == 106, "Other antifungal", working.df$INF_ANTIFUNG2_INT)
working.df$INF_ANTIFUNG2_INT <- as.factor(working.df$INF_ANTIFUNG2_INT)    
    
    
#INF_ANTIFUNG3_INT
working.df$INF_ANTIFUNG3_INT <- ifelse(as.numeric(working.df$INF_ANTIFUNG3_INT) %in% validAntifungal, 
                                       working.df$INF_ANTIFUNG3_INT, NA)    
working.df$INF_ANTIFUNG3_INT <- ifelse(working.df$INF_ANTIFUNG3_INT == 101, "Fluconazole", working.df$INF_ANTIFUNG3_INT)
working.df$INF_ANTIFUNG3_INT <- ifelse(working.df$INF_ANTIFUNG3_INT == 102, "Amphotericin B", working.df$INF_ANTIFUNG3_INT)
working.df$INF_ANTIFUNG3_INT <- ifelse(working.df$INF_ANTIFUNG3_INT == 103, "Ampho lipid formulation", working.df$INF_ANTIFUNG3_INT)
working.df$INF_ANTIFUNG3_INT <- ifelse(working.df$INF_ANTIFUNG3_INT == 104, "Echinocandin", working.df$INF_ANTIFUNG3_INT)
working.df$INF_ANTIFUNG3_INT <- ifelse(working.df$INF_ANTIFUNG3_INT == 105, "Voriconazole", working.df$INF_ANTIFUNG3_INT)
working.df$INF_ANTIFUNG3_INT <- ifelse(working.df$INF_ANTIFUNG3_INT == 106, "Other antifungal", working.df$INF_ANTIFUNG3_INT)
working.df$INF_ANTIFUNG3_INT <- as.factor(working.df$INF_ANTIFUNG3_INT)    
    
#INF_ANTIFUNG4_INT
working.df$INF_ANTIFUNG4_INT <- ifelse(as.numeric(working.df$INF_ANTIFUNG4_INT) %in% validAntifungal, 
                                       working.df$INF_ANTIFUNG4_INT, NA)    
working.df$INF_ANTIFUNG4_INT <- ifelse(working.df$INF_ANTIFUNG4_INT == 101, "Fluconazole", working.df$INF_ANTIFUNG4_INT)
working.df$INF_ANTIFUNG4_INT <- ifelse(working.df$INF_ANTIFUNG4_INT == 102, "Amphotericin B", working.df$INF_ANTIFUNG4_INT)
working.df$INF_ANTIFUNG4_INT <- ifelse(working.df$INF_ANTIFUNG4_INT == 103, "Ampho lipid formulation", working.df$INF_ANTIFUNG4_INT)
working.df$INF_ANTIFUNG4_INT <- ifelse(working.df$INF_ANTIFUNG4_INT == 104, "Echinocandin", working.df$INF_ANTIFUNG4_INT)
working.df$INF_ANTIFUNG4_INT <- ifelse(working.df$INF_ANTIFUNG4_INT == 105, "Voriconazole", working.df$INF_ANTIFUNG4_INT)
working.df$INF_ANTIFUNG4_INT <- ifelse(working.df$INF_ANTIFUNG4_INT == 106, "Other antifungal", working.df$INF_ANTIFUNG4_INT)
working.df$INF_ANTIFUNG4_INT <- as.factor(working.df$INF_ANTIFUNG4_INT)

#INF_AT_ADMISSION_YN
#Was bacterial pulmonary co-infection present at admission?
working.df$INF_AT_ADMISSION_YN <- ifelse(working.df$INF_AT_ADMISSION_YN == 1, "TRUE", working.df$INF_AT_ADMISSION_YN)
working.df$INF_AT_ADMISSION_YN <- ifelse(working.df$INF_AT_ADMISSION_YN == 0, "FALSE", working.df$INF_AT_ADMISSION_YN)
working.df$INF_AT_ADMISSION_YN <- as.factor(working.df$INF_AT_ADMISSION_YN)

#INF_DURING_ICU_YN
#Did the patient develop an infection at any point during ICU stay
working.df$INF_DURING_ICU_YN <- ifelse(working.df$INF_DURING_ICU_YN == 1, "TRUE", working.df$INF_DURING_ICU_YN)
working.df$INF_DURING_ICU_YN <- ifelse(working.df$INF_DURING_ICU_YN == 0, "FALSE", working.df$INF_DURING_ICU_YN)
working.df$INF_DURING_ICU_YN <- as.factor(working.df$INF_DURING_ICU_YN)

#INF_SEVERITY
#Infection severity
working.df$INF_SEVERITY <- ifelse(working.df$INF_SEVERITY == 1, "Sepsis", working.df$INF_SEVERITY)
working.df$INF_SEVERITY <- ifelse(working.df$INF_SEVERITY == 2, "Septic shock", working.df$INF_SEVERITY)
working.df$INF_SEVERITY <- as.factor(working.df$INF_SEVERITY)
#consistency check: 1 INF_SEVERITY:sepsis to INF_DURING_ICU_YN "FALSE"> changed INF_SEVERITY to NA 
working.df$INF_SEVERITY[working.df$INF_SEVERITY %in% "Sepsis" & working.df$INF_DURING_ICU_YN %in% "FALSE"]<-NA


#INF_PULMO_YN
#Bacterial pulmonary infection
working.df$INF_PULMO_YN <- ifelse(working.df$INF_PULMO_YN == 1, "TRUE", working.df$INF_PULMO_YN)
working.df$INF_PULMO_YN <- ifelse(working.df$INF_PULMO_YN == 0, "FALSE", working.df$INF_PULMO_YN)
working.df$INF_PULMO_YN <- as.factor(working.df$INF_PULMO_YN)

#INF_RESPIR_YN
#Fungal respiratory infection
working.df$INF_RESPIR_YN <- ifelse(working.df$INF_RESPIR_YN == 1, "TRUE", working.df$INF_RESPIR_YN)
working.df$INF_RESPIR_YN <- ifelse(working.df$INF_RESPIR_YN == 0, "FALSE", working.df$INF_RESPIR_YN)
working.df$INF_RESPIR_YN <- as.factor(working.df$INF_RESPIR_YN)

#INF_ABDO_YN
#Abdominal infection
working.df$INF_ABDO_YN <- ifelse(working.df$INF_ABDO_YN == 1, "TRUE", working.df$INF_ABDO_YN)
working.df$INF_ABDO_YN <- ifelse(working.df$INF_ABDO_YN == 0, "FALSE", working.df$INF_ABDO_YN)
working.df$INF_ABDO_YN <- as.factor(working.df$INF_ABDO_YN)

#INF_BACTEREMIA_YN
#Bacteremia (not catheter related)
working.df$INF_BACTEREMIA_YN <- ifelse(working.df$INF_BACTEREMIA_YN == 1, "TRUE", working.df$INF_BACTEREMIA_YN)
working.df$INF_BACTEREMIA_YN <- ifelse(working.df$INF_BACTEREMIA_YN == 0, "FALSE", working.df$INF_BACTEREMIA_YN)
working.df$INF_BACTEREMIA_YN <- as.factor(working.df$INF_BACTEREMIA_YN)

#INF_URINARY_YN
#Urinary tract infection
working.df$INF_URINARY_YN <- ifelse(working.df$INF_URINARY_YN == 1, "TRUE", working.df$INF_URINARY_YN)
working.df$INF_URINARY_YN <- ifelse(working.df$INF_URINARY_YN == 0, "FALSE", working.df$INF_URINARY_YN)
working.df$INF_URINARY_YN <- as.factor(working.df$INF_URINARY_YN)

#INF_CNS_YN
#CNS infection
working.df$INF_CNS_YN <- ifelse(working.df$INF_CNS_YN == 1, "TRUE", working.df$INF_CNS_YN)
working.df$INF_CNS_YN <- ifelse(working.df$INF_CNS_YN == 0, "FALSE", working.df$INF_CNS_YN)
working.df$INF_CNS_YN <- as.factor(working.df$INF_CNS_YN)

#INF_OTHER_YN
#Other infection
working.df$INF_OTHER_YN <- ifelse(working.df$INF_OTHER_YN == 1, "TRUE", working.df$INF_OTHER_YN)
working.df$INF_OTHER_YN <- ifelse(working.df$INF_OTHER_YN == 0, "FALSE", working.df$INF_OTHER_YN)
working.df$INF_OTHER_YN <- as.factor(working.df$INF_OTHER_YN)

#INF_CLABSI_YN
#CLABSI
working.df$INF_CLABSI_YN <- ifelse(working.df$INF_CLABSI_YN == 1, "TRUE", working.df$INF_CLABSI_YN)
working.df$INF_CLABSI_YN <- ifelse(working.df$INF_CLABSI_YN == 0, "FALSE", working.df$INF_CLABSI_YN)
working.df$INF_CLABSI_YN <- as.factor(working.df$INF_CLABSI_YN)

#INF_MDR_PATHO_YN
#Was an MDR pathogen involved?
working.df$INF_MDR_PATHO_YN <- ifelse(working.df$INF_MDR_PATHO_YN == 1, "TRUE", working.df$INF_MDR_PATHO_YN)
working.df$INF_MDR_PATHO_YN <- ifelse(working.df$INF_MDR_PATHO_YN == 0, "FALSE", working.df$INF_MDR_PATHO_YN)
working.df$INF_MDR_PATHO_YN <- as.factor(working.df$INF_MDR_PATHO_YN)

#INF_MDR_SPECIFY_LD
#Specify MDR pathogen involved
working.df$INF_MDR_SPECIFY_LD <- ifelse(working.df$INF_MDR_SPECIFY_LD == 1, "MRSA", working.df$INF_MDR_SPECIFY_LD)
working.df$INF_MDR_SPECIFY_LD <- ifelse(working.df$INF_MDR_SPECIFY_LD == 2, "VRE", working.df$INF_MDR_SPECIFY_LD)
working.df$INF_MDR_SPECIFY_LD <- ifelse(working.df$INF_MDR_SPECIFY_LD == 3, "MDR-PA", working.df$INF_MDR_SPECIFY_LD)
working.df$INF_MDR_SPECIFY_LD <- ifelse(working.df$INF_MDR_SPECIFY_LD == 4, "CRE", working.df$INF_MDR_SPECIFY_LD)
working.df$INF_MDR_SPECIFY_LD <- ifelse(working.df$INF_MDR_SPECIFY_LD == 5, "ESBL", working.df$INF_MDR_SPECIFY_LD)
working.df$INF_MDR_SPECIFY_LD <- ifelse(working.df$INF_MDR_SPECIFY_LD == 6, "Acinetobacter", working.df$INF_MDR_SPECIFY_LD)
working.df$INF_MDR_SPECIFY_LD <- as.factor(working.df$INF_MDR_SPECIFY_LD)
#consistency check: 1 acinetobacter MDR assigned to "FALSE" in INF_MDR_PATHO_YN above > I changed INF_MDR_PATHO_YN 
working.df$INF_MDR_PATHO_YN[working.df$INF_MDR_PATHO_YN %in% "FALSE" & working.df$INF_MDR_SPECIFY_LD %in% "Acinetobacter"]<-"TRUE"

#COAG_WITHOUT_ANTIMIC_INT
#Days alive without anti-microbial therapy at day 30
working.df$COAG_WITHOUT_ANTIMIC_INT <- ifelse(working.df$COAG_WITHOUT_ANTIMIC_INT == 'ND', NA, working.df$COAG_WITHOUT_ANTIMIC_INT)
working.df$COAG_WITHOUT_ANTIMIC_INT <- ifelse(working.df$COAG_WITHOUT_ANTIMIC_INT == '', NA, working.df$COAG_WITHOUT_ANTIMIC_INT)
working.df$COAG_WITHOUT_ANTIMIC_INT <- as.integer(working.df$COAG_WITHOUT_ANTIMIC_INT)
working.df$INF_WITHOUT_ANTIMIC_INT <- working.df$COAG_WITHOUT_ANTIMIC_INT # Rename this variable- was incorrect in dataset

working.df = working.df[,!(names(working.df) %in% c("COAG_WITHOUT_ANTIMIC_INT"))]
    
    
    
#REH_MOBIL_72H_YN
#Was the patient mobilized in the first 72h of ICU?
working.df$REH_MOBIL_72H_YN <- ifelse(working.df$REH_MOBIL_72H_YN == 1, TRUE, working.df$REH_MOBIL_72H_YN)
working.df$REH_MOBIL_72H_YN <- ifelse(working.df$REH_MOBIL_72H_YN == 0, FALSE, working.df$REH_MOBIL_72H_YN)
working.df$REH_MOBIL_72H_YN <- ifelse(working.df$REH_MOBIL_72H_YN == 2, NA, working.df$REH_MOBIL_72H_YN)
working.df$REH_MOBIL_72H_YN <- as.factor(working.df$REH_MOBIL_72H_YN)


#REH_72H_HIGH_IMS_INT
#Patient mobilized in first 72h - Highest IMS
#Set ND and missing to NA, remove data<0 and data>10
#I took IMS reference from Hodgson C, Heart & Lung, 2014, IMS scale from 0 to 10.
working.df$REH_72H_HIGH_IMS_INT <- ifelse(working.df$REH_72H_HIGH_IMS_INT == 'ND', NA, working.df$REH_72H_HIGH_IMS_INT)
working.df$REH_72H_HIGH_IMS_INT <- ifelse(working.df$REH_72H_HIGH_IMS_INT == '', NA, working.df$REH_72H_HIGH_IMS_INT)
working.df$REH_72H_HIGH_IMS_INT <- as.integer(working.df$REH_72H_HIGH_IMS_INT)
working.df$REH_72H_HIGH_IMS_INT <- ifelse(working.df$REH_72H_HIGH_IMS_INT >10 | working.df$REH_72H_HIGH_IMS_INT <0, NA, working.df$REH_72H_HIGH_IMS_INT)

#REH_MOBIL_VENT_72H_YN
#Was the patient mobilized in the first 72h of mechanical ventilation?
working.df$REH_MOBIL_VENT_72H_YN <- ifelse(working.df$REH_MOBIL_VENT_72H_YN == 1, TRUE, working.df$REH_MOBIL_VENT_72H_YN)
working.df$REH_MOBIL_VENT_72H_YN <- ifelse(working.df$REH_MOBIL_VENT_72H_YN == 0, FALSE, working.df$REH_MOBIL_VENT_72H_YN)
working.df$REH_MOBIL_VENT_72H_YN <- ifelse(working.df$REH_MOBIL_VENT_72H_YN == 2, NA, working.df$REH_MOBIL_VENT_72H_YN)
working.df$REH_MOBIL_VENT_72H_YN <- as.factor(working.df$REH_MOBIL_VENT_72H_YN)

#REH_72H_HIGH_IMS_VENT_INT
#Patient mobilized in first 72h of ventilation - Highest IMS
#Set ND and missing to NA, remove data<0 and data>10
#I took IMS reference from Hodgson C, Heart & Lung, 2014, IMS scale from 0 to 10.
working.df$REH_72H_HIGH_IMS_VENT_INT <- ifelse(working.df$REH_72H_HIGH_IMS_VENT_INT == 'ND', NA, working.df$REH_72H_HIGH_IMS_VENT_INT)
working.df$REH_72H_HIGH_IMS_VENT_INT <- ifelse(working.df$REH_72H_HIGH_IMS_VENT_INT == '', NA, working.df$REH_72H_HIGH_IMS_VENT_INT)
working.df$REH_72H_HIGH_IMS_VENT_INT <- as.integer(working.df$REH_72H_HIGH_IMS_VENT_INT)
working.df$REH_72H_HIGH_IMS_VENT_INT <- ifelse(working.df$REH_72H_HIGH_IMS_VENT_INT >10 | working.df$REH_72H_HIGH_IMS_VENT_INT <0, NA, working.df$REH_72H_HIGH_IMS_VENT_INT)

#REH_ECMO_HIGH_IMS_INT
#If the patient was on ECMO, highest achieved IMS while on ECMO
#Set ND and missing to NA, remove data<0 and data>10
#I took IMS reference from Hodgson C, Heart & Lung, 2014, IMS scale from 0 to 10.
working.df$REH_ECMO_HIGH_IMS_INT <- ifelse(working.df$REH_ECMO_HIGH_IMS_INT == 'ND', NA, working.df$REH_ECMO_HIGH_IMS_INT)
working.df$REH_ECMO_HIGH_IMS_INT <- ifelse(working.df$REH_ECMO_HIGH_IMS_INT == '', NA, working.df$REH_ECMO_HIGH_IMS_INT)
working.df$REH_ECMO_HIGH_IMS_INT <- as.integer(working.df$REH_ECMO_HIGH_IMS_INT)
working.df$REH_ECMO_HIGH_IMS_INT <- ifelse(working.df$REH_ECMO_HIGH_IMS_INT >10 | working.df$REH_ECMO_HIGH_IMS_INT <0, NA, working.df$REH_ECMO_HIGH_IMS_INT)
#there are several inconsistencies with working.df$RESP_ECMO_YN, to be verified

#OUTCOME_LD
#Outcome
working.df$OUTCOME_LD <- ifelse(working.df$OUTCOME_LD == 1, "Still in ICU", working.df$OUTCOME_LD)
working.df$OUTCOME_LD <- ifelse(working.df$OUTCOME_LD == 2, "Hospitalized", working.df$OUTCOME_LD)
working.df$OUTCOME_LD <- ifelse(working.df$OUTCOME_LD == 3, "Transfer to other facility", working.df$OUTCOME_LD)
working.df$OUTCOME_LD <- ifelse(working.df$OUTCOME_LD == 4, "Discharged alive", working.df$OUTCOME_LD)
working.df$OUTCOME_LD <- ifelse(working.df$OUTCOME_LD == 5, "Death", working.df$OUTCOME_LD)
working.df$OUTCOME_LD <- ifelse(working.df$OUTCOME_LD == 6, "Palliative discharge", working.df$OUTCOME_LD)
working.df$OUTCOME_LD <- ifelse(working.df$OUTCOME_LD == 7, NA, working.df$OUTCOME_LD)
####working.df$OUTCOME_LD <- as.factor(working.df$OUTCOME_LD, factor())
working.df$OUTCOME_LD <- factor(working.df$OUTCOME_LD, levels = c("Still in ICU", "Hospitalized", "Transfer to other facility","Discharged alive","Death","Palliative discharge"))#,"Unknown"))

#OUT_ICU_DURATION_INT
#Outcome : ICU admission duration
working.df$OUT_ICU_DURATION_INT <- ifelse(working.df$OUT_ICU_DURATION_INT == 'ND', NA, working.df$OUT_ICU_DURATION_INT)
working.df$OUT_ICU_DURATION_INT <- ifelse(working.df$OUT_ICU_DURATION_INT == '', NA, working.df$OUT_ICU_DURATION_INT)
working.df$OUT_ICU_DURATION_INT <- as.integer(working.df$OUT_ICU_DURATION_INT)

#OUT_DEAD_DURING_ICU_YN
#If dead, did patient die in the ICU?
## what is the difference between this variable and the last variable OUT_DEATH_DURING_ICU_YN ?
working.df$OUT_DEAD_DURING_ICU_YN <- ifelse(working.df$OUT_DEAD_DURING_ICU_YN == 1, "TRUE", working.df$OUT_DEAD_DURING_ICU_YN)
working.df$OUT_DEAD_DURING_ICU_YN <- ifelse(working.df$OUT_DEAD_DURING_ICU_YN == 0, "FALSE", working.df$OUT_DEAD_DURING_ICU_YN)
working.df$OUT_DEAD_DURING_ICU_YN <- as.factor(working.df$OUT_DEAD_DURING_ICU_YN)
#consistency with working.df$OUTCOME_LD: 1 patients transfered to other facility and death in ICU. this variable and OUT_DEATH_DURING_ICU_YN are inconsistent


#OUT_HOSP_DURATION_INT
#If discharged alive - hospital admission duration
working.df$OUT_HOSP_DURATION_INT <- ifelse(working.df$OUT_HOSP_DURATION_INT == 'ND', NA, working.df$OUT_HOSP_DURATION_INT)
working.df$OUT_HOSP_DURATION_INT <- ifelse(working.df$OUT_HOSP_DURATION_INT == '', NA, working.df$OUT_HOSP_DURATION_INT)
working.df$OUT_HOSP_DURATION_INT <- as.integer(working.df$OUT_HOSP_DURATION_INT)
#consistency with with working.df$OUTCOME_LD ok

#OUT_DISCHARGE_RRT_YN
#If discharged alive, was patient still on RRT?
working.df$OUT_DISCHARGE_RRT_YN <- ifelse(working.df$OUT_DISCHARGE_RRT_YN == 1, "TRUE", working.df$OUT_DISCHARGE_RRT_YN)
working.df$OUT_DISCHARGE_RRT_YN <- ifelse(working.df$OUT_DISCHARGE_RRT_YN == 0, "FALSE", working.df$OUT_DISCHARGE_RRT_YN)
working.df$OUT_DISCHARGE_RRT_YN <- as.factor(working.df$OUT_DISCHARGE_RRT_YN)
#consistency with working.df$ICU_RRT_DIAL_YN  and with working.df$OUTCOME_LD ok

#OUT_DEATH_DURING_ICU_YN
#If the outcome is 'Death', did the patient die during the ICU stay?
## what is the difference between this variable and the OUT_DEAD_DURING_ICU_YN above?
## I think this variable can be dropped, as it also has  has several values that need cleaning
## values are,    ,,   ,,,  ,,,,  0,,, 0,,,,  1,,, 1,,,,  <NA> 
#rename variable to "OUT_DEATH_DURING_ICU_YN", removing commas, transforming "" to NA
#working.df<-rename(working.df,OUT_DEATH_DURING_ICU_YN=`OUT_DEATH_DURING_ICU_YN,,,,`)
working.df$OUT_DEATH_DURING_ICU_YN<-gsub("\\,", "", working.df$OUT_DEATH_DURING_ICU_YN)
working.df$OUT_DEATH_DURING_ICU_YN <- ifelse(working.df$OUT_DEATH_DURING_ICU_YN == 1, "TRUE", working.df$OUT_DEATH_DURING_ICU_YN)
working.df$OUT_DEATH_DURING_ICU_YN <- ifelse(working.df$OUT_DEATH_DURING_ICU_YN == 0, "FALSE", working.df$OUT_DEATH_DURING_ICU_YN)
working.df$OUT_DEATH_DURING_ICU_YN <- ifelse(working.df$OUT_DEATH_DURING_ICU_YN == "", NA, working.df$OUT_DEATH_DURING_ICU_YN)
working.df$OUT_DEATH_DURING_ICU_YN <- as.factor(working.df$OUT_DEATH_DURING_ICU_YN)
#there is 1 patients "transfered to other facilities" in working.df$OUTCOME_LD and death in ICU here

    
    
######### DERIVED VARIABLES ###########
# Denoted by NEW_xxxx
    
# NEW_BMI
working.df <- working.df %>% 
  mutate(NEW_BMI = INC_WEIGHT_INT/(INC_HEIGHT_INT/100)^2)

working.df$NEW_BMI <- ifelse(working.df$NEW_BMI>100, NA, working.df$NEW_BMI)# Set some limits
working.df$NEW_BMI <- ifelse(working.df$NEW_BMI<10, NA, working.df$NEW_BMI)    # Set some limits


## Deal with anticoagulation free text issues using dictionary generated by Andrea Lavinio and Ari Ercole
anticoagulation_freetext.df <- read.csv('./dictionaries/anticoagulation_freetext_dictionary.csv',fileEncoding="UTF-8-BOM")
get_dose <- anticoagulation_freetext.df$bolus_units_day
get_infusion_dose <- anticoagulation_freetext.df$bolus_units_per_kg_day
names(get_dose) <- anticoagulation_freetext.df$COAG_DVT_DOSE_TXT
names(get_infusion_dose) <- anticoagulation_freetext.df$COAG_DVT_DOSE_TXT
working.df$NEW_COAG_DVT_DAILY_DOSE <- as.numeric(unname(get_dose[working.df$COAG_DVT_DOSE_TXT]))
working.df$TEMP_COAG_DVT_DOSE_INFUSION <- as.numeric(unname(get_infusion_dose[working.df$COAG_DVT_DOSE_TXT]))
working.df$TEMP_COAG_DVT_DOSE_INFUSION <- working.df$TEMP_COAG_DVT_DOSE_INFUSION * working.df$INC_WEIGHT_INT
# Where no daily dose but dose / kg specified, multiply by weight and merge columns
temp_mask <- is.na(working.df$NEW_COAG_DVT_DAILY_DOSE) & ! is.na(working.df$TEMP_COAG_DVT_DOSE_INFUSION)
working.df$NEW_COAG_DVT_DAILY_DOSE[temp_mask] <- working.df$TEMP_COAG_DVT_DOSE_INFUSION[temp_mask]
working.df <- subset(working.df, select = -c(TEMP_COAG_DVT_DOSE_INFUSION))

    
## Deal with antiplatelet free text issues using dictionary generated by Andrea Lavinio and Ari Ercole
antiplatelet_freetext.df <- read.csv('./dictionaries/antiplatelet_freetext_dictionary.csv',fileEncoding="UTF-8-BOM")
get_dose <- antiplatelet_freetext.df$antiplatelet_mg_day
names(get_dose) <- antiplatelet_freetext.df$COAG_ANTIPLAT_DOSE_TXT
working.df$NEW_COAG_ANTIPLAT_DAILY_DOSE <- as.numeric(unname(get_dose[working.df$COAG_DVT_DOSE_TXT]))



######### INCONCISTENCY RESOLUTION ###########
# Deal with inconsistencies between intubated patients denoted as not being invasively ventilated
working.df$RESP_INV_VENT_YN[working.df$RESP_INTUBATED_YN == TRUE] <- TRUE
working.df$RESP_INV_VENT_YN[working.df$RESP_INTUBATED_ICU_STAY_YN == TRUE] <- TRUE
working.df$RESP_INV_VENT_YN[working.df$NEW_COUNTRY_ID == '32' & working.df$NEW_SITE_ID == '05' & working.df$NEW_SUBJECT_ID == '02'] <- TRUE

# Deal with inconsistencies between not intubated patients at admisison or during ICU stay and invasive ventilation
working.df$RESP_INV_VENT_YN[working.df$RESP_INTUBATED_YN == FALSE & working.df$RESP_INTUBATED_ICU_STAY_YN == FALSE & is.na(working.df$RESP_INV_VENT_YN)] <- FALSE


# Deal with inconsistencies between invasive ventilated dependent variables and patients who were not invasively ventilated.
working.df$RESP_PRONE_INTUB_INT[working.df$RESP_INV_VENT_YN == FALSE] <- NA
working.df$RESP_MODE_RAD[working.df$RESP_INV_VENT_YN == FALSE] <- NA
working.df$RESP_PEEP_INT[working.df$RESP_INV_VENT_YN == FALSE] <- NA
working.df$RESP_TIDAL_INT[working.df$RESP_INV_VENT_YN == FALSE] <- NA
working.df$RESP_FIO2_INT[working.df$RESP_INV_VENT_YN == FALSE] <- NA
working.df$RESP_PF_RATIO_INT[working.df$RESP_INV_VENT_YN == FALSE] <- NA
working.df$RESP_PACO2_INT[working.df$RESP_INV_VENT_YN == FALSE] <- NA
working.df$RESP_DRIV_PRESS_INT[working.df$RESP_INV_VENT_YN == FALSE] <- NA
working.df$RESP_VENT_ROUTINE_YN[working.df$RESP_INV_VENT_YN == FALSE] <- NA
working.df$RESP_REINTUBATED_YN[working.df$RESP_INV_VENT_YN == FALSE] <- NA

# Deal with inconsistencies between invasive ventilation and ECMO based on info provided by the relevant centres + exclude 1 patient
working.df$RESP_INV_VENT_YN[working.df$NEW_COUNTRY_ID == '11' & working.df$NEW_SITE_ID == '06' & working.df$NEW_SUBJECT_ID == '25'] <- TRUE
working.df$RESP_ECMO_YN[working.df$NEW_COUNTRY_ID == '25' & working.df$NEW_SITE_ID == '02' & working.df$NEW_SUBJECT_ID == '08'] <- FALSE
working.df <- working.df[!(working.df$NEW_COUNTRY_ID=='16' & working.df$NEW_SITE_ID == '08' & working.df$NEW_SUBJECT_ID =='06'),]
   
return(working.df)
}


