#' Loading the latest file
load("path/SH.data_2007__2014_20160411.RData")

#' Subset all patients: - with SH (case group) or no SH (control group) 
#'                      - type 1 diabetes
#' FOR LOGISTIC REGRESSION
#' use last case
patients_SH_noSH_last_event=subset(x = Hypos_all_20160411, subset = (Hypos_all_20160411$control=="no" &Hypos_all_20160411$DiabetesTyp==1 &
                                                                    (Hypos_all_20160411$multiple.SH=="no" | Hypos_all_20160411$last.case_of_multiple.SH=="yes")) | 
                                                                    (Hypos_all_20160411$control=="yes" & Hypos_all_20160411$DiabetesTyp==1))

#' creating new age-stratified variable
#' used as covariate in logisitc regression 
patients_SH_noSH_last_event$age_cat_40[patients_SH_noSH_last_event$Alter<40]="0" # <40
patients_SH_noSH_last_event$age_cat_40[patients_SH_noSH_last_event$Alter>=40]="1" # >40
