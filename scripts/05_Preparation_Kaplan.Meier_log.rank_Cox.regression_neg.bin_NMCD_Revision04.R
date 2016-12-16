#' creating variable with cutoff
cutoff = as.Date("2011/12/31")

#' sort data after the patient ID and create two empty new variables in the data frame which are necessary for Kaplan-Meier etc.
all_patients_SH_type1 = all_patients_SH_type1[order(all_patients_SH_type1$patient_ID),]
all_patients_SH_type1$Hypo_2012_and_afterwards = NA
all_patients_SH_type1$Time_to_Hypo_after_2011_days = NA

#' define whether a patient experienced an SH after our set cut-off
#' further calculate the time from cutoff to SH-event
#' so far the time to SH after cutoff is calculated for all SH-events/patient which appeared after cutoff
#' filtering for first SH after cutoff is done later 
for(i in 1:nrow(all_patients_SH_type1)){
        patient = all_patients_SH_type1[i,]
        if(patient$HypoDate2>cutoff){
            all_patients_SH_type1$Hypo_2012_and_afterwards[i]=1
            if(patient$HypoDate2-cutoff>0){
                all_patients_SH_type1$Time_to_Hypo_after_2011_days[i]=patient$HypoDate2-cutoff
            }else {
                all_patients_SH_type1$Time_to_Hypo_after_2011_days[i]=NA
            }
        } else {
            all_patients_SH_type1$Hypo_2012_and_afterwards[i]=0
            all_patients_SH_type1$Time_to_Hypo_after_2011_days[i]=NA
        }
}
#' clean workspace
rm(i, patient)

#' calculate time with no SH after cutoff in months
all_patients_SH_type1$Time_to_Hypo_after_2011_month = all_patients_SH_type1$Time_to_Hypo_after_2011_days/(365/12)

#' create empty variable which will be used to specify the data point which will be used for Kaplan-Meier etc.
all_patients_SH_type1$Patient_for_Kaplan_Calculation = NA

#' splitting the dataframe for patient_ID
#' the result is a list of dataframes
#' every dataframe is referring to one patient
#' this is needed for the next loop where we will define which data-point is used for Kaplan-Meier etc.
all_patients_SH_type1_splitted = split(x = all_patients_SH_type1, f = all_patients_SH_type1$patient_ID)

#' creating an empty temporary dataframe which will hold information about which case will be used for Kaplan-Meier etc.
all_patients_SH_type1_new = data.frame(matrix(ncol = 7))
colnames(all_patients_SH_type1_new) = colnames(all_patients_SH_type1)
all_patients_SH_type1_new$Number_SH_before_2012 = NA


#' loop to fill the empty dataframe
#' the loop is based on the before created list of dataframes, each dataframe comprising one patient 
for(i in 1:length(all_patients_SH_type1_splitted)){
    part = all_patients_SH_type1_splitted[[i]]        # extracting one patient from the list
    part$Number_SH_before_2012 = NA                   # define new column with number of SH before cutoff --> initial NA
    if(all(is.na(part$Time_to_Hypo_after_2011_days)) & all(part$HypoDate2 < cutoff)){ # if there is no event after 2011 the first event is set as event for Kaplan-Meier
                                                                                      # since all data-points will set to the same number of SH before the cutoff (per patient)...
                                                                                      # it does not matter which event will be taken for analysis
                                                                                      # later this will defined as right-censored
        part$Patient_for_Kaplan_Calculation[1]=1
    }else if(any(!is.na(part$Time_to_Hypo_after_2011_days))){ # now if any event is after our cutoff, the event ...
                                                              # ...which has the smallest distance to our cutoff or in other words is the first event after the cutoff,   
                                                              # will be set as event of interest for Kaplan-Meier...
                                                              # Note: Since the object 'part' represents one patient, this done separately for each patient
                                                              
        part$Patient_for_Kaplan_Calculation[which(part$Time_to_Hypo_after_2011_days==min(part$Time_to_Hypo_after_2011_days, na.rm = T))]=1
    }
    
    # this loop is simply for calculing the number of SH-events before the cutoff --> later necessary for the grouping
    for(b in 1:nrow(part)) {
        part$Number_SH_before_2012[b] = sum(part$HypoDate2<=cutoff)
    }
    
    # here we collect the datapoints to the new temporary dataframe
    all_patients_SH_type1_new = rbind(all_patients_SH_type1_new, part)
}
#' Cleaning the workspace
rm(i, part, b)

#' deleting first line of temporary dataframe --> empty line
all_patients_SH_type1_new = all_patients_SH_type1_new[2:nrow(all_patients_SH_type1_new),]

#' transfer the date of the event to the temporary dataframe only in case if patient-ID in original and temporary dataframe are on same position
if(all(all_patients_SH_type1$patient_ID==all_patients_SH_type1_new$patient_ID)){
    all_patients_SH_type1_new$HypoDate2=all_patients_SH_type1$HypoDate2
}

#' merging the original and temporary dataset
all_patients_SH_type1_all = merge(x = all_patients_SH_type1_all, y = all_patients_SH_type1_new[,c(1,4:ncol(all_patients_SH_type1_new))], by = "SH.ID", all.x = T, sort = F)

#' stratification: number of SH before cut-off
all_patients_SH_type1_all$Kaplan_cat[all_patients_SH_type1_all$Number_SH_before_2012 <= 2] = 0
all_patients_SH_type1_all$Kaplan_cat[all_patients_SH_type1_all$Number_SH_before_2012 > 2] = 1

#' the final subset of patients which are used for the statistical models
Kaplan_subset = subset(x = all_patients_SH_type1_all, subset = all_patients_SH_type1_all$Patient_for_Kaplan_Calculation==1)

#' all patients which have an NA are censored
#' This means that thay did not experience any SH after the cutoff of 20111231
#' Therefore the time to the next SH is set to the maximum -> length of observation time after the cutoff 
Kaplan_subset$Time_to_Hypo_after_2011_month[is.na(Kaplan_subset$Time_to_Hypo_after_2011_month)] = 36
