#' Loading the example data
#' NOTE: LINE 12 AND 15 ARE BASED ON OUR DATA (Therefore commented out)
#' FOR THE EXAMPLE DATA, PLEASE START AT LINE 18 (after loading the data)
#' NOTE2: Example data is based on similar n 
load("path/example_data.RData")

#' Subset all cases: - with SH (case group) 
#'                   - type 1 diabetes
#'                  
#' Further subseting for single patients is realised in a separate script --> special subsetting for Kaplan Meier etc. is necessary                 
#' FOR Kaplan Meier, log-rank test , Cox regression, negative binomial regression 
###all_patients_SH_type1_all = subset(x = Hypos_all_20160411, subset = (Hypos_all_20160411$control=="no" & Hypos_all_20160411$DiabetesTyp==1))

#' extracting only necessary columns
###all_patients_SH_type1_all = all_patients_SH_type1_all[,c(1:33,54,109,110,116,117,59,150,56)]

#' create second, smaller subset for 
all_patients_SH_type1 = all_patients_SH_type1_all[,c(1,2,11)]
