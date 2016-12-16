#' LOGISTIC REGRESSION MODELS
#' order is adapted from forest-plot within the manuscript
#' 

#' varying prediction variables in logistic regression models
models=c("log_glm_care_homes", "log_glm_impaired_awareness_SH", "log_glm_Creatinine_Clearance_60ml",
         "log_glm_Creatinine_Clearance_30ml", "log_glm_Basal_insulin_NPH", "log_glm_Basal_insulin_Glargine", "log_glm_Basal_insulin_Detemir",
         "log_glm_Basal_insulin_CSII", "log_glm_short_acting_analogues", "log_glm_Depression", "log_glm_HLP", "log_glm_Dementia", "log_glm_malignancy",
         "log_glm_ß-blockers", "log_glm_diuretics", "log_glm_comb1", "log_glm_comb2")

#' column numbers of prediction variables in dataframe
models_in_df=c(133, 190, 128, 129, 82, 80, 84, 85, 104, 144, 136, 139, 140, 158, 163, 159, 160)

#' creating empty list for storing the models
glm_model_list=list()

#' run logistic regression models and store in list 'glm_model_list'
for(i in 1:length(models_in_df)){
        stat_model=glm(formula = patients_SH_noSH_last_event$control~patients_SH_noSH_last_event[,models_in_df[i]]+
                                              patients_SH_noSH_last_event$age_cat_40+patients_SH_noSH_last_event$sex+
                                              patients_SH_noSH_last_event$BMI_ln+patients_SH_noSH_last_event$HbA1c_ln+
                                              patients_SH_noSH_last_event$diabetes.duration_years+patients_SH_noSH_last_event$Charlson_Index, family = binomial)
        assign(x = models[i], value = stat_model)
        glm_model_list[[models[i]]]=get(x = models[i])
}
#' clean workspace
rm(list = models) 
rm(stat_model,i)

#' create empty final dataframe which will store model p-values, OR and CI
glm_data=data.frame(matrix(nrow = 18, ncol = 4))
colnames(glm_data)=c("p", "OR", "2.5%", "97.5%")

#' extract data from models
for(i in 1:17){
        glm_data[i,1]=round(coef(summary(glm_model_list[[i]]))[2,4], digits = 4)
        glm_data[i,2]=round(1/exp(coef(glm_model_list[[i]]))[2], digits = 3 )
        glm_data[i,3]=round(1/exp(confint(glm_model_list[[i]]))[2,2], digits = 2)
        glm_data[i,4]=round(1/exp(confint(glm_model_list[[i]]))[2,1], digits = 2)
        rownames(glm_data)[i]=names(glm_model_list)[i]
}
rm(i)  
glm_data=glm_data[1:17,]


# do multiple comparison correction (Benjamini & Hochberg 1995)
p.vals=glm_data[,1]
adj_p.vals=p.adjust(p = p.vals, method = "BH")
glm_data$adj.pval=round(x = adj_p.vals, digits = 4)


# write final results
write.table(x = glm_data, file = "path/logistic_results.txt", quote = F, sep = "\t", row.names = T, col.names = T)



# godness-of-fit test for logistic regression
library(ResourceSelection)

gof.df=data.frame(matrix(nrow = length(glm_model_list), ncol = 1))
colnames(gof.df)="Hosmer-Lemeshow p-value"

for(i in 1:length(glm_model_list)){
   model=glm_model_list[[i]]
   gof.df[i,1]= hoslem.test(x = model$y, fitted(model))[3]
   rownames(gof.df)[i]=names(glm_model_list)[i]
}
rm(i, model)
gof.df[,1]=round(x = gof.df[,1], digits = 4)

write.table(x = gof.df, file = "path/log.reg_gof.test.txt", quote = F, sep = "\t", row.names = T, col.names = T)
