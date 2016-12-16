library(survival)
library(plyr)
library(ggplot2)
library(survival)
library(gridExtra)
library(grid)
library(boot)
library(rms)

#' Kaplan Meier plot is based on a function found here:
#' https://statbandit.wordpress.com/2011/03/08/an-enhanced-kaplan-meier-plot/
#' Source code was copied and saved in script 'ggkm.R'
source("path/ggkm.R")


#' do the survival calculation using survfit and survdiff from the survival package
Kaplan.Meier = survfit(formula = Surv(time = Kaplan_subset$Time_to_Hypo_after_2011_month, event = Kaplan_subset$Hypo_2012_and_afterwards) ~ 
                                 Kaplan_subset$Kaplan_cat, data = Kaplan_subset)


#' default of argument rho==0 (see survdiff help) --> so this code represents a log-rank test
#' we see a significant difference between both groups
survdiff(formula = Surv(time = Kaplan_subset$Time_to_Hypo_after_2011_month, event = Kaplan_subset$Hypo_2012_and_afterwards) ~ 
            Kaplan_subset$Kaplan_cat, data = Kaplan_subset)


#' Since our plot is not from top left to bottom right but bottom left to top right we turn the probabilities
#' After this step they represent the probability to experience an SH instead of representing the probability to experience no SH
Kaplan.Meier$surv = 1 - Kaplan.Meier$surv


#' create the plot conducting the function ggkm
#' Note: Since the source code of the plot-function includes to start at a probability of 1 (100%) but we changed the probability as described above (line 23-25), ...
#' ... we adapted the source code on line 42. The line was adapted to:
#' zeros <- data.frame(time = 0, surv = 0, strata = factor(ystratalabs, levels=levels(.df$strata)), upper = 1, lower = 1)
#' in particular: argument 'surv' was set to 0   
ggkm(sfit = Kaplan.Meier, ylabs = "Probability To Experience an severe hypoglycaemia", xlabs="Time in months", ystrataname = "SH-Groups", pval = T, timeby = 6)


# confidence interval (based on mean) for the mean time without an severe hypoglycaemia
# Again with boot strapping --> see http://stats.stackexchange.com/questions/112829/how-do-i-calculate-confidence-intervals-for-a-non-normal-distribution
m1=mean(Kaplan_subset$Time_to_Hypo_after_2011_month[Kaplan_subset$Kaplan_cat==0])
m2=mean(Kaplan_subset$Time_to_Hypo_after_2011_month[Kaplan_subset$Kaplan_cat==1])


# function to obtain the mean
Bmean <- function(data, indices) {
   d <- data[indices] # allows boot to select sample 
   return(mean(d))
} 


# bootstrapping with 1000 replications 
results1 <- boot(data=Kaplan_subset$Time_to_Hypo_after_2011_month[Kaplan_subset$Kaplan_cat==0], statistic=Bmean, R = 1000)
results2 <- boot(data=Kaplan_subset$Time_to_Hypo_after_2011_month[Kaplan_subset$Kaplan_cat==1], statistic=Bmean, R = 1000)


# view results
results1
results2
plot(results1)
plot(results2)


# get 95% confidence interval 
boot.ci(results1, type=c("norm", "basic", "perc", "bca"))
boot.ci(results2, type=c("norm", "basic", "perc", "bca"))



# Cox regression

#' First subset data:
#' excluding all NA's since this will lead to problems during stepwise model selection
Cox_subset = subset(x = Kaplan_subset, subset = !is.na(Kaplan_subset$age) & !is.na(Kaplan_subset$sex) & !is.na(Kaplan_subset$BMI_ln) &
                       !is.na(Kaplan_subset$HbA1c_ln) & !is.na(Kaplan_subset$diabetes.duration_years) & !is.na(Kaplan_subset$Charlson_Index))


#' Do Cox regression with full model (all covariates as in logistic regression)
Cox.regression = coxph(formula = Surv(time = Cox_subset$Time_to_Hypo_after_2011_month, event = Cox_subset$Hypo_2012_and_afterwards) ~ 
                                 Cox_subset$Kaplan_cat+Cox_subset$age+Cox_subset$sex+Cox_subset$BMI_ln+Cox_subset$HbA1c_ln+Cox_subset$diabetes.duration_years+Cox_subset$Charlson_Index, data = Cox_subset, method = "efron")


#' conduct stepwise selection based on AIC with backward direction
step(Cox.regression, direction = "backward")


#' conduct the final model on the whole data-set
#' no interruption through NA's --> this was only for the stepwise procedure
#' in our real world scenario HbA1c and diabetes duration are spit out as covariates by the stepwise procedure
#' in our example data only diabetes duration is suggested as covariate
Cox.regression = coxph(formula = Surv(time = Kaplan_subset$Time_to_Hypo_after_2011_month, event = Kaplan_subset$Hypo_2012_and_afterwards) ~ 
                            Kaplan_subset$Kaplan_cat+Kaplan_subset$diabetes.duration_years, method = "efron")


#' show results of Cox regression
Cox.regression


#' check proportional hazard assumption
#' one assumption -- Kaplan Meier curves do not cross and have proportional trend -- is already fulfilled --> see Kaplan Meier plot

#' Schoenfeld residuals and plot of the Schoenfeld residuals versus log(time)
Cox.regression_PH = cox.zph(fit = Cox.regression, transform = 'log')
Cox.regression_PH
plot(Cox.regression_PH)


#' log-log-plot
plot(Kaplan.Meier, lty=2:3, fun="cloglog", xlab="Months", ylab="log-log survival plot")

#' in total: no violoation of PH assumption --> this is the same for our real world data
