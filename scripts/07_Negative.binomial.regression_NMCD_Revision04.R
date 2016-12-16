library(MASS)
library(pscl)

#' We only consider cases where SH was experienced after our cut-off for the negative binomial model , ...
#' ... we only include these cases in a newly created temporary dataframe
#' no censored cases are included in this analysis due to probable bias (see manuscript page 6, line 20)
Kaplan_subset2 = Kaplan_subset[Kaplan_subset$Hypo_2012_and_afterwards==1,]

#' creating the negative binomial model
neg.bin=glm.nb(formula = Kaplan_subset2$Number_SH_before_2012~Kaplan_subset2$Time_to_Hypo_after_2011_month)

#' creating the poisson model
#' one of the reviewers suggested to conduct a poisson model for the count data
#' we tested whether poisson will fit the data
poisson.model=glm(formula = Kaplan_subset2$Number_SH_before_2012~Kaplan_subset2$Time_to_Hypo_after_2011_month, family = poisson)


#' check overdispersion for poisson model
#' Conducting the vuong test
#' we see a clear result suggesting to use the negative binomial model
vuong(m1 = poisson.model, m2 = neg.bin)

#' checking AIC and BIC
#' both criteria suggest to use negative binomial
BIC(neg.bin)
BIC(poisson.model)
AIC(neg.bin)
AIC(poisson.model)

#' conduct dispersion test of package AER
#' we see a clear deviation from H0 which implies overdispersion
library(AER)
dispersiontest(object = poisson.model, trafo = 1)

#' At the end, negative binomial regression fits our data best

#' summary and get exponantiated coefficient
#' in our example data we see only a trend and no significant association
summary(neg.bin) 
exp(coef(neg.bin)[2])

#' this result belongs to page 9 (line 11-14) of the manuscript.
#' more information for this application of negative binomial regression can be found here:
#' http://www.ats.ucla.edu/stat/r/dae/nbreg.htm
