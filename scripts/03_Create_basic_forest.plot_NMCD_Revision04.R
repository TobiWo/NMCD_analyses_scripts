# execute the logistic regression "02_logistic_regression_analysis_NMCD_Revision04.R" script is necesary before creating the basic forest plot

x=NULL
for(i in 1:nrow(glm_data)) {
   x = c(x, glm_data[i,3])
   x = c(x, glm_data[i,2])
   x = c(x, glm_data[i,4])
}
rm(i)

y=c(17,17,17,
    16,16,16,
    15,15,15,
    14,14,14,
    13,13,13,
    12,12,12,
    11,11,11,
    10,10,10,
    9,9,9,
    8,8,8,
    7,7,7,
    6,6,6,
    5,5,5,
    4,4,4,
    3,3,3,
    2,2,2,
    1,1,1
    )

plot(x = x, y = y, log = "x", axes = T, xlim = c(1,20))

var1=1
var2=3
for(i in 1:17){
        lines(x = c(x[var1], x[var2]), y = c(y[var1], y[var2]), type = "l", col="red")
        var1=var1+3
        var2=var2+3
}


#' This is just the forest plot template
#' Since I did not found any good package for plotting forest plots I will do the main part manually with Inkscape
#' 
#' After completion of logistic regression please clean the workspace completely