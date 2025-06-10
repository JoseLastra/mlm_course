# Simple linear regression for lime data

limepH <- data.frame(lime=c(0,1,2,4,8), pH=c(5.71,5.75,5.84,6.16,6.47))

regr.object <- lm(pH ~ lime, data=limepH)

summary(regr.object)
# Locate standard error of slope in summary output
# Locate estimate of error standard deviation in summary output
# note: error standard deviation = sqrt(MSE) = "residual standard error"

anova(regr.object)
# Locate SSE, dfe, MSE in ANOVA table