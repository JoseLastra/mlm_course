# Simple linear regression for lime data

limepH <- data.frame(lime=c(0,1,2,4,8), pH=c(5.71,5.75,5.84,6.16,6.47))

regr.object <- lm(pH ~ lime, data=limepH)

confint(regr.object)
# Locate 95% confidence interval for slope