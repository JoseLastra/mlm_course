# Check if soiltype is factor = qualitative explanatory variable:
is.factor(loamysoils$soiltype)

# Fit linear model
lm.object <- lm(wgtgain ~ soiltype, data=loamysoils)  

# Ask for regression coefficients and their t-tests
coef(summary(lm.object))
