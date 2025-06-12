# Comparing FULL and REDUCED models with F-test

# First fit FULL MODEL
FM <- lm(wgtgain ~ soiltype, data=loamysoils)
anova(FM)
(SSE.FM <- deviance(FM))         # deviance function gives error sum of squares
(dfE.FM <- df.residual(FM))      # df.residual function gives error degrees of freedom

# Next fit REDUCED MODEL
RM <- lm(wgtgain ~ 1, data=loamysoils)   # strange model: intercept-only model
anova(RM)
(SSE.RM <- deviance(RM))
(dfE.RM <- df.residual(RM))

# Construct F-test statistic 
numeratorF <- (SSE.RM - SSE.FM)/(dfE.RM-dfE.FM)
denominatorF <- SSE.FM/dfE.FM
(F <- numeratorF / denominatorF)
(Pval <- 1-pf(F,dfE.RM-dfE.FM,dfE.FM))


# F-statistic can be obtained directly using anova() function:
anova(FM,RM)
