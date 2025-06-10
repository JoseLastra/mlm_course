# Exercise 10: analysis of field trial data from Split-plot design

# a
setwd("....") # Set working directory to whatever you need

# Read the data: Field, Block, T (type of fertilizer), V (Variety), y (yield)
mydata <- read.csv('exercise10.csv')
head(mydata)
summary(mydata)
dim(mydata)

# We ignore factor Block for the moment.

# b Making experimental factors:
mydata$field <- factor(mydata$Field)
mydata$t     <- factor(mydata$T,labels=c('T1','T2','T3'))
mydata$v     <- factor(mydata$V,labels=c('V1','V2','V3','V4'))

# c Ordinary two-way ANOVA, without fields
lmanova <- aov(y ~ t + v + t:v, data=mydata)
summary(lmanova)

# d Include field in the model

# d-i Model 1: split-plot, ANOVA method
model1 <- aov(y ~ t + v + t:v + Error(field), data=mydata)
summary(model1)

# d-ii Model 2: Anova with fixed field effects, field before fertilizer type
model2 <- aov(y ~ field + t + v + t:v, data=mydata)
summary(model2)

# d-iii Model 3: Anova with fixed field effects, field after fertilizer type
model3 <- aov(y ~ t + v + t:v + field, data=mydata)
summary(model3)

# e  One way Anova for type of fertilizer on means per field
library(doBy)
fieldmeans <- summaryBy(y ~ field + t, data=mydata, FUN=mean)
oneway.fertilizer <- aov(y.mean ~ t, data=fieldmeans)
summary(oneway.fertilizer)

# f  Estimate and test the variance component for field in the ANOVA method for mixed models

m1 <- aov(y ~ t + v + t:v + Error(field), data=mydata)
(m1sum <- summary(m1))

(msresfield <- m1sum$"Error: field"[[1]]$"Mean Sq"[2])
(msreswithin <- m1sum$"Error: Within"[[1]]$"Mean Sq"[3])

# Get the degrees of freedom of both
(df1 <- m1sum$"Error: field"[[1]]$"Df"[2])
(df2 <- m1sum$"Error: Within"[[1]]$"Df"[3])

# Calculate variance component for fields, residual error
(fieldvar <- (msresfield - msreswithin)/16)
(errorvar <- msreswithin)

# Calculate F value and p-value for field variance
(Fvaluefield <- msresfield/msreswithin)
(pvalfield <- pf(Fvaluefield, df1=df1, df2=df2, lower=FALSE))

# Print the results
cat("variance component for field:", fieldvar,"\n")
cat("variance component for error (=error variance):", errorvar,"\n")
cat("F statistic for variance component for field:", Fvaluefield,"\n")
cat("Degrees of freedom for F statistic: df1=", df1, "df2=",df2,"\n")
cat("P-value for variance component for field:", pvalfield,"\n")


# g REML method

# g-i Load packages for the REML method in mixed models

# if needed: install.packages(c("lme4","pbkrtest","lmerTest"))
# Make packages available to the script
library(lme4)

# g-ii 
# Split-plot, ANOVA method using aov
m1.anova <- aov(y ~ t + v + t:v + Error(field), data=mydata)
summary(m1.anova)

# Split-plot, REML method using lmer
m1.reml <- lmer(y ~ t + v + t:v + (1|field), data=mydata)
anova(m1.reml)

# g-iii  Variance components
vcomp <- VarCorr(m1.reml)
print(vcomp, comp=c("Variance","Std.Dev"))

summary(m1.reml)

# g-iv  Kenward-Roger F-test for interaction T x V
library(pbkrtest)
library(lmerTest)
m1.reml <- lmer(y ~ t + v + t:v + (1|field), data=mydata)
anova(m1.reml, ddf="Kenward-Roger", type="II")

# Alternative method: test for interaction FM versus RM
lmmA <- lmer(y ~ t + v + t:v + (1|field),data=mydata)
lmmB <- lmer(y ~ t + v + (1|field),data=mydata)
Fint <- KRmodcomp(lmmA,lmmB)
summary(Fint)

# g-v Tests for main effects after interaction

# Look in the anova table produced earlier, using type II comparisons
anova(m1.reml, ddf="Kenward-Roger", type="II")

# Alternatively, fit models without the interaction:
# Split-plot, REML, lmer, now without the interaction term
m2.reml <- lmer(y ~ t + v + (1|field), data=mydata)
anova(m2.reml)
vcomp2 <- VarCorr(m2.reml)
print(vcomp2, comp=c("Variance","Std.Dev"))

# Split-plot, Anova method, aov, now without the interaction term
m2.anova <- aov(y ~ t + v + Error(field), data=mydata)
summary(m2.anova)

# h Estimation of treatment means and differences
library(emmeans)

# h-i Fit again mixed effects model using REML method
m1.reml <- lmer(y ~ t + v + t:v + (1|field), data=mydata)
anova(m1.reml)
# Remove the interaction term, which is non-significant
m2.reml <- lmer(y ~ t + v + (1|field), data=mydata)
anova(m2.reml)

# h-ii variety means with standard errors and ci's
(varietymeans <- emmeans(m2.reml, ~ v))
plot(varietymeans, xlab="Yield", ylab="Variety")

# h-iii fertilizer means with standard errors and ci's
(fertilizermeans <- emmeans(m2.reml, ~ t))
plot(fertilizermeans, xlab="Yield", ylab="Fertilizer type")

# h-iv Post-hoc tests for variety and type of fertilizer
pairs(varietymeans, adjust="none")
pairs(fertilizermeans, adjust="none")

# h-v Compact letter displays, i.e. ordered table of variety means with letter indicators of significant differences
# we need packages multcomp and multcompView

library(multcomp)
library(multcompView)

cld(varietymeans, Letters=letters, reversed=TRUE, adjust="none")

# h-vi Redo the comparison among the means, now with Tukey correction for multiple comparisons
pairs(varietymeans, adjust="tukey")
cld(varietymeans, Letters=letters, reversed=TRUE, adjust="tukey")

# h-vii Post-hoc tests from results for the ANOVA method
m2.anova <- aov(y ~ t + v + Error(field), data=mydata)
emmeans(m2.anova, pairwise ~ v, adjust="none")


# h-viii Test the variance component for field (whole plot) by construction of LRT.
FM <- lmer(y ~ t + v + (1|field), data=mydata)
RM <- lm(y ~ t + v, data=mydata) # ordinary linear model!
(LL.FM <- logLik(FM))
(LL.RM <- logLik(RM, REML=TRUE))
(LRT <- 2*(LL.FM-LL.RM))
P <- pchisq(LRT,1, lower.tail=FALSE)/2
cat("P-value for variance component:", P)

# Compare with results from rand() function
library(lmerTest)
rand(m2.reml)


