# Exercise 8: Split-plot design

# Split-plot example on sorghum taken from Kuehl, pg 487.
# Two treatment factors: plant density (4 levels) and hybrid (3 hybrids).
# Plant density is main plot factor, hybrid subplot factor.
# Four blocks, each with 4 main plots, splitted into 3 subplots.
# In total 4x4x3 = 48 observations.

#-------------------------------#
# c) Read data, fit mixed model #
#-------------------------------#

# Read data into R 

block <- factor(rep(1:4, each=12))
density  <- factor(rep(rep(c(10,15,25,40), each=3), times=4))
mainplot <- factor(rep(1:16, each=3))
hybrid <- factor(rep(1:3, times=16 ))
subplot <- factor(1:48)
weight <- c(40.7,39.4,68.7,24.2,31.3,26.2,16.1,17.9,20.5,11.2,14.8,18.9,
            37.8,47.8,56.2,44.4,34.5,48.1,17.6,30.5,28.2,12.7,17.3,26.2,
            32.9,44.4,44.8,27.8,25.6,41.1,19.9,22.5,30.0,14.5,17.7,19.2,
            43.1,49.0,59.3,34.1,50.4,46.0,20.1,25.2,24.7,15.4,18.7,22.0)

(splitplot <- data.frame(block, density, mainplot, hybrid, subplot, weight))

# Fit appropriate mixed model, using function lme:
library(nlme)
sp1 <- lme(weight ~ density + hybrid + density:hybrid, random = ~ 1 | block / mainplot, data=splitplot)
summary(sp1)

# Alternatively, use function lmer from lme4 package:
library(lme4)
sp2 <- lmer(weight ~ density + hybrid + density:hybrid + (1 | block / mainplot), data=splitplot)

# Alternatively, use function aov, because it a balanced design:
sp3 <- aov(weight ~ density + hybrid + density:hybrid + Error(block / mainplot), data=splitplot)


#--------------------------------------------------------------------------------------------------------#
# d) Interaction between density and hybrid means that hybrids react differently at different densities  #
#--------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------#
# e) Test for interaction between density and hybrid  #
#-----------------------------------------------------#

anova(sp1)

anova(sp2)
# Notice again that no P-values are given; for that the packages lmerTest and pbkrtest are needed; skip now

summary(sp3)
# Strangely, for an object returned by the aov function, the summary function gives the anova table...

#-----------------------------------------------------#
# f) Test for main effects of density and hybrid      #
#-----------------------------------------------------#

# Check the anova results given earlier for main effect results; we need not remove the interaction in this balanced design


#---------------------------------------------------------------------------#
# h) Produce the relevant tables of means and standard error of differences #
#---------------------------------------------------------------------------#

library(emmeans)
emmeans(sp1, pairwise ~ density, adjust="none") # if adjust= not specified, Tukey adjustment of P-values is used
emmeans(sp1, pairwise ~ hybrid, adjust="none")


emmeans(sp2, pairwise ~ density, adjust="none") # if adjust= not specified, Tukey adjustment of P-values is used
emmeans(sp2, pairwise ~ hybrid, adjust="none")

#-------------------------------#
# i) Hand calculation of sed's  #
#-------------------------------#

# We calculate the sed's based upon the variance component estimates from the lmer mixed model results:
vc <- as.data.frame(VarCorr(sp2))
(var.mainplot <- vc[1,4]) # variance component for mainplots
(var.e <- vc[3,4]) # variance component for subplots (error)

r <- 4  # 4 blocks
a <- 4  # 4 levels of main plot factor
b <- 3  # 3 levels of sub plot factor

(MS.mainplot <- b*var.mainplot + var.e)
(MS.error <- var.e)
# These mean squares can be obtained directly from the aov() output

(sed.density <- sqrt(2*MS.mainplot /(r * b)))
(sed.hybrid <- sqrt(2*MS.error /(r * a)))
# Compare values with sed's obtained by emmeans