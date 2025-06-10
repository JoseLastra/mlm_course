# Exercise 9: Split-plot design without block factor

# Split-plot example on liver growth in mice, 
# taken from lecture notes "Variantiecomponenten" by L.R. Verdooren.
# Main plot factor: strain of mouse (4 levels); subplot factor: diet (3 levels).
# No block factor.
# From each strain 2 mother mice were taken: mother = main plot.
# From each mother 6 baby mice were taken: baby mouse = subplot.
# Three diets were randomized over 6 baby mice per mother.
# In total 2x4x6 = 48 observations. */


#------------------------------------------------------------------------------#
# b) Read data, fit mixed model, check out interaction between strain and diet #
#------------------------------------------------------------------------------#

# Make data available 

strain <- factor(rep(c("S1","S2","S3","S4"), each=12))
mother   <- factor(rep(rep(1:2, each=6), times=4))
diet     <- factor(rep(rep(c("D1","D2","D3"), each=2), times=8))

liverwgt <- c(3.44,3.48,3.38,3.19,3.56,3.73, 3.50,3.58,3.55,3.69,4.08,3.87, 2.30,2.46,2.32,2.28,2.61,2.66,
              3.02,3.23,2.75,2.83,3.06,3.32, 3.18,3.16,3.11,3.00,3.24,3.24, 3.17,3.20,3.02,2.76,3.16,3.29,
              2.69,2.91,2.37,2.31,2.84,2.66,2.70,2.44,2.28,2.31,2.72,2.70) 
  
(splitplot.mice <- data.frame(strain,mother,diet,liverwgt))


# using aov()
sp.mice1 <- aov(liverwgt ~ strain*diet + Error(mother:strain), data=splitplot.mice)
summary(sp.mice1)

# You could also make combination factor of strain and mother:
strainmother <- paste(strain,mother,sep=".")
sp.mice1 <- aov(liverwgt ~ strain*diet + Error(strainmother), data=splitplot.mice)
summary(sp.mice1)


# Fit appropriate mixed model, using lme4 
# Make sure that needed package(s) are loaded

library(lme4); library(lmerTest); library(pbkrtest)

sp.mice2 <- lmer(liverwgt ~ strain*diet  + (1 | mother:strain), data=splitplot.mice)
summary(sp.mice2)

anova(sp.mice2, ddf="Kenward-Roger")

# Significant interaction strain*diet ?

# Detach de packages lmerTest and pbkrtest
detach("package:lmerTest", unload=TRUE)
detach("package:pbkrtest", unload=TRUE)


#-----------------------------#
# c) Check out strain effects #
#-----------------------------#

# Look at earlier produced anova table or mixed model results

# For pairwise differences:
library(emmeans)
emmeans(sp.mice2, pairwise ~ strain, adjust="none") 


#---------------------------#
# d) Check out diet effects #
#---------------------------#

# Again, look at earlier produced anova table or mixed model results

emmeans(sp.mice2, pairwise ~ diet, adjust="none")


#------------------------------------------------------------------------------------#
# e) Is liver weight averaged over diets 1 and 3 significantly different from diet 2 #
#------------------------------------------------------------------------------------#

# Specify user-defined contrast, comparing average over diets 1 and 3 with diet 2
# Construct coefficient vector starting from 0.5 mu.1 - mu.2 + 0.5 mu.3, expand muij into mu+ai+bj+abij, and remove a1, b1, ab1j, abi1
library(multcomp)

K  <- rbind("diet 1+3 vs 2" = c(0, 0,0,0, -1,0.5, -0.25,-0.25,-0.25, 0.125,0.125,0.125))
lp <- glht(sp.mice2, linfct=K)  # glht = general linear hypothesis
summary(lp)




# Alternatively, using nlme package
library(nlme)

sp.mice3 <- lme(liverwgt ~ strain*diet, random = ~ 1 | strainmother, data=splitplot.mice)
anova(sp.mice3)

summary(sp.mice3)

K  <- rbind("diet 1+3 vs 2" = c(0, 0,0,0, -1,0.5,  -0.25,-0.25,-0.25, 0.125,0.125,0.125))
lp <- glht(sp.mice3, linfct=K)
summary(lp)

emmeans(sp.mice3, ~ diet)
