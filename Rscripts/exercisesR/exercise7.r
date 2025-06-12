# Exercise 7: BIBD = Balanced Incomplete Block Design

# -------------------------------------------------------------------------
# Balanced incomplete block design, from Cochran, W.G. and Cox, G.M. (1957)
# "Experimental Design, 2nd ed.", Wiley and Sons, New York, p. 488         
# (c)1957 by John Wiley and Sons, Inc. This material is used by permission 
# of John Wiley and Sons, Inc.                                             
# Example follows SAS example in book by Schabenberger & Pierce                    
# ------------------------------------------------------------------------- 
  
# 13 hybrids are to be compared, but block size is only 4.

#----------------------------#
# a) Read data into program  #
#----------------------------#

block <- factor(rep(1:13, each=4))
hybrid <- factor(c(3,6,9,11, 3,4,8,12, 10,11,12,13, 2,5,8,11, 7,8,9,10, 4,5,6,10, 1,5,9,12, 3,5,7,13, 
                   1,2,3,10, 2,4,9,13, 1,4,7,11, 1,6,8,13, 2,6,7,12))
yield <- c(25.3,19.9,29.0,24.6, 23.0,19.8,33.3,22.7, 16.2,19.3,31.7,26.6, 27.3,27.0,35.6,17.4, 23.4,30.5,30.8,32.4, 30.6,32.4,27.2,32.8,
           34.7,31.1,25.7,30.5, 34.4,32.4,33.3,36.9, 38.2,32.9,37.3,31.3, 28.7,30.7,26.9,35.3, 36.6,31.1,31.1,28.4, 31.8,33.7,27.8,41.1,
           30.3,31.5,39.3,26.7) 
  
(bibd <- data.frame(block, hybrid, yield))

table(bibd$hybrid)

table(bibd$hybrid, bibd$block) # rows correspond to hybrids, columns to blocks 

#---------------------------------------#
# b) intra-block analysis: blocks fixed #
#-------------------------------------- #

bibd.fixed <- lm(yield ~ block + hybrid, data=bibd)
anova(bibd.fixed)

# Compare predicted means and ordinary means:

library(emmeans)
emmeans(bibd.fixed, ~ hybrid)  # predicted means from model

tapply(bibd$yield, bibd$hybrid, mean)  # ordinary means


# Pairwise comparison between hybrids, e.g. by

summary(bibd.fixed) # check sed, e.g. for coefficient for hybrid2 is pairwise different of hybrid 2 with reference hybrid 1

emmeans(bibd.fixed, pairwise ~ hybrid, adjust="none")  # pairwise comparisons between hybrids

# User-defined contrasts:
# e.g. compare hybrids 1 and 2, and 2 and 3; coefficients for fixed block effects cancel, so are zero
# Parametrisation R: ic, b2,b3,..,b13, h2,h3,..,h13; b1 and h1 are references and are cancelled
library(multcomp)
K  <- rbind("hybrid 1 vs 2" = c(0, 0,0,0,0,0,0,0,0,0,0,0,0,  -1,0,0,0,0,0,0,0,0,0,0,0), 
            "hybrid 2 vs 3" = c(0, 0,0,0,0,0,0,0,0,0,0,0,0,   1,-1,0,0,0,0,0,0,0,0,0,0))
lp <- glht(bibd.fixed, linfct=K)  # glht = general linear hypothesis
summary(lp)

#---------------------------------------------------#
# c) Inter- and intra block analysis: blocks random #
#---------------------------------------------------#

library(nlme)
bibd.random <- lme(fixed= yield ~ hybrid, random = ~ 1 | block, data=bibd)
anova(bibd.random)

emmeans(bibd.random, ~ hybrid)  # predicted means from model

# Pairwise comparison between hybrids, e.g. by

summary(bibd.random) # check sed, e.g. for hybrid2

emmeans(bibd.random, pairwise ~ hybrid, adjust="none")  # predicted means from model

# User-defined contrasts
# e.g. compare hybrid 1 and 2, and 2 and 3
# Parametrisation R: ic, h2,h3,..,h13; h1 is reference and cancels
K  <- rbind("hybrid 1 vs 2" = c(0, -1,0,0,0,0,0,0,0,0,0,0,0), "hybrid 2 vs 3" = c(0, 1,-1,0,0,0,0,0,0,0,0,0,0))
lp <- glht(bibd.random, linfct=K)
summary(lp)

