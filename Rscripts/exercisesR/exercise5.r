# Exercise 5: Subsampling

# Example from Schabenberger and Pierce  7.6.3
# Mint plant data from Steel, Torrie and Dickey (1997  p. 159)

# a Read data into R 

night <- factor(rep(c("Low","High"), each=36))
hour <- factor(rep(rep(c(8,12,16), each=12), 2))
pot <- factor(rep(rep(c(1:3), each=4), 6))
growth <- c(3.5, 4.0, 3.0, 4.5, 2.5, 4.5, 5.5, 5.0, 3.0, 3.0, 2.5, 3.0, 
            5.0, 5.5, 4.0, 3.5, 3.5, 3.5, 3.0, 4.0, 4.5, 4.0, 4.0, 5.0, 
            5.0, 4.5, 5.0, 4.5, 5.5, 6.0, 5.0, 5.0, 5.5, 4.5, 6.5, 5.5,
            8.5, 6.0, 9.0, 8.5, 6.5, 7.0, 8.0, 6.5, 7.0, 7.0, 7.0, 7.0,
            6.0, 5.5, 3.5, 7.0, 6.0, 8.5, 4.5, 7.5, 6.5, 6.5, 8.5, 7.5,
            7.0, 9.0, 8.5, 8.5, 6.0, 7.0, 7.0, 7.0,11.0, 7.0, 9.0, 8.0)

(mintstems <- data.frame(night, hour, pot, growth))

# b Fixed effects model using lm()

lm4 <- lm(growth ~ hour + night + hour:night + pot %in% (hour*night), data=mintstems)
anova(lm4)
summary(lm4)
# Standard errors and tests for treatment effects are wrong!

# c use aov()
aov4 <- aov(growth ~ hour + night + hour:night + Error(pot:hour:night), data=mintstems)
summary(aov4)

# d Take mean square of pots out of anova table of lm(), and construct F-statistic "by hand".
# Use mean square of pots as denominator of F-statistics for hour, night and hour*night.

(MS.hour       <- anova(lm4)[1,3])
(MS.night      <- anova(lm4)[2,3])
(MS.hour.night <- anova(lm4)[3,3])
(MS.pot        <- anova(lm4)[4,3])
(F.hour        <- MS.hour/MS.pot)
(F.night       <- MS.night/MS.pot)
(F.hour.night  <- MS.hour.night/MS.pot)
(P.hour        <- 1-pf(F.hour,2,12))
(P.night       <- 1-pf(F.night,1,12))
(P.hour.night  <- 1-pf(F.hour.night,2,12))


# e Use lme4 package and add-on packages for testing
library(lme4)
library(lmerTest)
library(pbkrtest)

mm4 <- lmer(growth ~ hour + night + hour:night + (1 | pot:hour:night), data=mintstems)
summary(mm4)
anova(mm4, ddf="Kenward-Roger")

# Unload the add-on packages for testing
detach("package:lmerTest", unload=TRUE)
detach("package:pbkrtest", unload=TRUE)

# Try function lme() yourself

#hour2 <- factor(paste(mintstems$night,mintstems$hour, mintstems$pot))
#mm5 <- lme(fixed=growth ~ hour + night + hour:night, random = ~1 | hour2, data=mintstems)
#anova(mm5)

# f Simpler approach: calculate means per pot; next use means as response variable; now simple lm() can be used!

library(doBy)
mintmeans <- summaryBy(growth ~ hour + night + pot, fun.names="mean", data=mintstems)

lmom <- lm(growth.mean ~ hour + night + hour:night, data=mintmeans)
anova(lmom)

