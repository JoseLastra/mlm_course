# Exercise 11: Random coefficient model for prey fish swallowing

# Read data
setwd("E:/course2015_MLM/data") # change to proper working directory, if needed
acutirostris <- read.table("exercise11.prn", header=TRUE)
head(acutirostris)

acutirostris$y <- log(acutirostris$HandTime);  # We analyze the log of handling time;
acutirostris$PredNr <- factor(acutirostris$PredNr)
acutirostris$Prey <- factor(acutirostris$Prey)

orderi <- with(acutirostris, order(Prey, PredNr, RelPreyLgt))
acutirostris <- acutirostris[orderi,]
head(acutirostris)

plot(y ~ RelPreyLgt, col=Prey, data=acutirostris)

# First fit ordinary linear model with a regression line for each predator fish.
lmo <- lm(y ~ -1 + PredNr + RelPreyLgt:PredNr, data=acutirostris)
ic <- coef(lmo)[1:23]
slope <- coef(lmo)[24:46]
prey <- rep(levels(acutirostris$Prey), times=c(5,6,6,6))

rc <- data.frame(ic=ic,slope=slope,prey=prey)

# Plot this bundle of lines
library(ggplot2)
scatterplot <- qplot(x=RelPreyLgt, y=y, data=acutirostris)
scatterplot + geom_abline(aes(intercept=ic, slope=slope, colour=prey), data=rc)

# Random coefficient model with random intercepts and slopes
library(nlme)
lme.fish1 <- lme(y ~ Prey + RelPreyLgt + Prey:RelPreyLgt, random= ~ 1 + RelPreyLgt | PredNr, data=acutirostris)
summary(lme.fish1)
logLik(lme.fish1)
AIC(lme.fish1)

# Random intercepts only
lme.fish2 <- lme(y ~ Prey + RelPreyLgt + Prey*RelPreyLgt, random= ~ 1 | PredNr, data=acutirostris)
summary(lme.fish2)
logLik(lme.fish2)
AIC(lme.fish2)

anova(lme.fish1, lme.fish2)

# Ordinary linear model, assuming independent data
lm.fish3 <- lm(y ~ Prey + RelPreyLgt + Prey*RelPreyLgt, data=acutirostris)
logLik(lm.fish3, REML=TRUE)
(AIC3 <- -2*logLik(lm.fish3, REML=TRUE)+2*attributes(logLik(lm.fish3))$df) # takes some effort to get right AIC...

anova(lme.fish1, lme.fish2, lm.fish3)

# Random slopes and intercepts are not needed, it seems.


# Make plot of random coefficient model
lme.fish4 <- lme(y ~ -1 + Prey + Prey:RelPreyLgt, random= ~ 1 + RelPreyLgt | PredNr, data=acutirostris)
rc.mat <- coef(lme.fish4)
head(rc.mat)

ic.mat <- rc.mat[, 1:4]+rc.mat[,9]
slope.mat <- rc.mat[, 5:8]+rc.mat[,10]

ic.lme <- c(ic.mat[1:5,1], ic.mat[6:11,2], ic.mat[12:17,3], ic.mat[18:23,4])
slope.lme <- c(slope.mat[1:5,1], slope.mat[6:11,2], slope.mat[12:17,3], slope.mat[18:23,4])
rc.lme <- data.frame(ic.lme=ic.lme, slope.lme=slope.lme, prey=prey)

scatterplot <- qplot(x=RelPreyLgt, y=y, data=acutirostris)
scatterplot + geom_abline(aes(intercept=ic.lme, slope=slope.lme, colour=prey), data=rc.lme)


# Final model is just linear model with separate regression lines for four prey fish species.
lm.b <- coef(lm.fish3)
head(lm.b)

ic.lm <- c(lm.b[1], lm.b[1]+lm.b[2], lm.b[1]+lm.b[3],lm.b[1]+lm.b[4])
slope.lm <- c(lm.b[5], lm.b[5]+lm.b[6], lm.b[5]+lm.b[7],lm.b[5]+lm.b[8])
rc.lm <- data.frame(ic.lm=ic.lm, slope.lm=slope.lm, prey=c("Garra","Humilis","Niloticu","Tanapela"))

scatterplot <- qplot(x=RelPreyLgt, y=y, data=acutirostris)
scatterplot + geom_abline(aes(intercept=ic.lm, slope=slope.lm, colour=prey), data=rc.lm)


