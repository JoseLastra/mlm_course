# Oneway ANOVA lime GL

# a Read data
setwd("M:/LM/data")  # Change to whatever path you use yourself
limedata <- read.table("day_1/data/lime.prn", header=T)    # This reads in a raw ASCII datafile; column names are given on first line of datafile

limeGL <- limedata[limedata$lime=="GL",]
limeGL$ratef <- as.factor(limeGL$rate)  # Treat rate as factor = qualitative regressor
limeGL

# b One-way ANOVA
limeGL.oneway <- lm(pH ~ ratef, data=limeGL)
anova(limeGL.oneway) # anova show differences in means between groups
summary(limeGL.oneway) #std error is the same because we assume equal variances in groups 

# c Check assumptions
par(mfrow=c(2,2))

plot(limeGL.oneway)

par(mfrow=c(1,1))

# if needed, install the package: install.packages("car")
library(car)
leveneTest(limeGL$pH ~ limeGL$ratef)  # Levene's test for equality of variances 
# 

# d Calculate means and emmeans

tapply(limeGL$pH, limeGL$ratef, mean) # ordinary means in groups

# or function from doBy package
library(doBy)
summaryBy(pH ~ ratef, data=limeGL, FUN=c(mean))

# basic idea of estimated marginal means: first make a prediction dataset, next get predictions
pdata <- data.frame(ratef=levels(limeGL$ratef))
(emmeans <- predict(limeGL.oneway, newdata = pdata, se = TRUE))

# function emmeans from emmeans package can calculate predicted means based on fitted model
library(emmeans)
emmeans(limeGL.oneway, ~ ratef)


# e Pairwise comparisons using post-hoc tests, using emmeans() function of emmeans package;
# The emmeans() function uses the lm-object obtained earlier as argument.

emmeans(limeGL.oneway, pairwise ~ ratef, adjust="none")
emmeans(limeGL.oneway, pairwise ~ ratef, adjust="bonferroni")
emmeans(limeGL.oneway, pairwise ~ ratef, adjust="tukey")


# older way to get Tukey results
limeGL.aov <- aov(pH ~ ratef, data=limeGL)
round(TukeyHSD(limeGL.aov)$ratef, 3)
plot(TukeyHSD(limeGL.aov, "ratef" ))

# yet another way
pairwise.t.test(limeGL$pH, limeGL$ratef, p.adj = "none")
pairwise.t.test(limeGL$pH, limeGL$ratef, p.adj = "bonf")


# more possibilities using the multcomp package; skip if time is limited;

library(multcomp)
hsd <- glht(limeGL.oneway, linfct = mcp(ratef="Tukey")) 
 # glht=general linear hypothesis; linfct=linear function; mcp = multiple (pairwise) comparisons
confint(hsd)
summary(hsd)
cld(hsd)    # compact letter display

# Alternative way of getting estimates of differences: specify contrasts 
contr <- rbind("1-2" = c(1,-1,0,0,0),
               "1-3" = c(1,0,-1,0,0),
               "1-4" = c(1,0,0,-1,0),
               "1-5" = c(1,0,0,0,-1),
               "2-3" = c(0,1,-1,0,0),
               "2-4" = c(0,1,0,-1,0),
               "2-5" = c(0,1,0,0,-1),
               "3-4" = c(0,0,1,-1,0),
               "3-5" = c(0,0,1,0,-1),
               "4-5" = c(0,0,0,1,-1))
lsd <- glht(limeGL.oneway, linfct = mcp(ratef= contr))
summary(lsd)

