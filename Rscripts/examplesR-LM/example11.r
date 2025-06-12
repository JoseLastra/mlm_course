# Multiple linear regression

# setwd("M:/LM/data")  # Change to whatever path you use yourself

my <- read.csv("milkyieldMcGill.csv")    # This reads in a comma delimited file
my <- data.frame(my, milky=my$milkyield/1000) # Express milkyield in kiloliters
my

FM <- lm(milky ~ intake + density, data=my)

summary(FM)


anova(FM)

FM <- lm(milky ~ intake + density, data=my)
RM <- lm(milky ~ 1, data=my)
anova(RM, FM)

