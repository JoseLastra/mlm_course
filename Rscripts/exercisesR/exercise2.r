# Exercise 2: Split-plot design

# Example about feed ration and gender
# Main plots are animals.
# Main plot factor is gender.
# Subplot factor is feed ration: per animal two feed ratios.
# In total 24 observations.

# Read data into R

Animal      <- factor(rep(1:12,2))
Sex         <- factor(c(1,2,2,1,1,2,1,2,1,2,2,1,1,2,2,1,1,2,1,2,1,2,2,1))
levels(Sex) <- factor(c("M","F"))
Feed        <- factor(c(rep("A",12), rep("B",12)))

y <- c(31.85,27.31,31.09,31.77,34.62,31.28,36.00,32.39,34.35,27.56,29.05,31.98,
       32.03,26.84,29.99,31.47,34.24,31.02,35.50,32.24,34.48,27.14,29.14,31.49)
(splitplot <- data.frame(Animal, Sex, Feed, y))

# Correct split-plot analysis:
# As this is a balanced design, we may use function aov.
# Fit the two-way ANOVA model with random animal effects:
sp <- aov(y ~ Sex + Feed + Sex:Feed + Error(Animal), data=splitplot)
# ANOVA table (strangely enough obtained with function summary() for objects from aov() function)
summary(sp)
# Check that you have the same results as found on slide 57.

# Wrong analysis I: ignore main plots completely

lmo1 <- lm(y ~ Sex + Feed + Sex:Feed, data=splitplot)
anova(lmo1)
# Compare ANOVA table with correct ANOVA table.
# Which tests are wrong? Why are they wrong?
# Notice that the F-tests for interaction and for Feed are correct.


# Wrong analysis II: introduce main plots with fixed effects

lmo2 <- lm(y ~ Animal + Sex + Feed + Sex:Feed, data=splitplot)
lmo2 <- lm(y ~ Sex + Animal + Feed + Sex:Feed, data=splitplot)
anova(lmo2) # recall: anova() produced sequential (type I) sums of squares
# Compare ANOVA table with correct ANOVA table.
# Why is there no F-test for main effect of Sex?
# Can you understand why F-tests for Feed and Sex:Feed are correct?
# Sex and animal are co-founded so you miss the sex main effect 
# and the interaction with animal is not tested.


# Wrong analysis III: introduce main plots as nested within animals

lmo3 <- lm(y ~ Animal %in% Sex + Sex + Feed + Sex:Feed, data=splitplot) #animal nested within sex = Animal %in% Sex 
anova(lmo3)
# Now we do have an F-test for the main effect of Sex.
# But why is it wrong?
#lm does nod identify structure and calc F statistics against geral mse
# The F-tests for interaction and for Feed are correct.



# Different accuracies in split-plot analysis: standard errors of differences
library(emmeans)

emmeans(sp, pairwise ~ Sex, adjust="none")  # sed of sexes: 1.13

emmeans(sp, pairwise ~ Feed, adjust="none")  # sed of feed: 0.103

emmeans(sp, pairwise ~ Sex:Feed, adjust="none")  # sed of combinations of sex and feed; two different sed's:
 # sed is 1.13 for differences among two sexes (either with same or different feeds)
 # sed is 0.146 for differences among feed within the same sex!
# differences in SDE is given by the comparison lowr values shows whithin individual comparison, while larger values show indididual + sex differences


