# Exercise 1: paired or independent t-test or both?

# cf slide 12, showing schematically which observations are lost.

# paired samples t-test for animals 1-6
animal <- 1:6
feedA <- c(31.85, 27.31, 31.09, 31.77, 34.62, 31.28)
feedB <- c(32.03, 26.84, 29.99, 31.47, 34.24, 31.02)

(paired <- data.frame(animal=animal, feedA=feedA, feedB=feedB))

t.test(paired$feedA, paired$feedB, mu=0, paired = TRUE)
# notice that the default is paired = FALSE, so we need to specify paired = TRUE
# 

# independent samples t-test for animals 7-12 
animal <- 7:12
feed <- c(rep("A",3), rep("B",3))
y <- c(36.00, 32.39, 34.35, 27.14, 29.14, 31.49)
(indep <- data.frame(animal=animal, feed=feed, y=y))
# Notice the "long" format, instead of the "wide" format in paired analysis

t.test(y ~ feed, data=indep, var.equal=TRUE)


# join the two and use a mixed model

# An alternative way of specifying data is needed: 
# "long" format as in independent samples part

animal <- c(rep(1:6, each=2), 7:12)
feed <- as.factor(c(rep(c("A","B"), 6), rep("A",3), rep("B",3)))
y <- c(as.vector(t(cbind(feedA,feedB))), y)

# imbalanced data, so we need to use a mixed model
(all <- data.frame(animal=animal, feed=feed, y=y))

# use package lme4

library(lme4)

(mm1.1 <- lmer(y ~ feed + (1 | animal), data=all)) # random effect per animal (1 | animal)
summary(mm1.1)          # summary() of mixed model object does not provide p-values
